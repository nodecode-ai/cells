;;;; engine.lisp --- the OptMem store: an append-only log and a decay tree.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The on-disk format is the original's, unchanged: LOG.txt holds one
;;;; 320-byte record per memory, "#<id> <date> <text>" padded with spaces;
;;;; TREE/<size> holds the summaries of the blocks of that size, 288 bytes
;;;; each, in block order. A memory or a summary is found by seeking to its
;;;; offset, so there is no index file to keep in step with the log.
;;;;
;;;; The maths is the original's too: COVER tiles the log with aligned
;;;; power-of-two blocks, keeping a block whole only while its size is at
;;;; most ALPHA times its age, so detail decays with age. A NAP writes one
;;;; block's compression; a block can be built only when both its halves are,
;;;; which is why the naps fall out smallest-first.

(defpackage #:nodecode-optmem
  (:use #:cl)
  (:export #:optmem-error #:optmem-error-detail))

(in-package #:nodecode-optmem)

(nlk:define-peripheral optmem :not-running t :lock "optmem")

;;; --- the store's fixed sizes ------------------------------------------------

(defparameter +log-record+ 320
  "Bytes one LOG.txt record takes, the width the upstream `memo' tool writes.")
(defparameter +tree-record+ 288
  "Bytes one TREE/<size> summary takes.")
(defparameter +entry-chars+ 280
  "The longest one memory line, or one summary, in UTF-8 bytes.")
(defparameter +raw-max+ 16
  "Blocks up to this many memories compress straight from the raw log.")
(defparameter +wake-lines+ 96
  "The wake digest's default reading budget, in lines.")
(defparameter +part-chars+ 20000
  "The most one regex recall prints, in bytes.")

(defparameter +knobs+
  '(("WAKE_LINES" . 96) ("ENTRY_CHARS" . 280) ("RAW_MAX" . 16)
    ("PART_CHARS" . 20000) ("PART_LINES" . 500))
  "The store `config' file's knobs and their defaults, as `memo' has them.")

(defparameter +settable-knobs+ '("WAKE_LINES" "PART_CHARS")
  "Only these two reach the engine at run time. The rest are `memo'-parity
display values with no runtime effect, so a change to one is refused rather
than persisted as a silent no-op.")

(defparameter +knob-docs+
  '(("WAKE_LINES" . "memories printed by wake")
    ("ENTRY_CHARS" . "longest one memory line, in bytes")
    ("RAW_MAX" . "blocks up to this many memories compress raw")
    ("PART_CHARS" . "output paging: largest part, in bytes")
    ("PART_LINES" . "output paging: largest part, in lines")))

(defparameter +accents+
  (concatenate 'string
               "àáâãäåçèéêëìíîïñòóôõöùúûüýÿ")
  "Latin-1 letters folded to ASCII by FOLD, so `caçula' finds `cacula'.")
(defparameter +plain+
  "aaaaaaceeeeiiiinooooouuuuyy"
  "FOLD's answer, letter for letter with +ACCENTS+.")

;;; --- where the store is -----------------------------------------------------

(defun store-directory ()
  "The store this cell runs on: the section's `store', else <home>/optmem/."
  (let ((given (nlk:trimmed (or (setting :store) ""))))
    (if (plusp (length given))
        (uiop:ensure-directory-pathname (nlk:expand-home given))
        (nlk:home "optmem/"))))

(defun store-path (name) (merge-pathnames name (store-directory)))
(defun log-path () (store-path "LOG.txt"))
(defun config-path () (store-path "config"))
(defun tree-path (size) (merge-pathnames (format nil "~d" size) (store-path "TREE/")))

(defun ensure-store ()
  "Create the store's folders and its two files when they are absent. The
store is created deliberately, as `memo init' does; nothing is overwritten."
  (let ((directory (store-directory)))
    (ensure-directories-exist (store-path "TREE/"))
    (unless (probe-file (log-path))
      (with-open-file (out (log-path) :direction :output :if-exists :append
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
        (values)))
    (unless (probe-file (config-path))
      (write-config '()))
    directory))

;;; --- bytes in, bytes out ------------------------------------------------------

(defun file-size (path)
  "PATH's length in bytes, or 0 when it is absent."
  (or (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (and in (file-length in)))
      0))

(defun slurp-octets (path)
  "PATH's bytes, or an empty vector when it is absent or unreadable."
  (handler-case
      (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (if (null in)
            (make-array 0 :element-type '(unsigned-byte 8))
            (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
              (read-sequence octets in)
              octets)))
    (error () (make-array 0 :element-type '(unsigned-byte 8)))))

(defun write-record (path text record-size)
  "TEXT as one fixed-width record appended to PATH, padded with spaces and
closed with a newline, exactly as the upstream `memo' tool lays it down."
  (let ((bytes (nlk:utf-8-octets text)))
    (when (> (length bytes) (1- record-size))
      (fail "entry too long: ~d bytes, limit ~d" (length bytes) (1- record-size)))
    (let ((record (make-array record-size :element-type '(unsigned-byte 8)
                                           :initial-element 32)))
      (replace record bytes)
      (setf (aref record (1- record-size)) 10)
      (with-open-file (out path :direction :output :if-exists :append
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
        (write-sequence record out))))
  t)

(defun truncate-file (path octets)
  "PATH cut to its first OCTETS bytes, atomically, or left alone when shorter."
  (let ((whole (slurp-octets path)))
    (when (> (length whole) octets)
      (let ((temporary (merge-pathnames (format nil ".~a.tmp" (file-namestring path))
                                         (uiop:pathname-directory-pathname path))))
        (with-open-file (out temporary :direction :output :if-exists :supersede
                                       :if-does-not-exist :create
                                       :element-type '(unsigned-byte 8))
          (write-sequence whole out :end octets))
        (nlk:replace-file temporary path)))))

(defun repair-file (path record-size)
  "Drop a torn trailing record, as the original does before it appends."
  (let ((size (file-size path)))
    (when (plusp (mod size record-size))
      (truncate-file path (* (floor size record-size) record-size)))))

;;; --- the log ---------------------------------------------------------------

(defun parse-record (text)
  "TEXT — one record — as (ID DATE MEMORY), or NIL when it is not one."
  (let ((text (string-right-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (when (and (>= (length text) 3) (char= (char text 0) #\#))
      (let ((first-space (position #\Space text))
            (id-end nil) (date-end nil))
        (when (and first-space (> first-space 1)
                   (every #'digit-char-p (subseq text 1 first-space)))
          (setf id-end first-space
                date-end (position #\Space text :start (1+ first-space)))
          (when (and date-end (> date-end (1+ id-end)))
            (list (parse-integer text :start 1 :end id-end)
                  (subseq text (1+ id-end) date-end)
                  (subseq text (1+ date-end)))))))))

(defun log-records ()
  "Every record of the log as a vector of (ID DATE MEMORY), an unreadable
record reading as NIL: a record's id is its index, which is what the decay
tree's block ids count in."
  (let* ((octets (slurp-octets (log-path)))
         (count (floor (length octets) +log-record+))
         (records (make-array count :initial-element nil)))
    (loop for index below count
          do (setf (aref records index)
                   (parse-record (nlk:decode-octets
                                  (subseq octets (* index +log-record+)
                                          (* (1+ index) +log-record+))))))
    records))

(defun log-slice (records lo hi)
  "RECORDS LO..HI-1 that parsed, oldest first."
  (loop for index from (max 0 lo) below (min hi (length records))
        for record = (aref records index)
        when record collect record))

(defun append-memory (text &optional (date (today-iso)))
  "TEXT as one memory line appended to the log. => its id, the record's index."
  (let ((text (nlk:trimmed text)))
    (when (zerop (length text)) (fail "empty memory"))
    (when (find-if (lambda (character) (member character '(#\Newline #\Return))) text)
      (fail "a memory is one line"))
    (let ((bytes (nlk:utf-8-size text)))
      (when (> bytes +entry-chars+)
        (fail "too long: ~d bytes, limit ~d" bytes +entry-chars+)))
    (let ((records (log-records)))
      (repair-file (log-path) +log-record+)
      (let ((id (length records)))
        (write-record (log-path) (format nil "#~a ~a ~a" id date text) +log-record+)
        id))))

(defun today-iso ()
  "Today's date, the YYYY-MM-DD a memory line is stamped with."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

;;; --- the decay tree ----------------------------------------------------------

(defun tree-count (size)
  "How many summaries TREE/<size> holds."
  (floor (file-size (tree-path size)) +tree-record+))

(defun tree-get (lo hi)
  "Block LO..HI's summary, or NIL when it has none."
  (let* ((size (- hi lo))
         (index (floor lo size))
         (octets (slurp-octets (tree-path size))))
    (when (and (plusp size) (<= (* (1+ index) +tree-record+) (length octets)))
      (let ((text (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                                      (nlk:decode-octets
                                       (subseq octets (* index +tree-record+)
                                               (* (1+ index) +tree-record+))))))
        (and (plusp (length text)) text)))))

(defun tree-write (lo hi summary)
  "SUMMARY as block LO..HI's record, when its slot is the next one. => T
when written, NIL when the file moved under us."
  (let* ((size (- hi lo))
         (path (tree-path size)))
    (repair-file path +tree-record+)
    (when (= (tree-count size) (floor lo size))
      (write-record path summary +tree-record+)
      t)))

(defun block-problem (lo hi)
  "Why LO..HI is not a block id — an aligned power-of-two range, HI
EXCLUSIVE, which is how the wake digest prints one — or NIL when it is."
  (let ((size (- hi lo)))
    (cond ((or (not (integerp lo)) (not (integerp hi)) (not (plusp size)))
           (format nil "~a-~a is not a block: ids are integers and hi is exclusive" lo hi))
          ((or (< size 2) (plusp (logand size (1- size))) (plusp (mod lo size)) (minusp lo))
           (format nil "~a-~a is not a block. Copy the id printed by optmem_wake, like 16-31."
                   lo (1- hi)))
          (t nil))))

(defun pending-naps (records &optional limit)
  "The blocks that can be built and have not been, smallest first."
  (let ((total (length records))
        (todo '()))
    (loop for size = 2 then (* size 2)
          while (<= size total)
          do (loop with have = (tree-count size)
                   for index from have below (floor total size)
                   do (push (list (* index size) (* (1+ index) size)) todo)))
    (setf todo (nreverse todo))
    (if (and limit (< limit (length todo))) (subseq todo 0 limit) todo)))

(defun tile (total alpha)
  "TOTAL records tiled with aligned power-of-two blocks, a block kept whole
iff its size is at most ALPHA times its age. A bigger ALPHA reads coarser."
  (let ((root 1))
    (loop while (< root total) do (setf root (* root 2)))
    (let ((stack (list (list 0 root)))
          (out '()))
      (loop while stack
            do (destructuring-bind (lo hi) (pop stack)
                 (let ((size (- hi lo)))
                   (cond ((>= lo total))
                         ((and (> size 1) (or (> hi total) (> size (* alpha (- total lo)))))
                          (let ((mid (floor (+ lo hi) 2)))
                            (push (list mid hi) stack)
                            (push (list lo mid) stack)))
                         (t (push (list lo hi) out))))))
      (sort out #'< :key #'first))))

(defun splittable-index (blocks)
  "The last block that can open in two, or NIL — the original's choice, and
it is the newest end that gains detail first."
  (let ((found nil))
    (loop for block in blocks
          for index from 0
          when (> (- (second block) (first block)) 1) do (setf found index))
    found))

(defun cover (total budget)
  "The blocks the wake digest prints: at most BUDGET of them, finest at the
newest end. Detail decays with age; when everything fits, nothing is
compressed at all."
  (cond ((<= total 0) '())
        ((>= budget total) (loop for index below total collect (list index (1+ index))))
        (t (let ((low 0.0) (high 1.0))
             (loop repeat 60
                   do (let ((mid (/ (+ low high) 2.0)))
                        (if (> (length (tile total mid)) budget)
                            (setf low mid)
                            (setf high mid))))
             (let ((out (tile total high)))
               (loop for index = (splittable-index out)
                     while (and index (< (length out) budget))
                     do (let* ((block (nth index out))
                               (mid (floor (+ (first block) (second block)) 2)))
                          (setf out (append (subseq out 0 index)
                                            (list (list (first block) mid) (list mid (second block)))
                                            (subseq out (1+ index))))))
               out)))))

(defun nap-prompt (records lo hi)
  "The compression instruction for block LO..HI, as the original words it:
what to keep, how long the answer may be, and the memories themselves."
  (let* ((remaining (1- (length (pending-naps records))))
         (body (if (<= (- hi lo) +raw-max+)
                   (format nil "~{~a~^~%~}"
                           (loop for (id date memory) in (log-slice records lo hi)
                                 collect (format nil "  #~a ~a ~a" id date memory)))
                   (let ((mid (floor (+ lo hi) 2)))
                     (format nil "~{~a~^~%~}"
                             (loop for (from to) in (list (list lo mid) (list mid hi))
                                   collect (format nil "  #~a-~a ~a" from (1- to)
                                                   (or (tree-get from to) "(missing — rebuild)"))))))))
    (format nil "Compress memories #~a-~a into one line of at most ~a UTF-8 bytes (not characters).~%~
                 Aim well below the limit. After a size error, rewrite substantially shorter; do not retry with only small edits.~%~
                 Keep what has lasting effect, drop what does not. Invent nothing.~%~%~a~@[~%~d compressions remain~]~%"
            lo (1- hi) +entry-chars+ body (and (plusp remaining) remaining))))

(defun wake (records budget)
  "The wake digest, with completeness metadata: recent memories verbatim,
older ones as their summaries. A block in the cover whose summary is missing
means the digest cannot be written completely, so it is reported — with the
next nap's prompt — rather than papered over with a placeholder."
  (let ((total (length records)))
    (if (zerop total)
        (list :lines '() :complete t :missing '() :pending '() :nap nil :rebuild '() :budget budget)
        (let ((lines '()) (missing '()))
          (dolist (block (cover total budget))
            (destructuring-bind (lo hi) block
              (if (= hi (1+ lo))
                  (let ((record (first (log-slice records lo hi))))
                    (when record
                      (push (format nil "#~a ~a ~a" (first record) (second record) (third record))
                            lines)))
                  (let ((summary (tree-get lo hi)))
                    (if summary
                        (push (format nil "#~a-~a ~a" lo (1- hi) summary) lines)
                        (push (list lo hi) missing))))))
          (let* ((missing (nreverse missing))
                 (pending (pending-naps records))
                 (nap (when pending
                        (nap-prompt records (first (first pending)) (second (first pending)))))
                 (rebuild (remove-if (lambda (block)
                                       (member block pending :test #'equal))
                                     missing)))
            (list :lines (nreverse lines)
                  :complete (null missing)
                  :missing missing
                  :pending pending
                  :nap nap
                  :rebuild rebuild
                  :budget budget))))))

(defun apply-nap (lo hi summary)
  "Store SUMMARY as block LO..HI's compression. => \"compressed\",
\"already-settled\", or \"nothing-pending\"; a summary that cannot be stored
signals. Every check runs under the store's lock, so a parallel writer cannot
slip a block in between the check and the append."
  (let ((summary (nlk:trimmed summary)))
    (when (zerop (length summary)) (fail "empty summary"))
    (let ((bytes (nlk:utf-8-size summary)))
      (when (> bytes +entry-chars+)
        (fail "summary too long: ~d UTF-8 bytes, max ~d bytes; reduce by at least ~d bytes. Rewrite substantially shorter; do not truncate important facts."
              bytes +entry-chars+ (- bytes +entry-chars+))))
    (let ((problem (block-problem lo hi)))
      (when problem (fail "~a" problem)))
    (with-optmem-lock
      (let* ((records (log-records))
             (total (length records)))
        (when (> hi total)
          (fail "block ~a-~a is beyond the log: it holds ~d memories, so this range exceeds what exists."
                lo (1- hi) total))
        (cond ((tree-get lo hi) "already-settled")
              (t (let ((pending (pending-naps records 1)))
                   (cond ((null pending) "nothing-pending")
                         ((not (equal (first pending) (list lo hi)))
                          (fail "wrong block ~a-~a: blocks are built in order; the next is ~a-~a."
                                lo (1- hi) (first (first pending)) (1- (second (first pending)))))
                         ((tree-write lo hi summary) "compressed")
                         (t "race")))))))))

(defun forget (lo hi)
  "Drop block LO..HI and every block built on it; the raw log is untouched.
=> the blocks actually dropped, so a caller can tell a false success from a
real one."
  (let ((problem (block-problem lo hi)))
    (when problem (fail "~a" problem)))
  (with-optmem-lock
    (let ((total (length (log-records)))
          (gone '()))
      (loop for size = (- hi lo) then (* size 2)
            while (<= size total)
            do (let ((kept (floor lo size))
                     (have (tree-count size)))
                 (when (> have kept)
                   (loop for index from kept below have
                         do (push (list (* index size) (* (1+ index) size)) gone))
                   (truncate-file (tree-path size) (* kept +tree-record+)))))
      (nreverse gone))))

(defun zoom (lo hi)
  "Block LO..HI opened into its two halves: a one-memory half verbatim, a
larger one as its summary, or \"not compressed yet\"."
  (let ((problem (block-problem lo hi)))
    (when problem (fail "~a" problem)))
  (let* ((records (log-records))
         (total (length records)))
    (when (>= lo total)
      (fail "#~a-~a is beyond the memory: it holds ~d memories. Run optmem_wake."
            lo (1- hi) total))
    (let ((mid (floor (+ lo hi) 2))
          (halves '()))
      (dolist (half (list (list lo mid) (list mid hi)))
        (destructuring-bind (from to) half
          (when (< from total)
            (if (= to (1+ from))
                (let ((record (first (log-slice records from to))))
                  (when record
                    (push (format nil "#~a ~a ~a" (first record) (second record) (third record))
                          halves)))
                (push (format nil "#~a-~a ~a" from (1- to)
                              (or (tree-get from to) "not compressed yet"))
                      halves)))))
      (nreverse halves))))

;;; --- the store's `config' file ------------------------------------------------

(defun lines-of (text)
  (uiop:split-string (or text "") :separator '(#\Newline #\Return)))

(defun parse-knob (line)
  "LINE — `NAME = VALUE # what it does' — as (NAME . VALUE), or NIL."
  (let* ((line (subseq line 0 (or (position #\# line) (length line))))
         (line (string-trim '(#\Space #\Tab) line))
         (equals (position #\= line)))
    (when (and (plusp (length line)) equals)
      (let ((name (string-trim '(#\Space #\Tab) (subseq line 0 equals)))
            (value (ignore-errors (parse-integer line :start (1+ equals) :junk-allowed t))))
        (when (and value (assoc name +knobs+ :test #'string=))
          (cons name value))))))

(defun read-config ()
  "The store's `config' overrides, as an alist of (NAME . INTEGER)."
  (let ((text (or (nlk:read-text (config-path)) "")))
    (remove nil (mapcar #'parse-knob (lines-of text)))))

(defun knob (name)
  "NAME's value for this store: its `config' override, else the default."
  (or (cdr (assoc name (ignore-errors (read-config)) :test #'string=))
      (cdr (assoc name +knobs+ :test #'string=))))

(defun pad (text width)
  (concatenate 'string text (make-string (max 0 (- width (length text))) :initial-element #\Space)))

(defun write-config (over)
  "Write the store's `config', the way `memo config' lays it out: a
commented line means the default, an uncommented one an override."
  (let ((text (with-output-to-string (out)
                (format out "# OptMem sizes for this memory. A commented line means: follow the~%# tool's default. Edit with `optmem_config NAME=VALUE`.~%~%")
                (dolist (knob +knobs+)
                  (let* ((name (car knob))
                         (given (assoc name over :test #'string=)))
                    (format out "~a~a = ~a # ~a~%"
                            (if given "" "# ") (pad name 12)
                            (pad (princ-to-string (if given (cdr given) (cdr knob))) 6)
                            (cdr (assoc name +knob-docs+ :test #'string=))))))))
    (ensure-directories-exist (config-path))
    (nlk:with-output-file (out (config-path)) (write-string text out))
    over))

(defun wake-budget ()
  "The reading budget this store's wake digest prints: the store's own
WAKE_LINES override, else the lighter of the section's wake_lines and the
engine default."
  (let ((override (ignore-errors (cdr (assoc "WAKE_LINES" (read-config) :test #'string=))))
        (given (ignore-errors (setting :wake-lines))))
    (cond ((and (integerp override) (plusp override)) override)
          ((and (integerp given) (plusp given)) given)
          (t +wake-lines+))))

(defun part-chars ()
  (let ((limit (knob "PART_CHARS")))
    (if (and (integerp limit) (plusp limit)) limit +part-chars+)))

;;; --- recall -------------------------------------------------------------------

(defun fold (text)
  "TEXT lowercased with Latin-1 accents folded."
  (let ((down (string-downcase text)))
    (coerce (loop for character across down
                  for at = (position character +accents+)
                  collect (if at (char +plain+ at) character))
            'string)))

(defun tokenize (text)
  "TEXT's words, accented letters folded, everything else a separator."
  (let ((folded (fold text))
        (tokens '())
        (start nil))
    (loop for index from 0 below (length folded)
          for character = (char folded index)
          do (if (or (alpha-char-p character) (digit-char-p character))
                 (unless start (setf start index))
                 (when start
                   (push (subseq folded start index) tokens)
                   (setf start nil))))
    (when start (push (subseq folded start) tokens))
    (nreverse tokens)))

(defparameter +regex-hints+ ".*+?[](){}|^$\\"
  "Characters that mean the caller is speaking `memo''s language, a pattern.")

(defun word-count (text)
  (let ((count 0) (inside nil))
    (loop for character across text
          do (if (or (alpha-char-p character) (digit-char-p character))
                 (unless inside (setf inside t count (1+ count)))
                 (setf inside nil)))
    count))

(defun natural-language-p (query)
  "True when QUERY is prose rather than a `memo'-style regex: a sentence
compiled as a pattern matches nothing useful, so it goes to token search."
  (let ((text (nlk:trimmed (or query ""))))
    (cond ((zerop (length text)) nil)
          ((member (char text (1- (length text))) '(#\? #\!) :test #'char=) t)
          ((find-if (lambda (character) (find character +regex-hints+ :test #'char=)) text) nil)
          (t (>= (word-count text) 3)))))

(defun pattern (query)
  "QUERY as a case-insensitive scanner; a broken pattern signals."
  (cl-ppcre:create-scanner query :case-insensitive-mode t))

(defun plan-recall (query mode)
  "The mode QUERY is searched in: \"regex\", \"token\" or \"bm25\".

AUTO keeps the original `memo recall' regex for pattern-like queries and
routes prose to token search. An explicitly requested broken regex signals,
so the caller is told the pattern is wrong instead of getting nothing."
  (let ((requested (string-downcase (nlk:trimmed (or mode "auto")))))
    (when (zerop (length requested)) (setf requested "auto"))
    (unless (member requested '("auto" "regex" "bm25" "token") :test #'string=)
      (fail "unknown recall mode ~s; allowed: auto, regex, bm25, token" mode))
    (when (string= requested "auto")
      (setf requested (if (natural-language-p query) "token" "regex")))
    (when (string= requested "regex")
      (handler-case (progn (pattern query) nil)
        (error (condition)
          (if (string= (string-downcase (nlk:trimmed (or mode "auto"))) "regex")
              (fail "invalid regex ~s: ~a" query condition)
              (return-from plan-recall "token")))))
    requested))

(defun record-line (record)
  (format nil "#~a ~a ~a" (first record) (second record) (third record)))

(defun regex-hits (records scanner)
  "The records SCANNER matches, oldest first, each (RESULT . BYTES)."
  (let ((hits '()))
    (loop for record across records
          when record
            do (let ((line (record-line record)))
                 (when (cl-ppcre:scan scanner line)
                   (push (cons (list 1.0 (first record) (second record) (third record))
                               (1+ (nlk:utf-8-size line)))
                         hits))))
    (nreverse hits)))

(defun newest-within (hits budget)
  "HITS — oldest first, each (RESULT . BYTES) — as (values NEWEST-FIRST
TOTAL), the newest matches that fit BUDGET bytes, as `memo recall' caps them."
  (let* ((vector (coerce hits 'vector))
         (count (length vector))
         (size 0)
         (start count))
    (loop for index from (1- count) downto 0
          for cost = (cdr (aref vector index))
          while (<= (+ size cost) budget)
          do (incf size cost)
             (setf start index))
    (values (loop for index from (1- count) downto start
                  collect (car (aref vector index)))
            count)))

(defun recall-substring (records query topk)
  "A normalized literal scan for QUERY's three longest tokens: the fallback
that finds a rare identifier BM25's tokenizer would split."
  (let* ((tokens (sort (remove-if (lambda (token) (< (length token) 3))
                                  (remove-duplicates (tokenize query) :test #'string=))
                       #'> :key #'length))
         (taken (subseq tokens 0 (min 3 (length tokens)))))
    (if (null taken)
        '()
        (let ((scanner (pattern (format nil "~{~a~^|~}"
                                         (mapcar #'cl-ppcre:quote-meta-chars taken))))
              (hits '()))
          (loop for record across records
                when record
                  do (when (cl-ppcre:scan scanner (fold (record-line record)))
                       (push (list 1.0 (first record) (second record) (third record)) hits)))
          (let ((newest (nreverse hits)))
            (if (plusp topk)
                (subseq newest 0 (min topk (length newest)))
                newest))))))

(defun recall-bm25 (records query topk)
  "Accent-normalized BM25, the optional ranked search. TOPK 0 means every
ranked hit, which is how a truthful total is counted."
  (let* ((documents (loop for record across records
                           when record collect (cons record (tokenize (third record)))))
         (count (length documents))
         (terms (remove-duplicates (tokenize query) :test #'string=))
         (frequency (make-hash-table :test #'equal))
         (average (if (plusp count)
                      (/ (loop for document in documents
                               sum (length (cdr document)))
                         count)
                      0))
         (k1 1.5)
         (b 0.75))
    (dolist (document documents)
      (dolist (token (remove-duplicates (cdr document) :test #'string=))
        (incf (gethash token frequency 0))))
    (let ((scored '()))
      (dolist (document documents)
        (destructuring-bind (record . tokens) document
          (let ((seen (make-hash-table :test #'equal))
                (length (length tokens))
                (score 0.0))
            (dolist (token tokens) (incf (gethash token seen 0)))
            (dolist (term terms)
              (let ((df (gethash term frequency)))
                (when df
                  (let ((idf (/ (+ (- count df) 0.5) (+ df 0.5)))
                        (tf (gethash term seen 0)))
                    (incf score (* idf (/ (* tf (1+ k1))
                                          (+ tf (* k1 (- 1 b (* b (/ length average))))))))))))
            (when (plusp score)
              (push (cons score record) scored)))))
      (let ((ranked (mapcar (lambda (pair)
                              (list (car pair) (first (cdr pair))
                                    (second (cdr pair)) (third (cdr pair))))
                            (sort scored #'> :key #'car))))
        (if (plusp topk) (subseq ranked 0 (min topk (length ranked))) ranked)))))

(defun recall-token (records query topk)
  "Natural-language retrieval: BM25 ranking plus a literal fallback."
  (let* ((ranked (recall-bm25 records query 0))
         (seen (mapcar #'second ranked))
         (extra '()))
    (dolist (hit (recall-substring records query 0))
      (unless (member (second hit) seen :test #'=)
        (push (second hit) seen)
        (push hit extra)))
    (let ((all (append ranked (nreverse extra))))
      (if (plusp topk) (subseq all 0 (min topk (length all))) all))))

(defun recall-meta (query &key (mode "auto") (topk 0))
  "=> (values RESULTS TOTAL TRUNCATED MODE-USED). RESULTS are the newest
matches in regex mode and the best ranked ones otherwise; TOTAL is every
match, so a vague pattern is never reported as if it were all of them."
  (let* ((records (log-records))
         (resolved (plan-recall query mode)))
    (if (zerop (length records))
        (values '() 0 nil resolved)
        (cond ((string= resolved "regex")
               (multiple-value-bind (newest total)
                   (newest-within (regex-hits records (pattern query)) (part-chars))
                 (let ((returned (if (plusp topk)
                                     (subseq newest 0 (min topk (length newest)))
                                     newest)))
                   (values returned total (> total (length returned)) resolved))))
              (t (let* ((found (if (string= resolved "token")
                                   (recall-token records query topk)
                                   (recall-bm25 records query topk)))
                        (full (if (plusp topk)
                                  (if (string= resolved "token")
                                      (length (recall-token records query 0))
                                      (length (recall-bm25 records query 0)))
                                  (length found))))
                   (values found full (> full (length found)) resolved)))))))
