;;;; checks.lisp --- what a document must keep, and the report that says so.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two kinds of check, and the difference is the whole point of the
;;;; original. STRUCTURE is read off the document alone: an unclosed fence,
;;;; a skipped heading level, a table without a delimiter row, trailing
;;;; whitespace, a citation pair GFM reads as one link, a list that cannot be
;;;; formatted twice the same way. INTEGRITY is read off a comparison with
;;;; the untouched source snapshot: the protected tokens a rewrite must not
;;;; lose (URLs, versions, hashes, dates, paths, environment names, code),
;;;; the ordered sequence whose items must keep their positions, and the
;;;; ledger records that must stay on their own lines. A PASS is Markdown
;;;; quality proven against a source; it is not a claim that the source is
;;;; true, and no report here says so.

(in-package #:nodecode-wolfmarkdown)

(defstruct issue
  "One finding: the check that made it, the line it is on, and what it says."
  (check nil)
  (line nil)
  (message ""))

(defun remediation-for (check)
  "The one line of advice CHECK's findings carry."
  (case check
    (:fences "Close the opening fence with a matching marker of equal or greater length.")
    (:headings "Do not skip heading levels; follow the source outline.")
    (:tables "Give GFM tables a delimiter row and consistent cell counts, or keep non-tables as prose.")
    (:frontmatter "Close YAML frontmatter with ---.")
    (:whitespace "End the file with one newline, keep at most one blank line between blocks, and use two-space hard breaks only.")
    (:lists "Write one unordered marker throughout, and a dot after every ordered one.")
    (:citations "Separate adjacent numeric citations with a space ([6] [26]); GFM reads the pair as one link.")
    (:formatting "Run markdown_clean on the file, then verify again.")
    (:idempotence "Re-run the formatter; a second pass must change nothing.")
    (:integrity "Restore the missing token from the untouched source snapshot.")
    (:ordinals "Keep every ordered item at the position it had in the source.")
    (:records "Keep each source record on a line of its own, in order.")
    (t "Fix the reported Markdown issue, then verify again.")))

;;; --- protected tokens ---------------------------------------------------------

(defstruct (token-index (:constructor make-token-index))
  "What one text carries: TOKEN -> CLASS for the strong and the weak, and
(CLASS TOKEN) -> COUNT for every class."
  (strong (make-hash-table :test #'equal))
  (weak (make-hash-table :test #'equal))
  (counts (make-hash-table :test #'equal)))

(defun add-token (index token class &key weak)
  "Count TOKEN once in INDEX under CLASS, as a weak one when WEAK."
  (when (and token (plusp (length token)))
    (when weak
      (setf (gethash token (token-index-weak index)) t))
    (setf (gethash token (token-index-strong index)) class)
    (incf (gethash (list class token) (token-index-counts index) 0))))

(defun word-character-p (character)
  "Whether CHARACTER can sit inside a word the extractor reads."
  (not (member character '(#\Space #\Tab #\Newline #\Return #\( #\) #\[ #\] #\{
                            #\} #\< #\> #\" #\' #\` #\| #\*))))

(defun words-of (text)
  "TEXT's words, split where a word cannot continue."
  (let ((words '())
        (start nil))
    (dotimes (index (length text))
      (if (word-character-p (char text index))
          (unless start (setf start index))
          (when start
            (push (subseq text start index) words)
            (setf start nil))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(defun trim-token (word)
  "WORD without the punctuation that ended it."
  (string-right-trim '(#\. #\, #\; #\: #\! #\? #\Space #\Tab)
                     (string-left-trim '(#\Space #\Tab) word)))

(defun digits-only-p (text)
  "Whether every character of TEXT is a digit, and there is one."
  (and (plusp (length text)) (every #'digit-char-p text)))

(defun url-token-p (word)
  "Whether WORD is an http or https URL."
  (and (>= (length word) 8)
       (or (and (search "http://" word) (zerop (search "http://" word)))
           (and (search "https://" word) (zerop (search "https://" word))))))

(defun hex-token-p (word)
  "Whether WORD is a hash: 40 or more hex digits."
  (and (>= (length word) 40)
       (every (lambda (character) (digit-char-p character 16)) word)))

(defun iso-date-token-p (word)
  "Whether WORD is an ISO date, YYYY-MM-DD."
  (and (= (length word) 10)
       (digits-only-p (subseq word 0 4))
       (char= (char word 4) #\-)
       (digits-only-p (subseq word 5 7))
       (char= (char word 7) #\-)
       (digits-only-p (subseq word 8 10))))

(defun semver-token-p (word)
  "Whether WORD is a three-part version, an optional leading v included."
  (let* ((text (if (and (plusp (length word)) (char-equal (char word 0) #\v))
                   (subseq word 1)
                   word))
         (parts (uiop:split-string text :separator ".")))
    (and (>= (length parts) 3)
         (digits-only-p (first parts))
         (digits-only-p (second parts))
         (let ((third (third parts)))
           (and (plusp (length third))
                (digit-char-p (char third 0))
                (every (lambda (character)
                         (or (alphanumericp character) (member character '(#\- #\+))))
                       third))))))

(defun percent-token-p (word)
  "Whether WORD is a number with a percent sign."
  (let ((end (length word)))
    (and (>= end 2)
         (char= (char word (1- end)) #\%)
         (let ((body (subseq word 0 (1- end))))
           (and (plusp (length body))
                (every (lambda (character)
                         (or (digit-char-p character) (char= character #\.))) body))))))

(defun currency-token-p (word)
  "Whether WORD is a money amount."
  (and (> (length word) 1)
       (find (char word 0) "$£€")
       (let ((body (subseq word 1)))
         (and (plusp (length body))
              (every (lambda (character)
                       (or (digit-char-p character)
                           (member character '(#\, #\.)))) body)))))

(defun path-token-p (word)
  "Whether WORD is a path: a slash, and nothing a path cannot carry."
  (and (find #\/ word)
       (> (length word) 2)
       (not (url-token-p word))
       (every (lambda (character)
                (or (alphanumericp character)
                    (member character '(#\/ #\. #\_ #\- #\~ #\% #\+ #\@))))
              word)))

(defun env-scoped-token-p (word)
  "Whether WORD names an environment variable with a dollar sign."
  (let ((body (cond ((and (search "${" word) (zerop (search "${" word)))
                     (string-trim "}" (subseq word 2)))
                    ((and (plusp (length word)) (char= (char word 0) #\$))
                     (subseq word 1))
                    (t nil))))
    (and body (plusp (length body)) (env-name-p body))))

(defun env-name-p (word)
  "Whether WORD is a shell variable name: upper case, digits, underscores."
  (let ((body (if (and (plusp (length word)) (char= (char word (1- (length word))) #\=))
                  (subseq word 0 (1- (length word)))
                  word)))
    (and (find #\_ body)
         (>= (length body) 3)
         (every (lambda (character)
                  (or (upper-case-p character) (digit-char-p character)
                      (char= character #\_)))
                body))))

(defun camel-token-p (word)
  "Whether WORD is a camelCase identifier."
  (and (plusp (length word))
       (lower-case-p (char word 0))
       (find-if #'upper-case-p word)
       (every #'alphanumericp word)))

(defun snake-token-p (word)
  "Whether WORD is a snake_case identifier."
  (and (find #\_ word)
       (>= (length word) 3)
       (every (lambda (character)
                (or (lower-case-p character) (digit-char-p character)
                    (char= character #\_)))
              word)))

(defun classify-token (word)
  "(VALUES TOKEN CLASS WEAK) for WORD, or NIL when it is prose."
  (let ((token (trim-token word)))
    (when (plusp (length token))
      (cond ((url-token-p token) (values token :url nil))
            ((hex-token-p token) (values token :hex nil))
            ((iso-date-token-p token) (values token :iso-date nil))
            ((semver-token-p token) (values token :semver nil))
            ((percent-token-p token) (values token :percent nil))
            ((env-scoped-token-p token) (values token :env nil))
            ((currency-token-p token) (values token :currency nil))
            ((path-token-p token) (values token :path nil))
            ((env-name-p token) (values token :env t))
            ((camel-token-p token) (values token :camel t))
            ((snake-token-p token) (values token :snake t))))))

(defun scan-text-tokens (index text &key code)
  "Count every token TEXT carries, strong when CODE says it is inside code."
  (dolist (word (words-of text))
    (multiple-value-bind (token class weak) (classify-token word)
      (when token
        (add-token index token class :weak (and weak (not code)))))))

(defun scan-link-targets (index line)
  "Count the URL of every inline link on LINE."
  (let ((from 0))
    (loop
      (let ((open (search "](" line :start2 from)))
        (unless open (return))
        (let ((close (position #\) line :start (+ open 2))))
          (unless close (return))
          (let ((target (subseq line (+ open 2) close)))
            (when (url-token-p (string-trim '(#\Space #\Tab) target))
              (add-token index (string-trim '(#\Space #\Tab) target) :url)))
          (setf from (+ close 1)))))))

(defun token-index (text)
  "TEXT's protected tokens: the strong ones a rewrite must not lose, the
weak ones it should keep, and the counts each class appeared with."
  (let* ((lines (lines-of text))
         (mask (fence-mask lines))
         (index (make-token-index)))
    (dotimes (line-number (length lines))
      (let ((line (aref lines line-number)))
        (if (aref mask line-number)
            (unless (or (blank-line-p line) (fence-open (strip-quotes line)))
              (add-token index (string-trim '(#\Space #\Tab) line) :code)
              (scan-text-tokens index line :code t))
            (let ((spans (inline-code-spans line)))
              (dolist (span spans)
                (add-token index (subseq line (car span) (cdr span)) :code)
                (add-token index (subseq line (1+ (car span)) (1- (cdr span))) :code))
              (scan-text-tokens index (mask-spans line spans))
              (scan-link-targets index line)))))
    index))

(defun class-maps (index)
  "INDEX's counts as a hash of CLASS => (hash TOKEN => COUNT)."
  (let ((maps (make-hash-table)))
    (maphash (lambda (key count)
               (destructuring-bind (class token) key
                 (let ((map (or (gethash class maps)
                                (setf (gethash class maps) (make-hash-table :test #'equal)))))
                   (setf (gethash token map) count))))
             (token-index-counts index))
    maps))

(defun strong-token-p (index token)
  "Whether INDEX counts TOKEN as one a rewrite must not lose."
  (and (gethash token (token-index-strong index))
       (not (gethash token (token-index-weak index)))))

(defun compare-tokens (before after)
  "=> (values MISSING MUTATED DECREASED), each a list of (TOKEN CLASS) or
(FROM TO CLASS) entries: a strong token the candidate lost, a value that
changed into another of its class, and a count that fell without a
counterpart. Two documents with the same protected tokens answer NIL NIL NIL."
  (let ((missing '())
        (mutated '())
        (decreased '())
        (before-maps (class-maps before))
        (after-maps (class-maps after)))
    (maphash (lambda (token class)
               (unless (gethash token (token-index-strong after))
                 (push (list token class) missing)))
             (token-index-strong before))
    (maphash (lambda (class before-map)
               (let ((after-map (or (gethash class after-maps) (make-hash-table :test #'equal)))
                     (lost '())
                     (gained '()))
                 (maphash (lambda (token count)
                            (when (and (strong-token-p before token)
                                       (< (gethash token after-map 0) count)
                                       (plusp (gethash token after-map 0)))
                              (push token lost)))
                          before-map)
                 (maphash (lambda (token count)
                            (when (and (gethash token (token-index-strong after))
                                       (not (gethash token (token-index-weak after)))
                                       (> count (gethash token before-map 0)))
                              (push token gained)))
                          after-map)
                 (setf lost (sort lost #'string<)
                       gained (sort gained #'string<))
                 (loop for token in lost
                       for replacement = (pop gained)
                       do (if replacement
                              (push (list token replacement class) mutated)
                              (push (list token class) decreased)))))
             before-maps)
    (values (nreverse missing) (nreverse mutated) (nreverse decreased))))

;;; --- ordered sequences and ledger records -----------------------------------

(defun collapse-spaces (text)
  "TEXT with every run of whitespace brought to one space."
  (let ((out (make-string-output-stream))
        (space nil))
    (loop for character across text
          do (if (member character '(#\Space #\Tab #\Newline #\Return))
                 (setf space t)
                 (progn (when space (write-char #\Space out) (setf space nil))
                        (write-char character out))))
    (string-trim " " (get-output-stream-string out))))

(defun punctuation-only-p (word)
  "Whether WORD carries no letter or digit."
  (every (lambda (character) (not (alphanumericp character))) word))

(defun item-anchor (text)
  "A short identity for a list item: its leading phrase, at most five
words, up to an em dash, punctuation at the end removed. Emphasis and
whitespace changes do not move it."
  (let* ((collapsed (collapse-spaces text))
         (dash (format nil " ~a " (code-char #x2014)))
         (cut (search dash collapsed))
         (base (if cut (subseq collapsed 0 cut) collapsed))
         (words (let ((all (nlk:split-words base)))
                  (subseq all 0 (min 5 (length all))))))
    (loop while (and (> (length words) 1) (punctuation-only-p (car (last words))))
          do (setf words (butlast words)))
    (string-right-trim ",:;" (format nil "~{~a~^ ~}" words))))

(defun normalise-heading (text)
  "TEXT as a heading identity: spaces collapsed, lower case, a leading
number removed."
  (let ((collapsed (string-downcase (collapse-spaces text)))
        (index 0))
    (loop while (and (< index (length collapsed)) (digit-char-p (char collapsed index)))
          do (incf index))
    (when (and (> index 0) (< index (length collapsed))
               (member (char collapsed index) '(#\. #\))))
      (incf index))
    (string-left-trim " " (subseq collapsed index))))

(defun ordinal-runs (text)
  "Each ordered list with two or more items: (CONTEXT ITEMS LINE), ITEMS a
list of (ANCHOR . POSITION) in effective numbering, CONTEXT the normalised
headings above it, LINE where it starts."
  (let* ((lines (lines-of text))
         (mask (fence-mask lines))
         (stack '())
         (runs '())
         (start nil)
         (first-number 1)
         (items '())
         (line-number nil))
    (labels ((context () (format nil "~{~a~^/~}" (mapcar #'cdr stack)))
             (flush ()
               (when (and items (>= (length items) 2))
                 (push (list start (nreverse items) line-number) runs))
               (setf items '() start nil line-number nil)))
      (dotimes (index (length lines))
        (let ((line (aref lines index)))
          (cond
            ((aref mask index) (flush))
            ((blank-line-p line) nil)
            (t
             (multiple-value-bind (depth heading) (heading-of line)
               (if depth
                   (progn (flush)
                          (loop while (and stack (>= (car (first stack)) depth))
                                do (setf stack (cdr stack)))
                          (push (cons depth (normalise-heading heading)) stack))
                   (multiple-value-bind (number indent item-text) (ordered-item-of line)
                     (declare (ignore indent))
                     (cond
                       (number
                        (unless items
                          (setf start (context)
                                first-number number
                                line-number (1+ index)))
                        (push (cons (item-anchor item-text)
                                    (+ first-number (length items)))
                              items))
                       ((and items (>= (indent-width line) 2)) nil)
                       (t (flush))))))))))
      (flush)
      (nreverse runs))))

(defun longest-common-run (left right)
  "The length of the longest common subsequence of LEFT and RIGHT: how much
of one sequence survives, in order, in the other."
  (let* ((width (length right))
         (previous (make-array (1+ width) :initial-element 0)))
    (dolist (one left)
      (let ((current (make-array (1+ width) :initial-element 0)))
        (loop for column from 1 to width
              do (setf (aref current column)
                       (if (equal one (nth (1- column) right))
                           (1+ (aref previous (1- column)))
                           (max (aref previous column) (aref current (1- column))))))
        (setf previous current)))
    (aref previous width)))

(defun check-ordinals (source candidate)
  "The ordered items of SOURCE that CANDIDATE lost or moved."
  (let ((issues '())
        (candidate-runs (ordinal-runs candidate))
        (claimed '()))
    (dolist (run (ordinal-runs source))
      (destructuring-bind (context source-items line) run
        (let* ((anchors (mapcar #'car source-items))
               (best nil))
          (dolist (other candidate-runs)
            (when (and (equal (first other) context) (not (member other claimed :test #'eq)))
              (let ((score (longest-common-run anchors (mapcar #'car (second other)))))
                (when (or (null best) (> score (car best)))
                  (setf best (cons score other))))))
          (when (and best (>= (car best) (max 1 (ceiling (length source-items) 2))))
            (push (cdr best) claimed)
            (dolist (item source-items)
              (let* ((anchor (car item))
                     (expected (cdr item))
                     (found (find anchor (second (cdr best)) :key #'car :test #'equal)))
                (cond
                  ((null found)
                   (push (make-issue :check :ordinals :line line
                                     :message (format nil "Ordered item \"~a\" was not found in the candidate; its position ~d cannot be preserved."
                                                      anchor expected))
                         issues))
                  ((/= (cdr found) expected)
                   (push (make-issue :check :ordinals :line line
                                     :message (format nil "Ordered item \"~a\" is at position ~d but was at position ~d."
                                                      anchor (cdr found) expected))
                         issues)))))))))
    (nreverse issues)))

(defun record-runs (text)
  "Each run of three or more consecutive record lines, as (LABEL . ANCHOR) pairs."
  (let* ((lines (lines-of text))
         (mask (fence-mask lines))
         (runs '())
         (current '()))
    (labels ((flush ()
               (when (>= (length current) 3)
                 (push (nreverse current) runs))
               (setf current '())))
      (dotimes (index (length lines))
        (let ((line (aref lines index)))
          (if (aref mask index)
              (flush)
              (multiple-value-bind (label anchor) (record-of line)
                (declare (ignore index))
                (if label
                    (push (cons label anchor) current)
                    (flush))))))
      (flush)
      (nreverse runs))))

(defun check-records (source candidate)
  "The source records CANDIDATE no longer carries, on their own lines and in order."
  (let* ((records (apply #'append (record-runs source)))
         (candidate-lines (lines-of candidate))
         (positions (let ((mask (fence-mask (lines-of candidate)))
                         (found '()))
                     (dotimes (index (length candidate-lines))
                       (unless (aref mask index)
                         (multiple-value-bind (label anchor)
                             (record-of (aref candidate-lines index))
                           (when label (push (cons (cons label anchor) (1+ index)) found)))))
                     (nreverse found)))
         (issues '())
         (cursor 0))
    (dolist (record records)
      (let ((found (assoc record positions :test #'equal)))
        (cond
          ((null found)
           (push (make-issue :check :records
                             :message (format nil "Record [~a] ~a was not found on a line of its own in the candidate."
                                              (car record) (cdr record)))
                 issues))
          ((< (cdr found) cursor)
           (push (make-issue :check :records :line (cdr found)
                             :message (format nil "Record [~a] ~a is out of order; each record must keep its place."
                                              (car record) (cdr record)))
                 issues))
          (t (setf cursor (cdr found))))))
    (nreverse issues)))

;;; --- the document's own shape -------------------------------------------------

(defun check-fences (lines)
  "The one issue an unclosed fence makes."
  (multiple-value-bind (mask open-line) (fence-mask lines)
    (declare (ignore mask))
    (when open-line
      (list (make-issue :check :fences :line open-line
                        :message (format nil "Unclosed fence starting on line ~d." open-line))))))

(defun check-headings (lines mask)
  "Every place the heading depth skips a level."
  (let ((issues '())
        (previous nil))
    (dotimes (index (length lines))
      (unless (aref mask index)
        (multiple-value-bind (depth text) (heading-of (aref lines index))
          (declare (ignore text))
          (when depth
            (when (and previous (> depth (1+ previous)))
              (push (make-issue :check :headings :line (1+ index)
                                :message (format nil "Heading level skips from h~d to h~d."
                                                 previous depth))
                    issues))
            (setf previous depth)))))
    (nreverse issues)))

(defun check-tables (lines mask)
  "Table blocks without a delimiter row, and blocks whose cells disagree."
  (let ((issues '())
        (block '()))
    (labels ((flush ()
               (when block
                 (let ((start (first (car (last block))))
                       (rows (remove-if #'delimiter-row-p (mapcar #'cdr block))))
                   (unless (some (lambda (entry) (delimiter-row-p (cdr entry))) block)
                     (push (make-issue :check :tables :line start
                                       :message (format nil "Table-like block starting on line ~d has no delimiter row." start))
                           issues))
                   (let ((counts (mapcar (lambda (row) (length (split-pipes row))) rows)))
                     (when (and counts (not (every (lambda (count) (= count (first counts))) counts)))
                       (push (make-issue :check :tables :line start
                                         :message (format nil "Table starting on line ~d has inconsistent cell counts." start))
                             issues)))))
               (setf block '())))
      (dotimes (index (length lines))
        (let ((line (aref lines index)))
          (if (and (not (aref mask index)) (table-line-p line))
              (push (cons (1+ index) line) block)
              (flush))))
      (flush))
    (nreverse issues)))

(defun check-frontmatter (lines)
  "Frontmatter that opens and never closes."
  (when (and (plusp (length lines))
             (equal "---" (string-trim '(#\Space #\Tab #\Return) (aref lines 0))))
    (let ((closed nil))
      (loop for index from 1 below (length lines)
            while (not closed)
            do (when (equal "---" (string-trim '(#\Space #\Tab #\Return) (aref lines index)))
                 (setf closed t)))
      (unless closed
        (list (make-issue :check :frontmatter :line 1
                          :message "YAML frontmatter opens and never closes."))))))

(defun check-whitespace (lines mask text)
  "Trailing whitespace, runs of blank lines, and the file's last newline."
  (let ((issues '())
        (blank-run 0))
    (dotimes (index (length lines))
      (let ((line (aref lines index)))
        (when (blank-line-p line)
          (incf blank-run)
          (when (and (= blank-run 3) (not (aref mask index)))
            (push (make-issue :check :whitespace :line (1+ index)
                              :message "Document has three or more blank lines in a row.")
                  issues)))
        (unless (blank-line-p line)
          (setf blank-run 0))
        (unless (aref mask index)
          (when (and (plusp (length line))
                     (member (char line (1- (length line))) '(#\Space #\Tab))
                     (not (trailing-hard-break-p line)))
            (push (make-issue :check :whitespace :line (1+ index)
                              :message (format nil "Trailing whitespace on line ~d." (1+ index)))
                  issues)))))
    (unless (and (plusp (length text)) (char= (char text (1- (length text))) #\Newline))
      (push (make-issue :check :whitespace :message "Document must end with exactly one newline.") issues))
    (when (and (>= (length text) 2)
               (char= (char text (- (length text) 2)) #\Newline))
      (push (make-issue :check :whitespace :message "Document must end with exactly one newline, not two.") issues))
    (nreverse issues)))

(defun check-lists (lines mask list-style)
  "Unordered markers other than LIST-STYLE, and an ordered marker closing with a paren."
  (let ((issues '()))
    (dotimes (index (length lines))
      (unless (aref mask index)
        (multiple-value-bind (marker indent text) (unordered-item-of (aref lines index))
          (declare (ignore indent text))
          (when marker
            (unless (char= marker (char list-style 0))
              (push (make-issue :check :lists :line (1+ index)
                                :message (format nil "Unordered list marker ~c on line ~d; this document writes ~c."
                                                 marker (1+ index) (char list-style 0)))
                    issues)))))
      (unless (aref mask index)
        (let* ((body (strip-quotes (aref lines index)))
               (indent (leading-space-count body))
               (number (ordered-item-of (aref lines index))))
          (when (and number (< (1+ indent) (length body))
                     (char= (char body (1+ indent)) #\)))
            (push (make-issue :check :lists :line (1+ index)
                              :message (format nil "Ordered marker ) on line ~d; this document writes ."
                                               (1+ index)))
                  issues)))))
    (nreverse issues)))

(defun check-citations (lines mask)
  "Adjacent numeric citation pairs, which GFM reads as one reference link."
  (let ((issues '()))
    (dotimes (index (length lines))
      (unless (aref mask index)
        (let* ((line (aref lines index))
               (masked (mask-spans line (inline-code-spans line))))
          (dolist (triple (adjacent-citations masked))
            (push (make-issue :check :citations :line (1+ index)
                              :message (format nil "Adjacent citations [~a][~a] are read as one reference link; write [~a] [~a]."
                                               (first triple) (second triple)
                                               (first triple) (second triple)))
                  issues)))))
    (nreverse issues)))

;;; --- the report ---------------------------------------------------------------

(defun heading-label (checks)
  "The report's word for CHECKS' outcome: Pass, Fail, or Skipped."
  (cond ((null checks) "Pass")
        ((eq checks :skipped) "Skipped")
        (t "Fail")))

(defun render-issue (issue)
  "One finding as the report writes it."
  (format nil "- ~(~a~)~@[ on line ~d~]: ~a ~a"
          (issue-check issue) (issue-line issue) (issue-message issue)
          (remediation-for (issue-check issue))))

(defun verify-text (text &key source path (max-issues 20))
  "TEXT verified against its own shape, and against SOURCE when given.
=> (values REPORT PASS-P), REPORT the text the tool answers with."
  (let* ((lines (lines-of text))
         (mask (fence-mask lines))
         (style (list-style))
         (issues (append (check-fences lines)
                         (check-headings lines mask)
                         (check-tables lines mask)
                         (check-frontmatter lines)
                         (check-whitespace lines mask text)
                         (check-lists lines mask style)
                         (check-citations lines mask)))
         (formatting (multiple-value-bind (cleaned fixes)
                         (clean-text text)
                       (declare (ignore fixes))
                       (if (equal cleaned text)
                           nil
                           (list (make-issue :check :formatting
                                             :message "The document is not in the cell's canonical form.")))))
         (idempotence (let* ((once (clean-text text))
                             (twice (clean-text once)))
                        (unless (equal once twice)
                          (list (make-issue :check :idempotence
                                            :message "The formatter changes its own output.")))))
         (integrity (when source
                      (multiple-value-bind (missing mutated decreased)
                          (compare-tokens (token-index source) (token-index text))
                        (append
                         (mapcar (lambda (entry)
                                   (make-issue :check :integrity
                                               :message (format nil "Protected ~(~a~) token missing: ~a"
                                                                (second entry) (first entry))))
                                 missing)
                         (mapcar (lambda (entry)
                                   (make-issue :check :integrity
                                               :message (format nil "Protected ~(~a~) value changed: ~a -> ~a"
                                                                (third entry) (first entry) (second entry))))
                                 mutated)
                         (mapcar (lambda (entry)
                                   (make-issue :check :integrity
                                               :message (format nil "Protected ~(~a~) token appears fewer times: ~a (~d -> ~d)"
                                                                (second entry) (first entry) (third entry) (fourth entry))))
                                 decreased)))))
         (structure (when source
                      (append (check-ordinals source text)
                              (check-records source text))))
         (all (remove-duplicates (append issues integrity structure)
                                  :test (lambda (left right)
                                          (and (equal (issue-check left) (issue-check right))
                                               (equal (issue-message left) (issue-message right))))))
         (checks (list (cons "Fences" (check-fences lines))
                       (cons "Headings" (check-headings lines mask))
                       (cons "Tables" (check-tables lines mask))
                       (cons "Frontmatter" (check-frontmatter lines))
                       (cons "Whitespace" (check-whitespace lines mask text))
                       (cons "Lists" (check-lists lines mask style))
                       (cons "Citations" (check-citations lines mask))
                       (cons "Formatting" formatting)
                       (cons "Idempotence" idempotence)
                       (cons "Content integrity" (if source integrity :skipped))
                       (cons "Structure" (if source structure :skipped))))
         (pass (null all)))
    (values
     (with-output-to-string (out)
       (format out "WolfMarkDown~%Operation: Verify~@[~%File: ~a~]~%" path)
       (dolist (check checks)
         (format out "~a: ~a~%" (car check) (heading-label (cdr check))))
       (format out "Result: ~a~%" (if pass "PASS" "FAIL"))
       (when all
         (format out "~%Issues:~%")
         (let ((shown (if (> (length all) max-issues) (subseq all 0 max-issues) all)))
           (dolist (issue shown) (format out "~a~%" (render-issue issue)))
           (when (> (length all) max-issues)
             (format out "- and ~d more~%" (- (length all) max-issues))))))
     pass)))

(defun inventory-text (text &optional path)
  "What a document is made of: the source map a rewrite is planned from."
  (let* ((lines (lines-of text))
         (mask (fence-mask lines))
         (index (token-index text))
         (headings '())
         (tables '())
         (code 0))
    (dotimes (line-number (length lines))
      (let ((line (aref lines line-number)))
        (when (aref mask line-number)
          (when (fence-open (strip-quotes line)) (incf code)))
        (unless (aref mask line-number)
          (multiple-value-bind (depth heading) (heading-of line)
            (when depth (push (format nil "~v@{#~} ~a" depth heading) headings)))
          (when (table-line-p line) (push (1+ line-number) tables)))))
    (with-output-to-string (out)
      (format out "WolfMarkDown~%Operation: Outline~@[~%File: ~a~]~%" path)
      (format out "~%Headings: ~d~%~{  ~a~%~}" (length headings) (nreverse headings))
      (format out "Fenced blocks: ~d~%" code)
      (format out "Table lines: ~d~@[ (first on line ~d)~]~%" (length tables)
              (car (last tables)))
      (format out "Ordered lists: ~d~%" (length (ordinal-runs text)))
      (format out "Record runs: ~d~%" (length (record-runs text)))
      (format out "Protected tokens:~%")
      (let ((rows '()))
        (maphash (lambda (key count)
                   (destructuring-bind (class token) key
                     (push (list class count token) rows)))
                 (token-index-counts index))
        (let ((classes (sort (remove-duplicates (mapcar #'first rows)) #'string<)))
          (dolist (class classes)
            (let* ((mine (remove-if-not (lambda (row) (eq (first row) class)) rows))
                   (count (reduce #'+ (mapcar #'second mine)))
                   (examples (subseq (mapcar #'third mine) 0 (min 3 (length mine)))))
              (format out "  ~(~a~): ~d~@[ (~{~a~^, ~})~]~%" class count examples))))
      (format out "Protected-token classes are what an edit must keep; the count is appearances.~%")))))

