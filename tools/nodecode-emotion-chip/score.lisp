;;;; score.lisp --- the bracket scanner: a reply's delivery score as tokens.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The grammar is the original's: a leading [[register: ...]] carries a
;;;; whole reply; [word] or [word, word] before a sentence sets its delivery
;;;; (slightly / very for intensity); [op: words] scopes one operator inside
;;;; a sentence; [sound] names a laugh, a sigh or a pause; [speaker: N] is
;;;; the one voice switch. Bracket groups that are code (items[0], x[i][j],
;;;; f()[k], [1], [::1]) and Markdown link targets stay text, and so does a
;;;; wiki link. A cue's words are passed through exactly as written:
;;;; nothing here invents or upgrades one.

(in-package #:nodecode-emotion-chip)

(defparameter +span-ops+ '("soft" "whisper" "stress" "slow" "fast" "smiling" "emphasis" "forceful")
  "The span operators the grammar parses.")

(defparameter +gap+ (string (code-char #xE000))
  "Stands where a token was removed: render turns it into a space only between
two word characters, so dropping a cue never fuses the words around it
(hap[laugh]py stays hap py).")

(defparameter +abbreviations+ '("mr" "mrs" "ms" "dr" "prof" "sr" "jr" "st" "mt" "vs" "e.g" "i.e")
  "A period after one of these does not end a sentence.")

(defparameter +closers+ (list #\" #\' (code-char #x201D) (code-char #x2019) #\) #\]))

(defparameter +space+ '(#\Space #\Tab))

;;; --- tokens -----------------------------------------------------------------------------

(defstruct cue raw parts rung words)
(defstruct span op text raw)
(defstruct event id raw)
(defstruct speaker id raw)
(defstruct plain s)
(defstruct sentence tokens)
(defstruct header fields raw)
(defstruct dropped kind value (reason ""))
(defstruct score header sentences source dropped)
(defstruct rendered text params notes dropped dialect group)

;;; --- small readers ----------------------------------------------------------------------

(defun whitespace-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun space-p (char)
  (member char +space+ :test #'char=))

(defun blankp (string)
  (every #'whitespace-p string))

(defun trimmed (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun squeezed (string)
  (cl-ppcre:regex-replace-all "\\s+" string " "))

(defun split-on (char string)
  "STRING split at every CHAR, empty pieces kept."
  (loop with out = '() with start = 0
        for at = (position char string :start start)
        do (push (subseq string start at) out)
        while at do (setf start (1+ at))
        finally (return (nreverse out))))

(defun one-group (groups)
  "The one register group SCAN-TO-STRINGS answered, as a string."
  (if (or (null groups) (stringp groups)) groups (aref groups 0)))

(defun event-table ()
  "Sound name and vendor spelling => sound id, off the data tables."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row *sounds*) (setf (gethash (first row) table) (first row)))
    (dolist (row *sound-aliases*) (setf (gethash (first row) table) (second row)))
    table))

(defun sound-kind (id)
  (second (assoc id *sounds* :test #'string=)))

(defun lexicon (word)
  "WORD as a delivery word => (VALUES GROUP AROUSAL), NIL when it is none."
  (let* ((word (squeezed (string-downcase (trimmed word))))
         (row (or (find word *emotions* :key #'first :test #'string=)
                  (find word *aliases* :key #'first :test #'string=))))
    (values (and row (second row)) (if row (third row) 0))))

(defun rung-of (part)
  "PART's intensity and the word without it => (VALUES RUNG REST)."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "^(slightly|a little|somewhat|very|extremely|deeply|really)\\s+" part)
    (if match
        (let ((word (one-group groups)))
          (values (if (member (string-downcase word) '("slightly" "a little" "somewhat") :test #'string=)
                      "slightly" "very")
                  (subseq part (length match))))
        (values "plain" part))))

(defun cue-of (raw)
  "RAW, as written inside the brackets, as a cue."
  (let ((parts (mapcar #'trimmed (split-on #\, raw)))
        (words '())
        (rung "plain"))
    (loop for part in parts
          for first = t then nil
          do (multiple-value-bind (part-rung rest) (rung-of part)
               (when first (setf rung part-rung))
               (push (string-downcase rest) words)))
    (make-cue :raw raw :parts parts :rung rung :words (nreverse words))))

(defun header-cue (header)
  "HEADER's delivery words as one cue, in the order written."
  (when header
    (let ((values (loop for part in (split-on #\; (header-raw header))
                        for colon = (position #\: part)
                        for value = (trimmed (if colon (subseq part (1+ colon)) part))
                        when (plusp (length value)) collect value)))
      (when values (cue-of (format nil "~{~a~^, ~}" values))))))

;;; --- what is syntax, and what is text -----------------------------------------------------

(defun matching-bracket (string i)
  "The index of the bracket closing the one at I, nested pairs counted, or NIL."
  (loop with depth = 0
        for j from i below (length string)
        for char = (char string j)
        do (cond ((char= char #\[) (incf depth))
                 ((char= char #\]) (decf depth) (when (zerop depth) (return j))))
        finally (return nil)))

(defun ident-before-p (string i)
  "Whether the character before I ends an identifier (ASCII only, as the words are)."
  (and (plusp i)
       (let ((char (char string (1- i))))
         (and (< (char-code char) 128) (or (alphanumericp char) (char= char #\_))))))

(defun call-before-p (string i)
  "Whether I follows a call's closing parenthesis: its ( sits against a word."
  (and (plusp i) (char= (char string (1- i)) #\))
       (loop with depth = 0
             for j from (1- i) downto (max 0 (- i 202))
             for char = (char string j)
             do (cond ((char= char #\)) (incf depth))
                      ((char= char #\() (decf depth)
                       (when (zerop depth) (return (ident-before-p string j))))
                      (t nil))
             finally (return nil))))

(defun in-code-p (string i kept)
  "Whether the group at I continues a word or a code expression: x[k], x[i][j], f()[k]."
  (or (ident-before-p string i)
      (and (plusp i) (char= (char string (1- i)) #\]) (= kept (1- i)))
      (call-before-p string i)))

(defun span-opener (string i)
  "The span operator opening at I => (VALUES OP END), or NIL."
  (let ((end (1+ i)) (n (length string)))
    (loop while (and (< end n) (space-p (char string end))) do (incf end))
    (let ((from end))
      (loop while (and (< end n) (alpha-char-p (char string end))) do (incf end))
      (let ((word (string-downcase (subseq string from end))))
        (when (and (plusp (length word)) (member word +span-ops+ :test #'string=)
                   (< end n) (char= (char string end) #\:))
          (incf end)
          (loop while (and (< end n) (space-p (char string end))) do (incf end))
          (values word end))))))

(defun letter-p (string)
  (some #'alpha-char-p string))

(defun header-shaped-p (inner)
  "Whether a double-bracket group reads as a register header rather than a wiki link."
  (cond ((or (find #\[ inner) (blankp inner)) t)
        (t (let ((keys (loop for part in (split-on #\; inner)
                             for colon = (position #\: part)
                             when colon collect (trimmed (subseq part 0 colon)))))
             (cond (keys (every (lambda (key)
                                  (or (string= (string-downcase key) "register")
                                      (cl-ppcre:scan "^[a-z][a-z _-]{0,30}$" key)))
                                keys))
                   ((find #\; inner) t)
                   (t (let ((events (event-table)))
                        (every (lambda (part)
                                 (and (plusp (length part))
                                      (or (gethash (squeezed (string-downcase part)) events)
                                          (nth-value 0 (lexicon part)))))
                               (mapcar (lambda (part) (nth-value 1 (rung-of (trimmed part))))
                                       (split-on #\, inner))))))))))

;;; --- sentence boundaries ------------------------------------------------------------------

(defun abbreviation-before-p (string i)
  (let ((start (loop for j from (1- i) downto (max 0 (- i 8))
                     while (or (alphanumericp (char string j)) (char= (char string j) #\.))
                     finally (return (1+ j)))))
    (member (string-downcase (subseq string start i)) +abbreviations+ :test #'string=)))

(defun boundary-positions (string)
  "Where a sentence ends: just past its punctuation, its closers and its whitespace."
  (let ((out '()) (n (length string)))
    (loop for i from 0 below n
          when (find (char string i) ".!?…" :test #'char=)
            do (let ((j (1+ i)))
                 (loop repeat 2
                       while (and (< j n) (member (char string j) +closers+ :test #'char=))
                       do (incf j))
                 (let ((k j))
                   (loop while (and (< k n) (whitespace-p (char string k))) do (incf k))
                   (when (and (> k j) (not (abbreviation-before-p string i)))
                     (push k out)))))
    (nreverse out)))

;;; --- one bracket group ---------------------------------------------------------------------

(defun drop (notes kind value &optional (reason ""))
  (push (make-dropped :kind kind :value value :reason reason) notes))

(defun flatten-span (op body notes)
  "BODY of [OP: ...] as items: each run of words wrapped in the span, a nested group parsed."
  (loop for piece in (top-level-pieces body)
        append (if (eq (car piece) :group)
                   (group-items (cdr piece) notes)
                   (let* ((raw (cdr piece))
                          (run (remove-if (lambda (c) (find c "[]")) raw)))
                     (when (or (find #\[ raw) (find #\] raw))
                       (drop notes "syntax" "[" "nested"))
                     (if (plusp (length (trimmed run)))
                         (list " "
                               (make-span :op op :text (trimmed run)
                                          :raw (format nil "[~a: ~a]" op (trimmed run)))
                               " ")
                         '())))))

(defun group-items (group notes)
  "One bracket GROUP's contribution, in the order written: tokens, and text as strings."
  (let* ((inner (subseq group 1 (1- (length group))))
         (raw (trimmed inner)))
    (cond
      ((zerop (length raw)) (drop notes "syntax" "[]" "empty") '())
      ;; a group with no letter names no cue: [1], [::1], [0]
      ((not (letter-p raw)) (list group))
      (t (multiple-value-bind (op end) (span-opener group 0)
           (cond
             (op
              (let ((body (subseq group end (1- (length group)))))
                (cond
                  ((blankp body) (drop notes "span" op "empty") '())
                  ((or (find #\[ body) (find #\] body)) (flatten-span op body notes))
                  (t (list (make-span :op op :text body :raw group))))))
             ((cl-ppcre:scan "^speaker:" raw)
              (let ((id (trimmed (subseq raw (length "speaker:")))))
                (if (and (plusp (length id)) (<= (length id) 3) (every #'digit-char-p id))
                    (list (make-speaker :id id :raw raw))
                    (progn (drop notes "speaker" raw "not a number") '()))))
             ((gethash (squeezed (string-downcase raw)) (event-table))
              (list (make-event :id (gethash (squeezed (string-downcase raw)) (event-table)) :raw raw)))
             ;; [happy [laugh]] => the cue's own words as one cue, then the inner tokens
             ((position #\[ inner)
              (let ((words (loop for piece in (top-level-pieces inner)
                                 when (eq (car piece) :text)
                                   append (let ((run (trimmed (string-trim " ,"
                                                                           (remove-if (lambda (c) (find c "[]"))
                                                                                      (cdr piece))))))
                                            (when (plusp (length run)) (list run))))))
                (append (when words (list (cue-of (format nil "~{~a~^ ~}" words)) " "))
                        (loop for piece in (top-level-pieces inner)
                              when (eq (car piece) :group)
                                append (group-items (cdr piece) notes)))))
             (t (list (cue-of inner)))))))))

(defun top-level-pieces (string)
  "STRING split into (:text . run) and (:group . \"[...]\") pieces, a nested group kept whole."
  (let ((out '()) (start 0) (i 0) (n (length string)))
    (loop while (< i n) do
      (if (char= (char string i) #\[)
          (let ((close (matching-bracket string i)))
            (if close
                (progn (push (cons :text (subseq string start i)) out)
                       (push (cons :group (subseq string i (1+ close))) out)
                       (setf start (1+ close) i (1+ close)))
                (incf i)))
          (incf i)))
    (push (cons :text (subseq string start)) out)
    (loop for piece in (nreverse out)
          when (or (eq (car piece) :group) (plusp (length (cdr piece))))
            collect piece)))

;;; --- the scanner ------------------------------------------------------------------------------

(defun leading-header (text)
  "A leading [[register: ...]] is the reply's header => (VALUES HEADER BODY)."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "^\\s*\\[\\[([^\\[\\]]*)\\]\\]" text)
    (let ((inner (one-group groups)))
      (if (and match (or (string= inner (string-downcase inner)) (header-shaped-p inner)))
          (let ((raw (trimmed inner)))
            (values (make-header :raw raw
                                 :fields (or (loop for part in (split-on #\; raw)
                                                   for colon = (position #\: part)
                                                   when colon
                                                     collect (cons (trimmed (subseq part 0 colon))
                                                                   (trimmed (subseq part (1+ colon)))))
                                             (list (cons "register" raw))))
                    (cl-ppcre:regex-replace "^\\s*\\.(\\s+|$)" (subseq text (length match)) "")))
          (values nil text)))))

(defun link-after (body start)
  "A Markdown link's target opening at START => (VALUES START END TARGET), or NIL."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "^\\s*\\(([^\\s()]+)\\)" body :start start)
    (when match
      (values start (+ start (length match)) (or (one-group groups) "")))))

(defun plain-end (body i n)
  (or (position-if (lambda (c) (or (char= c #\[) (char= c #\]))) body :start i) n))

(defun scan-single (body i kept notes)
  "The single-bracket group at I => (VALUES ITEMS NEXT KEPT)."
  (let* ((close (matching-bracket body i))
         (raw (trimmed (subseq body (1+ i) close))))
    (multiple-value-bind (link-from link-to target) (link-after body (1+ close))
      (declare (ignore link-from))
      (cond
        ;; code that continues an identifier or a call, or a chained subscript
        ((and (in-code-p body i kept)
              (not (span-opener body i))
              (not (position #\[ body :start (1+ i) :end close)))
         (values (list (subseq body i (1+ close))) (1+ close) close))
        ;; [docs](https://x.io) is a link: its words and its target are text
        ((and (plusp (length raw)) link-to (cl-ppcre:scan "[/.:#]" target))
         (values (list (format nil "~a (~a)" raw target)) link-to kept))
        (t (values (group-items (subseq body i (1+ close)) notes) (1+ close) kept))))))

(defun scan-double (body i notes)
  "The double-bracket group at I => (VALUES ITEMS NEXT KEPT)."
  (let* ((end (search "]]" body :start2 (+ i 2)))
         (close (matching-bracket body i))
         (inner (and end (subseq body (+ i 2) end))))
    (cond
      ((null end) (drop notes "syntax" "[[" "unmatched") (values '() (1+ i) -1))
      ((and (find #\[ inner) close (in-code-p body i -1) (not (span-opener body (1+ i))))
       (values (list (subseq body i (1+ close))) (1+ close) close))
      ((header-shaped-p inner)
       (drop notes "header" "misplaced")
       (values (list +gap+) (+ end 2) -1))
      ((in-code-p body i -1)
       (values (list (subseq body i (+ end 2))) (+ end 2) (1+ end)))
      (t (values (append (list (if (or (ident-before-p body i) (call-before-p body i)) +gap+ ""))
                         (list inner) (list +gap+))
                 (+ end 2) -1)))))

(defun parse-score (text &optional data)
  "TEXT as a score => a SCORE. DATA is accepted and ignored: the tables are the cell's."
  (declare (ignore data))
  (multiple-value-bind (header body) (leading-header text)
    (let ((sentences '()) (tokens '()) (dropped '()) (kept -1) (i 0) (n (length body)))
      (labels ((flush ()
                 (when tokens
                   (push (make-sentence :tokens (nreverse tokens)) sentences)
                   (setf tokens nil)))
               (add-text (string)
                 (when (plusp (length string))
                   (if (and tokens (plain-p (first tokens)))
                       (setf (plain-s (first tokens))
                             (concatenate 'string (plain-s (first tokens)) string))
                       (push (make-plain :s string) tokens))))
               (plain (string)
                 (let ((pos 0))
                   (dolist (end (boundary-positions string))
                     (add-text (subseq string pos end))
                     (flush)
                     (setf pos end))
                   (add-text (subseq string pos))))
               (add-items (items)
                 (dolist (item items)
                   (if (stringp item) (add-text item) (push item tokens)))))
        (loop while (< i n) do
          (let ((char (char body i)))
            (cond
              ((char= char #\]) (drop dropped "syntax" "]" "stray") (incf i))
              ((char/= char #\[)
               (let ((end (plain-end body i n)))
                 (plain (subseq body i end))
                 (setf i end)))
              ((and (< (1+ i) n) (char= (char body (1+ i)) #\[))
               (multiple-value-bind (items next new-kept) (scan-double body i dropped)
                 (add-items items)
                 (setf kept new-kept i next)))
              ((null (matching-bracket body i))
               (drop dropped "syntax" "[" "unmatched")
               (incf i))
              (t (multiple-value-bind (items next new-kept) (scan-single body i kept dropped)
                   (add-items items)
                   (setf kept new-kept i next))))))
        (flush)
        (make-score :header header :sentences (nreverse sentences) :source text
                    :dropped (nreverse dropped))))))
