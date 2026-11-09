;;;; text.lisp --- the lines, the fenced regions, and the formatter.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here reads a document as LINES and asks one question of
;;;; each: is it inside a fenced code block? FENCE-MASK answers that once,
;;;; and every check and the formatter share the answer, so a rule never
;;;; fires on the inside of a code block. The scan is line-oriented, not a
;;;; parse of GFM: an ATX heading, a pipe table, an ordered list, a fenced
;;;; block and a ledger record are recognised by their opening shape, and a
;;;; construct this reader does not know (setext headings, HTML blocks) is
;;;; left alone rather than guessed at. That is the honest subset the README
;;;; names.

;;; --- lines ------------------------------------------------------------------

(in-package #:nodecode-wolfmarkdown)

(defun lines-of (text)
  "TEXT's lines as a vector. A text ending in a newline has a last empty
line, so the count a check reports is the count an editor shows."
  (let ((lines '())
        (start 0)
        (length (length text)))
    (loop for index from 0 below length
          when (char= (char text index) #\Newline)
            do (push (subseq text start index) lines)
               (setf start (1+ index)))
    (push (subseq text start length) lines)
    (coerce (nreverse lines) 'vector)))

(defun join-lines (lines)
  "LINES joined with a newline between and one after the last."
  (format nil "~{~a~%~}" (coerce lines 'list)))

(defun blank-line-p (line)
  "Whether LINE holds nothing but whitespace."
  (every (lambda (character) (member character '(#\Space #\Tab #\Return))) line))

(defun indent-width (line)
  "How many columns LINE's leading whitespace covers, a tab to the next stop of four."
  (let ((width 0)
        (index 0))
    (loop while (< index (length line))
          for character = (char line index)
          do (cond ((char= character #\Space) (incf width) (incf index))
                   ((char= character #\Tab) (incf width (- 4 (mod width 4))) (incf index))
                   (t (return))))
    width))

(defun count-run (string start character)
  "How many CHARACTERs in a row STRING has from START."
  (let ((end start))
    (loop while (and (< end (length string)) (char= (char string end) character))
          do (incf end))
    (- end start)))

(defun strip-quotes (line)
  "LINE with its blockquote prefixes taken off, so '> > `x`' is '`x`'."
  (let ((start 0))
    (loop
      (let ((index start)
            (spaces 0))
        (loop while (and (< index (length line)) (char= (char line index) #\Space) (< spaces 4))
              do (incf index)
                 (incf spaces))
        (if (and (< index (length line)) (char= (char line index) #\>))
            (progn (setf start (1+ index))
                   (when (and (< start (length line)) (char= (char line start) #\Space))
                     (setf start (1+ start))))
            (return))))
    (subseq line start)))

(defun leading-space-count (line)
  "How many spaces LINE starts with, counting nothing else."
  (let ((index 0))
    (loop while (and (< index (length line)) (char= (char line index) #\Space))
          do (incf index))
    index))

;;; --- fenced code ------------------------------------------------------------

(defun fence-open (body)
  "(CHAR . LENGTH) when BODY opens a fence: three or more backticks or
tildes, indented at most three columns, no backtick in a backtick fence's
info string. => NIL for a line that opens nothing."
  (let* ((indent (leading-space-count body))
         (character (and (< indent (length body)) (char body indent))))
    (when (and (<= indent 3) character (member character '(#\` #\~)))
      (let ((length (count-run body indent character)))
        (when (>= length 3)
          (let ((info (subseq body (+ indent length))))
            (unless (and (char= character #\`) (find #\` info))
              (cons character length))))))))

(defun fence-close-p (body open)
  "Whether BODY closes the fence OPEN opened: the same character, a run at
least as long, and nothing else on the line."
  (let* ((indent (leading-space-count body))
         (character (and (< indent (length body)) (char body indent))))
    (and (<= indent 3)
         character
         (char= character (car open))
         (let ((length (count-run body indent character)))
           (and (>= length (cdr open))
                (blank-line-p (subseq body (+ indent length))))))))

(defun fence-mask (lines)
  "A vector with T for every line inside a fenced block, its opening and
closing lines included. => (values MASK OPEN-LINE), OPEN-LINE the 1-based
line an unclosed fence opened on, or NIL when every fence closed."
  (let* ((count (length lines))
         (mask (make-array count :initial-element nil))
         (open nil)
         (open-line nil))
    (dotimes (index count)
      (let ((body (strip-quotes (aref lines index))))
        (cond (open
               (setf (aref mask index) t)
               (when (fence-close-p body open)
                 (setf open nil open-line nil)))
              (t
               (let ((opened (fence-open body)))
                 (when opened
                   (setf open opened
                         open-line (1+ index)
                         (aref mask index) t)))))))
    (values mask open-line)))

;;; --- the shapes a check reads -------------------------------------------------

(defun heading-of (line)
  "The depth of the ATX heading LINE opens and its text, or NIL for a line
that opens none."
  (let* ((body (strip-quotes line))
         (indent (leading-space-count body))
         (depth (and (<= indent 3) (count-run body indent #\#))))
    (when (and depth (<= 1 depth 6)
               (< (+ indent depth) (length body))
               (member (char body (+ indent depth)) '(#\Space #\Tab)))
      (values depth (string-trim '(#\Space #\Tab #\Return)
                                 (subseq body (+ indent depth)))))))

(defun ordered-item-of (line)
  "(VALUES NUMBER INDENT TEXT) when LINE opens an ordered list item: at
most three leading spaces, up to nine digits, a dot or a closing paren, and
a space. => NIL otherwise."
  (let* ((body (strip-quotes line))
         (indent (leading-space-count body))
         (digits 0))
    (when (<= indent 3)
      (loop while (and (< (+ indent digits) (length body))
                       (digit-char-p (char body (+ indent digits))))
            do (incf digits))
      (let ((after (+ indent digits)))
        (when (and (plusp digits) (<= digits 9) (< after (length body))
                   (member (char body after) '(#\. #\))))
          (let ((start (1+ after)))
            (when (and (< start (length body))
                       (member (char body start) '(#\Space #\Tab)))
              (values (parse-integer (subseq body indent after))
                      indent
                      (string-trim '(#\Space #\Tab #\Return) (subseq body start))))))))))

(defun unordered-item-of (line)
  "(VALUES MARKER INDENT TEXT) when LINE opens an unordered list item."
  (let* ((body (strip-quotes line))
         (indent (leading-space-count body)))
    (when (and (< indent (length body))
               (member (char body indent) '(#\- #\* #\+))
               (< (1+ indent) (length body))
               (member (char body (1+ indent)) '(#\Space #\Tab)))
      (values (char body indent)
              indent
              (string-trim '(#\Space #\Tab #\Return) (subseq body (1+ indent)))))))

(defun record-of (line)
  "(VALUES LABEL ANCHOR) when LINE is a ledger record: an optional list
marker, a bracketed label of one to three digits, and the value after it."
  (let* ((body (strip-quotes line))
         (indent (leading-space-count body)))
    (when (<= indent 3)
      (let ((start indent))
        (when (and (< (1+ start) (length body))
                   (member (char body start) '(#\- #\* #\+))
                   (char= (char body (1+ start)) #\Space))
          (incf start 2))
        (when (and (< start (length body)) (char= (char body start) #\[))
          (let ((digits 0))
            (loop while (and (< (+ start 1 digits) (length body))
                             (digit-char-p (char body (+ start 1 digits))))
                  do (incf digits))
            (let ((after (+ start 1 digits)))
              (when (and (plusp digits) (<= digits 3) (< after (length body))
                         (char= (char body after) #\]))
                (let ((rest (string-left-trim '(#\Space #\Tab)
                                               (subseq body (1+ after)))))
                  (when (plusp (length rest))
                    (values (subseq body (1+ start) (1+ digits))
                            (first (nlk:split-words rest)))))))))))))

;;; --- tables -----------------------------------------------------------------

(defun split-pipes (line)
  "LINE's cells, split on unescaped pipes, a leading and trailing pipe removed."
  (let ((text (string-trim '(#\Space #\Tab) line)))
    (when (and (plusp (length text)) (char= (char text 0) #\|))
      (setf text (subseq text 1)))
    (when (and (plusp (length text)) (char= (char text (1- (length text))) #\|))
      (setf text (subseq text 0 (1- (length text)))))
    (let ((cells '())
          (current (make-string-output-stream)))
      (loop for index from 0 below (length text)
            for character = (char text index)
            do (if (char= character #\|)
                   (let ((slashes 0))
                     (loop for back from (1- index) downto 0
                           while (char= (char text back) #\\)
                           do (incf slashes))
                     (if (oddp slashes)
                         (write-char character current)
                         (progn (push (get-output-stream-string current) cells))))
                   (write-char character current)))
      (push (get-output-stream-string current) cells)
      (mapcar (lambda (cell) (string-trim '(#\Space #\Tab) cell)) (nreverse cells)))))

(defun has-unescaped-pipe-p (line)
  "Whether LINE carries a pipe that no backslash escapes."
  (loop for index from 0 below (length line)
        when (char= (char line index) #\|)
          do (let ((slashes 0))
               (loop for back from (1- index) downto 0
                     while (char= (char line back) #\\)
                     do (incf slashes))
               (when (evenp slashes) (return t)))))

(defun delimiter-cell-p (cell)
  "Whether CELL is dashes with optional alignment colons."
  (let* ((text (string-trim '(#\Space #\Tab) cell))
         (start (if (and (plusp (length text)) (char= (char text 0) #\:)) 1 0))
         (end (if (and (> (length text) start)
                       (char= (char text (1- (length text))) #\:))
                  (1- (length text))
                  (length text))))
    (and (> end start)
         (every (lambda (character) (char= character #\-)) (subseq text start end)))))

(defun delimiter-row-p (line)
  "Whether LINE is a GFM delimiter row: every cell dashes with optional
alignment colons."
  (and (has-unescaped-pipe-p line)
       (let ((cells (split-pipes line)))
         (and cells (every #'delimiter-cell-p cells)))))

(defun table-line-p (line)
  "Whether LINE can be a table row: it carries a pipe and opens nothing
else (a list item, a quote, a heading)."
  (let* ((body (strip-quotes line))
         (indent (leading-space-count body))
         (head (and (< indent (length body)) (char body indent))))
    (and (has-unescaped-pipe-p body)
         (not (member head '(#\- #\* #\+ #\> #\#)))
         (not (and head (digit-char-p head) (ordered-item-of line))))))

;;; --- inline code and citation markers ----------------------------------------

(defun inline-code-spans (line)
  "The (START . END) spans of LINE's inline code, one backtick run each.
A span covers its backticks, so masking it keeps every column."
  (let ((spans '())
        (index 0)
        (length (length line)))
    (loop while (< index length)
          do (if (char= (char line index) #\`)
                 (let* ((width (count-run line index #\`))
                        (closing (search (make-string width :initial-element #\`) line
                                         :start2 (+ index width))))
                   (if closing
                       (progn (push (cons index (+ closing width)) spans)
                              (setf index (+ closing width)))
                       (setf index (+ index width))))
                 (incf index)))
    (nreverse spans)))

(defun mask-spans (line spans)
  "LINE with every (START . END) span in SPANS blanked to spaces."
  (let ((copy (copy-seq line)))
    (dolist (span spans copy)
      (loop for index from (car span) below (min (cdr span) (length copy))
            do (setf (char copy index) #\Space)))))

(defun escaped-at-p (line index)
  "Whether the character at INDEX is escaped by an odd run of backslashes."
  (let ((slashes 0))
    (loop for back from (1- index) downto 0
          while (char= (char line back) #\\)
          do (incf slashes))
    (oddp slashes)))

(defun adjacent-citations (line)
  "The (LABEL-LEFT LABEL-RIGHT COLUMN) triples of LINE's adjacent numeric
citations -- [6][26], which GFM reads as one reference link. COLUMN is the
1-based index of the closing bracket to the left, where the space goes."
  (let ((found '())
        (length (length line)))
    (labels ((digits (from)
               (let ((end from))
                 (loop while (and (< end length) (digit-char-p (char line end)))
                       do (incf end))
                 (- end from))))
      (loop for index from 0 below length
            when (and (char= (char line index) #\[)
                      (plusp (digits (1+ index)))
                      (<= (digits (1+ index)) 3))
              do (let* ((width (digits (1+ index)))
                        (close (+ index 1 width)))
                   (when (and (< close length)
                              (char= (char line close) #\])
                              (< (1+ close) length)
                              (char= (char line (1+ close)) #\[)
                              (plusp (digits (+ close 2)))
                              (<= (digits (+ close 2)) 3))
                     (let* ((right (+ close 2))
                            (right-width (digits right))
                            (right-close (+ right right-width)))
                       (when (and (< right-close length)
                                  (char= (char line right-close) #\])
                                  (not (escaped-at-p line index)))
                         (push (list (subseq line (1+ index) close)
                                     (subseq line right right-close)
                                     (1+ close))
                               found))))))
      (nreverse found))))

(defun separate-citations (line)
  "(VALUES LINE FIXED): LINE with a space put between every adjacent pair
of numeric citations found outside inline code."
  (let* ((spans (inline-code-spans line))
         (found (adjacent-citations (mask-spans line spans)))
         (result line)
         (applied 0))
    (dolist (triple (reverse found))
      (let ((column (third triple)))
        (setf result (concatenate 'string
                                  (subseq result 0 column)
                                  " "
                                  (subseq result column)))
        (incf applied)))
    (values result applied)))

;;; --- the formatter ----------------------------------------------------------

(defun trailing-hard-break-p (line)
  "Whether LINE ends in exactly two spaces: a Markdown hard break, kept."
  (let ((end (length line)))
    (and (>= end 2)
         (char= (char line (1- end)) #\Space)
         (char= (char line (- end 2)) #\Space)
         (not (and (>= end 3) (char= (char line (- end 3)) #\Space))))))

(defun trim-trailing (line)
  "LINE without trailing whitespace, its hard break kept when it has one."
  (let ((text (string-right-trim '(#\Space #\Tab #\Return) line)))
    (if (trailing-hard-break-p line) (concatenate 'string text "  ") text)))

(defun normalise-list-marker (line list-style)
  "LINE with an unordered marker in LIST-STYLE and an ordered marker
written with a dot."
  (let* ((body (strip-quotes line))
         (indent (leading-space-count body))
         (want (char list-style 0)))
    (cond
      ((and (< (1+ indent) (length body))
            (member (char body indent) '(#\- #\* #\+))
            (member (char body (1+ indent)) '(#\Space #\Tab)))
       (concatenate 'string (subseq line 0 indent) (string want) (subseq line (1+ indent))))
      (t
       (multiple-value-bind (number item-indent text) (ordered-item-of line)
         (declare (ignore text))
         (if (null number)
             line
             (let ((dot (+ item-indent (length (princ-to-string number)))))
               (if (char= (char body dot) #\.)
                   line
                   (concatenate 'string (subseq line 0 dot) "." (subseq line (1+ dot)))))))))))

(defun line-kind (line &optional previous-kind)
  "What LINE is for the blank-line rules: :heading, :fence, :list-item,
:blank or :text. PREVIOUS-KIND makes an indented continuation line part of
the list item above it."
  (cond ((blank-line-p line) :blank)
        ((heading-of line) :heading)
        ((ordered-item-of line) :list-item)
        ((unordered-item-of line) :list-item)
        ((and (eq previous-kind :list-item)
              (>= (indent-width (strip-quotes line)) 2))
         :list-item)
        (t :text)))

(defun blank-needed-p (previous next)
  "Whether a blank line belongs between a PREVIOUS and a NEXT line kind."
  (or (eq next :heading)
      (eq previous :heading)
      (eq next :fence)
      (eq previous :fence)
      (and (eq next :list-item) (not (eq previous :list-item)))
      (and (eq previous :list-item) (not (eq next :list-item)))))

(defun clean-lines (lines list-style)
  "LINES in canonical form: no trailing whitespace but a hard break, one
blank line at most, a blank line around headings, fences and lists, the
LIST-STYLE unordered marker, a dot after every ordered one."
  (let ((mask (fence-mask lines))
        (out '())
        (previous-kind nil)
        (pending-blank nil))
    (labels ((emit (line kind)
               (push line out)
               (setf previous-kind kind
                     pending-blank nil))
             (emit-blank ()
               (when (and out (not pending-blank))
                 (push "" out)
                 (setf pending-blank t))))
      (dotimes (index (length lines))
        (let ((line (aref lines index)))
          (cond
            ((aref mask index)
             (when (and (fence-open (strip-quotes line))
                        previous-kind
                        (not (eq previous-kind :fence)))
               (emit-blank))
             (emit line :fence))
            ((blank-line-p line)
             (emit-blank))
            (t
             (let ((kind (line-kind line previous-kind)))
               (when (and previous-kind (blank-needed-p previous-kind kind))
                 (emit-blank))
               (emit (normalise-list-marker (trim-trailing line) list-style) kind))))))
      (let ((result (nreverse out)))
        (loop while (and result (equal (car (last result)) ""))
              do (setf result (butlast result)))
        result))))

;;; --- the published formatter ---------------------------------------------------

(defun list-style ()
  "The unordered marker the formatter writes, from the section when the cell
runs and '-' when it does not."
  (or (ignore-errors (setting :list-style)) "-"))

(defun clean-text (text)
  "TEXT in the cell's canonical Markdown form. Fenced blocks are kept as
they are written, every other line loses its trailing whitespace, runs of
blank lines become one, headings and fences get the blank line around them,
list markers are normalised, and the file ends with exactly one newline.
=> (values CLEANED SAFE-FIXES), SAFE-FIXES the adjacent citations separated."
  (let* ((lines (lines-of text))
         (mask (fence-mask lines))
         (separated 0)
         (fixed (make-array (length lines) :initial-element "")))
    (dotimes (index (length lines))
      (multiple-value-bind (line count)
          (if (aref mask index)
              (values (aref lines index) 0)
              (separate-citations (aref lines index)))
        (setf (aref fixed index) line)
        (incf separated count)))
    (values (join-lines (clean-lines fixed (list-style))) separated)))
