;;;; text.lisp --- Godot's text, read the way Godot writes it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Readers, no Godot and no network: a quoted Godot string with its escapes,
;;;; an INI-style header line ([node name="Player" type="Node2D"]), and
;;;; KEY = VALUE. The two file formats this cell reads -- project.godot, whose
;;;; values run over several lines, and *.tscn -- are both these in different
;;;; arrangements.

(in-package #:nodecode-game-editor)

;;; --- a string Godot wrote ------------------------------------------------------

(defun blankp (char)
  "Whether CHAR is one of the two blanks a header separates on."
  (or (char= char #\Space) (char= char #\Tab)))

(defun read-godot-string (text start)
  "The string that opens at TEXT's START, one past its quote => its value and
the index just past its closing quote. Godot escapes quote, backslash, n, t
and r; any other escaped character is that character."
  (let ((out (make-string-output-stream)) (i start) (n (length text)) (done nil))
    (loop while (and (< i n) (not done))
          do (let ((char (char text i)))
               (cond ((char= char #\") (incf i) (setf done t))
                     ((and (char= char #\\) (< (1+ i) n))
                      (let ((escaped (char text (1+ i))))
                        (write-char (case escaped
                                      (#\n #\Newline) (#\t #\Tab) (#\r #\Return)
                                      (t escaped))
                                    out))
                      (incf i 2))
                     (t (write-char char out) (incf i)))))
    (values (get-output-stream-string out) i)))

(defun plain-value (text)
  "TEXT without the quotes around a Godot string and with its escapes read;
anything else -- a Vector2(...), a NodePath(...), a bare number -- as it is
written."
  (if (and (> (length text) 1)
           (char= #\" (char text 0))
           (char= #\" (char text (1- (length text)))))
      (read-godot-string text 1)
      text))

;;; --- header lines: [node name="Player" type="Node2D"] -------------------------

(defun header-line-p (line)
  "Whether LINE opens a section, which in .tscn and project.godot means it
starts with a square bracket."
  (and (plusp (length line)) (char= #\[ (char line 0))))

(defun opener-p (char)
  "Whether CHAR opens a bracketed value: a square bracket, a brace or a
parenthesis."
  (or (char= char #\[) (char= char #\{) (char= char (code-char 40))))

(defun closer-p (char)
  "Whether CHAR closes a bracketed value: a square bracket, a brace or a
parenthesis."
  (or (char= char #\]) (char= char #\}) (char= char (code-char 41))))

(defun read-bracketed (text start)
  "The bracketed value that opens at TEXT's START -- a node's groups=[...],
an instance=ExtResource(...) -- => its text and the index just past its
closing bracket, quotes respected."
  (let ((depth 0) (in-string nil) (i start) (n (length text)) (done nil))
    (loop while (and (< i n) (not done))
          do (let ((char (char text i)))
               (cond ((char= char #\\) (incf i))
                     ((char= char #\") (setf in-string (not in-string)))
                     ((and (not in-string) (opener-p char)) (incf depth))
                     ((and (not in-string) (closer-p char)) (decf depth)))
               (incf i)
               (when (and (<= depth 0) (not in-string) (> i start))
                 (setf done t))))
    (values (subseq text start i) i)))

(defun parse-attributes (text)
  "TEXT's KEY=VALUE pairs, keys downcased, in the order written: a quoted
value may hold blanks, a bracketed one may hold anything."
  (let ((attrs '()) (i 0) (n (length text)))
    (loop
      (loop while (and (< i n) (blankp (char text i))) do (incf i))
      (unless (< i n) (return))
      (let ((start i))
        (loop while (and (< i n) (char/= #\= (char text i))) do (incf i))
        (let ((key (string-trim " " (subseq text start (min i n)))))
          (when (< i n) (incf i))
          (multiple-value-bind (value next)
              (cond ((and (< i n) (char= #\" (char text i)))
                     (read-godot-string text (1+ i)))
                    ((and (< i n) (opener-p (char text i)))
                     (read-bracketed text i))
                    (t (let ((from i))
                         (loop while (and (< i n) (not (blankp (char text i))))
                               do (incf i))
                         (values (subseq text from i) i))))
            (setf i next)
            (when (plusp (length key))
              (push (cons (string-downcase key) value) attrs))))))
    (nreverse attrs)))

(defun header-name (line)
  "LINE's section name: what a [NAME ...] header opens with, downcased."
  (let* ((text (string-left-trim " \t" line))
         (end (or (position-if (lambda (char) (or (blankp char) (char= char #\])))
                                text)
                  (length text))))
    (string-downcase (subseq text 1 end))))

(defun parse-header (line)
  "LINE as [KIND key=value ...] => (VALUES kind attributes), KIND downcased.
Only the outer brackets go: a node's groups=[...] keeps its own close."
  (let* ((text (string-trim " \t" line))
         (text (if (and (plusp (length text)) (char= #\[ (char text 0)))
                   (subseq text 1 (if (char= #\] (char text (1- (length text))))
                                      (1- (length text))
                                      (length text)))
                   text))
         (space (position-if #'blankp text))
         (kind (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (subseq text space) "")))
    (values kind (parse-attributes rest))))

;;; --- a value line: position = Vector2(120, 64) --------------------------------

(defun split-assignment (line)
  "LINE as KEY = VALUE at the first equals sign outside a string
=> (VALUES key value), or NIL when the line carries no assignment."
  (let ((in-string nil) (found nil) (i 0) (n (length line)))
    (loop while (and (< i n) (not found))
          do (let ((char (char line i)))
               (cond ((char= char #\\) (incf i))
                     ((char= char #\") (setf in-string (not in-string)))
                     ((and (char= char #\=) (not in-string)) (setf found i)))
               (incf i)))
    (when (and found (plusp found))
      (values (string-trim " " (subseq line 0 found))
              (string-trim " " (subseq line (1+ found)))))))

;;; --- a whole file of them ------------------------------------------------------

(defun comment-line-p (line)
  "Whether LINE is blank or a semicolon comment, the two a section skips."
  (let ((trimmed (string-left-trim " \t" line)))
    (or (zerop (length trimmed)) (char= #\; (char trimmed 0)))))

(defun parse-ini (text)
  "TEXT as an INI-style file => (SECTION (KEY . VALUE) ...) ..., in file
order, keys as written and section names downcased. A line that is neither a
header nor an assignment continues the value above it, which is how Godot
writes the input map and a long array."
  (let ((sections '()) (current nil) (last nil))
    (dolist (line (nlk:lines text))
      (let ((line (string-right-trim " \t" line)))
        (cond ((comment-line-p line))
              ((header-line-p (string-left-trim " \t" line))
               (setf current (cons (header-name line) nil)
                     last nil)
               (push current sections))
              ((and current (split-assignment line))
               (multiple-value-bind (key value) (split-assignment line)
                 (setf last (cons key value))
                 (push last (cdr current))))
              (last
               (setf (cdr last) (concatenate 'string (cdr last) " "
                                             (string-trim " \t" line)))))))
    (mapcar (lambda (section) (cons (car section) (nreverse (cdr section))))
            (nreverse sections))))

(defun ini-section (ini name)
  "INI's section NAME, as its (KEY . VALUE) pairs, or NIL."
  (cdr (assoc (string-downcase name) ini :test #'string=)))

(defun ini-get (ini section key)
  "INI's KEY in SECTION, as the file spells it, or NIL."
  (cdr (assoc key (ini-section ini section) :test #'string=)))

;;; --- rows, cut the one way -----------------------------------------------------

(defun rows-text (rows offset limit &key (more "pass offset ~d"))
  "ROWS from OFFSET, at most LIMIT of them, and a last line counting what was
left, so a reader knows how to ask for the rest."
  (let* ((total (length rows))
         (start (min (max 0 (or offset 0)) total))
         (end (min total (+ start (max 1 (or limit 1))))))
    (format nil "~{~a~^~%~}~@[~%~d more row~:p; ~a~]"
            (subseq rows start end) (- total end)
            (and (> total end) (format nil more end)))))
