;;;; cite.lisp --- the citations a brief makes, and what the pack says about each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two scans over a folded copy of the brief. The first finds the statutes a
;;;; passage names (codigo civil, ley 19.880, decreto ley 3.500). The second
;;;; finds what the original plugin's grammar finds: an article prefix -- art.,
;;;; articulo, articulos, arts. -- then an enumeration of numbers (1545,
;;;; numero 9999, articulo primero), each with its square-degree, its Latin
;;;; suffix (bis, ter, quater, ...) and, after a suffix, a letter (152 quater
;;;; N). A citation is paired with the statute its sentence names; when the
;;;; sentence names none, it is unresolvable, which is the safe direction.

(in-package #:nodecode-dike)

(defstruct (citation (:constructor make-citation (start end raw token norm)))
  "One article the brief invokes: where it sits, its comparable token, and the
statute whose name it is read against (a slug, or NIL)."
  start end raw token norm)

(defstruct (named (:constructor make-named (start end designation slugs)))
  "One statute a passage names: its designation as written and the slugs it can
mean (none when the pack does not carry it)."
  start end designation slugs)

;;; --- small scans --------------------------------------------------------------------

(defun match-at (s i prefix)
  "Whether PREFIX stands at I."
  (and (<= (+ i (length prefix)) (length s))
       (string= prefix s :start2 i :end2 (+ i (length prefix)))))

(defun word-start-p (s i)
  "Whether a word begins at I."
  (or (zerop i) (not (alphanumericp (char s (1- i))))))

(defun word-end-p (s j)
  "Whether a word ends at J."
  (or (>= j (length s)) (not (alphanumericp (char s j)))))

(defun skip-spaces (s j)
  "J past any whitespace."
  (loop while (and (< j (length s))
                    (member (char s j) '(#\Space #\Tab #\Newline #\Return)))
        do (incf j) finally (return j)))

(defun digits-at (s j)
  "The index past the number at J: digits, with a dot only between digits, so
the full stop that ends a sentence stays out of the number."
  (let ((k j))
    (loop while (and (< k (length s)) (digit-char-p (char s k))) do (incf k))
    (loop while (and (< (1+ k) (length s)) (char= (char s k) #\.)
                     (digit-char-p (char s (1+ k))))
          do (incf k 2)
             (loop while (and (< k (length s)) (digit-char-p (char s k))) do (incf k)))
    k))

;;; --- the statutes a passage names --------------------------------------------------

(defparameter +ordinal-words+
  '("primero" "segundo" "tercero" "cuarto" "quinto" "sexto" "septimo" "octavo"
    "noveno" "decimo" "undecimo" "duodecimo" "final"))

(defparameter +latin-suffixes+
  '("quinquies" "quater" "sexies" "septies" "octies" "nonies" "bis" "ter"))

(defparameter +number-words+ '("numeros" "numero" "nros" "nro" "num"))

(defun number-word-end (s j)
  "The index past a word that introduces a number (numero, nro, num) at J, or NIL."
  (loop for word in +number-words+
        when (and (match-at s j word) (word-end-p s (+ j (length word))))
          return (let ((k (+ j (length word))))
                   (if (and (< k (length s)) (char= (char s k) #\.)) (1+ k) k))))

(defun article-prefix-end (s i)
  "Where an article prefix starting at I ends, or NIL when none starts there."
  (when (and (word-start-p s i) (match-at s i "art"))
    (let ((j (+ i 3)))
      (cond ((match-at s j "iculos") (incf j 6))
            ((match-at s j "iculo") (incf j 5))
            ((and (match-at s j "s") (word-end-p s (1+ j))) (incf j)))
      (when (and (< j (length s)) (char= (char s j) #\.)) (incf j))
      (setf j (skip-spaces s j))
      (cond ((match-at s j "n°") (skip-spaces s (+ j 2)))
            ((match-at s j "nº") (skip-spaces s (+ j 2)))
            (t (or (number-word-end s j) j))))))

(defun ordinal-at (s j)
  "The folded ordinal word at J, or NIL."
  (loop for word in +ordinal-words+
        when (and (match-at s j word) (word-end-p s (+ j (length word))))
          return word))

(defun suffix-at (s j)
  "The Latin suffix at J and the index past it, as two values, else NIL."
  (loop for word in +latin-suffixes+
        when (and (match-at s j word) (word-end-p s (+ j (length word))))
          return (values word (+ j (length word)))))

(defun article-letter (s j)
  "The letter that individualises an article after a suffix, and where it ends.
Only after a suffix: chilean law has no article `161 G', and a bare letter would
let the `y' of `articulos 15 ter y 16' pass for one. A conjunction needs its
closing parenthesis."
  (let ((k (skip-spaces s j)))
    (when (and (< k (length s)) (alpha-char-p (char s k)))
      (let* ((letter (string (char s k))) (m (1+ k))
             (closed (and (< m (length s)) (char= (char s m) #\))))
             (after (if closed (1+ m) m)))
        (cond ((and (< after (length s)) (alphanumericp (char s after))) nil)
              ((and (not closed) (member letter '("y" "e" "o" "u") :test #'equal)) nil)
              (t (values letter after)))))))

(defun citation-number-at (s j)
  "The token, the end of one cited article at J, and whether the match is a
transitory article: three values, NIL when no citation starts there."
  (cond
    ((> (digits-at s j) j)
     (let* ((end (digits-at s j))
            (n (remove #\. (subseq s j end)))
            (k (skip-spaces s (if (and (< end (length s)) (member (char s end) '(#\º #\°)))
                                  (1+ end) end))))
       (multiple-value-bind (suffix after) (suffix-at s k)
         (if suffix
             (multiple-value-bind (letter m) (article-letter s after)
               (values (if letter (format nil "~d-~a-~a" (parse-integer n) suffix letter)
                           (format nil "~d-~a" (parse-integer n) suffix))
                       (or m after)))
             (values (format nil "~d" (parse-integer n)) end)))))
    ((ordinal-at s j)
     (let ((word (ordinal-at s j))) (values word (+ j (length word)))))
    (t nil)))

(defun transitory-at (s j)
  "The token and end of a transitory article (art. t. 5, articulo transitorio
5) at J, or NIL. Read before the ordinal form, which alone names no article."
  (let ((k nil))
    (cond ((match-at s j "transitorio") (setf k (+ j 12)))
          ((match-at s j "transitoria") (setf k (+ j 12)))
          ((and (match-at s j "t") (word-end-p s (1+ j))) (setf k (1+ j))))
    (when k
      (when (and (< k (length s)) (char= (char s k) #\.)) (incf k))
      (let ((k2 (skip-spaces s k)))
        (let ((e (digits-at s k2)))
          (when (> e k2)
            (values (format nil "t-~d" (parse-integer (remove #\. (subseq s k2 e)))) e)))))))

(defun separator-end (s j)
  "Where a list separator at J ends, or NIL: a comma or semicolon, or the words
y and e, which must be followed by another citation."
  (let ((k (skip-spaces s j)))
    (cond ((>= k (length s)) nil)
          ((member (char s k) '(#\, #\;)) (skip-spaces s (1+ k)))
          ((and (member (char s k) '(#\y #\e))
                (word-start-p s k) (word-end-p s (1+ k)))
           (skip-spaces s (1+ k)))
          (t nil))))
