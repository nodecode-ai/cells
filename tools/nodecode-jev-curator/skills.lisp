;;;; skills.lisp --- the inventory: SKILL.md packages under the section's roots.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A skill is one SKILL.md, as Hermes and Nodecode both write one: a
;;;; leading --- block naming the skill and its description, then the steps.
;;;; Its digest is the sha256 of that file's bytes, which is what every
;;;; judgment is bound to and what an apply re-checks before it moves a
;;;; folder. Reading is bounded per file and per walk, so a stray huge file
;;;; under a root costs one refusal, never a scan that never ends.

(in-package #:nodecode-jev-curator)

(defparameter +skill-file-name+ "SKILL.md"
  "The one file a skill is.")

(defparameter +skill-byte-cap+ 262144
  "The most bytes of one SKILL.md that are read: 256 KiB, the bound the
original applies to one package.")

(defparameter +walk-depth+ 12
  "How deep under a root the walk goes.")

(defparameter +skip-directories+
  '(".git" ".hg" ".svn" ".venv" "node_modules" "__pycache__" ".cache" "vendor" "archive" "archived")
  "Folders the walk never enters.")

(defstruct (skill (:constructor %make-skill (name path description text digest)))
  "One SKILL.md: NAME as its front matter writes it, PATH the file, DESCRIPTION
one line, TEXT the body, DIGEST the sha256 of the whole file."
  (name "" :read-only t)
  (path nil :read-only t)
  (description "" :read-only t)
  (text "" :read-only t)
  (digest "" :read-only t))

(defun skill-roots ()
  "The section's roots as directory pathnames."
  (mapcar (lambda (root) (uiop:ensure-directory-pathname (nlk:expand-home root)))
          (or (setting :roots) '())))

(defun skill-files (root)
  "Every (PATH . ID) under ROOT that is a SKILL.md, ID its folder below ROOT;
symlink cycles and the skipped folders are never entered."
  (let ((out '())
        (seen (make-hash-table :test #'equal)))
    (labels ((walk (directory id depth)
               (when (> depth +walk-depth+) (return-from walk))
               (let ((key (ignore-errors (namestring (uiop:truename* directory)))))
                 (when (and key (gethash key seen)) (return-from walk))
                 (when key (setf (gethash key seen) t)))
               (dolist (file (ignore-errors (uiop:directory-files directory)))
                 (when (string-equal (file-namestring file) +skill-file-name+)
                   (push (cons file (if (plusp (length id)) id (file-namestring directory))) out)))
               (dolist (sub (ignore-errors (uiop:subdirectories directory)))
                 (let ((name (car (last (pathname-directory sub)))))
                   (when (and (stringp name) (plusp (length name))
                              (not (char= #\. (char name 0)))
                              (not (member name +skip-directories+ :test #'string-equal)))
                     (walk sub (if (plusp (length id)) (format nil "~a/~a" id name) name)
                           (1+ depth)))))))
      (walk (uiop:ensure-directory-pathname root) "" 0)
      (sort (nreverse out) (lambda (a b) (string< (cdr a) (cdr b)))))))

(defun split-front-line (line)
  "LINE as (KEY . VALUE) when it reads `key: value', KEY lowercased, VALUE
without its surrounding quotes; NIL otherwise."
  (let ((colon (position #\: line)))
    (when (and colon (plusp colon))
      (let ((key (string-downcase (nlk:trimmed (subseq line 0 colon))))
            (value (nlk:trimmed (subseq line (1+ colon)))))
        (when (and (plusp (length value)) (member (char value 0) '(#\" #\')))
          (setf value (nlk:trimmed (string-trim (string (char value 0)) value))))
        (when (plusp (length key)) (cons key value))))))

(defun front-matter (text)
  "TEXT's leading --- block as an alist of lowercased keys, and the body
after it; a text with no block is all body."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (meta '()))
    (if (and lines (string= (nlk:trimmed (first lines)) "---"))
        (let ((rest (rest lines)))
          (loop for line in rest
                for index from 0
                do (cond ((string= (nlk:trimmed line) "---")
                          (return (values (nreverse meta)
                                          (format nil "~{~a~^~%~}" (nthcdr (1+ index) rest)))))
                         (t (let ((pair (split-front-line line)))
                              (when pair (push pair meta)))))
                finally (return (values (nreverse meta) ""))))
        (values '() text))))

(defun meta-value (meta key)
  "META's value under KEY, a lowercased string; NIL when absent or blank."
  (let ((pair (assoc key meta :test #'equal)))
    (and pair (nonblank (cdr pair)))))

(defun first-body-line (body)
  "BODY's first non-blank line with its leading # marks off, as a description."
  (loop for line in (uiop:split-string body :separator '(#\Newline))
        for trimmed = (nlk:trimmed line)
        when (plusp (length trimmed))
          return (nlk:clip (nlk:trimmed (string-left-trim "#" trimmed)) 160 :ellipsis "...")
        finally (return "")))

(defun parse-skill (path id)
  "PATH read as a skill named ID when its front matter names none, or NIL
when the file is absent, unreadable or past the cap."
  (let ((text (nlk:read-text path +skill-byte-cap+)))
    (when text
      (multiple-value-bind (meta body) (front-matter text)
        (%make-skill (or (meta-value meta "name") id)
                     path
                     (or (meta-value meta "description") (first-body-line body))
                     body
                     (nlk:sha256-text text))))))

(defun collect-inventory (&key roots)
  "Every skill under ROOTS (the section's roots when it names none), one per
name, sorted by name; a root that is absent yields nothing."
  (let ((seen (make-hash-table :test #'equal))
        (out '()))
    (dolist (root (or roots (skill-roots)))
      (dolist (entry (skill-files root))
        (let ((skill (handler-case (parse-skill (car entry) (cdr entry))
                       (error () nil))))
          (when (and skill (not (gethash (skill-name skill) seen)))
            (setf (gethash (skill-name skill) seen) t)
            (push skill out)))))
    (sort (nreverse out) #'string< :key #'skill-name)))

(defun protected-p (name &optional (protected (setting :protected)))
  "Whether NAME is on the section's protected list: a skill apply never moves."
  (and protected (member name protected :test #'equal) t))
