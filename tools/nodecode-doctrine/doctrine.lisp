;;;; doctrine.lisp --- the nine articles as data, the two renderings, and the
;;;; map from an enforcer the doctrine names to the cell that carries it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The wording lives in doctrine.md beside this file: the original plugin's
;;;; own axiom_agent/DOCTRINE.md, shipped unchanged and read with the image's
;;;; reader, so no second copy of the prose is kept in Lisp. An article is its
;;;; heading line through the rule that closes the list; its enforcers are the
;;;; names on its own *Enforced by:* line, which is the original's parse.
;;;;
;;;; Nothing here writes, dials or imports anything: one file is read from the
;;;; folder this cell sits in, and one folder listing of the home's cells is
;;;; probed, which is how the report knows what is installed.

(in-package #:nodecode-doctrine)

;;; --- the caps, and the prose that is the cell's one authority -------------------

(defparameter +doctrine-file+ "doctrine.md"
  "The doctrine's own file in this folder, the original's axiom_agent/DOCTRINE.md.")

(defparameter +compact-cap+ 1800
  "How much of the doctrine the compact form puts in a request, as the original capped it.")

(defparameter +full-cap+ 4000
  "How much of the doctrine the full form puts in a request, as the original capped it.")

(defparameter +additions-cap+ 800
  "How much of the owner's additions a request carries, label and truncation note included.")

(defparameter +compact-truncated+ "\n[Doctrine truncated; /doctrine show for the full text.]"
  "What the compact form ends with when the articles did not all fit.")

(defparameter +full-truncated+ "\n[Doctrine truncated; /doctrine show for the full text.]"
  "What the full form ends with when the articles did not all fit.")

(defparameter +additions-truncated+ "\n[Owner additions truncated.]"
  "What the owner's additions end with when they did not all fit.")

(defparameter *footer*
  "This cell enforces nothing itself; ENFORCED means a cell carrying an enforcer the article names is installed in this home, not verified coverage."
  "The line every rendering ends with, so a report is never read as a guarantee.")

(defparameter +enforcers+
  '(("hermes-axioma" . "nodecode-axioma")
    ("hermes-gatekeeper" . nil)
    ("standing" . nil)
    ("tripwire" . "nodecode-tripwire")
    ("handydandy" . "nodecode-handydandy"))
  "Each enforcer plugin the doctrine names, and the cell that carries it, or NIL
when no cell does yet. The doctrine's wording is the original's; this map is
what makes its report mean something in this harness.")

(defparameter +usage+
  "Usage: /doctrine [status | show [1-9 or I-IX] | plugins]"
  "What the command answers with when it does not understand its argument.")

;;; --- one article -----------------------------------------------------------------

(defstruct (article (:constructor make-article (number title text)))
  "One article: its roman NUMBER, its TITLE, and its TEXT, which is the
heading line through the rule that closes the list."
  (number "" :type string)
  (title "" :type string)
  (text "" :type string))

;;; --- the file ------------------------------------------------------------------

(defun cell-directory ()
  "The folder this cell's source sits in, which holds doctrine.md."
  ;; The .asd's own directory, which ASDF keeps even after a peripheral is
  ;; compiled into the cache; the loaded file's directory is the fallback for
  ;; a folder loaded by hand.
  (or (ignore-errors (asdf:system-source-directory "nodecode-doctrine"))
      (ignore-errors (uiop:pathname-directory-pathname
                      (or *load-truename* *compile-file-truename*)))))

(defun doctrine-path ()
  "The doctrine's path in this folder."
  (merge-pathnames +doctrine-file+ (cell-directory)))

(defvar *text* nil
  "The doctrine's bytes, read once; a layer that rewrites the file restarts the cell to reread it.")

(defvar *articles* nil
  "The parsed articles, in order, built on the first read.")

(defun read-doctrine ()
  "The doctrine's text, or a refusal naming the file that ships with the folder."
  (or *text*
      (setf *text*
            (or (nlk:read-text (namestring (doctrine-path)))
                (fail "the doctrine file (~a) is missing or unreadable: reinstall the folder, ~
~a ships with it" (doctrine-path) +doctrine-file+)))))

(defun doctrine-text ()
  "The doctrine's whole text, as the file states it, a model reading it."
  (read-doctrine))

(defun heading-of (line)
  "The (NUMBER . TITLE) a '## IV. Proof over claims' line carries, or NIL."
  (when (uiop:string-prefix-p "## " line)
    (let* ((rest (subseq line 3))
           (dot (position #\. rest)))
      (when (and dot (plusp dot) (every (lambda (char) (find char "IVXLCDM")) (subseq rest 0 dot)))
        (cons (subseq rest 0 dot) (nlk:trimmed (subseq rest (1+ dot))))))))

(defun finish-article (number title lines)
  "An ARTICLE out of the heading's NUMBER and TITLE and its body LINES, reversed."
  (make-article number title
                 (format nil "~{~a~^~%~}" (reverse lines))))

(defun parse-articles (text)
  "TEXT's articles, in order: each starts at its heading and ends before the
next heading or the rule that closes the list."
  (let ((articles '())
        (number nil)
        (title nil)
        (lines '()))
    (flet ((emit-article ()
             (when number
               (push (finish-article number title lines) articles)
               (setf number nil title nil lines '()))))
      (dolist (line (nlk:lines text))
        (let ((heading (heading-of line)))
          (cond (heading
                 (emit-article)
                 (setf number (car heading) title (cdr heading))
                 (push line lines))
                ((and number (string= (nlk:trimmed line) "---")) (emit-article))
                (number (push line lines)))))
      (emit-article))
    (nreverse articles)))

(defun doctrine-articles ()
  "The nine articles, parsed once."
  (or *articles* (setf *articles* (parse-articles (read-doctrine)))))

(defun doctrine-axiom ()
  "The doctrine's opening line, the one it states above the articles."
  (or (loop for line in (nlk:lines (read-doctrine))
             when (uiop:string-prefix-p "> " line)
               return (nlk:trimmed (subseq line 2)))
      ""))

(defun ensure-doctrine ()
  "Read and parse the file now, so a folder that cannot refuses its start."
  (doctrine-axiom)
  (doctrine-articles)
  (length (doctrine-articles)))

;;; --- an article's own words, and the enforcers it names -------------------------

(defun article-body (article)
  "ARTICLE's words: its heading line and its *Enforced by:* line taken off."
  (let ((lines (loop for line in (nlk:lines (article-text article))
                     unless (uiop:string-prefix-p "*Enforced by:" line)
                       collect line)))
    (nlk:trimmed (format nil "~{~a~^ ~}" (rest lines)))))

(defun first-sentence (text)
  "TEXT's opening sentence, whitespace collapsed, at most 300 characters."
  (let* ((flat (nlk:trimmed (format nil "~{~a~^ ~}" (nlk:lines text))))
         (dot (search ". " flat)))
    (if (and dot (< dot 300))
        (subseq flat 0 (1+ dot))
        (nlk:clip flat 300))))

(defun article-enforcers (article)
  "The enforcer plugin names ARTICLE's *Enforced by:* line names, in the order it names them."
  (let ((line (find-if (lambda (line) (uiop:string-prefix-p "*Enforced by:" line))
                        (nlk:lines (article-text article)))))
    (when line
      (mapcar #'cdr
              (sort (loop for (name . cell) in +enforcers+
                           for at = (search name line)
                           when at collect (cons at name))
                    #'< :key #'car)))))

(defun article-by (which)
  "The article WHICH names by number (1-9) or roman numeral (I-IX), or NIL."
  (let ((which (string-upcase (nlk:trimmed (princ-to-string which)))))
    (loop for index from 1
          for article in (doctrine-articles)
          when (or (string= which (princ-to-string index))
                   (string= which (string-upcase (article-number article))))
            return article)))

;;; --- what is installed in this home ---------------------------------------------

(defun expand-home (path)
  "PATH with a leading ~ read the way a shell reads it."
  (if (and (plusp (length path)) (char= #\~ (char path 0)))
      (namestring (merge-pathnames (subseq path (min 2 (length path))) (user-homedir-pathname)))
      path))

(defun cells-root ()
  "Where this home's cells sit: the section's cells_dir, else the home's cells folder."
  (let ((named (setting :cells-dir)))
    (if (and named (plusp (length named)))
        (uiop:ensure-directory-pathname (expand-home named))
        (nlk:home "cells/"))))

(defun cell-installed-p (root name)
  "Whether a cell called NAME sits in ROOT. Presence is enabled, so an
installed folder is the whole question."
  (and name (probe-file (merge-pathnames (format nil "~a/" name)
                                         (uiop:ensure-directory-pathname root)))))

(defun article-state (article &optional (root (cells-root)))
  "ENFORCED, naming the enforcers whose cell is installed, or ADVISED."
  (let ((live (loop for name in (article-enforcers article)
                     for cell = (cdr (assoc name +enforcers+ :test #'string=))
                     when (cell-installed-p root cell)
                       collect name)))
    (if live
        (format nil "ENFORCED (~{~a~^, ~})" live)
        "ADVISED")))

(defun enforced-count (root)
  "How many of the nine articles have an installed cell behind them."
  (count-if (lambda (article)
              (let ((state (article-state article root)))
                (uiop:string-prefix-p "ENFORCED" state)))
            (doctrine-articles)))

;;; --- the two renderings --------------------------------------------------------

(defun capped (text cap note)
  "TEXT at most CAP characters, ending in NOTE when it had to be cut."
  (if (<= (length text) cap)
      text
      (concatenate 'string (subseq text 0 (max 0 (- cap (length note)))) note)))

(defun compact-text (&optional (root (cells-root)))
  "The doctrine as a request carries it compactly: the axiom, each article's
heading and opening sentence with its state, and the footer."
  (capped (format nil "~a~%~{~a~%~}~a"
                  (doctrine-axiom)
                  (loop for article in (doctrine-articles)
                        collect (format nil "~a. ~a: ~a - ~a"
                                        (article-number article)
                                        (article-title article)
                                        (first-sentence (article-body article))
                                        (article-state article root)))
                  *footer*)
          +compact-cap+ +compact-truncated+))

(defun full-text (&optional (root (cells-root)))
  "The doctrine as a request carries it whole, with one install-state line
per article and the footer."
  (capped (format nil "~a~%Install state:~%~{~a~%~}~a"
                  (doctrine-text)
                  (loop for article in (doctrine-articles)
                        collect (format nil "~a: ~a" (article-number article)
                                        (article-state article root)))
                  *footer*)
          +full-cap+ +full-truncated+))

;;; --- the owner's own additions -------------------------------------------------

(defun additions-path ()
  "The owner's additions file: the section's additions, else doctrine.md in
this home. The cell never creates it."
  (let ((named (setting :additions)))
    (if (and named (plusp (length named)))
        (expand-home named)
        (nlk:home "doctrine.md"))))

(defun owner-additions (&optional cap)
  "The owner's additions file as the label and its words, capped when CAP
names a request's share of it, or \"\" when there is none."
  (let* ((path (additions-path))
         (text (ignore-errors (nlk:read-text (namestring path))))
         (text (and text (nlk:trimmed text))))
    (cond ((or (null text) (zerop (length text))) "")
          (t (let ((body (format nil "~%Owner additions:~%~a" text)))
               (if cap (capped body cap +additions-truncated+) body))))))

(defun context-text ()
  "What one request carries: the doctrine in the section's form, then the
owner's additions under their own cap."
  (let ((root (cells-root)))
    (concatenate 'string
                 (if (string= (setting :form) "full")
                     (full-text root)
                     (compact-text root))
                 (owner-additions +additions-cap+))))

;;; --- the report the command answers ---------------------------------------------

(define-verb status-text (&optional root)
  "The nine articles, the enforcer each names and its state in this home, as
the /doctrine table."
  (let* ((root (or root (cells-root)))
         (rows (list "No. | Article | Enforcer(s) | State")))
    (dolist (article (doctrine-articles))
      (let ((names (article-enforcers article)))
        (push (format nil "~a | ~a | ~a | ~a"
                      (article-number article)
                      (article-title article)
                      (if names (format nil "~{~a~^, ~}" names) "none (doctrine only)")
                      (article-state article root))
              rows)))
    (push *footer* rows)
    (push (format nil "~d of 9 enforced on this install" (enforced-count root)) rows)
    (let ((unported (loop for (name . cell) in +enforcers+
                          unless cell collect name)))
      (when unported
        (push (format nil "No cell here carries ~{~a~^, ~}: the articles naming them read ADVISED until one does."
                      unported)
              rows)))
    (format nil "~{~a~^~%~}" (nreverse rows))))

(define-verb plugins-text (&optional root)
  "Each enforcer the doctrine names, the cell that carries it and whether
that cell is installed in this home."
  (let* ((root (or root (cells-root)))
         (rows (list (format nil "Enforcer | Cell | Installed in ~a" (namestring root)))))
    (dolist (pair +enforcers+)
      (destructuring-bind (name . cell) pair
        (push (format nil "~a | ~a | ~a"
                      name
                      (or cell "none yet")
                      (cond ((null cell) "no cell carries it")
                            ((cell-installed-p root cell) "yes")
                            (t "not found")))
              rows)))
    (push "Installed is a folder of that name in the cells folder: presence is enabled here, and this report proves nothing about what a cell would stop."
          rows)
    (push *footer* rows)
    (format nil "~{~a~^~%~}" (nreverse rows))))

(define-verb show-text (&optional which)
  "The whole doctrine followed by the owner's additions, or one article by
number (1-9) or roman numeral (I-IX)."
  (let ((which (and which (nlk:trimmed (princ-to-string which)))))
    (if (or (null which) (zerop (length which)))
        (concatenate 'string (doctrine-text) (owner-additions))
        (let ((article (article-by which)))
          (or (and article (article-text article)) +usage+)))))
