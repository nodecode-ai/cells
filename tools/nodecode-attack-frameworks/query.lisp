;;;; query.lisp --- the five read four ways: whole, by name, by words, by what to grep.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches the network. The table is in DATA.LISP and every verb
;;;; answers from it, which is the whole of the "direct access" this cell gives.

(in-package #:nodecode-attack-frameworks)

;;; --- reading one entry ----------------------------------------------------------

(defun frameworks ()
  "Every framework in the table, most-starred first."
  (sort (copy-list +frameworks+) #'> :key (lambda (f) (getf f :stars))))

(defun framework-name (f) (getf f :name))
(defun framework-repo (f) (getf f :repo))
(defun framework-url (f) (getf f :url))
(defun framework-stars (f) (getf f :stars))
(defun framework-licence (f) (getf f :licence))
(defun framework-language (f) (getf f :language))
(defun framework-pushed (f) (getf f :pushed))
(defun framework-what (f) (getf f :what))
(defun framework-authorization (f) (getf f :authorization))
(defun framework-evidence (f) (getf f :evidence))
(defun framework-hunt (f) (getf f :hunt))

(defun star-total ()
  "The stars the five have drawn, as the post counts them."
  (reduce #'+ (frameworks) :key #'framework-stars))

(defun star-text (n)
  "A star count with thousands separators."
  (let* ((s (princ-to-string n))
         (length (length s))
         (out (make-string-output-stream)))
    (loop for character across s
          for index from 0
          do (when (and (plusp index) (zerop (mod (- length index) 3)))
               (write-char #\, out))
             (write-char character out))
    (get-output-stream-string out)))

;;; --- matching -------------------------------------------------------------------

(defun contains-p (needle haystack)
  "Whether NEEDLE appears in HAYSTACK, ignoring case; NIL when either is empty."
  (and (stringp haystack)
       (plusp (length haystack))
       (stringp needle)
       (plusp (length needle))
       (not (null (search (string-downcase needle) (string-downcase haystack))))))

(defun hunt-kind (pair) (car pair))
(defun hunt-what (pair) (cdr pair))

(defun framework-mentions-p (f text)
  "Whether TEXT appears in anything the entry says, the hunt strings included."
  (or (contains-p text (framework-name f))
      (contains-p text (framework-repo f))
      (contains-p text (framework-licence f))
      (contains-p text (framework-language f))
      (contains-p text (framework-what f))
      (contains-p text (framework-authorization f))
      (contains-p text (framework-evidence f))
      (some (lambda (pair) (contains-p text (hunt-what pair))) (framework-hunt f))))

(defun frameworks-matching (&key text licence)
  "The frameworks that carry TEXT anywhere, or are under LICENCE (mit, apache, agpl)."
  (let ((licence (if (and (stringp licence) (string= "" licence)) nil licence))
        (text (if (and (stringp text) (string= "" text)) nil text)))
    (declare (ignorable text))
    (remove-if-not (lambda (f)
                     (and (or (null licence) (contains-p licence (framework-licence f)))
                          (or (null text)
                              (and (stringp text) (plusp (length text))
                                   (framework-mentions-p f text)))))
                   (frameworks))))

(defun framework-by-name (name)
  "The framework NAME names, exactly first and then by fragment; NIL when none does."
  (when (and (stringp name) (plusp (length (string-trim '(#\Space #\Tab) name))))
    (let* ((name (string-trim '(#\Space #\Tab) name))
           (all (frameworks)))
      (or (find name all :key #'framework-name :test #'string-equal)
          (find name all :key (lambda (f) (framework-name f)) :test (lambda (a b) (contains-p a b)))
          (find name all :key #'framework-repo :test (lambda (a b) (contains-p a b)))))))

;;; --- what a person is handed ----------------------------------------------------

(defun hunt-lines (f &key (prefix "    "))
  "The hunt defaults of F, one line each, each with the kind of thing it is."
  (format nil "~{~a~}"
          (loop for pair in (framework-hunt f)
                collect (format nil "~a- ~a: ~a~%" prefix (hunt-kind pair) (hunt-what pair)))))

(defun framework-block (f)
  "One framework, whole: the repository, what it is, how it fails to ask, and what it leaves."
  (format nil "~a -- ~a, ~a stars, ~a, ~a, pushed ~a~%~@[  what: ~a~%~]~@[  asks first: ~a~%~]~@[  seen: ~a~%~]~@[  hunt:~%~a~]~@[  ~a~%~]"
          (framework-name f) (framework-repo f) (star-text (framework-stars f))
          (framework-licence f) (framework-language f) (framework-pushed f)
          (framework-what f) (framework-authorization f) (framework-evidence f)
          (let ((hunt (framework-hunt f))) (and hunt (hunt-lines f :prefix "    ")))
          (framework-url f)))

(defun brief-line (f)
  "One line for a listing: what it is, and where."
  (format nil "~a (~a, ~a stars, ~a): ~a~%    ~a"
          (framework-name f) (framework-repo f) (star-text (framework-stars f))
          (framework-licence f) (framework-what f) (framework-url f)))

(defun framework-list-text (rows &key heading)
  "ROWS as a listing under HEADING."
  (if rows
      (format nil "~a~%~{~a~^~%~}"
              (or heading (format nil "~a frameworks" (length rows)))
              (mapcar #'brief-line rows))
      (format nil "no framework in the table matches that. ~a"
              (format nil "~{~a~^, ~}" (mapcar #'framework-name (frameworks))))))

(defun hunt-text (&optional name)
  "The hunt defaults of every framework, or of the one NAME names, one line each."
  (let ((rows (if name (let ((f (framework-by-name name))) (and f (list f))) (frameworks))))
    (if rows
        (format nil "~{~a~^~%~}"
                (loop for f in rows
                      collect (format nil "~a~@[~%~a~]"
                                      (format nil "~a (~a)" (framework-name f) (framework-licence f))
                                      (and (framework-hunt f) (hunt-lines f :prefix "  ")))))
        (format nil "no framework in the table is named ~s." name))))

(defun summary-text ()
  "How many, how starred, and what the post concluded."
  (format nil "~a frameworks, ~a stars between them, all observed in real intrusions.~%~
Licences: ~a.~%~
None of the five enforces authorization in code by default, and three tell the model it is already~%authorized.~%~
~a~%~
~a"
          (length (frameworks)) (star-text (star-total))
          (format nil "~{~a~^, ~}"
                  (sort (remove-duplicates (mapcar #'framework-licence (frameworks)) :test #'string=) #'string<))
          +thesis+ +tell-text+))

(defun source-line ()
  "The provenance, and the warning that a default moves between releases."
  (format nil "Source: ~a. A default can change in any release, so read a hunt string as a lead, not as a signature." +source+))
