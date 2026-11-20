;;;; repro.lisp --- a case becomes one Lisp form that says whether its fault stands.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; REPRO-FORM turns a case (capture.lisp) into the text of one form the
;;;; runner asks in a sealed fresh process (runner.lisp). It answers 1 while
;;;; the fault stands, 0 once it is gone, and 2 when that process cannot judge
;;;; it - the function's package is not there, the generic function is not
;;;; one - which is never a pass and never a finding. Symbols ride the text as
;;;; package and name strings, found with FIND-SYMBOL, so a form never fails
;;;; to read where a cell is missing.
;;;;
;;;; Most faults need no call at all, and their form calls nothing:
;;;;   - a generic function with no method: does it have a primary method for
;;;;     the arguments' classes now (COMPUTE-APPLICABLE-METHODS-USING-CLASSES)?
;;;;   - a function that is not defined: is it FBOUNDP now?
;;;;   - a variable that is not bound: is it BOUNDP now?
;;;; Any other fault is a :CALL repro, the recorded call made again, and only
;;;; when its arguments read back whole; without them it has no repro.
;;;;
;;;; REPRO-DEFTEST is the same check as a DEFTEST for the cell that owns the
;;;; function (OWNING-CELL, from where the function was defined): the test an
;;;; upstream fix lands with, in that cell's own test suite and CI.

(in-package #:nodecode-selffix)

(defun case-of (item)
  "The case behind ITEM: a backlog item's :CASE or :FILE, else ITEM itself."
  (or (getf item :case)
      (and (getf item :file) (read-case (getf item :file)))
      item))

(defun name-parts (fname)
  "FNAME, a symbol or (SETF symbol), as (SETF-P PACKAGE NAME) strings."
  (let ((symbol (if (consp fname) (second fname) fname)))
    (list (consp fname) (package-name (symbol-package symbol)) (symbol-name symbol))))

(defun found-symbol-form (fname)
  "Text of a form answering FNAME's symbol in the asking process, or NIL when its package or name is not there."
  (destructuring-bind (setf-p package name) (name-parts fname)
    (declare (ignore setf-p))
    (format nil "(and (find-package ~s) (find-symbol ~s ~s))" package name package)))

(defun fname-form (fname symbol-var)
  "Text naming the function FNAME stands for, with its symbol in SYMBOL-VAR."
  (if (consp fname) (format nil "(list 'setf ~a)" symbol-var) symbol-var))

(defun class-forms (classes)
  "Text of a list form finding each class of CLASSES, NIL for one not there."
  (format nil "(list ~{~a~^ ~})"
          (mapcar (lambda (class)
                    (format nil "(let ((s ~a)) (and s (find-class s nil)))" (found-symbol-form class)))
                  classes)))

(defun args-text (case)
  "The case's arguments as readable text, or NIL when they did not read back."
  (let ((args (getf case :args)))
    (and (listp args) (readable-text args))))

(defun repro-kind (case)
  "How CASE is reproduced: :METHOD, :FBOUND, :BOUND, :CALL, or NIL for not at all."
  (let ((type (getf case :type)) (fn (getf case :function)))
    (cond ((null fn) nil)
          ((and (subtypep type 'sb-pcl::no-applicable-method-error) (getf case :classes)) :method)
          ((subtypep type 'undefined-function) :fbound)
          ((and (subtypep type 'unbound-variable) (getf case :name)) :bound)
          ((args-text case) :call))))

(defun repro-form (case)
  "The text of the form that says whether CASE's fault stands (1), is gone (0)
or cannot be judged (2), and its kind: (values TEXT KIND), KIND :CHECK for a
form that calls nothing, :CALL for one that makes the recorded call again.
NIL when CASE has no repro."
  (let* ((fn (getf case :function))
         (kind (repro-kind case)))
    (case kind
      (:method
       (values (format nil "(let* ((s ~a) (gf (and s (fboundp ~a) (fdefinition ~a))) (classes ~a))~%  ~
                              (cond ((not (typep gf 'generic-function)) 2)~%        ~
                                    ((some #'null classes) 2)~%        ~
                                    ((some (lambda (m) (null (method-qualifiers m)))~%               ~
                                           (sb-mop:compute-applicable-methods-using-classes gf classes)) 0)~%        ~
                                    (t 1)))"
                       (found-symbol-form fn) (fname-form fn "s") (fname-form fn "s")
                       (class-forms (getf case :classes)))
               :check))
      (:fbound
       (values (format nil "(let ((s ~a)) (cond ((not (find-package ~s)) 2) ((and s (fboundp ~a)) 0) (t 1)))"
                       (found-symbol-form fn) (second (name-parts fn)) (fname-form fn "s"))
               :check))
      (:bound
       (let ((name (getf case :name)))
         (values (format nil "(let ((s ~a)) (cond ((not (find-package ~s)) 2) ((and s (boundp s)) 0) (t 1)))"
                         (found-symbol-form name) (second (name-parts name)))
                 :check)))
      (:call
       (destructuring-bind (setf-p tpackage tname) (name-parts (getf case :type))
         (declare (ignore setf-p))
         (values (format nil "(let ((s ~a) (type (and (find-package ~s) (find-symbol ~s ~s))))~%  ~
                                (if (not (and s type (fboundp ~a))) 2~%      ~
                                    (handler-case (progn (apply (fdefinition ~a) (let ((*read-eval* nil)) (read-from-string ~s))) 0)~%        ~
                                      (error (c) (if (typep c type) 1 2)))))"
                         (found-symbol-form fn) tpackage tname tpackage (fname-form fn "s") (fname-form fn "s")
                         (args-text case))
                 :call))))))

;;; --- the deftest an upstream fix lands with -----------------------------------

(defun source-file (fname)
  "Where FNAME was defined, a pathname, or NIL."
  (let ((source (or (first (ignore-errors (sb-introspect:find-definition-sources-by-name fname :generic-function)))
                    (first (ignore-errors (sb-introspect:find-definition-sources-by-name fname :function))))))
    (and source (sb-introspect:definition-source-pathname source))))

(defun owning-cell (fname)
  "The cell that owns FNAME: (values SYSTEM FOLDER), SYSTEM the name of the
.asd nearest above its source file (\"nodecode\" for the core), FOLDER that
file's folder; NIL when the source is not known."
  (let ((file (source-file fname)))
    (when file
      (loop for dir = (uiop:pathname-directory-pathname file) then (uiop:pathname-parent-directory-pathname dir)
            for asd = (first (directory (merge-pathnames "*.asd" dir)))
            repeat 12
            when asd return (values (pathname-name asd) dir)
            until (equal dir (uiop:pathname-parent-directory-pathname dir))))))

(defun lisp-name (symbol)
  "SYMBOL as source text with its package."
  (with-case-syntax (prin1-to-string symbol)))

(defun repro-deftest (case &key (fp (case-fingerprint case)))
  "CASE's check as the text of a DEFTEST for the owning cell's test suite, or
NIL when CASE has no :CHECK repro. A :CALL repro stays a finding: its
arguments are the user's data and do not travel upstream."
  (let ((fn (getf case :function))
        (why (format nil "turn ~a failed with ~a~@[ (seen ~d times)~]"
                     (getf case :turn) (lisp-name (getf case :type)) (getf case :count))))
    (case (repro-kind case)
      (:method
       (format nil "(deftest selffix-~a ()~%  (is (some (lambda (m) (null (method-qualifiers m)))~%            ~
                    (sb-mop:compute-applicable-methods-using-classes~%             #'~a (list~{ (find-class '~a)~})))~%      ~s))~%"
               fp (lisp-name fn) (mapcar #'lisp-name (getf case :classes))
               (format nil "~a has a method for ~{~a~^, ~}: ~a" (lisp-name fn) (mapcar #'lisp-name (getf case :classes)) why)))
      (:fbound
       (format nil "(deftest selffix-~a ()~%  (is (fboundp '~a) ~s))~%"
               fp (lisp-name fn) (format nil "~a is defined: ~a" (lisp-name fn) why)))
      (:bound
       (format nil "(deftest selffix-~a ()~%  (is (boundp '~a) ~s))~%"
               fp (lisp-name (getf case :name)) (format nil "~a is bound: ~a" (lisp-name (getf case :name)) why))))))
