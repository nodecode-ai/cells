;;;; support.lisp --- the vibeleak test runner, a scratch tree and a scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; VIBELEAK-CELL- prefix, and RUN-VIBELEAK-TESTS runs exactly that slice.
;;;;
;;;; Two seams, both closed: the local review reads a tree this file builds
;;;; under the process's temporary directory, and the one network call
;;;; (NODECODE-VIBELEAK::HTTP) is stubbed, so no test opens a socket.

(in-package #:nodecode.test)

(define-test-slice "vibeleak" "VIBELEAK-CELL-" :start nodecode-vibeleak:start-cell)

;;; --- a tree to review ---------------------------------------------------------

(defvar *vibeleak-trees* '()
  "Every scratch tree this run made, deleted by the next suite's start.")

(defvar *vibeleak-tree-stamp* (format nil "~d" (get-universal-time))
  "When this run made its first tree: a name no earlier run's tree has.")

(defvar *vibeleak-tree-count* 0
  "How many scratch trees this run has made: a name that cannot repeat.")

(defun vibeleak-tree (&rest files)
  "A fresh scratch folder holding FILES, each (RELATIVE-PATH . CONTENT).
=> the folder's namestring, with a trailing slash."
  (let* ((root (format nil "~avibeleak-~a-~d/" (uiop:temporary-directory)
                       *vibeleak-tree-stamp* (incf *vibeleak-tree-count*)))
         (directory (uiop:ensure-directory-pathname root)))
    (ignore-errors (uiop:delete-directory-tree directory :validate t))
    (ensure-directories-exist (merge-pathnames "probe" directory))
    (dolist (file files)
      (let ((path (merge-pathnames (car file) directory)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create :external-format :utf-8)
          (write-string (cdr file) out))))
    (pushnew directory *vibeleak-trees* :test #'equal)
    (namestring directory)))

(defun vibeleak-forget-trees ()
  (dolist (tree *vibeleak-trees*)
    (ignore-errors (uiop:delete-directory-tree tree :validate t)))
  (setf *vibeleak-trees* '()))

;;; --- the cell, started and stopped -------------------------------------------

(defmacro with-vibeleak ((&rest config-pairs) &body body)
  "Run BODY with the cell started on CONFIG-PAIRS and stopped on unwind."
  `(with-cell-stop ((vibeleak-start ,@config-pairs))
     ,@body))

;;; --- the scripted seam -------------------------------------------------------

(defstruct (vibeleak-call (:copier nil)) method url headers content)

(defun vibeleak-header (call name)
  (cdr (assoc name (vibeleak-call-headers call) :test #'string-equal)))

(defmacro with-scripted-vibeleak ((calls &key answer) &body body)
  "Run BODY with the cell's one network call stubbed: every call recorded
into CALLS, oldest first, and ANSWER -- a function of the call answering
(values TEXT STATUS) -- answering it."
  `(let ((,calls '()))
     (with-stubbed-fdefinition (nodecode-vibeleak::http
                                 (method url &key headers content timeout)
                                 (declare (ignore timeout))
                                 (let ((call (make-vibeleak-call :method method :url url
                                                                 :headers headers :content content)))
                                   (setf ,calls (append ,calls (list call)))
                                   (funcall ,answer call)))
       ,@body)))

;;; --- calling what the cell contributes ---------------------------------------

(defun vibeleak-tool (name &rest pairs)
  "The registered tool NAME, called the way the engine calls it: the
declaration materializes the record, the body answers the text."
  (let ((entry (nle::find-tool name)))
    (assert entry () "the ~a tool is not registered" name)
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun vibeleak-json (text)
  "TEXT as a decoded JSON object."
  (let ((json (ignore-errors (nlk:decode-json text))))
    (assert (hash-table-p json) () "not a JSON object: ~a" text)
    json))

(defun vibeleak-field (object key)
  "OBJECT's member KEY, or NIL when OBJECT is not an object or has none."
  (and (hash-table-p object) (gethash key object)))

;;; --- the API's answers -------------------------------------------------------

(defun vibeleak-scan-body (&key (id "11111111-1111-4111-8111-111111111111") (domain "example.com")
                                  (grade "C") (score 66) (extra-findings '()))
  "One saved scan, as the API's envelope carries it."
  (nlk:encode-json-object
   (nlk:make-json-object
    "ok" t
    "data" (nlk:make-json-object
            "scan" (nlk:make-json-object
                    "id" id "domain" domain "grade" grade "score" score
                    "coverage" (nlk:make-json-object
                                "status" "complete"
                                "completedModules" (coerce '("headers" "tls" "php") 'vector)
                                "failedModules" #())
                    "findings" (coerce (append extra-findings) 'vector))))))

(defun vibeleak-finding (title severity &key (module "php") (category "input-handling"))
  (nlk:make-json-object "title" title "severity" severity "module" module
                         "category" category "evidence" "evidence text"
                         "description" "description text"
                         "remediation" "do the smallest justified fix"))
