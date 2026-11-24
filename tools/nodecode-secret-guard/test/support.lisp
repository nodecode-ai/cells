;;;; support.lisp --- the secret-guard test runner and its helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Secret-guard tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic posture) under a SECRET-GUARD-CELL- name
;;;; prefix, so RUN-SECRET-GUARD-TESTS runs exactly that slice. Nothing here
;;;; reaches outside the image: the store is a file in the run's scratch home,
;;;; the environment variable a key comes from is set by the test, and the
;;;; advice on :TOOL runs against a `next' the test writes.

(in-package #:nodecode.test)

(define-test-slice "secret-guard" "SECRET-GUARD-CELL-" :start nodecode-secret-guard:start-cell)

(defparameter +sample-key+ "sk-live-0123456789abcdefghijklmnop"
  "A value shaped like an API key the tests store and expect back scrubbed.")

(defparameter +short-value+ "abc9"
  "A value too short to scrub: four characters turn up in ordinary text.")

(defparameter +credential-path+ "cat ~/.aws/credentials"
  "A command the gate refuses: a reader pointed at a credential store.")

(defvar *store-counter* 0
  "How many stores these tests have made; each one its own file.")

(defun fresh-store (&rest pairs)
  "PAIRS with a store_file no other test in this run uses: the section a key
test starts on."
  (list* "store_file"
         (namestring (nlk:home (format nil "secret-guard-test/store-~d.json"
                                        (incf *store-counter*))))
         pairs))

(defun guard-hook ()
  "The cell's advice on :TOOL, or NIL while it is not started."
  (cell-hook "nodecode-secret-guard" :tool))

(defun guard-tool (name request)
  "NAME's registered tool body, called with its decoded contract the way the
engine calls it."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry) request)))

(defun guard-list (&optional (name ""))
  "secret_list, asking for NAME or for every name."
  (guard-tool "secret_list"
              (nodecode-secret-guard:decode-secret-list-request
               (nlk:json-object "name" name))))

(defun guard-request (name &optional (why ""))
  "secret_request for NAME."
  (guard-tool "secret_request"
              (nodecode-secret-guard:decode-secret-request-request
               (nlk:json-object "name" name "why" why))))

(defun guard-check (text)
  "secret_check over TEXT."
  (guard-tool "secret_check"
              (nodecode-secret-guard:decode-secret-check-request
               (nlk:json-object "text" text))))

(defun guard-slash (&optional (args ""))
  "/secret ARGS in a session, answering its text."
  (nodecode-secret-guard:run-slash args "s1"))

(defun guard-env (name value)
  "Put VALUE into the environment under NAME, and answer NAME."
  (sb-posix:setenv name value 1)
  name)

(defun guard-passes (text)
  "TEXT through the advice as a call that ran and answered exactly TEXT."
  (funcall (guard-hook)
           (list :name "eval" :arguments (nlk:json-object) :call-id "c-9")
           (lambda (op) (declare (ignore op)) text)))

(defun guard-call (name &rest pairs)
  "The :TOOL advice, called the way the engine calls it: the call NAME with
PAIRS as its arguments, and NEXT answering TEXT. => the answer, and whether
NEXT ran."
  (let ((ran nil)
        (answer nil))
    (setf answer
          (funcall (guard-hook)
                   (list :name name :arguments (apply #'nlk:make-json-object pairs) :call-id "c-1")
                   (lambda (op)
                     (declare (ignore op))
                     (setf ran t)
                     "the call answered")))
    (values answer ran)))
