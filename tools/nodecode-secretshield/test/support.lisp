;;;; support.lisp --- the shield's test slice and its helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic posture) under a SECRETSHIELD-CELL- name
;;;; prefix, so RUN-SECRETSHIELD-TESTS runs exactly that slice. Nothing here
;;;; reaches outside the image or the network: the scanner reads a string, the
;;;; hook runs against a `next' the test writes, and the tool is called the
;;;; way the engine's body calls it.
;;;;
;;;; Every credential is built at run time from a prefix and a deterministic
;;;; fill, so the repository holds no credential-shaped literal.

(in-package #:nodecode.test)

(define-test-slice "secretshield" "SECRETSHIELD-CELL-"
  :start nodecode-secretshield:start-cell)

(defparameter ss-alnum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "What the formats this slice builds draw their bodies from.")

(defun ss-fill (chars n &optional (seed 0))
  "N characters of CHARS in a fixed, mixed-looking order."
  (let ((out (make-string-output-stream)))
    (dotimes (i n)
      (write-char (char chars (mod (+ (* i 7) (* seed 13) 3) (length chars))) out))
    (get-output-stream-string out)))

(defun ss-openai-token ()
  "A credential in the OpenAI shape: sk- and forty body characters."
  (concatenate 'string "sk-" (ss-fill ss-alnum 40)))

(defun ss-bearer-token ()
  "A bearer credential's body: twenty-four characters."
  (ss-fill ss-alnum 24 2))

(defun ss-ledger-file ()
  "The ledger file of this scratch home."
  (nodecode-secretshield:incident-path))

(defun ss-forget-ledger ()
  "Start a test from an empty ledger."
  (ignore-errors (delete-file (ss-ledger-file))))

(defun ss-command (text)
  "The arguments hash a tool call arrives with, its command TEXT."
  (nlk:json-object "command" text))

(defun ss-call (arguments &key (result :ran))
  "The shield hook's answer for a call carrying ARGUMENTS, the arguments the
`next' was handed, and whether `next' ran:
=> (values ANSWER SEEN-ARGUMENTS CALLED-P)."
  (let ((seen nil) (called nil))
    (values (funcall (cell-hook "nodecode-secretshield" :tool)
                      (list :name "terminal" :arguments arguments :call-id "c1")
                      (lambda (op) (setf called t seen (getf op :arguments)) result))
            seen called)))

(defun ss-status ()
  "The shield_status tool through its registration, the way the body is called."
  (let ((entry (nle::find-tool "shield_status")))
    (funcall (third entry) (nlk:json-object))))

(defun ss-slash (args)
  "/secretshield ARGS in a session, answering its text."
  (nodecode-secretshield::run-slash args "s1"))
