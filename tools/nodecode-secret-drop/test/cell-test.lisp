;;;; cell-test.lisp --- lifecycle, the store, the environment, the redaction, the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a real store in a
;;;; scratch home and a stubbed fold: that a start installs the tools, the
;;;; command, the manual, the two hooks, the section and the notice, and a stop
;;;; takes every one of them back; that a wrong setting refuses; that a value
;;;; dropped in reaches the environment a shell command inherits and never the
;;;; answer; that a stop puts the environment back; that the redaction rule
;;;; replaces a credential-shaped value and leaves a short word-like one alone;
;;;; that the advice redacts the wire without touching the log; and that the
;;;; model is told to ask for a drop and shown names, never values.

(in-package #:nodecode.test)

(define-test-slice "secret-drop" "SECRET-DROP-CELL-" :start nodecode-secret-drop:start-cell)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "secret-drop"
  (:hooks 'nodecode.evolved::wire-messages-from-events 'nle::read-harness-sections)
  (:command "secret-drop")
  (:help :secret-drop)
  (:running (is (nle::find-tool "list_secrets") "the list tool is registered")
            (is (nle::find-tool "request_secret") "and the request tool")
            (is (eq t (nodecode-secret-drop::setting :passthrough)) "the environment is offered by default")
            (is (= 8 (nodecode-secret-drop::setting :min-redaction-chars)) "with the original's floor"))
  (:stopped (is (null (nle::find-tool "list_secrets")) "stop unregisters the tools")
            (is (null (nle::find-tool "request_secret"))))
  (:refused ("min_redaction_chars" 0) ("store" 5)))

;;; --- a store of its own ------------------------------------------------------------

(defvar *secret-drop-stores* 0 "How many scratch stores this run made.")

(defmacro with-drop ((&rest pairs) &body body)
  "BODY with the cell started on a fresh store under this run's home, and any
PAIRS added to the section."
  `(let ((store (namestring
                 (nlk:home (format nil "secret-drop-cell-~d/store.lisp"
                                   (incf *secret-drop-stores*))))))
     (with-cell-stop ((secret-drop-start "store" store ,@pairs))
       ,@body)))

(defun secret-drop-message (role text)
  "A folded provider message, as the wire fold hands one to the advice."
  (nlk:json-object "role" role "content" text))

;;; --- the drop ---------------------------------------------------------------------

(deftest secret-drop-cell-a-drop-reaches-the-environment-not-the-answer (with-drop ()
  (multiple-value-bind (text dialog)
      (cell-entry "nodecode-secret-drop" "secret-drop" "SD_DEPLOY_TOKEN s3cr3t-value-42")
    (declare (ignore dialog))
    (is (search "SD_DEPLOY_TOKEN" text) "the answer names the variable")
    (is (not (search "s3cr3t-value-42" text)) "and never repeats the value"))
  (is (equal "s3cr3t-value-42" (uiop:getenv "SD_DEPLOY_TOKEN")) "a shell command inherits it")
  (is (equal '("SD_DEPLOY_TOKEN") (nodecode-secret-drop:stored-names)) "the store holds the name")
  (is (search "SD_DEPLOY_TOKEN"
              (uiop:read-file-string (nodecode-secret-drop:store-path))) "and so does the file on disk")))

(deftest secret-drop-cell-stop-puts-the-environment-back ()
  (let* ((store (namestring (nlk:home "secret-drop-cell-stop/store.lisp")))
         (stop (secret-drop-start "store" store)))
    (cell-entry "nodecode-secret-drop" "secret-drop" "SD_STOP_TOKEN tok-1234-abcd")
    (is (equal "tok-1234-abcd" (uiop:getenv "SD_STOP_TOKEN")) "it is there while the cell runs")
    (funcall stop)
    (is (null (uiop:getenv "SD_STOP_TOKEN")) "and gone once it stops")
    (is (null (nle::find-tool "list_secrets")) "with the tools")))

;;; --- the redaction ----------------------------------------------------------------

(deftest secret-drop-cell-redacts-what-is-credential-shaped-and-leaves-prose (with-drop ()
  (cell-entry "nodecode-secret-drop" "secret-drop" "SD_REDACT_TOKEN tok-9f3a2b")
  (let ((text (nodecode-secret-drop:redact-text "the token tok-9f3a2b is here")))
    (is (not (search "tok-9f3a2b" text)) "a credential-shaped value is replaced")
    (is (search "[SECRET-DROP]" text) "with the placeholder standing where it was"))
  (cell-entry "nodecode-secret-drop" "secret-drop" "SD_LONG_TOKEN abcdefghijklmnopqrstuvwxyz")
  (is (search "[SECRET-DROP]" (nodecode-secret-drop:redact-text
                                "abcdefghijklmnopqrstuvwxyz is the value")) "a long letter string is specific enough to search for")
  (cell-entry "nodecode-secret-drop" "secret-drop" "SD_WORD password")
  (is (equal "the password file"
             (nodecode-secret-drop:redact-text "the password file")) "a short word-like value is stored but never searched for")
  (is (equal "administrator" (nodecode-secret-drop:redact-text "administrator")) "so prose is left as it was")))

(deftest secret-drop-cell-redacts-the-wire-and-not-the-log (with-drop ()
  (cell-entry "nodecode-secret-drop" "secret-drop" "SD_WIRE_TOKEN w1r3-token-88")
  (let* ((system (secret-drop-message "system" "the token w1r3-token-88"))
         (tool (secret-drop-message "tool" "w1r3-token-88 printed by a command"))
         (hook (cell-hook "nodecode-secret-drop" 'nodecode.evolved::wire-messages-from-events))
         (answer (multiple-value-list
                  (funcall hook (lambda (&rest args)
                                  (declare (ignore args))
                                  (values (list system tool) :clean)))))
         (messages (first answer)))
    (is (eq :clean (second answer)) "the fold's clean flag comes back untouched")
    (is (search "w1r3-token-88" (gethash "content" (first messages))) "the system prompt is never read")
    (is (not (search "w1r3-token-88" (gethash "content" (second messages)))) "a value a tool printed is redacted on the way out")
    (is (search "w1r3-token-88" (gethash "content" tool)) "the folded message keeps what was written: the log is untouched")
    (is (not (eq tool (second messages))) "what changed is a copy"))))

(deftest secret-drop-cell-removes-one-and-reports-what-is-there (with-drop ()
  (cell-entry "nodecode-secret-drop" "secret-drop" "SD_KEEP_TOKEN keep-value-99")
  (cell-entry "nodecode-secret-drop" "secret-drop" "SD_DROP_TOKEN drop-value-88")
  (multiple-value-bind (text dialog)
      (cell-entry "nodecode-secret-drop" "secret-drop" "SD_DROP_TOKEN")
    (is (and dialog (search "SD_DROP_TOKEN" text)) "/secret-drop NAME alone opens the field for that one value"))
  (let ((text (nth-value 0 (cell-entry "nodecode-secret-drop" "secret-drop" "rm SD_DROP_TOKEN"))))
    (is (search "SD_DROP_TOKEN removed" text) "rm takes the name out"))
  (is (null (uiop:getenv "SD_DROP_TOKEN")) "and the environment with it")
  (is (equal '("SD_KEEP_TOKEN") (nodecode-secret-drop:stored-names)) "the other one stays")
  (let ((text (nth-value 0 (cell-entry "nodecode-secret-drop" "secret-drop" "doctor"))))
    (is (search "SD_KEEP_TOKEN" text) "doctor names what is stored")
    (is (not (search "keep-value-99" text)) "and never a value"))))

;;; --- what the model sees ----------------------------------------------------------

(deftest secret-drop-cell-names-what-is-stored-and-never-the-value (with-drop ()
  (cell-entry "nodecode-secret-drop" "secret-drop" "SD_LIST_TOKEN list-value-77")
  (let ((text (nodecode-secret-drop::list-answer
               (nodecode-secret-drop::make-list-secrets-call :name ""))))
    (is (search "SD_LIST_TOKEN" text) "the name is shown")
    (is (not (search "list-value-77" text)) "and never the value"))
  (let ((text (nodecode-secret-drop::request-answer
               (nodecode-secret-drop::make-request-secret-call
                :name "SD_REQUEST_TOKEN" :why "a deploy"))))
    (is (search "SD_REQUEST_TOKEN" text) "a request names what the task needs")
    (is (search "SD_REQUEST_TOKEN" (second (cell-notice "nodecode-secret-drop"))) "and stands on the operator's board until they drop it"))))

(deftest secret-drop-cell-tells-the-model-to-ask-for-a-drop (with-drop ()
  (let ((text (cell-section "s1" "secret-drop")))
    (is (and (stringp text) (search "/secret-drop" text)) "the section names the command")
    (is (search "request_secret" text) "and the tool to ask with")
    (is (search "list_secrets" text) "and the one that names what is stored"))))
