;;;; cell-test.lisp --- the codex-truly cell against a stubbed backend.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP and the sign-in a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches chatgpt.com, the environment or the
;;;; operator's files. The tool is called the way a model's call reaches it —
;;;; through the real dispatch, contract and all — so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "codex-truly" "CODEX-TRULY-CELL-" :start nodecode-codex-truly:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *catalog-json*
  (concatenate 'string
               "{\"models\":["
               "{\"slug\":\"gpt-6.1-sol\",\"context_window\":500000,\"max_context_window\":872000},"
               "{\"slug\":\"gpt-6-sol\",\"context_window\":272000,\"max_context_window\":872000}]}"))

(defparameter *calls* nil
  "Every request the stub answered, oldest first, as (METHOD URL CONTENT HEADERS).")

(defparameter *probe-answer* nil
  "The function a /responses request is answered with, or NIL for accept-everything.")

(defmacro with-codex (&body body)
  "BODY with a sign-in answering: no auth store, no environment."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "OPENAI_CODEX_OAUTH_TOKEN") "test-token"))
     ,@body))

(defun word-count (content)
  (let ((count 0) (start 0))
    (loop for at = (search "word " content :start2 start)
          while at do (incf count) (setf start (+ at 5)))
    (or count 0)))

(defun completed-frame (tokens)
  (format nil "data: {\"type\":\"response.completed\",\"response\":{\"usage\":{\"input_tokens\":~d}}}~%" tokens))

(defun rejected-frame ()
  (concatenate 'string
               "data: {\"type\":\"error\",\"error\":{\"code\":\"context_length_exceeded\","
               "\"message\":\"input exceeds the context window of this model\"}}~%"))

(defun accept-up-to-900k (content)
  "A stub backend: an input of at most 900,000 words completes, anything larger is refused."
  (let ((words (word-count content)))
    (if (> words 900000)
        (values (rejected-frame) 200)
        (values (completed-frame words) 200))))

(defmacro with-codex-wire (&body body)
  "BODY with NLK:HTTP answering from the catalog fixture and the probe stub."
  `(let ((*calls* nil)
         (saved *probe-answer*))
     (unwind-protect
          (with-stubbed-fdefinition (nlk:http (method target &rest options)
                                      (push (list method target (getf options :content)
                                                  (getf options :headers))
                                            *calls*)
                                      (if (search "/models" target)
                                          (values *catalog-json* 200)
                                          (funcall (or *probe-answer* #'accept-up-to-900k)
                                                   (or (getf options :content) ""))))
            ,@body)
       (setf *probe-answer* saved))))

(defun answer (id arguments)
  "The tool's answer to ARGUMENTS, as the model reads it: the real dispatch."
  (nle::message-content (execute-wire-call id "codex_truly" arguments)))

(defun header-value (headers name)
  (cdr (assoc name headers :test #'string-equal)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "codex-truly"
  (:config)
  (:help :codex-truly)
  (:command "codex-truly")
  (:fixture with-codex)
  (:refused ("target_context_window" 1000) ("timeout" 1)))

;;; --- status ---------------------------------------------------------------------

(deftest codex-truly-cell-status-reads-the-catalog (with-codex)
  (with-cell-stop ((codex-truly-start))
    (with-codex-wire ()
      (let ((table (cell-entry "nodecode-codex-truly" "codex-truly" "full")))
        (is (search "gpt-6.1-sol" table) "the account's own catalog is what is reported")
        (is (search "500,000" table) "its advertised window")
        (is (search "919,956" table) "and the largest input a live probe accepted")
        (is (= 1 (length *calls*)) "status reads the catalog once")
        (let ((call (first *calls*)))
          (is (eq :get (first call)) "a GET")
          (is (search "/models" (second call)) "of the account-scoped catalog")
          (is (equal "Bearer test-token" (header-value (fourth call) "authorization")) "carrying the sign-in as its bearer")
          (is (equal "codex_cli_rs" (header-value (fourth call) "originator")) "and naming the client the backend insists on"))))))

(deftest codex-truly-cell-says-how-to-sign-in (with-codex)
  (with-cell-stop ((codex-truly-start))
    (with-codex-wire ()
      (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
        (let ((table (cell-entry "nodecode-codex-truly" "codex-truly" "")))
          (is (search "sign in" table) "an unsigned profile is told how to sign in")
          (is (search "/openai-codex" table) "by name")
          (is (null *calls*) "and nothing left the machine"))))))

;;; --- apply and revert ---------------------------------------------------------------

(defparameter *config-writes* nil)
(defparameter *config-rows* nil)

(defmacro with-config (&body body)
  "BODY with the shared config's provider rows stubbed, and every write recorded."
  `(let ((*config-writes* nil))
     (with-stubbed-fdefinition (nle:config-get (path)
                                 (declare (ignore path))
                                 (or *config-rows* (vector)))
       (with-stubbed-fdefinition (nle:config-set (path value)
                                   (push (list path value) *config-writes*)
                                   "providers.openai-codex.models: set")
         ,@body))))

(deftest codex-truly-cell-apply-writes-the-wider-window (with-codex)
  (with-cell-stop ((codex-truly-start))
    (with-codex-wire ()
      (with-config ()
        (setf *config-rows* (vector (list :id "gpt-6.1-sol" :context_window 500000)))
        (let ((object (nlk:decode-json
                       (answer "apply-1" "{\"action\":\"apply\"}"))))
          (is (eq t (nlk:json-value object :boolean "ok")) "the write is reported as done")
          (is (= 1 (length *config-writes*)) "one write of the provider's model rows")
          (let ((path (first (first *config-writes*)))
                (rows (second (first *config-writes*))))
            (is (equal '("providers" "openai-codex" "models") path) "at the row Nodecode reads a window from")
            (is (= 2 (length rows)) "both catalog models are managed")
            (is (every (lambda (row) (= 900000 (getf row :context_window))) rows) "each given the target window")
            (is (equal "gpt-6.1-sol" (getf (aref rows 0) :id)) "and the row it already had keeps its id")))))))

(deftest codex-truly-cell-a-dry-run-writes-nothing (with-codex)
  (with-cell-stop ((codex-truly-start))
    (with-codex-wire ()
      (with-config ()
        (answer "apply-2" "{\"action\":\"apply\",\"dry_run\":true}")
        (is (null *config-writes*) "a preview leaves the config alone")))))

(deftest codex-truly-cell-revert-takes-back-only-the-target (with-codex)
  (with-cell-stop ((codex-truly-start))
    (with-codex-wire ()
      (with-config ()
        (setf *config-rows*
              (vector (list :id "gpt-6.1-sol" :context_window 900000)
                      (list :id "gpt-6-sol" :context_window 872000)))
        (answer "revert-1" "{\"action\":\"revert\"}")
        (is (= 1 (length *config-writes*)) "one write")
        (let ((rows (second (first *config-writes*))))
          (is (= 1 (length rows)) "the row set to the target is gone")
          (is (equal "gpt-6-sol" (getf (aref rows 0) :id)) "and the person's own window is left alone"))))))

;;; --- probe ------------------------------------------------------------------------

(deftest codex-truly-cell-probe-bisects-the-live-ceiling (with-codex)
  (with-cell-stop ((codex-truly-start))
    (with-codex-wire ()
      (setf *probe-answer* #'accept-up-to-900k)
      (let ((object (nlk:decode-json (answer "probe-1" "{\"action\":\"probe\",\"model\":\"gpt-6.1-sol\"}"))))
        (is (eq t (nlk:json-value object :boolean "ok")) "the probe answers")
        (let ((largest (nlk:json-value object :integer "largest_accepted"))
              (smallest (nlk:json-value object :integer "smallest_rejected")))
          (is (and largest (< largest 900001)) "the largest accepted is under the backend's limit")
          (is (and smallest (> smallest 900000)) "the smallest rejection is above it")
          (is (< largest smallest) "so the true ceiling sits between the two"))
        (let ((probes (remove-if-not (lambda (call) (search "/responses" (second call))) *calls*)))
          (is (plusp (length probes)) "each round is one request to the Responses endpoint")
          (is (equal "Bearer test-token"
                     (header-value (fourth (first probes)) "authorization")) "on the same sign-in"))))))

;;; --- the contract -----------------------------------------------------------------

(deftest codex-truly-cell-a-call-without-an-action-is-refused-before-the-body (with-codex)
  (with-cell-stop ((codex-truly-start))
    (with-codex-wire ()
      (let ((text (answer "bad-1" "{\"model\":\"gpt-6.1-sol\"}")))
        (is (search "preflight" text) "the contract refuses it")
        (is (search "action" text) "naming the member")
        (is (null *calls*) "before any request")))))

;;; --- a verb with nothing started ---------------------------------------------------

(deftest codex-truly-cell-a-verb-refuses-while-not-running (with-codex)
  (setf nodecode-codex-truly::*codex-truly* nil)
  (with-codex-wire ()
    (let ((table (nodecode-codex-truly:status)))
      (is (search "not running" table) "the refusal says the cell is off")
      (is (null *calls*) "and nothing left the machine"))))
