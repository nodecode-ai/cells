;;;; cell-test.lisp --- the gate's lifecycle, its window, and its lines.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The seams the cell reads are the cell's own: the clock and the wait are
;;;; two specials a test binds, and the provider transport is a stub, so every
;;;; test here runs offline and nothing sleeps.

(in-package #:nodecode.test)

(define-test-slice "lightning-ai" "LIGHTNING-AI" :start nodecode-lightning-ai:start-cell)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "lightning-ai"
  (:config "hosts" (vector "lightning.ai"))
  (:hooks 'nodecode.evolved::walk-provider-stream)
  (:command "lightning-ai")
  (:help :lightning-ai)
  (:running
   (is (search "lightning.ai" (nodecode-lightning-ai::status-text)) "the report names the host the gate holds"))
  (:refused nil))

;;; --- helpers -----------------------------------------------------------------

(defvar *waits* '()
  "What the stubbed sleeper was asked for while a test ran, newest first.")

(defmacro with-gate-clock ((&key (start 1000.0)) &body body)
  "Run BODY on a clock of the test's own: the gate's sleeper moves it forward
by exactly what it asked for, and records the ask in *WAITS*."
  `(let* ((now ,start)
          (*waits* '())
          (nodecode-lightning-ai::*clock* (lambda () now))
          (nodecode-lightning-ai::*sleeper* (lambda (seconds)
                                               (push seconds *waits*)
                                               (incf now seconds))))
     ,@body))

(defun lightning-ai-hook ()
  "The installed advice on the provider transport, or NIL when it is not running."
  (cell-hook "nodecode-lightning-ai" 'nodecode.evolved::walk-provider-stream))

(defun lightning-ai-provider-config (endpoint)
  "A frozen provider config pointing at ENDPOINT, as a turn carries one."
  (nle::make-effective-provider-config
   :provider "lightning" :lane "openai-completions" :endpoint endpoint
   :model "fixture" :api-key "inert"))

(defun lightning-ai-call (endpoint &key error-text)
  "The advice funcalled by hand around a stubbed transport => (values ANSWER CALLS)."
  (let ((calls 0))
    (values (funcall (lightning-ai-hook)
                      (lambda (&rest args)
                        (declare (ignore args))
                        (incf calls)
                        (if error-text (error "~a" error-text) :answer))
                      :fold :config (lightning-ai-provider-config endpoint))
            calls)))

;;; --- the window --------------------------------------------------------------

(deftest lightning-ai-gate-waits-when-the-window-is-full ()
  (with-cell-stop ((lightning-ai-start "hosts" (vector "lightning.ai") "max_per_minute" 2))
    (with-gate-clock ()
      (multiple-value-bind (answer calls)
          (lightning-ai-call "https://lightning.ai/api/v1/chat/completions")
        (is (eq :answer answer) "a call under the limit goes through")
        (is (= 1 calls) "and is made once"))
      (lightning-ai-call "https://lightning.ai/api/v1/chat/completions")
      (is (null *waits*) "two calls fit in one window")
      (lightning-ai-call "https://lightning.ai/api/v1/chat/completions")
      (is (= 1 (length *waits*)) "the third waits for the next minute")
      (is (< 19 (first *waits*) 22)
          (format nil "the wait runs to the boundary and the settle margin; it was ~a"
                  (first *waits*))))))

(deftest lightning-ai-other-hosts-are-never-held ()
  (with-cell-stop ((lightning-ai-start "hosts" (vector "lightning.ai") "max_per_minute" 1))
    (with-gate-clock ()
      (lightning-ai-call "https://api.example.com/v1/chat/completions")
      (lightning-ai-call "https://api.example.com/v1/chat/completions")
      (is (null *waits*) "a provider that is not Lightning AI is never made to wait")
      (is (search "0 gated" (nodecode-lightning-ai::status-text)) "and nothing is counted for it"))))

(deftest lightning-ai-a-429-parks-the-rest-of-the-window ()
  (with-cell-stop ((lightning-ai-start "hosts" (vector "lightning.ai") "max_per_minute" 15))
    (with-gate-clock ()
      (lightning-ai-call "https://lightning.ai/api/v1/chat/completions")
      (is (signals-error error
                         (lightning-ai-call "https://lightning.ai/api/v1/chat/completions"
                                             :error-text "HTTP 429 Too Many Requests")) "the provider's refusal reaches the caller")
      (lightning-ai-call "https://lightning.ai/api/v1/chat/completions")
      (is (= 1 (length *waits*)) "the server said its counter was spent, so the next call waits after only two"))))

;;; --- the command, and a start that refuses ------------------------------------

(deftest lightning-ai-command-reports-the-window-and-resets-it ()
  (with-cell-stop ((lightning-ai-start "hosts" (vector "lightning.ai") "max_per_minute" 15))
    (with-gate-clock ()
      (lightning-ai-call "https://lightning.ai/api/v1/chat/completions")
      (let ((status (nodecode-lightning-ai::run-slash "" nil)))
        (is (search "1 of 15" status) "the report counts this window")
        (is (search "lightning.ai" status) "and names the host it gates"))
      (is (search "cleared" (nodecode-lightning-ai::run-slash "reset" nil)) "reset clears the counters")
      (is (search "0 of 15" (nodecode-lightning-ai::run-slash "status" nil)) "and the window starts again")
      (is (search "unknown argument" (nodecode-lightning-ai::run-slash "bogus" nil)) "an unknown word refuses"))))

(deftest lightning-ai-a-start-with-no-hosts-refuses-and-installs-nothing ()
  (let ((text (refusal-text nlk:config-refusal (lightning-ai-start))))
    (is (search "hosts" text) "the refusal names the member to set")
    (is (null (lightning-ai-hook)) "and nothing was installed")))
