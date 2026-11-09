;;;; cell-test.lisp --- lifecycle, the detectors, the advice, the tool and /vaultknox.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring and its own rules: a start
;;;; installs one advice, one tool, one command and one manual topic and a
;;;; stop takes them back, a section the declaration cannot read refuses and
;;;; leaves nothing behind, the detectors replace what they say they replace
;;;; and leave prose alone, the advice scrubs the messages of a request
;;;; without touching the folded message itself, it rewrites a secret-asking
;;;; assistant turn, it hands the fold's second value back, and a narrowed
;;;; detector list stops the others looking.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "vaultknox"
  (:hooks 'nodecode.evolved::wire-messages-from-events)
  (:command "vaultknox")
  (:running (is (nle::find-tool "scan_secrets") "the tool is registered")
            (is (= 28 (length (nodecode-vaultknox::setting :detectors))) "every detector runs by default")
            (is (assoc :vaultknox nle::*help-topics*) "(help :vaultknox) is on the manual"))
  (:stopped (is (null (nle::find-tool "scan_secrets")) "stop unregisters the tool")
            (is (null (vaultknox-wire-hook)) "and takes the advice off the fold")
            (is (null (nle::find-registered-command "vaultknox")) "and the command"))
  (:refused ("detectors" (vector "passport"))))

(deftest vaultknox-cell-refuses-a-shape-it-cannot-run (with-saved-globals (nle:*tools*))
  (is (signals-error nlk:config-refusal (vaultknox-start "detectors" "openai")) "a string where the array belongs is refused")
  (is (null (nle::find-tool "scan_secrets")) "and the refused start installed nothing")
  (is (null (vaultknox-wire-hook)) "not even the advice"))

;;; --- the detectors -------------------------------------------------------------

(deftest vaultknox-cell-redacts-the-keys-it-knows (with-cell-stop ((vaultknox-start))
  (let ((text (format nil "openai ~a, github ~a, aws AKIAIOSFODNN7EXAMPLE, jwt eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.abcdefghijklmnop"
                       +openai-key+ +github-token+))
        (redacted nil))
    (multiple-value-bind (scrubbed hits) (nodecode-vaultknox:redact-text text)
      (setf redacted scrubbed)
      (is (= 4 (length hits)) "four secrets, no more"))
    (is (not (search +openai-key+ redacted)) "an OpenAI key is named, not shown")
    (is (not (search +github-token+ redacted)) "and so is a GitHub token")
    (is (not (search "AKIAIOSFODNN7EXAMPLE" redacted)) "an AWS key id is a secret")
    (is (not (search "eyJhbGciOiJIUzI1NiJ9" redacted)) "and a JSON web token is one")
    (is (search "[REDACTED-SENSITIVE-VALUE]" redacted) "the placeholder stands where they were"))))

(deftest vaultknox-cell-names-what-it-found (with-cell-stop ((vaultknox-start))
  (let ((hits (nodecode-vaultknox:scan-hits (format nil "key ~a and ~a" +openai-key+ +github-token+))))
    (is (= 2 (length hits)) "one group a secret")
    (is (equal "OpenAI API key" (nodecode-vaultknox::hit-name (first hits))) "and the first is named")
    (is (equal "critical" (nodecode-vaultknox::hit-severity (first hits))) "with its severity"))))

(deftest vaultknox-cell-names-an-overlap-once (with-cell-stop ((vaultknox-start))
  (is (= 1 (length (nodecode-vaultknox:scan-hits "api_key=abcdefghijklmnopqrstuvwx"))) "a value two patterns read is one secret")
  (is (equal "hello world, build 12345678" (nodecode-vaultknox:redact-text "hello world, build 12345678")) "plain prose is left whole")
  (is (equal "sk-short" (nodecode-vaultknox:redact-text "sk-short")) "a prefix without a body is no key")))

(deftest vaultknox-cell-a-narrowed-list-stops-the-others-looking (with-cell-stop ((vaultknox-start "detectors" (vector "openai")))
  (is (equal "[REDACTED-SENSITIVE-VALUE]" (nodecode-vaultknox:redact-text +openai-key+)) "the listed detector runs")
  (is (equal "AKIAIOSFODNN7EXAMPLE" (nodecode-vaultknox:redact-text "AKIAIOSFODNN7EXAMPLE")) "and an unlisted one does not")))

;;; --- the advice on the fold -----------------------------------------------------

(deftest vaultknox-cell-scrubs-the-wire-not-the-log (with-cell-stop ((vaultknox-start))
  (let* ((system (vaultknox-message "system" (format nil "the key is ~a" +openai-key+)))
         (user (vaultknox-message "user" (format nil "my key is ~a thanks" +openai-key+)))
         (assistant (vaultknox-message "assistant" "paste your api key in the box"))
         (answer (multiple-value-list (vaultknox-wire-answer (list system user assistant))))
         (scrubbed (first answer))
         (user-text (gethash "content" (second scrubbed)))
         (assistant-text (gethash "content" (third scrubbed))))
    (is (eq :clean (second answer)) "the fold's clean flag comes back untouched")
    (is (search +openai-key+ (gethash "content" (first scrubbed))) "the system prompt is never read")
    (is (not (search +openai-key+ user-text)) "a key in a user message is redacted on the way out")
    (is (search "[REDACTED-SENSITIVE-VALUE]" user-text) "with the placeholder standing where it was")
    (is (search "Security notice" assistant-text) "an assistant turn that asks for a key is rewritten")
    (is (not (search "paste your api key" assistant-text)) "the asking phrase is gone")
    (is (search +openai-key+ (gethash "content" user)) "the folded message is untouched: the log keeps the original")
    (is (not (eq user (second scrubbed))) "what changed is a copy"))))

;;; --- the tool and the command ---------------------------------------------------

(deftest vaultknox-cell-the-tool-and-the-command-answer (with-cell-stop ((vaultknox-start))
  (let ((answer (vaultknox-tool-call (format nil "here it is: ~a" +github-token+))))
    (is (search "[REDACTED-SENSITIVE-VALUE]" answer) "the tool answers the redacted text")
    (is (not (search +github-token+ answer)) "and never the token itself")
    (is (search "redacted 1 secret value" answer) "and says what it found"))
  (is (search "unknown detector(s) ignored" (vaultknox-tool-call "nothing here" "passport")) "a detector it does not know is named, not obeyed")
  (is (search "nothing found" (vaultknox-tool-call "nothing to see")) "and an empty result says so")
  (is (search "vaultknox: 28 detector(s) watching" (vaultknox-slash "")) "the command answers its state")
  (is (search "[REDACTED-SENSITIVE-VALUE]" (vaultknox-slash "key AKIAIOSFODNN7EXAMPLE")) "or previews any text")))
