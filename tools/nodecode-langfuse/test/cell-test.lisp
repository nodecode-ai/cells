;;;; cell-test.lisp --- lifecycle, the batch, the capture, /langfuse.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the config surface (a
;;;; healthy section installs both hooks and the command, a vetoed one
;;;; installs nothing, a section without keys and a base URL that is not a web
;;;; address are the loader's loud refusals), that one turn becomes one
;;;; ingestion batch carrying a trace, a generation with the round's usage and
;;;; cost, and a span per tool call, that a refused batch stays held and
;;;; /langfuse send retries it, that sanitized redacts before it cuts and
;;;; metadata carries no content, and that /langfuse answers every verb
;;;; through the stubbed endpoint.

(in-package #:nodecode.test)

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "langfuse"
  (:fixture with-stubbed-ingestion)
  (:config "capture" "sanitized")
  (:hooks :frame :tool)
  (:command "langfuse")
  (:running (is (equal "https://langfuse.test" (nodecode-langfuse::base-url)))
            (is (equal "https://langfuse.test/api/public/ingestion"
                       (nodecode-langfuse:ingestion-url)))
            (is (hash-table-p nodecode-langfuse::*traces*) "the held batches have a home")
            (is (search "capture sanitized" (langfuse:status))))
  (:stopped (is (null nodecode-langfuse::*traces*) "stop forgets what it held"))
  (:refused ("capture" "everything")
            ("base_url" "not a web address")))

;;; --- one turn, one batch ------------------------------------------------------

(deftest langfuse-cell-a-turn-sends-one-trace-with-its-call-and-its-span (with-langfuse ())
  (cell-publish "nodecode-langfuse"
                (cell-frame-op "turn.usage" :turn "t1" :payload (usage-payload)))
  (is (null *posts*) "a round on its own sends nothing")
  (is (equal "done" (run-tool-call "sh" (nlk:json-object "command" "ls -la") "done")) "the tool answers exactly what it would have")
  (is (null *posts*) "and the call waits with its trace")
  (end-the-turn)
  (is (= 1 (length *posts*)) "the turn's end sends one batch")
  (is (equal "https://langfuse.test/api/public/ingestion" (getf (first *posts*) :url)))
  (is (search "Basic " (cdr (assoc "authorization" (getf (first *posts*) :headers) :test #'equal))) "the project's keys ride as basic auth")
  (is-present (trace (event-of-type "trace-create"))
    "the batch opens with its trace, so every span below it has a home"
    (let ((body (event-body trace)))
      (is (equal "Nodecode turn" (gethash "name" body)))
      (is (equal "s1" (gethash "sessionId" body)) "one Langfuse session per Nodecode session")
      (is (equal "nodecode" (aref (gethash "tags" body) 0)))
      (is (equal "sanitized" (gethash "capture_mode" (gethash "metadata" body))))
      (is (equal "completed" (gethash "status" (gethash "metadata" body))))))
  (is-present (generation (event-of-type "generation-create"))
    "the round is a generation"
    (let* ((body (event-body generation))
           (usage (gethash "usageDetails" body))
           (metadata (gethash "metadata" body)))
      (is (equal "claude-langfuse-1" (gethash "model" body)))
      (is (equal 1200 (gethash "input" usage)))
      (is (equal 300 (gethash "output" usage)))
      (is (equal 4000 (gethash "cache_read_input_tokens" usage)))
      (is (equal 120 (gethash "output_reasoning_tokens" usage)))
      (is (< (abs (- 0.0123d0 (coerce (gethash "total" (gethash "costDetails" body))
                                       'double-float)))
             1d-9) "the cost the core priced rides as costDetails")
      (is (equal "stop" (gethash "finish_reason" metadata)))))
  (is-present (span (event-of-type "span-create"))
    "the call is a span under it"
    (let ((body (event-body span)))
      (is (equal "sh" (gethash "name" body)))
      (is (equal "ls -la" (gethash "command" (gethash "input" body))))
      (is (equal "done" (gethash "output" body)))
      (is (null (gethash "level" body)) "a call that worked is not an error"))))

(deftest langfuse-cell-a-failed-call-is-an-error-span (with-langfuse ())
  (cell-publish "nodecode-langfuse"
                (cell-frame-op "turn.usage" :turn "t1" :payload (usage-payload)))
  (run-tool-call "eval" (nlk:json-object "form" "(/ 1 0)")
                 "ERROR: DIVISION-BY-ZERO: /")
  (end-the-turn)
  (let ((body (event-body (event-of-type "span-create"))))
    (is (equal "ERROR" (gethash "level" body)) "a result that says ERROR is drawn failed")
    (is (equal "eval" (gethash "name" body)))))

(deftest langfuse-cell-a-failed-turn-is-a-failed-trace (with-langfuse ())
  (cell-publish "nodecode-langfuse"
                (cell-frame-op "turn.usage" :turn "t1" :payload (usage-payload)))
  (end-the-turn "t1" "turn.failed")
  (is (equal "failed" (gethash "status"
                                (gethash "metadata"
                                         (event-body (event-of-type "trace-create")))))))

;;; --- a refused batch is held, and /langfuse send retries it -------------------

(deftest langfuse-cell-a-refused-batch-is-held-and-send-retries-it (with-langfuse ())
  (setf *answer* "HTTP 500 from langfuse")
  (cell-publish "nodecode-langfuse"
                (cell-frame-op "turn.usage" :turn "t1" :payload (usage-payload)))
  (end-the-turn)
  (is (= 1 (length *posts*)) "the batch went once")
  (is (search "HTTP 500" nodecode-langfuse::*last-error*) "the refusal is kept")
  (is (= 1 (nodecode-langfuse::held-count)) "and the batch is still held")
  (is (search "HTTP 500" (langfuse:status)) "the status line says why")
  (setf *answer* t)
  (is (search "sent 1 trace" (cell-entry "nodecode-langfuse" "langfuse" "send")) "send retries what is held")
  (is (= 2 (length *posts*)) "a second attempt")
  (is (zerop (nodecode-langfuse::held-count)) "and it is gone once it went")
  (is (null nodecode-langfuse::*last-error*))
  (is (search "nothing held" (cell-entry "nodecode-langfuse" "langfuse" "send"))))

;;; --- capture ------------------------------------------------------------------

(deftest langfuse-cell-sanitized-redacts-then-cuts (with-langfuse ("max_chars" 200))
  (is (equal "the key <redacted> is pasted"
             (nodecode-langfuse:redact-string "the key sk-lf-abcdef123456 is pasted")))
  (is (equal "password=<redacted>"
             (nodecode-langfuse:redact-string "password=hunter2")))
  (is (equal "the key <redacted> is pasted again"
             (nodecode-langfuse:redact-string "the key pk-lf-9f8e7d6c5b4a is pasted again")) "a pasted project key is replaced wherever it stands")
  (let ((cut (nodecode-langfuse:capture-string (make-string 500 :initial-element #\x))))
    (is (= 200 (length (subseq cut 0 (or (search "…" cut) 0)))) "cut at the section's ceiling")
    (is (search "300 chars more" cut) "and it says how much was left out"))
  (is (= 20 (length (nodecode-langfuse:capture-string (make-string 20 :initial-element #\x)))) "text under the ceiling is whole"))

(deftest langfuse-cell-metadata-mode-carries-no-content (with-langfuse ("capture" "metadata"))
  (cell-publish "nodecode-langfuse"
                (cell-frame-op "turn.usage" :turn "t1" :payload (usage-payload)))
  (run-tool-call "sh" (nlk:json-object "command" "ls -la") "done")
  (end-the-turn)
  (let* ((body (event-body (event-of-type "span-create")))
         (input (gethash "command" (gethash "input" body))))
    (is (eq t (gethash "omitted" input)) "the arguments are a shape stub")
    (is (= 6 (gethash "chars" input)) "that says how long they were")
    (is (eq t (gethash "omitted" (gethash "output" body))) "and so is the result"))
  (is (equal "metadata" (gethash "capture_mode"
                                  (gethash "metadata" (event-body (event-of-type "trace-create")))))))

;;; --- the endpoint, and the verbs ------------------------------------------------

(deftest langfuse-cell-the-url-and-the-auth-are-the-projects (with-langfuse ("base_url" "https://langfuse.test/"))
  (is (equal "https://langfuse.test/api/public/ingestion" (langfuse:ingestion-url)) "a trailing slash on the base is not doubled")
  (is (equal "Basic dXNlcjpwYXNz" (langfuse:basic-auth "user" "pass")) "RFC 4648 basic auth over the two keys"))

(deftest langfuse-cell-the-slash-command-answers-through-stubbed-io (with-langfuse ())
  (let ((line (cell-entry "nodecode-langfuse" "langfuse" "")))
    (is (search "api/public/ingestion" line))
    (is (search "capture sanitized" line))
    (is (search "0 traces held" line)))
  (is (search "a test trace went" (cell-entry "nodecode-langfuse" "langfuse" "test")) "the smallest proof the keys work")
  (is (= 1 (length *posts*)))
  (is (equal "nodecode-langfuse test"
             (gethash "name" (event-body (event-of-type "trace-create")))))
  (is (search "nothing held" (cell-entry "nodecode-langfuse" "langfuse" "show")))
  (is (search "dropped" (cell-entry "nodecode-langfuse" "langfuse" "clear")))
  (is (search "not one of its choices" (cell-entry "nodecode-langfuse" "langfuse" "wobble"))))

;;; --- a section that cannot work refuses -----------------------------------------

(deftest langfuse-cell-a-section-without-keys-refuses ()
  (with-stubbed-ingestion
    (is (signals-error nlk:config-refusal
          (nodecode-langfuse:start-cell
           (nlk:json-object "langfuse" (nlk:json-object "capture" "sanitized")))) "no public and secret key: the loader's refusal")
    (is (null (cell-hook "nodecode-langfuse" :frame)) "and nothing was installed")
    (is (null (cell-hook "nodecode-langfuse" :tool)))
    (is (search "web address"
                (handler-case
                    (progn (nodecode-langfuse:start-cell
                            (nlk:json-object
                             "langfuse" (nlk:json-object "public_key" "pk-lf-test"
                                                          "secret_key" "sk-lf-test"
                                                          "base_url" "not a web address")))
                           "")
                  (nlk:config-refusal (condition) (nlk:config-refusal-detail condition)))) "a base URL that is not a web address refuses too")))
