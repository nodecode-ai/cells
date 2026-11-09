;;;; cell-test.lisp --- lifecycle, the two hook points, the tools, /otel.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: a start installs the two
;;;; hooks and the three tools and a stop takes them back, a bad section
;;;; refuses with nothing installed, a tool call becomes one span with its
;;;; arguments and its outcome, a turn's durable facts become spans, and the
;;;; local file answers what was kept — bounded, newest first. The one seam
;;;; that reaches outside is NODECODE-OTEL::*POST*: WITH-STUBBED-EXPORT
;;;; captures every post, so nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "otel" "OTEL-CELL-" :start nodecode-otel:start-cell)

(defvar *posts* '()
  "(URL BODY HEADERS) the stubbed endpoint received, newest first.")

(defvar *post-answer* t
  "What the stubbed endpoint answers: T, or the reason it refuses with.")

(defmacro with-stubbed-export (&body body)
  "Run BODY with the endpoint stubbed: every post lands on *POSTS*."
  `(with-saved-globals (nodecode-otel::*post* nodecode-otel::*pending*
                        nodecode-otel::*last-export* nodecode-otel::*recorded*
                        nodecode-otel::*exported* nodecode-otel::*stored*
                        nle:*tools*)
     (setf *posts* '()
           *post-answer* t
           nodecode-otel::*pending* nil
           nodecode-otel::*last-export* nil
           nodecode-otel::*recorded* 0
           nodecode-otel::*exported* 0
           nodecode-otel::*post*
           (lambda (url body headers)
             (push (list url body headers) *posts*)
             (if (eq *post-answer* t)
                 t
                 (values nil *post-answer*))))
     ,@body))

(defun mark-record (name &optional (detail ""))
  "The MARK a tool call would materialize, for a test that skips the wire."
  (nodecode-otel::decode-mark (nlk:json-object "name" name "detail" detail)))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "otel"
  (:config "endpoint" "" "local_store" t)
  (:hooks :tool :frame)
  (:command "otel")
  (:running
   (is (nle::find-tool "otel_status") "otel_status is registered")
   (is (nle::find-tool "otel_spans") "otel_spans is registered")
   (is (nle::find-tool "otel_mark") "otel_mark is registered")
   (is (equal "" (nodecode-otel::setting :endpoint)) "local only, as the section says")
   (is (equal "preview" (nodecode-otel::setting :content-capture)) "a preview by default"))
  (:stopped
   (is (null (nle::find-tool "otel_status")) "stop unregisters the tools")
   (is (null (nle::find-tool "otel_mark")) "all three"))
  (:refused
   ("content_capture" "verbose")
   ("local_store" nil "endpoint" "")))

;;; --- one tool call, through the real dispatch -----------------------------------

(deftest otel-cell-mark-records-and-sends-one-span ()
  (with-stubbed-export
    (with-session-store ("s1")
      (with-cell-stop ((otel-start "endpoint" "http://collector.test/v1/traces" "local_store" nil))
        (let ((answer (execute-wire-call
                       "call-1" "otel_mark"
                       "{\"name\":\"first\",\"detail\":\"a marker\"}")))
          (is (equal "tool" (gethash "role" answer)) "a tool message comes back")
          (is (search "marked" (princ-to-string (nle::message-content answer)))
              "and it says the mark was kept"))
        (is (= 2 nodecode-otel::*recorded*) "the mark and the call that made it")
        (nodecode-otel::flush-spans)
        (is (plusp (length *posts*)) "the spans went out")
        (let* ((post (first *posts*))
               (body (second post))
               (headers (third post)))
          (is (equal "http://collector.test/v1/traces" (first post)) "to the endpoint the section names")
          (is (search "mark first" body) "the mark is a span in the request")
          (is (search "tool otel_mark" body) "and so is the call that made it")
          (is (search "stringValue" body) "an OTLP attribute")
          (is (equal "application/json" (cdr (assoc "content-type" headers :test #'string=)))
              "posted as JSON"))))))

;;; --- the tool hook -------------------------------------------------------------

(deftest otel-cell-a-failed-tool-call-is-an-error-span ()
  (with-stubbed-export
    (with-cell-stop ((otel-start "endpoint" "http://collector.test/v1/traces" "local_store" nil))
      (let ((hook (cell-hook "nodecode-otel" :tool)))
        (is (functionp hook) "the :tool hook is installed")
        (let ((answer (funcall hook (list :name "eval"
                                         :arguments (nlk:json-object "form" "(+ 1 1)")
                                         :call-id "c1")
                               (lambda (op) (declare (ignore op)) (nle:failure "ERROR: boom")))))
          (is (equal "ERROR: boom" answer) "the call's own answer passes through untouched"))
        (let ((span (first nodecode-otel::*pending*)))
          (is (equal "tool eval" (nodecode-otel::span-name span)) "one span per call")
          (is (equal "error" (nodecode-otel::span-status span)) "a failed call is an error span")
          (let ((arguments (cdr (assoc "nodecode.tool.arguments"
                                        (nodecode-otel::span-attrs span) :test #'string=))))
            (is (search "(+ 1 1)" arguments) "with the arguments it was called on")
            (is (search "form" arguments) "as the call wrote them"))
          (is (= 11 (cdr (assoc "nodecode.tool.result_chars"
                                 (nodecode-otel::span-attrs span) :test #'string=)))
              "and how much came back"))))))

;;; --- a turn's durable facts -----------------------------------------------------

(deftest otel-cell-frame-facts-become-spans ()
  (with-stubbed-export
    (with-cell-stop ((otel-start "endpoint" "http://collector.test/v1/traces" "local_store" nil))
      (cell-publish "nodecode-otel"
                    (cell-frame-op "turn.usage"
                                   :payload (nlk:json-object "model" "a-model" "provider" "a-provider"
                                                              "input-tokens" 12 "output-tokens" 3))
                    nil)
      (cell-publish "nodecode-otel"
                    (cell-frame-op "turn.provider_request"
                                   :payload (nlk:json-object "model" "a-model" "provider" "a-provider"
                                                              "ttft-ms" 120 "stream-duration-ms" 900))
                    nil)
      (let ((names (mapcar #'nodecode-otel::span-name (reverse nodecode-otel::*pending*))))
        (is (member "usage a-provider/a-model" names :test #'equal) "a round's usage is a span")
        (is (member "chat a-provider/a-model" names :test #'equal) "and so is the round itself"))
      (let ((usage (find "usage a-provider/a-model" nodecode-otel::*pending*
                         :key #'nodecode-otel::span-name :test #'equal)))
        (is (= 12 (cdr (assoc "gen_ai.usage.input_tokens"
                               (nodecode-otel::span-attrs usage) :test #'string=)))
            "with the token counts the turn loop recorded"))
      ;; An ordinary frame, and a durable one this cell does not trace, add nothing.
      (cell-publish "nodecode-otel" (cell-frame-op "item_delta") nil)
      (cell-publish "nodecode-otel" (cell-frame-op "turn.tool_result") nil)
      (is (= 2 (length nodecode-otel::*pending*)) "one span per traced fact, and only those"))))

;;; --- the local file, and /otel --------------------------------------------------

(deftest otel-cell-the-local-file-is-bounded-and-answers-the-tools ()
  (with-stubbed-export
    (with-session-store ("s1")
      (with-cell-stop ((otel-start "endpoint" "" "local_store" t "max_spans" 3))
        (nodecode-otel::clear-store)
        (dotimes (index 5)
          (nodecode-otel::otel-mark (mark-record (format nil "m~d" index))))
        (is (= 3 (length (nodecode-otel::read-store))) "the file keeps max_spans of them")
        (let ((text (nodecode-otel::spans-text 10 "")))
          (is (search "mark m4" text) "newest first, and the newest is there")
          (is (null (search "mark m1" text)) "the trimmed ones are gone"))
        (let ((answer (execute-wire-call "call-2" "otel_spans"
                                        "{\"limit\":2,\"match\":\"mark\"}")))
          (is (equal "tool" (gethash "role" answer)) "the query tool answers through dispatch")
          (is (search "mark m4" (princ-to-string (nle::message-content answer)))
              "with the newest marked span, read off the local file"))
        (let ((status (nodecode-otel::otel-status)))
          (is (search "keeping spans locally" status) "status says where the spans stay")
          (is (search "capture preview" status) "and how much text rides along"))
        (is (search "cleared 3 spans" (nodecode-otel::run-otel "clear" "s1"))
            "/otel clear empties the file")
        (is (null (nodecode-otel::read-store)) "and it is empty")))))

(deftest otel-cell-a-missing-sink-refuses-the-start ()
  (with-stubbed-export
    (is (search "would go nowhere"
                (handler-case
                    (progn (otel-start "endpoint" "" "local_store" nil) "")
                  (nlk:config-refusal (condition) (nlk:config-refusal-detail condition))))
        "no endpoint and no local file is a refusal, in words that name the keys")
    (is (null (nle::find-tool "otel_mark")) "and nothing was installed")))
