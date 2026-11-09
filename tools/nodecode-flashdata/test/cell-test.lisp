;;;; cell-test.lisp --- the flashdata cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every endpoint is a stubbed NLK:HTTP, or the cell's own REQUEST seam;
;;;; every payload is a literal JSON text. What is proved: the cell installs
;;;; and stops, a search is one POST in the shape the MCP endpoint expects, a
;;;; JSON body and an event stream both read, a refused status and an isError
;;;; result each read as one line, a section with no key refuses, the YouTube
;;;; tool picks its operation, a long answer is cut and says so, and one
;;;; handshake serves many calls.

(in-package #:nodecode.test)

;;; --- the slice ------------------------------------------------------------------

(define-test-slice "flashdata" "FLASHDATA-CELL-" :start nodecode-flashdata:start-cell)

(define-cell-lifecycle-tests "flashdata"
  (:config "api_key" "fd-test-key")
  (:help :flashdata)
  (:command "flashdata")
  (:running (is (search "flashdata_youtube" (nle:help :flashdata)) "the manual names the tools")
            (is (and (search "data.flashdata.dev" (nodecode-flashdata::root)) t) "the endpoint is the hosted one"))
  (:refused ("api_key" "fd-test-key" "timeout_seconds" 0))
  (:idle flashdata:flashdata-error (flashdata:search "battery recycling")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter +session-headers+
  '(("mcp-session-id" . "fd-session-1") ("content-type" . "application/json"))
  "The header alist an initialize answers with: the session this process then names.")

(defparameter +handshake+
  (concatenate 'string
               "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-06-18\","
               "\"capabilities\":{},\"serverInfo\":{\"name\":\"flashdata\"}}}")
  "What the endpoint answers an initialize with.")

(defun mcp-quote (text)
  "TEXT as a JSON string."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across text
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (t (write-char character out))))
    (write-char #\" out)))

(defun mcp-content (text &key is-error)
  "One tools/call answer whose content is TEXT."
  (format nil (concatenate 'string
                          "{\"jsonrpc\":\"2.0\",\"id\":9,\"result\":{\"content\":["
                          "{\"type\":\"text\",\"text\":~a}],\"isError\":~a}}")
          (mcp-quote text) (if is-error "true" "false")))

(defun mcp-structured (payload-json &key is-error)
  "One tools/call answer carrying PAYLOAD-JSON as its structuredContent."
  (format nil (concatenate 'string
                          "{\"jsonrpc\":\"2.0\",\"id\":9,\"result\":{\"content\":[],"
                          "\"structuredContent\":~a,\"isError\":~a}}")
          payload-json (if is-error "true" "false")))

(defun flashdata-script (&rest answers)
  "An ANSWER for the stubs: ANSWERS are (TEXT STATUS HEADERS) lists, given out in order; a call
past the last one answers what a notification does."
  (let ((remaining (copy-list answers)))
    (lambda (payload)
      (declare (ignore payload))
      (values-list (or (pop remaining) (list "" 202 nil))))))

(defmacro with-flashdata-request ((calls answer) &body body)
  "BODY with the cell's own network seam stubbed: ANSWER, a function of the payload text, answers
(values TEXT STATUS HEADERS); CALLS collects every payload, newest first."
  `(let ((,calls '()))
     (with-stubbed-fdefinition (nodecode-flashdata::request (payload &key timeout)
                                  (push payload ,calls)
                                  (funcall ,answer payload))
       ,@body)))

(defmacro with-flashdata-wire ((calls answer) &body body)
  "BODY with NLK:HTTP stubbed: ANSWER, a function of the request's body text, answers
(values TEXT STATUS HEADERS); CALLS collects (METHOD URL HEADERS TEXT), newest first."
  `(let ((,calls '()))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content (timeout 30)
                                                    connect-timeout &allow-other-keys)
                                  (let ((text (if (stringp content)
                                                  content
                                                  (sb-ext:octets-to-string
                                                   content :external-format :utf-8))))
                                    (push (list method url headers text) ,calls)
                                    (funcall ,answer text)))
       ,@body)))

(defun oldest-first (calls)
  "CALLS, oldest first."
  (reverse calls))

(defun exchange-header (headers name)
  "HEADERS' value for NAME, case-insensitively, or NIL."
  (cdr (assoc name headers :test #'string-equal)))

(defun last-post (calls)
  "The newest payload CALLS recorded."
  (car (last (oldest-first calls))))

;;; --- what a call is ----------------------------------------------------------------

(deftest flashdata-cell-a-search-is-one-post-the-endpoint-understands ()
  (with-cell-stop ((flashdata-start "api_key" "fd-test-key"))
    (let ((calls '())
          (answer (flashdata-script
                   (list +handshake+ 200 +session-headers+)
                   (list "" 202 nil)
                   (list (mcp-structured
                          (concatenate 'string
                                       "{\"results\":[{\"position\":1,"
                                       "\"title\":\"Battery recycling pilot\","
                                       "\"link\":\"https://example.org/pilot\","
                                       "\"snippet\":\"A city pilot\"}],\"count\":1}"))
                         200 nil))))
      (with-flashdata-wire (calls answer)
        (let ((text (flashdata:search "battery recycling" :num 3)))
          (is (search "Battery recycling pilot" text) "the result's title is read")
          (is (search "https://example.org/pilot" text) "its link too")
          (is (search "A city pilot" text) "and its snippet")
          (let ((call (car calls)))
            (is (eq :post (first call)) "the call is a POST")
            (is (equal "https://data.flashdata.dev/mcp" (second call)) "to the hosted endpoint")
            (is (equal "fd-test-key" (exchange-header (third call) "x-api-key")) "carrying the section's key")
            (is (search "google_search" (fourth call)) "naming the operation")
            (is (search "battery recycling" (fourth call)) "and its query")
            (is (search "\"num\":3" (fourth call)) "with the count the call gave")))))))

(deftest flashdata-cell-one-handshake-serves-many-calls ()
  (with-cell-stop ((flashdata-start "api_key" "fd-test-key"))
    (let ((calls '())
          (answer (flashdata-script
                   (list +handshake+ 200 +session-headers+)
                   (list "" 202 nil)
                   (list (mcp-content "first") 200 nil)
                   (list (mcp-content "second") 200 nil))))
      (with-flashdata-wire (calls answer)
        (is (equal "first" (flashdata:search "one")))
        (is (equal "second" (flashdata:news "two")))
        (is (= 4 (length calls)) "two calls cost one handshake and two calls")
        (is (equal "fd-session-1" (exchange-header (third (third (oldest-first calls))) "mcp-session-id")) "the session the server named rides the later calls")))))

(deftest flashdata-cell-an-event-stream-reads-too ()
  (with-cell-stop ((flashdata-start "api_key" "fd-test-key"))
    (let ((calls '())
          (answer (flashdata-script
                   (list +handshake+ 200 +session-headers+)
                   (list "" 202 nil)
                   (list (format nil "event: message~%data: ~a~%~%" (mcp-content "streamed answer"))
                         200 '(("content-type" . "text/event-stream"))))))
      (with-flashdata-request (calls answer)
        (is (search "streamed answer" (flashdata:search "one")) "the event's data is the answer")))))

;;; --- refusals -----------------------------------------------------------------------

(deftest flashdata-cell-a-refused-key-is-one-line ()
  (with-cell-stop ((flashdata-start "api_key" "fd-test-key"))
    (let ((calls '())
          (answer (flashdata-script
                   (list +handshake+ 200 +session-headers+)
                   (list "" 202 nil)
                   (list "{\"error\":\"unauthorized\"}" 401 nil))))
      (with-flashdata-request (calls answer)
        (let ((message (handler-case (flashdata:search "one")
                         (flashdata:flashdata-error (condition) (princ-to-string condition)))))
          (is (search "401" message) "the refusal names the status")
          (is (search "api_key" message) "and the member to check"))))))

(deftest flashdata-cell-a-section-with-no-key-refuses ()
  (let ((message (handler-case (flashdata-start)
                   (nlk:config-refusal (condition) (princ-to-string condition)))))
    (is (search "FLASHDATA_API_KEY" message) "the refusal names the variable")
    (is (search "flashdata.dev" message) "and where a key is made")
    (is (null nodecode-flashdata::*flashdata*) "and the cell did not start")))

(deftest flashdata-cell-an-error-result-reads-as-an-error ()
  (with-cell-stop ((flashdata-start "api_key" "fd-test-key"))
    (let ((calls '())
          (answer (flashdata-script
                   (list +handshake+ 200 +session-headers+)
                   (list "" 202 nil)
                   (list (mcp-content "the account has no Credits left" :is-error t) 200 nil))))
      (with-flashdata-request (calls answer)
        (let ((text (flashdata:search "one")))
          (is (and (>= (length text) 7) (equal "ERROR: " (subseq text 0 7))) "an isError answer starts with it")
          (is (search "no Credits left" text) "and carries the server's own words"))))))

(deftest flashdata-cell-an-unknown-operation-is-refused ()
  (with-cell-stop ((flashdata-start "api_key" "fd-test-key"))
    (let ((message (handler-case
                       (nodecode-flashdata::tool-flashdata-call
                        (nodecode-flashdata::decode-flashdata-call
                         (nlk:make-json-object "operation" "google_maps" "arguments"
                                                (nlk:make-json-object "query" "coffee"))))
                     (error (condition) (princ-to-string condition)))))
      (is (search "operation must be one of" message) "the refusal lists the operations")
      (is (search "google_places" message) "including the one places is"))))

;;; --- the tools -----------------------------------------------------------------------

(deftest flashdata-cell-the-youtube-tool-picks-its-operation ()
  (with-cell-stop ((flashdata-start "api_key" "fd-test-key"))
    (let ((calls '())
          (answer (flashdata-script
                   (list +handshake+ 200 +session-headers+)
                   (list "" 202 nil)
                   (list (mcp-content "a transcript") 200 nil))))
      (with-flashdata-request (calls answer)
        (is (search "a transcript"
                    (nodecode-flashdata::tool-flashdata-youtube
                     (nodecode-flashdata::decode-flashdata-youtube
                      (nlk:make-json-object "operation" "transcript"
                                             "video_id" "dQw4w9WgXcQ" "auto" t)))) "a transcript answers")
        (let ((payload (last-post calls)))
          (is (search "youtube_transcript" payload) "the operation is YouTube's transcript")
          (is (search "dQw4w9WgXcQ" payload) "with the video it names")
          (is (search "\"auto\":true" payload) "and the track it asked for"))))))

(deftest flashdata-cell-a-long-answer-is-cut-and-says-so ()
  (with-cell-stop ((flashdata-start "api_key" "fd-test-key" "max_chars" 200))
    (let* ((calls '())
           (long (format nil "~{~a~^ ~}" (loop repeat 120 collect "word")))
           (answer (flashdata-script
                    (list +handshake+ 200 +session-headers+)
                    (list "" 202 nil)
                    (list (mcp-content long) 200 nil)
                    (list (mcp-content long) 200 nil))))
      (with-flashdata-request (calls answer)
        (let ((text (flashdata:search "one")))
          (is (< (length text) (length long)) "the answer is shorter than the server's")
          (is (search "more characters" text) "and says how to read the rest"))
        (is (> (length (flashdata:search "one" :full t)) 400) "while :full keeps all of it")))))

;;; --- the operator's command -----------------------------------------------------------

(deftest flashdata-cell-the-command-shows-the-connection ()
  (with-cell-stop ((flashdata-start "api_key" "fd-test-key"))
    (let ((text (nodecode-flashdata::run-command "" nil)))
      (is (search "data.flashdata.dev" text) "the endpoint is named")
      (is (search "key set" text) "the key is reported, never printed")
      (is (null (search "fd-test-key" text)) "so the key itself is not in it")
      (is (search "flashdata_search" text) "and the tools it puts in front of them"))))
