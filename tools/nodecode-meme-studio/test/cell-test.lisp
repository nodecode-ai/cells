;;;; cell-test.lisp --- the meme-studio cell, end to end over a scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defparameter +tiny-png+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  "A one-pixel PNG, as base64: a picture item the cell has to write out.")

(defparameter +png-signature+ '(137 80 78 71 13 10 26 10)
  "The first eight bytes of every PNG, which a saved picture has to start with.")

;;; --- lifecycle, settings and registration -----------------------------------------

(define-cell-lifecycle-tests "meme-studio"
  (:help :meme-studio)
  (:command "meme")
  (:running
   (is (nle::find-tool "suggest_templates") "the first-step tool is registered")
   (is (nle::find-tool "create_meme") "and the one that makes the meme")
   (is (equal "en" (nodecode-meme-studio::setting :language)) "the studio's language defaults to English")
   (is (equal "https://memes.mrhyde.dev/mcp" (nodecode-meme-studio::setting :endpoint)) "and every call goes to Meme Studio's one endpoint"))
  (:stopped
   (is (null (nle::find-tool "suggest_templates")) "stop takes the tools back out"))
  (:refused ("language" "fr") ("timeout_seconds" 0) ("max_image_bytes" 10)
            ("endpoint" "http://memes.example/mcp"))
  (:idle nodecode-meme-studio:meme-studio-error
         (nodecode-meme-studio::call "get_template" (nlk:make-json-object "template" "g63vdy"))))

;;; --- the tools, over the scripted endpoint ---------------------------------------

(deftest meme-studio-cell-suggests-templates (with-meme-studio ()
  (with-scripted-mcp (calls :result (mcp-text-result "{\"ok\":true,\"situation\":\"deploying on a friday\"}"))
    (let ((text (meme-tool "suggest_templates" "situation" "deploying on a friday")))
      (is (search "deploying on a friday" text) "the server's own answer comes back")
      (is (= 1 (length calls)) "one RFC call answers one tool")
      (is (equal "tools/call" (mcp-call-method (first calls))) "a tools/call")
      (is (equal "suggest_templates" (mcp-call-tool (first calls))) "to the tool the model named")
      (is (equal "deploying on a friday"
                 (nlk:json-value (mcp-call-arguments (first calls)) :string "situation")) "with the situation it was given")))))

(deftest meme-studio-cell-makes-a-meme (with-meme-studio ()
  (with-scripted-mcp (calls :result (mcp-picture-result
                                      "{\"ok\":true,\"deliver\":\"picture\"}"
                                      +tiny-png+ "image/png"))
    (let* ((text (meme-tool "create_meme"
                             "template" "g63vdy"
                             "captions" (vector (nlk:make-json-object "text" "my code" "slot" 1))))
           (start (or (search "Picture: " text) (error "no picture line in ~s" text)))
           (path (subseq text (+ start 9) (search " (" text :start2 start)))
           (signature (with-open-file (in path :element-type '(unsigned-byte 8))
                        (let ((bytes (make-array 8 :element-type '(unsigned-byte 8))))
                          (read-sequence bytes in)
                          (coerce bytes 'list)))))
      (is (probe-file path) "the picture the server sent is a file on disk")
      (is (equal +png-signature+ signature) "with the bytes it sent, not a retyped copy")
      (is (search "\"deliver\":\"picture\"" text) "and the server's own deliver word beside it")
      (is (equal "en" (nlk:json-value (mcp-call-arguments (first calls)) :string "language")) "the section's language rides a call that names none")))))

(deftest meme-studio-cell-answers-an-error-as-text (with-meme-studio ()
  (with-scripted-mcp (calls :result (mcp-error-result
                                      "{\"ok\":false,\"message\":\"No template with id zzzzzz.\"}"))
    (let ((text (meme-tool "get_template" "template" "zzzzzz")))
      (declare (ignore calls))
      (is (eql 0 (search "ERROR: " text)) "an answer the server marks as an error opens with ERROR")
      (is (search "No template with id zzzzzz." text) "and carries what it said")))))

(deftest meme-studio-cell-reports-a-refused-status (with-meme-studio ()
  ;; The server answers busy and rate_limited with a status rather than a
  ;; result: one line of repair, never a signalled condition.
  (with-stubbed-fdefinition (nodecode-meme-studio::post (request)
                               (declare (ignore request))
                               (values "{\"ok\":false,\"error\":\"busy\"}" 429))
    (let ((text (meme-tool "popular_templates" "kind" "new")))
      (is (search "HTTP 429" text) "a busy server is named with its status")
      (is (search "busy" text) "and its body")))))

(deftest meme-studio-cell-reads-a-streamed-answer (with-meme-studio ()
  ;; A Streamable HTTP server may answer one event, not a bare body.
  (with-stubbed-fdefinition (nodecode-meme-studio::post (request)
                               (declare (ignore request))
                               (values (format nil "event: message~%data: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"hi\"}]}}~%~%")
                                       200))
    (is (equal "hi" (meme-tool "get_template" "template" "g63vdy")) "a streamed answer is read like a JSON one"))))

(deftest meme-studio-cell-command-finds-a-template (with-meme-studio ()
  (with-scripted-mcp (calls :result (mcp-text-result "{\"ok\":true}"))
    (let ((text (meme-command "friday deploy")))
      (is (search "ok" text) "the command answers the suggestion")
      (is (equal "friday deploy"
                 (nlk:json-value (mcp-call-arguments (first calls)) :string "situation")) "for the joke it was given")))
  (multiple-value-bind (text dialog) (meme-command "")
    (is (stringp text) "bare /meme answers a line")
    (is dialog "and a row to type the joke in"))))
