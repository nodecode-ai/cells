;;;; adapter-test.lisp --- candidates, plans and the channel entry, offline.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun wa-agent-message (&key (from "15550001111") (id "wamid.1") (text "hi") (type "text"))
  "One inbound message in the platform's webhook shape."
  (nlk:json-object "id" id "from" from "timestamp" 1700000000 "type" type
                    "profile" (nlk:json-object "name" "kim")
                    :when (string= type "text")
                    "text" (nlk:json-object "body" text)))

(deftest channel-whatsapp-agent-a-message-is-a-candidate ()
  (let ((candidate (first (ncwa:message-candidates (wa-agent-message)))))
    (is candidate "a message with an id and a sender is a candidate")
    (is (equal "hi" (nlk:json-value candidate :string "text")))
    (is (equal "15550001111" (nck:source-field candidate "channel-id")) "the chat is the sender: an agent chat is a direct message")
    (is (equal "wamid.1" (nck:source-field candidate "message-id")))
    (is (equal 1700000000000 (gethash "sent_ms" (gethash "source" candidate)))))
  (is (null (ncwa:message-candidates (wa-agent-message :from ""))) "a message without a sender is no candidate")
  (is (null (ncwa:message-candidates (wa-agent-message :from "9999") :agent-id "9999")) "the agent's own message is not an ask"))

(deftest channel-whatsapp-agent-a-reaction-is-no-ask ()
  (is (null (ncwa:message-candidates
             (nlk:json-object "id" "wamid.9" "from" "15550001111" "type" "reaction"
                              "reaction" (nlk:json-object "message_id" "wamid.1" "emoji" "👍")))) "a reaction says what someone thought, not a thing to answer"))

(defun wa-temp-file (name content)
  "NAME under a fresh temporary directory, holding CONTENT, as a pathname."
  (let* ((directory (merge-pathnames (format nil "ncwa-~a/" (random 1000000))
                                      (uiop:temporary-directory)))
         (path (merge-pathnames name directory)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
      (write-sequence (map 'vector #'char-code content) out))
    path))

(deftest channel-whatsapp-agent-a-file-goes-as-its-kind ()
  (is (equal "image" (ncwa:media-kind #p"/tmp/photo.JPG")))
  (is (equal "video" (ncwa:media-kind #p"/tmp/clip.mp4")))
  (is (equal "audio" (ncwa:media-kind #p"/tmp/note.ogg")))
  (is (equal "document" (ncwa:media-kind #p"/tmp/report.pdf")))
  (is (equal "image/jpeg" (ncwa:media-mime #p"/tmp/photo.jpg"))))

(deftest channel-whatsapp-agent-a-file-is-uploaded-then-sent ()
  (let* ((path (wa-temp-file "shot.png" "png"))
         (executor (nck:make-recording-executor
                    :responses (list (nck:make-scripted-response
                                      200 (nlk:json-object "id" "media-1")))))
         (plan (ncwa:file-plan (nlk:json-object "channel-id" "15550001111") path
                               :executor executor :reply-to "wamid.0"
                               :caption "the build"))
         (plans (nck:recording-executor-plans executor)))
    (is (= 1 (length plans)) "the upload went out from here")
    (is (equal "media" (nck:request-plan-path (first plans))))
    (is (equal "image/png" (cdr (assoc "type" (nck:request-plan-body (first plans))
                                         :test #'equal))) "the multipart names the type it uploads as")
    (is (equal "messages" (nck:request-plan-path plan)))
    (is (equal "image" (nlk:json-value (nck:request-plan-body plan) :string "type")))
    (is (equal "media-1" (nlk:json-value (nck:request-plan-body plan) :string "image" "id")))
    (is (equal "the build" (nlk:json-value (nck:request-plan-body plan) :string "image" "caption")))
    (is (equal "wamid.0" (nlk:json-value (nck:request-plan-body plan) :string "context" "message_id")))))

(deftest channel-whatsapp-agent-a-file-arrives ()
  (let* ((candidate (first (ncwa:message-candidates
                            (nlk:json-object "id" "wamid.2" "from" "15550001111"
                                             "type" "image"
                                             "image" (nlk:json-object "id" "media-9"
                                                                       "mime_type" "image/png"
                                                                       "file_size" 3)))))
         (article (aref (nlk:json-array candidate "attachments") 0))
         (octets (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
    (is (equal "media-9" (gethash "download" article)) "the platform's id rides to the fetch")
    (ncwa:attach-media-fetchers (ncwa::%make-wa-adapter :key "k" :base ncwa:+api-base+)
                                 candidate)
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                               (declare (ignore))
                               (if (search "/media/" url)
                                   (values "{\"url\": \"https://lookaside.fbsbx.com/whatsapp/1\", \"file_size\": 3}"
                                           200)
                                   (values octets 200)))
      (is (equalp octets (funcall (gethash "fetch" article))) "the thunk answers the bytes"))))

(deftest channel-whatsapp-agent-a-file-is-off-when-so-said ()
  (let ((adapter (ncwa::%make-wa-adapter :key "k" :media-enabled nil))
        (candidate (first (ncwa:message-candidates
                           (nlk:json-object "id" "wamid.3" "from" "15550001111"
                                            "type" "image"
                                            "image" (nlk:json-object "id" "media-9"))))))
    (ncwa:attach-media-fetchers adapter candidate)
    (is (null (gethash "fetch" (aref (nlk:json-array candidate "attachments") 0))) "media_enabled false: nothing is fetched")))

(deftest channel-whatsapp-agent-plans-are-the-api-calls ()
  (let ((send (ncwa:send-text-plan "15550001111" "hello" :reply-to "wamid.0")))
    (is (equal "POST" (nck:request-plan-method send)))
    (is (equal "messages" (nck:request-plan-path send)))
    (is (equal "15550001111" (nlk:json-value (nck:request-plan-body send) :string "to")))
    (is (equal "hello" (nlk:json-value (nck:request-plan-body send) :string "text" "body")))
    (is (equal "wamid.0" (nlk:json-value (nck:request-plan-body send) :string "context" "message_id"))))
  (let ((poll (ncwa:updates-plan :offset "c1" :timeout 3)))
    (is (equal "GET" (nck:request-plan-method poll)))
    (is (search "updates?timeout=3" (nck:request-plan-path poll)) "the long poll names its wait")
    (is (search "offset=c1" (nck:request-plan-path poll)) "and where it reads from"))
  (let ((send-media (ncwa:send-media-plan "15550001111" "media-1" "document"
                                          :filename "report.pdf")))
    (is (equal "document" (nlk:json-value (nck:request-plan-body send-media) :string "type")))
    (is (equal "report.pdf" (nlk:json-value (nck:request-plan-body send-media)
                                             :string "document" "filename"))))
  (let ((typing (ncwa:status-plan "wamid.1" :typing t)))
    (is (equal "statuses" (nck:request-plan-path typing)))
    (is (equal "text" (nlk:json-value (nck:request-plan-body typing) :string "typing_indicator" "type")))))

(defun wa-key-file ()
  "A file holding a key, the only way a secret is named: never inline."
  (let ((path (merge-pathnames "ncwa-test.key" (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string "k" out))
    (uiop:native-namestring path)))

(deftest channel-whatsapp-agent-a-missing-setting-refuses ()
  (is (signals-error nlk:config-refusal
        (ncwa:start-channel (nlk:json-object "api_key_file" (wa-key-file)))) "a section that names no user refuses before a socket opens"))

(deftest channel-whatsapp-agent-starts-and-stops ()
  (let* ((section (nlk:json-object "api_key_file" (wa-key-file) "allowed_users" (vector "15550001111")
                                    "poll_timeout_seconds" 0))
         (executor (nck:make-recording-executor
                    :responses (list (nck:make-scripted-response
                                      200 (nlk:json-object "ok" t)))))
         (stop (ncwa:start-channel section :executor executor)))
    (is (functionp stop) "a section with a key and a user starts a lane")
    (sleep 0.3)
    (funcall stop)
    (is t "and its stop thunk returns")))

(deftest channel-whatsapp-agent-an-answer-is-whatsapp-markdown ()
  (is (equal "*bold*" (ncwa:to-whatsapp "**bold**")) "bold is one star here")
  (is (equal "_it_" (ncwa:to-whatsapp "*it*")) "a starred word is italic")
  (is (equal "~gone~" (ncwa:to-whatsapp "~~gone~~")))
  (is (equal "*Heading*" (ncwa:to-whatsapp "# Heading")))
  (is (equal "the docs (https://x.test)" (ncwa:to-whatsapp "[the docs](https://x.test)")) "a link is its words and its address")
  (is (equal "5*2*3" (ncwa:to-whatsapp "5*2*3")) "times is not emphasis")
  (is (equal "__init__.py" (ncwa:to-whatsapp "__init__.py")) "a name is not emphasis")
  (is (equal "`**kept**`" (ncwa:to-whatsapp "`**kept**`")) "code is as it was written")
  (is (equal "```" (ncwa:to-whatsapp "```")) "an unclosed fence is code to the end")
  (is (equal "hi" (ncwa:to-whatsapp (format nil "~chi" #\u200b))) "zero-width characters go"))

(deftest channel-whatsapp-agent-a-long-answer-is-several-messages ()
  (let* ((sentence "The quick brown fox jumps over the lazy dog. ")
         (text (with-output-to-string (out) (dotimes (i 300) (write-string sentence out))))
         (chunks (ncwa:split-formatted text)))
    (is (> (length chunks) 2) "an answer past the ceiling is several messages")
    (is (every (lambda (chunk) (<= (ncwa:utf16-length chunk) ncwa:+text-limit+)) chunks) "no message passes the platform's ceiling")
    (is (equal (length (remove #\Space text))
               (length (remove #\Space (apply #'concatenate 'string chunks)))) "the cut loses no word"))
  (is (equal 2 (ncwa:utf16-length "😀")) "an emoji is two units, as WhatsApp counts")
  (is (= 2 (length (ncwa:split-formatted (make-string 5000 :initial-element #\a)))) "a line with no break in it is cut hard")
  (let ((plans (ncwa:send-text-plans "15550001111" "**one**" :reply-to "wamid.0")))
    (is (= 1 (length plans)))
    (is (equal "*one*" (nlk:json-value (nck:request-plan-body (first plans)) :string "text" "body")) "the answer leaves as WhatsApp shows it")))

(deftest channel-whatsapp-agent-a-caption-rides-or-leads ()
  (multiple-value-bind (on-file text-first) (ncwa:formatted-caption #p"/tmp/shot.png" "**the build**")
    (is (equal "*the build*" on-file) "a caption is converted too")
    (is (null text-first)))
  (multiple-value-bind (on-file text-first) (ncwa:formatted-caption #p"/tmp/note.ogg" "listen")
    (is (null on-file) "an audio carries no caption")
    (is (equal "listen" text-first) "so its words go first"))
  (multiple-value-bind (on-file text-first)
      (ncwa:formatted-caption #p"/tmp/shot.png" (make-string 1500 :initial-element #\x))
    (is (null on-file) "a caption past 1024 units does not ride on the file")
    (is (= 1500 (length text-first)) "the whole of it is said instead")))

(deftest channel-whatsapp-agent-answered-words-are-not-answered-twice ()
  (let ((adapter (ncwa::%make-wa-adapter :key "k")))
    (is (not (ncwa::answered-p adapter "wamid.1")))
    (ncwa::remember-answered adapter "wamid.1")
    (is (ncwa::answered-p adapter "wamid.1") "a message it answered is not answered again")
    (dotimes (i 600) (ncwa::remember-answered adapter (format nil "wamid.~d" i)))
    (is (= 512 (length (ncwa::wa-adapter-seen adapter))) "the window of remembered ids is bounded")
    (let ((lane (ncwa::%make-wa-adapter :key "k")))
      (setf (ncwa::wa-adapter-started lane) 1700000100)
      (is (ncwa::backlog-p lane 1700000000) "what the platform kept from before the lane stood is not an ask")
      (is (not (ncwa::backlog-p lane 1700000200)))))
  (with-stubbed-fdefinition (nlk:store-open-p () t)
    (with-stubbed-fdefinition
        (nlk:session-state-get (session key &key ws)
          "{\"offset\":\"c9\",\"started\":1700000000,\"seen\":[\"wamid.a\",\"wamid.b\"]}")
      (let ((adapter (ncwa::read-place (ncwa::%make-wa-adapter :key "k"))))
        (is (equal "c9" (ncwa::wa-adapter-offset adapter)) "a restart resumes at the platform's own cursor")
        (is (equal '("wamid.b" "wamid.a") (ncwa::wa-adapter-seen adapter)) "and does not answer what it answered before")))))
