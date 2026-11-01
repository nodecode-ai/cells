;;;; adapter-test.lisp --- the iMessage lane end to end, over a fake BlueBubbles server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane runs as it runs for an operator: its live executor and its poll
;;;; lap against fake-bluebubbles.lisp on 127.0.0.1, the real nodecode gateway
;;;; behind it (WITH-ADAPTER-LANE, the provider a stub). Covers: the first read
;;;; answering nothing said before it, a direct message answered plainly with
;;;; the speaker's contact name, a group ask answered inline under it and
;;;; continued in its thread unnamed, the kit's marks as tapbacks, a server
;;;; without the Private API served plainly, a numbered reply pressing a
;;;; choice, files both ways, a refused password, and the probe. The digest
;;;; flow itself is the kit's (host-test.lisp).

(in-package #:nodecode.test)

(defparameter +dm+ "iMessage;-;+15550001")
(defparameter +group+ "iMessage;+;chat42")

(defun imessage-section (fake password-path soul-path &rest members)
  "channels.imessage against FAKE, the password in PASSWORD-PATH, MEMBERS over it."
  (let ((section (nlk:json-object "url" (fake-bb-url fake) "password_file" password-path
                                  "request_timeout_seconds" 5 "poll_ms" 100
                                  :opt "soul_file" soul-path)))
    (loop for (key value) on members by #'cddr
          do (setf (gethash key section) value))
    section))

(defmacro with-imessage-lane ((fake &rest members) &body body)
  "BODY against a started iMessage lane over FAKE -- MEMBERS the section's own
-- once it reads past where the database stood; the provider answers
`imessage reply'. The message row is kept in a scratch folder."
  `(with-imessage-state-dir (state-dir)
     (let ((nci::*cursor-state-directory* state-dir))
       (with-adapter-lane (:reply "imessage reply" :token +fake-bb-password+ :start nci:start-channel
                           :bindings ((executor nil))
                           :section (imessage-section ,fake token-path soul-path ,@members)
                           :plans nil
                           :after ((is (adapter-threads-stopped-p "channel-imessage"))))
         (is (await (:timeout 10) (plusp (fake-bb-polls ,fake))) "the lane reads on past its first look")
         ,@body))))

(deftest channel-imessage-dm-answered-backlog-not ()
  (with-fake-bb (fake)
    (fake-bb-chat fake +dm+ :people '("+15550001"))
    (setf (fake-bb-contacts fake) '(("+15550001" . "Kim")))
    (fake-bb-say fake +dm+ "+15550001" "said before" :guid "OLD")
    (with-imessage-lane (fake "allowed_users" (vector "+1 (555) 000-1") "owner" (vector "+15550001"))
      (fake-bb-say fake +dm+ "+15550001" "hi bot" :guid "ASK1")
      (is-present (answer (await (:timeout 15) (first (bb-sends fake "imessage reply"))))
        "the answer was sent"
        (is (equal +dm+ (gethash "chatGuid" answer)))
        (is (null (gethash "selectedMessageGuid" answer)) "a direct message is answered plainly")
        (is (equal "private-api" (gethash "method" answer))))
      (is-lane-carrying "imessage" "imessage-iMessage;-;+15550001" "imessage-iMessage;-;+15550001-mASK1"
                        "imessage-channel" "(iMessage handle +15550001)" "(nci:request")
      (is (wire-row "Kim (operator) [mASK1 u+15550001]: hi bot" (first messages-seen) "content"))
      (is (await (:timeout 15) (plusp (count-session-events "imessage-iMessage;-;+15550001-mASK1" "turn.completed"))))
      (is (= 1 (length (bb-sends fake "imessage reply"))) "what was said before the first read opened nothing")
      (is (not (nlk:session-exists-p "imessage-iMessage;-;+15550001-mOLD")))
      (is (bb-calls fake "chat" +dm+ "typing") "the bot typed while it worked")
      (is (await (:timeout 5) (member "DELETE" (bb-call-methods fake "chat" +dm+ "typing") :test #'equal)))
      (is (await (:timeout 5) (let ((file (first (directory (merge-pathnames "*.cursor" nci::*cursor-state-directory*)))))
                                (and file (equal (princ-to-string (fake-bb-row fake))
                                                 (nlk:trimmed (uiop:read-file-string file))))))))))

(deftest channel-imessage-group-ask-threads-and-goes-on ()
  (with-fake-bb (fake)
    (fake-bb-chat fake +group+ :people '("+15550001" "+15550002") :name "Crew")
    (with-imessage-lane (fake "allowed_chats" (vector +group+) "allowed_users" (vector "+15550001")
                              "mention_names" (vector "@nodecode") "reactions" t)
      (let ((lane "imessage-iMessage;+;chat42-mROOT"))
        (fake-bb-say fake +group+ "+15550001" "nodecode: what is up" :guid "ROOT")
        (is-present (answer (await (:timeout 15) (first (bb-sends fake "imessage reply"))))
          "the answer was sent"
          (is (equal "ROOT" (gethash "selectedMessageGuid" answer)) "inline, under the ask"))
        (is (await (:timeout 15) (plusp (count-session-events lane "turn.completed"))))
        (is (some (lambda (row) (search "[mROOT u+15550001]: what is up"
                                        (or (ignore-errors (nlk:json-value row :string "content")) "")))
                  (coerce (first messages-seen) 'list)) "the model reads the ask, not the name that called the bot")
        (is (await (:timeout 10) (member "like" (mapcar (lambda (body) (gethash "reaction" body))
                                                        (bb-calls fake "message" "react"))
                                         :test #'equal)) "the ask is tapped back done")
        (is (equal '("emphasize" "ROOT") (let ((first (first (bb-calls fake "message" "react"))))
                                           (list (gethash "reaction" first) (gethash "selectedMessageGuid" first)))))
        ;; A group line that names nobody is context, not an ask.
        (fake-bb-say fake +group+ "+15550001" "just chatting" :guid "CHAT")
        ;; In the thread the bot answers in, no name is needed, and the lane
        ;; the ask opened takes the next turn.
        (fake-bb-say fake +group+ "+15550001" "and more" :guid "MORE" :root "ROOT")
        (is (await (:timeout 15) (= 2 (length (bb-sends fake "imessage reply")))) "the follow-up was answered")
        (is (equal "MORE" (gethash "selectedMessageGuid" (second (bb-sends fake "imessage reply")))))
        (is (await (:timeout 15) (= 2 (count-session-events lane "turn.completed"))))
        (is (= 2 (count-session-events lane "turn.started")) "one lane, two turns")
        (is (not (nlk:session-exists-p "imessage-iMessage;+;chat42-mMORE")) "no second lane")
        (is (not (nlk:session-exists-p "imessage-iMessage;+;chat42-mCHAT")) "chatter opened nothing")))))

(deftest channel-imessage-without-the-private-api-sends-plainly ()
  (with-fake-bb (fake :private-api nil)
    (fake-bb-chat fake +group+ :people '("+15550001" "+15550002"))
    (with-imessage-lane (fake "allowed_users" (vector "+15550001") "mention_names" (vector "nodecode")
                              "reactions" t)
      (fake-bb-say fake +group+ "+15550001" "@nodecode hello" :guid "ASK")
      (is-present (answer (await (:timeout 15) (first (bb-sends fake "imessage reply"))))
        "the answer was sent"
        (is (equal "apple-script" (gethash "method" answer)))
        (is (null (gethash "selectedMessageGuid" answer)) "into the chat, not inline"))
      (is (await (:timeout 15) (plusp (count-session-events "imessage-iMessage;+;chat42-mASK" "turn.completed"))))
      (sleep 0.5)
      (is (null (bb-calls fake "message" "react")) "no tapback is asked for")
      (is (null (bb-calls fake "chat")) "nor the typing bubble"))))

(deftest channel-imessage-a-numbered-reply-presses-a-choice ()
  (with-fake-bb (fake)
    (fake-bb-chat fake +dm+ :people '("+15550001"))
    (with-imessage-lane (fake "allowed_users" (vector "+15550001"))
      (let* ((adapter nci::*imessage-adapter*)
             (host (nci::imessage-adapter-host adapter))
             (posted (nck:execute-plan (nck:host-executor host)
                                       (nci:message-plan (nci::imessage-adapter-book adapter) (list :channel-id +dm+)
                                                         (imessage-chunk "Pick one")
                                                         :controls (list (list (nck:choice "Help" "/help")
                                                                               (nck:choice "Status" "/status")))))))
        (is (nck:execution-ok-p posted))
        (is (equal (format nil "Pick one~%~%1. Help~%2. Status") (gethash "message" (first (bb-sends fake "Pick one")))))
        (is (null (gethash "__choices" (first (bb-sends fake "Pick one")))) "the choices never reach the server")
        (fake-bb-say fake +dm+ "+15550001" "1" :guid "PICK")
        (is (await (:timeout 15) (some (lambda (body) (not (search "Pick one" (or (gethash "message" body) ""))))
                                       (bb-calls fake "message" "text"))) "the press was answered")
        (is (not (nlk:session-exists-p "imessage-iMessage;-;+15550001-mPICK")) "a press opens no lane of its own")))))

(deftest channel-imessage-files-both-ways ()
  (with-fake-bb (fake)
    (fake-bb-chat fake +dm+ :people '("+15550001"))
    (with-imessage-lane (fake "allowed_users" (vector "+15550001"))
      (let* ((adapter nci::*imessage-adapter*)
             (host (nci::imessage-adapter-host adapter))
             (candidate (nci::attach-fetchers
                         adapter
                         (nci:imessage-message-candidate
                          (nlk:json-object "guid" "F1" "text" "" "handle" (nlk:json-object "address" "+15550001")
                                           "attachments" (vector (nlk:json-object "guid" "A1" "mimeType" "image/heic"
                                                                                  "transferName" "IMG.HEIC"))
                                           "chats" (vector (nlk:json-object "guid" +dm+)))))))
        (is (equal "bytes of A1" (sb-ext:octets-to-string
                                  (funcall (gethash "fetch" (aref (nlk:json-array candidate "attachments") 0)))
                                  :external-format :utf-8)) "a file comes down from the server")
        (with-temp-file (picture :contents "PNG" :type "png")
          (let ((sent (nck:execute-plan (nck:host-executor host)
                                        (nci:file-plan (nci::imessage-adapter-book adapter) (list :channel-id +dm+)
                                                       picture :content "the *chart*"))))
            (is (nck:execution-ok-p sent) (nck:execution-error sent))
            (is-present (upload (first (bb-calls fake "message" "attachment"))) "the file was uploaded"
              (is (equal +dm+ (form-field upload "chatGuid")))
              (is (equal (file-namestring picture) (form-field upload "name")))
              (is (equal "private-api" (form-field upload "method"))))
            (is (equal "the chart" (gethash "message" (first (bb-sends fake "chart")))) "its words after it")))))))

(deftest channel-imessage-a-refused-password-is-loud ()
  (with-fake-bb (fake)
    (with-temp-file (password-path :contents "wrong" :type "txt")
      (let ((refusal (signals-error nlk:config-refusal
                       (nci:start-channel (imessage-section fake password-path nil "allowed_users" (vector "+15550001"))))))
        (is (search "would not answer" (princ-to-string refusal)))
        (is (search "not authorized" (princ-to-string refusal)) "the server's reason is said")
        (is (not (search "wrong" (princ-to-string refusal))) "the password never is")))))

(deftest channel-imessage-probe-names-what-the-server-sees ()
  (with-fake-bb (fake)
    (fake-bb-chat fake +dm+ :people '("+15550001"))
    (fake-bb-chat fake +group+ :people '("+15550001" "+15550002") :name "Crew")
    (setf (fake-bb-contacts fake) '(("+15550001" . "Kim")))
    (with-temp-file (password-path :contents +fake-bb-password+ :type "txt")
      (let* ((section (imessage-section fake password-path nil "allowed_users" (vector "+15550001")))
             (text (nci:probe-channel section)))
        (is (search "server: ok -- BlueBubbles 1.9.9 on macOS 14.5, signed in as bot@icloud.com" text) text)
        (is (search "private api: on" text) text)
        (is (search "Crew iMessage;+;chat42, DM with Kim iMessage;-;+15550001" text) text)
        (is (search "people: Kim +15550001, +15550002" text) text)
        (is (not (search +fake-bb-password+ text)) "the password is never said")
        (is (equal `((,+group+ . "Crew") (,+dm+ . "DM with Kim")) (nci:probe-chat-choices section)))
        (is (equal '(("+15550001" . "Kim") ("+15550002" . "+15550002")) (nci:probe-user-choices section)))))))
