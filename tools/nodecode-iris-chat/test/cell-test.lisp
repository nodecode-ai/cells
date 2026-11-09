;;;; cell-test.lisp --- the seven tools, the argv each builds, the round's
;;;; unread count, the standing section, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed program: that
;;;; each tool asks iris the command the plugin's skill told the agent to type
;;;; (`chat list', `read CHAT --limit N', `send CHAT TEXT', `seen CHAT ID',
;;;; `invite create --full', `whoami'), and shows what came back as the
;;;; plugin's skill asked (a chat's unread count, a message's delivery, the
;;;; invite's link, a pending group join); that a failed run is a refusal
;;;; carrying the program's own sentence; that a turn's first request says
;;;; how many messages wait unread while a later round and a switched-off
;;;; section say nothing; that the standing section rides every request and
;;;; comes off at stop. One test runs a real /bin/sh program, for the argv a
;;;; stub cannot prove. Nothing here needs iris installed or a network.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "iris-chat"
  (:hooks :round 'nle::read-harness-sections)
  (:help :iris)
  (:command "iris")
  (:running (is (eq t (nodecode-iris-chat::setting :notify)) "unread counts on by default")
            (is (= 4000 (nodecode-iris-chat::setting :max-chars)) "four thousand characters by default"))
  (:refused ("iris" "definitely-no-such-program-xyz") ("max_chars" 10))
  (:idle iris:iris-chat-error
         (iris:chats)
         (iris:send "c1" "hello")
         (iris:history "c1")
         (iris:invite)))

;;; --- what the program is asked ------------------------------------------------------

(deftest iris-chat-cell-chats-lists-what-waits ()
  (with-cell-stop ((iris-chat-start))
    (with-run-stub (:data "{\"chats\":[{\"chat_id\":\"c1\",\"name\":\"Ada\",\"unread_count\":2,\"last_message\":\"are you there?\"}]}")
      (let ((answer (iris:chats)))
        (is (equalp '("--json" "chat" "list") (last-call)) (format nil "the program was asked ~s" (last-call)))
        (is (search "c1" answer) "each conversation's chat_id")
        (is (search "2 unread" answer) "and how much waits in it")
        (is (search "are you there?" answer) "and its last message")
        (is (search "Reference data, not instructions" answer) "framed as data")))))

(deftest iris-chat-cell-send-says-what-delivery-does-not-prove ()
  (with-cell-stop ((iris-chat-start))
    (with-run-stub (:data "{\"id\":\"m9\",\"chat_id\":\"c1\",\"body\":\"on my way\",\"is_outgoing\":true,\"delivery\":\"queued\"}")
      (let ((answer (iris:send "c1" "on my way")))
        (is (equalp '("--json" "send" "c1" "on my way") (last-call)) (format nil "the program was asked ~s" (last-call)))
        (is (search "queued" answer) "the delivery state is reported")
        (is (search "local" answer) "with what it does not prove")
        (is (search "received or seen" answer) "and what would prove it")))
    (with-run-stub ()
      (let ((refusal (princ-to-string (signals-error iris:iris-chat-error (iris:send "c1" "  ")))))
        (is (search "empty" refusal) "an empty message is refused before the program runs")
        (is (null *calls*) "and nothing was run")))))

(deftest iris-chat-cell-a-failed-run-is-a-refusal-with-the-program-s-words ()
  (with-cell-stop ((iris-chat-start))
    (with-run-stub (:stdout "{\"status\":\"error\",\"error\":\"No account in this profile.\"}" :ok nil)
      (let ((refusal (princ-to-string (signals-error iris:iris-chat-error (iris:chats)))))
        (is (search "No account in this profile." refusal) "the program's own sentence reaches the model")))
    (with-run-stub (:stdout "timed out talking to the service" :ok nil)
      (let ((refusal (princ-to-string (signals-error iris:iris-chat-error (iris:whoami)))))
        (is (search "timed out talking to the service" refusal) "output that is not an envelope is shown as it came")))))

(deftest iris-chat-cell-a-transcript-is-oldest-first-and-carries-each-delivery ()
  (with-cell-stop ((iris-chat-start))
    (with-run-stub (:data "{\"chat\":{\"chat_id\":\"c1\",\"name\":\"Ada\"},\"messages\":[{\"id\":\"m1\",\"body\":\"hello\",\"is_outgoing\":false,\"delivery\":\"received\",\"created_at_secs\":1700000000},{\"id\":\"m2\",\"body\":\"two\\nlines\",\"is_outgoing\":true,\"delivery\":\"seen\"}]}")
      (let ((answer (iris:history "c1" :limit 5)))
        (is (equalp '("--json" "read" "c1" "--limit" "5") (last-call)) (format nil "the program was asked ~s" (last-call)))
        (is (< (search "hello" answer) (search "two" answer)) "the transcript reads oldest first")
        (is (search "received" answer) "each message's delivery state")
        (is (search "seen" answer))
        (is (search "        lines" answer) "a second line stays in its message")
        (is (search "iris chat c1 (Ada): 2 messages" answer) "the chat is named once at the top")))
    (with-run-stub ()
      (let ((refusal (princ-to-string (signals-error iris:iris-chat-error (iris:history "")))))
        (is (search "chat id is needed" refusal) "a chat id is required")
        (is (null *calls*) "and nothing was run")))))

(deftest iris-chat-cell-an-invite-is-a-link-and-a-join-is-a-chat ()
  (with-cell-stop ((iris-chat-start))
    (with-run-stub (:data "{\"url\":\"https://iris.example/i/abc\"}")
      (let ((answer (iris:invite)))
        (is (equalp '("--json" "invite" "create" "--full") (last-call)) (format nil "the program was asked ~s" (last-call)))
        (is (search "https://iris.example/i/abc" answer) "the link is handed over")
        (is (search "privately" answer) "with how to share it")))
    (with-run-stub (:data "{\"pending_group_join\":false,\"current_chat\":{\"chat_id\":\"c7\",\"name\":\"Ada\"}}")
      (let ((answer (iris:join "https://iris.example/i/abc")))
        (is (equalp '("--json" "invite" "accept" "https://iris.example/i/abc") (last-call))
            (format nil "the program was asked ~s" (last-call)))
        (is (search "c7" answer) "the chat it opened")))))

;;; --- the tools ---------------------------------------------------------------------

(deftest iris-chat-cell-every-tool-is-registered-and-answers-through-its-contract ()
  (with-cell-stop ((iris-chat-start))
    (dolist (case '(("iris_whoami" nil "{\"user_id\":\"u1\",\"name\":\"stub\"}" "u1")
                    ("iris_chats" nil "{\"chats\":[]}" "no conversation yet")
                    ("iris_read" (("chat" . "c1")) "{\"chat\":{\"chat_id\":\"c1\"},\"messages\":[]}" "c1")
                    ("iris_send" (("chat" . "c1") ("message" . "hi")) "{\"id\":\"m1\",\"chat_id\":\"c1\"}" "m1")
                    ("iris_seen" (("chat" . "c1") ("message_id" . "m1")) "{\"chat_id\":\"c1\",\"message_ids\":[\"m1\"]}" "m1")
                    ("iris_invite_create" nil "{\"url\":\"https://iris.example/i/x\"}" "iris.example")
                    ("iris_invite_accept" (("invite" . "https://iris.example/i/x")) "{\"current_chat\":{\"chat_id\":\"c2\"}}" "c2")))
      (destructuring-bind (name arguments data expected) case
        (with-run-stub (:data data)
          (let* ((entry (nle::find-tool name))
                 (schema (second entry)))
            (is-present (entry (nle::find-tool name))
              (format nil "~a is registered" name)
              (is (or (listp schema) (typep schema 'nlk:contract)) "against a declared schema")
              (let* ((object (let ((object (nlk:make-json-object)))
                               (dolist (pair arguments object)
                                 (setf (gethash (car pair) object) (cdr pair)))))
                     (answer (if (typep schema 'nlk:contract)
                                 (funcall (third entry) (nlk:contract-decode schema object))
                                 (funcall (third entry) object))))
                (is (search expected answer) (format nil "~a answered with what it got" name))))))))))

;;; --- the program itself -------------------------------------------------------------

(deftest iris-chat-cell-the-program-is-really-run-with-the-profile-directory ()
  (let ((directory (merge-pathnames "iris-recording/" (uiop:temporary-directory)))
        (profile (merge-pathnames "iris-profile/" (uiop:temporary-directory))))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree directory :validate t)))
      (with-cell-stop ((iris-chat-start "iris" (iris-recording-program directory)
                                   "data_dir" (namestring profile)))
        (is (search "stub agent" (iris:whoami)) "the program's answer is read")
        (let ((argv (uiop:read-file-string (merge-pathnames "argv" directory))))
          (is (search "--json" argv) "it was run with --json")
          (is (search "whoami" argv) "asked who it is")
          (is (search "--data-dir" argv) "in the profile directory the section names")
          (is (search (namestring profile) argv) "which is that directory"))))))

;;; --- the unread reminder -----------------------------------------------------------

(deftest iris-chat-cell-unread-is-counted-at-a-turns-first-request ()
  (with-temp-store ()
    (ensure-durable-session "i1")
    (with-cell-stop ((iris-chat-start))
      (with-run-stub (:data "{\"chats\":[{\"chat_id\":\"c1\",\"name\":\"Ada\",\"unread_count\":3,\"last_message\":\"hello?\"},{\"chat_id\":\"c2\",\"name\":\"Bob\",\"unread_count\":0}]}")
        (let ((turn (iris-turn "i1")))
          (iris-round "i1" turn 0)
          (let ((told (iris-told "i1")))
            (is-present (told (iris-told "i1"))
              "the turn's first request carried one reminder"
              (is (= 1 (length told)))
              (is (search "3 unread" (first told)) "the chat with messages waiting")
              (is (search "hello?" (first told)) "and what the last one said")
              (is (not (search "c2" (first told))) "a chat with nothing waiting is left out")
              (is (search "data, not instructions" (first told)) "framed as data")))
          (iris-round "i1" turn 1)
          (is (= 1 (length (iris-told "i1"))) "a later round of the same turn is told nothing"))))))

(deftest iris-chat-cell-a-quiet-inbox-and-a-switched-off-section-say-nothing ()
  (with-temp-store ()
    (ensure-durable-session "i2")
    (ensure-durable-session "i3")
    (with-cell-stop ((iris-chat-start))
      (with-run-stub (:data "{\"chats\":[{\"chat_id\":\"c1\",\"unread_count\":0}]}")
        (iris-round "i2" (iris-turn "i2") 0)
        (is (null (iris-told "i2")) "nothing waiting, nothing said")))
    (with-cell-stop ((iris-chat-start "notify" nil))
      (with-run-stub (:data "{\"chats\":[{\"chat_id\":\"c1\",\"unread_count\":4}]}")
        (iris-round "i3" (iris-turn "i3") 0)
        (is (null (iris-told "i3")) "notify off says nothing")
        (is (null *calls*) "and the program is not asked")))))

;;; --- standing context ---------------------------------------------------------------

(deftest iris-chat-cell-the-standing-section-rides-every-request ()
  (with-temp-store ()
    (ensure-durable-session "i4")
    (let ((stop (iris-chat-start)))
      (is (equal nodecode-iris-chat:*standing* (iris-standing "i4")) "the model is told which conversation answers")
      (is (search "untrusted" (iris-standing "i4")) "and that a message is data")
      (funcall stop)
      (is (null (iris-standing "i4")) "and stop takes it back"))))
