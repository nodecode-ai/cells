;;;; adapter-test.lisp --- the WhatsApp lane end to end, over a fake driver.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane runs as it runs for an operator: its lap, its executor and the
;;;; wire against fake-driver.lisp, the real nodecode gateway behind it
;;;; (WITH-ADAPTER-LANE, the provider a stub). Covers: a direct message
;;;; answered quoting its ask, typed at and marked read; the account's own
;;;; messages and a stranger's never answered; group chatter observed and a
;;;; mention or a quote of the bot answered; a reaction on the bot's answer
;;;; read as context; a poll's vote pressing its choice; a call declined; a
;;;; backlog past catch_up_minutes skipped; an unlinked device saying how to
;;;; link; linking by code; and the probe. The digest flow itself is the
;;;; kit's (host-test.lisp).

(in-package #:nodecode.test)

(deftest channel-whatsapp-dm-answered-quoting-the-ask ()
  (let ((fake (make-fake-wa)))
    (with-whatsapp-lane (fake "allowed_users" (vector "+1 555 111 0000") "owner" (vector +wa-kim+))
      (fake-wa-push fake (wa-message +wa-kim+ "hi bot" :id "ASK1"))
      (is-present (answer (await (:timeout 15) (first (wa-answers fake "whatsapp reply"))))
        "the answer was sent"
        (is (equal (wa-jid +wa-kim+) (gethash "chat" answer)) "in the chat it was asked in")
        (is (equal "ASK1" (gethash "quote" answer)) "quoting the ask"))
      (is (fake-wa-calls fake "typing") "the bot typed while it worked")
      (is (some (lambda (args) (equalp #("ASK1") (gethash "ids" args))) (fake-wa-calls fake "read")))
      (let ((room (format nil "whatsapp-~a" (wa-jid +wa-kim+))))
        (is-lane-carrying "whatsapp" room (format nil "~a-mASK1" room) "whatsapp-chat"
                          "(WhatsApp number 15551110000)" "(ncw:request"))
      (is (wire-row "kim (operator) [mASK1 u15551110000]: hi bot" (first messages-seen) "content"))
      ;; The account's own words from its phone, and a stranger's, ask nothing.
      (fake-wa-push fake (wa-message +wa-kim+ "note to self" :id "MINE" :from-me t))
      (fake-wa-push fake (wa-message +wa-lee+ "let me in" :id "STRANGER" :push-name "lee"))
      (sleep 1.5)
      (is (= 1 (length (wa-answers fake "whatsapp reply"))) "only the allowed person's ask was answered")
      (is (not (nlk:session-exists-p (format nil "whatsapp-~a-mMINE" (wa-jid +wa-kim+))))))))

(deftest channel-whatsapp-group-chatter-mentions-and-quotes ()
  (let ((fake (make-fake-wa)))
    (fake-wa-group fake +wa-group+ "Team" (list +wa-kim+ :admin t) (list +wa-lee+))
    (with-whatsapp-lane (fake "allowed_users" (vector +wa-kim+) "allowed_chats" (vector +wa-group+))
      (fake-wa-push fake (wa-message +wa-kim+ "just chatting" :id "CHAT" :group +wa-group+))
      (fake-wa-push fake (wa-message +wa-kim+ "@15550000000 what is up" :id "M1" :group +wa-group+
                                                                          :mentions-me t))
      (is-present (answer (await (:timeout 15) (first (wa-answers fake "whatsapp reply"))))
        "the mention was answered"
        (is (equal +wa-group+ (gethash "chat" answer)))
        (is (equal "M1" (gethash "quote" answer))))
      (is (some (lambda (row) (search "[mM1 u15551110000]: what is up"
                                      (or (ignore-errors (nlk:json-value row :string "content")) "")))
                (coerce (first messages-seen) 'list)) "the model reads the ask, not the mention that routed it")
      (let ((candidate (ncw:whatsapp-message-candidate (wa-message +wa-kim+ "x" :group +wa-group+))))
        (ncw::note-chat ncw::*whatsapp-adapter* candidate)
        (is (search "WhatsApp group 120363000000000001@g.us (Team)"
                    (ncw:whatsapp-where-text candidate ncw::*whatsapp-adapter*)) "the lane knows the group by name"))
      (is (not (nlk:session-exists-p (format nil "whatsapp-~a-mCHAT" +wa-group+))) "chatter opened nothing")
      ;; Quoting the bot's answer goes on with that lane, no mention needed.
      (is (await (:timeout 15) (plusp (count-session-events (format nil "whatsapp-~a-mM1" +wa-group+)
                                                            "turn.completed"))))
      (let ((answer-id (gethash "__id" (first (wa-answers fake "whatsapp reply")))))
        (is (stringp answer-id) "the answer's id is known")
        (fake-wa-push fake (wa-message +wa-kim+ "and then?" :id "M2" :group +wa-group+
                                                            :quote (list answer-id t "whatsapp reply")))
        (is (await (:timeout 15) (= 2 (length (wa-answers fake "whatsapp reply")))) "the follow-up was answered")
        (is (await (:timeout 15) (= 2 (count-session-events (format nil "whatsapp-~a-mM1" +wa-group+)
                                                            "turn.completed"))) "in the lane it continues")))))

(deftest channel-whatsapp-reactions-votes-and-calls ()
  (let ((fake (make-fake-wa)))
    (with-whatsapp-lane (fake "allowed_users" (vector +wa-kim+))
      (let* ((adapter ncw::*whatsapp-adapter*)
             (host (ncw::whatsapp-adapter-host adapter))
             (chat (wa-jid +wa-kim+)))
        (fake-wa-push fake (wa-message +wa-kim+ "hello" :id "ASK1"))
        (is (await (:timeout 15) (wa-answers fake "whatsapp reply")))
        (is (await (:timeout 15) (plusp (count-session-events (format nil "whatsapp-~a-mASK1" chat) "turn.completed"))))
        (let ((answer-id (nlk:json-value (first (last (fake-wa-calls fake "send"))) :string "quote")))
          (declare (ignore answer-id)))
        ;; A reaction on the bot's answer is context, never an ask.
        (let* ((sent (loop for key being the hash-keys of (ncw::whatsapp-book-sent (ncw::whatsapp-adapter-book adapter))
                           collect (subseq key (1+ (position #\| key)))))
               (reaction (nlk:json-object "event" "reaction" "chat" chat "group" nil "from" (wa-person +wa-kim+)
                                          "push_name" "kim" "target_id" (first sent) "target_from_me" t
                                          "emoji" "🔥")))
          (is (eq :observe (ncw:whatsapp-handle-event adapter reaction)) "a reaction on the bot's message is read"))
        ;; A poll the bot posted: a vote says its choice's line.
        (let ((posted (nck:execute-plan (nck:host-executor host)
                                        (ncw:send-message-plan (ncw::whatsapp-adapter-book adapter) (list :channel-id chat)
                                                               (wa-chunk "Ship it?")
                                                               :controls (list (list (nck:choice "Ship" "ship it now")
                                                                                     (nck:choice "Wait" "wait")))))))
          (is (nck:execution-ok-p posted))
          (let ((poll-id (await (:timeout 5)
                           (loop for (id . choices) in (alexandria:hash-table-alist
                                                        (ncw::whatsapp-book-polls (ncw::whatsapp-adapter-book adapter)))
                                 when (assoc "Ship" choices :test #'equal) return id))))
            (is (stringp poll-id) "the poll's options are kept against their presses")
            (fake-wa-push fake (nlk:json-object "event" "poll_vote" "chat" chat "group" nil "from" (wa-person +wa-kim+)
                                                "push_name" "kim" "poll_id" poll-id "options" (vector "Ship")))
            (is (await (:timeout 15) (= 2 (length (wa-answers fake "whatsapp reply")))) "the vote was answered as its line")
            (is (some (lambda (rows) (some (lambda (row) (search "ship it now" (or (ignore-errors (nlk:json-value row :string "content")) "")))
                                           (coerce rows 'list)))
                      messages-seen) "the model read the choice's line")))
        ;; A call is declined, and the caller told to send a voice message.
        (fake-wa-push fake (nlk:json-object "event" "call" "from" (wa-person +wa-kim+) "call_id" "CALL1"))
        (is (await (:timeout 10) (fake-wa-calls fake "reject_call")) "the call was declined")
        (is (await (:timeout 10) (wa-answers fake "voice message")) "and the caller told what works")))))

(deftest channel-whatsapp-catch-up-skips-what-is-too-old ()
  (let ((fake (make-fake-wa)))
    (with-whatsapp-lane (fake "allowed_users" (vector +wa-kim+) "catch_up_minutes" 30)
      (fake-wa-push fake (wa-message +wa-kim+ "from long ago" :id "OLD"
                                                               :timestamp (- (ncw::unix-ms) (* 2 60 60 1000))))
      (fake-wa-push fake (wa-message +wa-kim+ "from a while ago" :id "LATE"
                                                                  :timestamp (- (ncw::unix-ms) (* 10 60 1000))))
      (is (await (:timeout 15) (= 1 (length (wa-answers fake "whatsapp reply")))))
      (sleep 1)
      (is (= 1 (length (wa-answers fake "whatsapp reply"))) "the old one was skipped")
      (is (equal "LATE" (gethash "quote" (first (wa-answers fake "whatsapp reply")))) "the late one answered")
      (is (some (lambda (rows) (some (lambda (row) (search "while you were offline" (or (ignore-errors (nlk:json-value row :string "content")) "")))
                                     (coerce rows 'list)))
                messages-seen) "and the model read how late it was"))))

(deftest channel-whatsapp-linking-and-the-probe ()
  (let ((fake (make-fake-wa :linked nil)))
    (with-whatsapp-lane (fake "allowed_users" (vector +wa-kim+))
      (is (await (:timeout 10) (search "not linked yet" (or (getf (nck:channel-status "whatsapp") :detail) ""))))
      (is (search "ncw:link :phone" (ncw:probe-channel nil)) "and so does the probe")
      (is (equal "ABCD-EFGH" (getf (ncw:link :phone "+1 555 000 0000") :code)) "a code to type on the phone")
      (is (equal "+1 555 000 0000" (gethash "phone" (first (fake-wa-calls fake "pair_phone")))))))
  (let ((fake (make-fake-wa)))
    (fake-wa-group fake +wa-group+ "Team" (list +wa-kim+ :admin t) (list +wa-lee+))
    (with-whatsapp-lane (fake "allowed_users" (vector +wa-kim+))
      (is (await (:timeout 10) (search "linked as +15550000000" (or (getf (nck:channel-status "whatsapp") :detail) ""))))
      (let ((text (ncw:probe-channel nil)))
        (is (search "device: linked as +15550000000 (Nodecode), connected" text) text)
        (is (search (format nil "Team ~a (2)" +wa-group+) text) text)
        (is (search "person 0000 15551110000" text) text))
      (is (equal (list (cons +wa-group+ "Team")) (ncw:probe-chat-choices nil)))
      (is (getf (ncw:link) :linked) "a linked device says so rather than linking again"))))
