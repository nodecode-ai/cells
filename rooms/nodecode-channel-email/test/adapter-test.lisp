;;;; adapter-test.lisp --- the email lane end to end, over the real gateway.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The fake inbox stands where IMAP is, the scripted executor where SMTP is;
;;;; the kit, the gateway and the lanes are real.

(in-package #:nodecode.test)

(defun email-room (sender root)
  (format nil "email-~a" (nce:thread-channel sender root)))

(defun email-line-seen-p (needle messages)
  "Whether some message the provider was handed carries NEEDLE in its content."
  (some (lambda (row)
          (let ((content (ignore-errors (nlk:json-value row :string "content"))))
            (and content (search needle content))))
        (coerce messages 'list)))

(deftest channel-email-ask-answered-as-one-threaded-reply ()
  (with-email-lane (inbox)
    (inbox-push inbox (email-raw :body (format nil "hi bot~%~%On Mon kim wrote:~%> old")))
    (is-present (answer (await (:timeout 20) (first (email-sends inbox)))) "the answer was sent"
      (is (equal "kim@example.com" (getf answer :to)))
      (is (equal "Re: Hello" (getf answer :subject)))
      (is (search "email reply" (getf answer :text)))
      (let ((sent (nce:parse-message (getf answer :data))))
        (is (equal "<m1@example.com>" (nce:header sent "in-reply-to")) "threaded under the ask")
        (is (equal "<m1@example.com>" (nce:header sent "references")))
        (is (equal "Nodecode <bot+nc@example.com>" (nce:header sent "from")))
        (is (equal "email reply" (nce:mail-text sent)) "the plain part is the Markdown as typed")
        (is (search "email reply</p>" (or (nce:mail-html sent) "")) "the HTML part draws it")))
    (let* ((room (email-room "kim@example.com" "<m1@example.com>"))
           (lane (format nil "~a-m~a" room (nce:mail-id "<m1@example.com>"))))
      (is-lane-carrying "email" room lane "email-channel" "email address" "(nce:send-mail"))
    (is (email-line-seen-p "Subject: Hello" (first messages-seen)) "the first message carries its subject")
    (is (not (email-line-seen-p "> old" (first messages-seen))) "the quoted thread is cut")
    (sleep 2)
    (is (= 1 (length (email-sends inbox))) "one turn, one email: the card and its edits send nothing")))

(deftest channel-email-reply-goes-on-in-the-same-room ()
  (with-email-lane (inbox)
    (inbox-push inbox (email-raw))
    (is-present (first-answer (await (:timeout 20) (first (email-sends inbox)))) "the first answer was sent"
      (let ((answer-id (getf first-answer :message-id)))
        (inbox-push inbox (email-raw :subject "Re: Hello" :message-id "<m2@example.com>"
                                     :in-reply-to answer-id
                                     :references (format nil "<m1@example.com> ~a" answer-id)
                                     :body "and the second question"))
        (is-present (second-answer (await (:timeout 20) (second (email-sends inbox)))) "the reply was answered"
          (let ((sent (nce:parse-message (getf second-answer :data))))
            (is (equal "<m2@example.com>" (nce:header sent "in-reply-to")))
            (is (equal (format nil "<m1@example.com> ~a <m2@example.com>" answer-id)
                       (nce:header sent "references")))
            (is (equal "Re: Hello" (getf second-answer :subject)) "one Re:, never two")))
        (is (email-line-seen-p "hi bot" (first messages-seen)) "the room remembers the first exchange")
        (is (not (email-line-seen-p "Subject: Re: Hello" (first messages-seen))) "a reply carries no subject line")
        (is (nlk:session-exists-p (format nil "~a-m~a" (email-room "kim@example.com" "<m1@example.com>")
                                          (nce:mail-id "<m1@example.com>"))))))))

(deftest channel-email-strangers-machines-and-forgeries-get-nothing ()
  (with-email-lane (inbox)
    (inbox-push inbox (email-raw :from "eve@example.com" :message-id "<e1@x>"))
    (inbox-push inbox (email-raw :message-id "<a1@x>" :extra '("Auto-Submitted: auto-replied")))
    (inbox-push inbox (email-raw :message-id "<f1@x>" :auth "mx.example.com; dmarc=fail header.from=example.com"))
    (inbox-push inbox (email-raw :message-id "<f2@x>" :auth nil))
    (inbox-push inbox (email-raw :message-id "<o1@x>" :to "someone-else@example.com"))
    (inbox-push inbox (email-raw :message-id "<ok@x>" :body "the real one"))
    (inbox-push inbox (email-raw :message-id "<ok@x>" :body "the real one"))
    (is-present (answer (await (:timeout 20) (first (email-sends inbox)))) "the real one was answered"
      (is (equal "<ok@x>" (nce:header (nce:parse-message (getf answer :data)) "in-reply-to"))))
    (sleep 3)
    (is (= 1 (length (email-sends inbox))) "nothing else was answered, and a message read twice once")))

(deftest channel-email-mail-that-comes-while-running-is-read-whatever-the-catch-up ()
  ;; catch_up_minutes 0 once dropped every live message: the window was
  ;; measured back from each read, so mail IDLE had just announced was
  ;; already "too old".
  (let* ((started 4000000000)
         (lane (nce::%make-email-adapter :started started :catch-up-minutes 0 :max-bytes 1000)))
    ;; arrived after the start, read minutes later
    (is (eq :read (nce::mail-verdict lane 500 (+ started 1))))
    (is (eq :read (nce::mail-verdict lane 500 (+ started 3600))))
    ;; came while the lane was down, outside a window of 0
    (is (eq :too-old (nce::mail-verdict lane 500 (- started 1))))
    ;; over max_megabytes
    (is (eq :too-big (nce::mail-verdict lane 1001 (+ started 1))))
    ;; a window reaches back from the start, not from now
    (setf (nce::email-adapter-catch-up-minutes lane) 60)
    (is (eq :read (nce::mail-verdict lane 500 (- started 3599))))
    (is (eq :too-old (nce::mail-verdict lane 500 (- started 3601))))
    ;; a server that gives no date or size is read
    (is (eq :read (nce::mail-verdict lane nil nil)))))
