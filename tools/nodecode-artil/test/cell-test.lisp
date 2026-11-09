;;;; cell-test.lisp --- the tools, the command, the verbs, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed program: that
;;;; its start installs the six tools, the command and the manual and its stop
;;;; takes them back, that each tool and the command ask artil the argv the
;;;; plugin's own account takes, that a call which fails, prints nothing or runs
;;;; past its deadline says why, and that a bad id, a missing attachment, a new
;;;; email with no subject and a message with nobody to send to are each refused
;;;; before the program runs. One test runs a real /bin/sh program, for the argv
;;;; a stub cannot prove. Nothing here needs artil installed or a network.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "artil"
  (:help :artil)
  (:command "artil")
  (:running (is (= 90 (nodecode-artil::setting :timeout-seconds)) "a minute and a half by default")
            (is (plusp (length (nodecode-artil::setting :program))) "and a program is named"))
  (:refused ("program" "definitely-no-such-program-xyz") ("timeout_seconds" 0))
  (:idle artil:artil-error (artil:inbox "email")))

;;; --- the tools ---------------------------------------------------------------------

(deftest artil-cell-the-inbox-tool-asks-for-one-channel-structurally ()
  (with-cell-stop ((artil-start))
    (with-artil-stub (:stdout +listing+)
      (is-present (entry (nle::find-tool "artil_inbox"))
        "artil_inbox is registered"
        (is (hash-table-p (nlk:contract-schema (second entry))) "against a declared contract")
        (let* ((call (nlk:contract-decode (second entry) (nlk:make-json-object "channel" "email")))
               (answer (funcall (third entry) call)))
          (is (search "mail:7" answer) "the listing it printed is the answer")
          (is (equal '("email" "--json") (last-args)) "asked for email, structured, as the skill says"))))))

(deftest artil-cell-reading-a-message-passes-its-id ()
  (with-cell-stop ((artil-start))
    (with-artil-stub (:stdout +message+)
      (let* ((entry (nle::find-tool "artil_read"))
             (call (nlk:contract-decode (second entry)
                                         (nlk:make-json-object "channel" "telegram" "id" "tg:9")))
             (answer (funcall (third entry) call)))
        (is (search "The bridge closed on Friday" answer) "the message whole is the answer")
        (is (equal '("telegram" "read" "tg:9") (last-args)) "by its own id, on its own channel")))))

(deftest artil-cell-sending-builds-the-argvs-the-cli-takes ()
  (with-cell-stop ((artil-start))
    (with-artil-stub (:stdout "queued")
      (let ((schema (second (nle::find-tool "artil_send")))
            (run (third (nle::find-tool "artil_send"))))
        (flet ((send (&rest pairs)
                 (funcall run (nlk:contract-decode schema (apply #'nlk:make-json-object pairs)))))
          (is (search "queued" (send "channel" "email" "body" "Thanks"
                                      "to" (vector "alice.example") "subject" "Re: the bridge")) "the CLI's answer is the answer")
          (is (equal '("email" "send" "--to" "alice.example" "--subject" "Re: the bridge"
                       "--" "Thanks")
                     (last-args)) "a new email names who and its subject, and the body after --")
          (send "channel" "email" "body" "Noted" "reply_to" "mail:7"
                "cc" (vector "bob.example"))
          (is (equal '("email" "send" "--reply-to" "mail:7" "--cc" "bob.example" "--" "Noted")
                     (last-args)) "a reply answers the thread, and cc the others the mail named")
          (send "channel" "sms" "body" "Look at this" "forward" "sms:3"
                "to" (vector "the-desk"))
          (is (equal '("sms" "send" "--forward" "sms:3" "--to" "the-desk" "--" "Look at this")
                     (last-args)) "a forward carries the note and who it goes on to")
          (send "channel" "sms" "body" "On my way" "to" (vector "the-desk"))
          (is (equal '("sms" "send" "--to" "the-desk" "--" "On my way") (last-args)) "a new text needs no subject"))))))

(deftest artil-cell-an-agent-is-written-to-at-its-message-url ()
  (with-cell-stop ((artil-start))
    (with-artil-stub (:stdout "queued")
      (let* ((entry (nle::find-tool "artil_message_agent"))
             (call (nlk:contract-decode (second entry)
                                         (nlk:make-json-object "agent" "bookkeeper"
                                                                "body" "Send the ledger")))
             (answer (funcall (third entry) call)))
        (is (search "queued" answer) "the CLI's answer is the answer")
        (is (equal '("url" "send" "bookkeeper" "--" "Send the ledger") (last-args)) "another agent is written to at its Message URL")))
    (with-artil-stub (:stdout +team+)
      (is (search "bookkeeper" (funcall (third (nle::find-tool "artil_team")) (nlk:make-json-object))) "artil_team lists the workspace")
      (is (equal '("team") (last-args)) "as artil team does"))
    (with-artil-stub (:stdout "signed in as the books account")
      (is (search "the books account"
                  (funcall (third (nle::find-tool "artil_status")) (nlk:make-json-object))) "artil_status says who the account is")
      (is (equal '("auth" "status") (last-args)) "as artil auth status does"))))

;;; --- every refusal -----------------------------------------------------------------

(deftest artil-cell-every-way-a-call-fails-says-why ()
  (with-cell-stop ((artil-start))
    (with-artil-stub (:exit 3 :stderr "nobody is signed in: run artil auth login")
      (is (search "nobody is signed in" (artil-refusal #'artil:inbox "email")) "the program's own words ride the refusal"))
    (with-artil-stub (:stdout "")
      (is (search "printed nothing" (artil-refusal #'artil:inbox "email")) "silence is named"))
    (with-artil-stub (:timed-out t)
      (is (search "did not finish" (artil-refusal #'artil:team)) "a call past its deadline says so"))
    (with-artil-stub ()
      (is (search "no such file"
                  (artil-refusal #'artil:send-message "email" "hi" :to '("alice.example")
                                  :subject "the bridge" :attach '("no-such-file.pdf"))) "an attachment that is not there is named")
      (is (search "subject" (artil-refusal #'artil:send-message "email" "hi"
                                             :to '("alice.example"))) "a new email without a subject says so")
      (is (search "needs to" (artil-refusal #'artil:send-message "sms" "hi")) "a message with nobody to send to says so")
      (is (search "dash" (artil-refusal #'artil:read-message "email" "--json")) "an id that could be read as a flag is refused")
      (is (search "none of" (artil-refusal #'artil:inbox "fax")) "and so is a channel it has not")
      (is (null *runs*) "none of those reached the program"))))

;;; --- the command -------------------------------------------------------------------

(deftest artil-cell-the-command-runs-the-cli-with-the-words-typed ()
  (with-cell-stop ((artil-start))
    (with-artil-stub (:stdout +listing+)
      (is (search "mail:7" (cell-entry "nodecode-artil" "artil" "email")) "the operator reads the listing")
      (is (equal '("email") (last-args)) "the words typed are the argv")
      (let ((before *runs*))
        (is (search "/artil team" (cell-entry "nodecode-artil" "artil" "")) "no argument says what the tools are for")
        (is (equal before *runs*) "and runs nothing")))))

;;; --- the real program --------------------------------------------------------------

(deftest artil-cell-the-program-is-really-run-with-its-argv-and-its-client ()
  (let ((directory (merge-pathnames "artil-running/" (uiop:temporary-directory))))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree directory :validate t)))
      (with-cell-stop ((artil-start "program" (artil-recording-program directory)))
        (is (search "mail:7" (artil:inbox "email")) "the program's answer is the listing")
        (let ((argv (uiop:read-file-string (merge-pathnames "argv" directory))))
          (is (search "email" argv) "it was asked for email")
          (is (search "--json" argv) "and for the structured listing"))
        (is (string= "nodecode" (uiop:read-file-string (merge-pathnames "client" directory))) "and told which harness it runs for")))))
