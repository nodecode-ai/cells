;;;; cell-test.lisp --- the lifecycle, the memory, briefs, the quiet ones, drafts.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's wiring and its judgement: that a start
;;;; installs the six tools, /wechat and the manual topic and a stop takes every
;;;; one back; that a note written down comes back out of the file with its
;;;; words, its kind and its day; that the same person spelled differently is
;;;; one person; that a brief carries their notes, their open drafts and the one
;;;; line worth saying; that someone no note has touched for quiet_days is named
;;;; as quiet, with the plan they mentioned; that a draft is a draft until the
;;;; operator marks it, and that the cell says it sent nothing; that the report
;;;; counts the window; that keep_notes drops the oldest; and that a refusal
;;;; says why -- both a config one and a tool contract's.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "wechat-assistant"
  (:help :wechat)
  (:command "wechat")
  (:running (is (nle::find-tool "wechat_note") "the note tool is registered")
            (is (nle::find-tool "wechat_report") "and the report tool")
            (is (equal nodecode-wechat-assistant::+primer+ (nle:help :wechat)) "the manual is the cell's own")
            (is (= 21 (nodecode-wechat-assistant::setting :quiet-days)) "quiet_days defaults to three weeks")
            (is (= 5 (nodecode-wechat-assistant::setting :brief-notes)) "a brief carries five notes"))
  (:stopped (is (null (nle::find-tool "wechat_note")) "stop unregisters the tools"))
  (:refused ("quiet_days" 0) ("brief_notes" 0) ("keep_notes" 5))
  (:idle wechat:wechat-error
         (wechat:note :contact "Lin" :text "moving in March" :kind "plan")
         (wechat:contacts)))

;;; --- the memory ----------------------------------------------------------------

(deftest wechat-cell-a-note-comes-back-out-of-the-file ()
  (with-wechat ()
    (is-carrying (wechat-answer (wechat:note :contact "Lin" :text "moving to Chengdu in March"
                                    :kind "plan"))
                 ("noted #1 on Lin" "the answer names the note") ("plan" "and its kind"))
    (is-carrying (wechat-answer (wechat:contacts)) ("1 contact" "the person exists") ("Lin — 1 note" "with one note"))
    (is-carrying (wechat-answer (wechat:brief "Lin")) ("moving to Chengdu in March" "the words come back")
                 ("plan" "with the kind"))
    (is (= 1 (wechat-note-count)) "one note in the file")))

(deftest wechat-cell-the-same-person-spelled-differently-is-one-person ()
  (with-wechat ()
    (wechat:note :contact "Lin" :text "waiting on the results" :kind "said")
    (wechat:note :contact "  lin " :text "the results came back clear" :kind "fact")
    (is (search "1 contact" (wechat-answer (wechat:contacts))) "one person, not two")
    (is-carrying (wechat-answer (wechat:brief "lin")) ("2 notes" "both notes under the one person"))))

(deftest wechat-cell-a-note-without-words-refuses ()
  (with-wechat ()
    (is (signals-error wechat:wechat-error (wechat:note :contact "Lin" :text "  ")))
    (is (signals-error wechat:wechat-error (wechat:note :contact "" :text "hello")))
    (is (signals-error wechat:wechat-error (wechat:note :contact "Lin" :text "x" :kind "gift")) "a kind outside the five refuses")))

;;; --- briefs and the quiet ones ---------------------------------------------------

(deftest wechat-cell-a-brief-says-what-to-say-next ()
  (with-wechat ()
    (wechat:note :contact "Kai" :text "interview on the 14th" :kind "plan" :at "-21d")
    (let ((brief (wechat-answer (wechat:brief "Kai"))))
      (is-carrying brief ("interview on the 14th" "their plan")
                     ("gone quiet 21 day" "how long it has been")
                     ("the plan from" "and that the plan is due"))
      (is (search "Kai" brief) "under their name"))))

(deftest wechat-cell-followups-names-the-quietest-first ()
  (with-wechat ()
    (wechat-add-quiet "Ana" 3)
    (wechat-add-quiet "Bo" 40)
    (wechat-add-quiet "Cy" 60)
    (let ((answer (wechat-answer (wechat:followups))))
      (is (search "2 quiet for 21+ day" answer) "two have gone quiet")
      (is (< (search "Cy" answer) (search "Bo" answer)) "the quietest first")
      (is (null (search "Ana" answer)) "the one seen this week is not named"))
    (is (search "3 quiet for 3+" (princ-to-string
                            (nodecode-wechat-assistant::followups 5 3))) "a wider threshold names Ana too")))

(deftest wechat-cell-nobody-quiet-says-so ()
  (with-wechat ()
    (wechat:note :contact "Ana" :text "said hello")
    (is (search "nobody has gone quiet" (wechat-answer (wechat:followups))))))

;;; --- drafts ----------------------------------------------------------------------

(deftest wechat-cell-a-draft-is-never-sent ()
  (with-wechat ()
    (is-carrying (wechat-answer (wechat:draft :contact "Lin" :text "How is the move going?"))
                 ("draft #1 for Lin" "the draft exists") ("not sent" "and says so"))
    (is-carrying (wechat-answer (wechat:drafts)) ("How is the move going?" "it is listed")
                 ("the cell sends none of them" "with the boundary stated"))
    (is-carrying (wechat-answer (wechat:brief "Lin")) ("How is the move going?" "and a brief shows it"))
    (is-carrying (wechat-answer (wechat:mark 1 "sent")) ("is now sent" "the operator marks it")
                 ("the cell sent nothing" "and nothing was sent by the cell"))
    (is (search "no open draft" (wechat-answer (wechat:drafts))) "so the open list is empty")
    (is (search "sent" (wechat-answer (wechat:drafts "all"))) "though the draft is still there")))

(deftest wechat-cell-a-draft-status-outside-the-three-refuses ()
  (with-wechat ()
    (wechat:draft :contact "Lin" :text "hello there")
    (is (signals-error wechat:wechat-error (wechat:mark 1 "posted")))
    (is (signals-error wechat:wechat-error (wechat:mark 9 "sent")) "and an id with no draft")))

;;; --- the report, retention, the tool boundary ---------------------------------------

(deftest wechat-cell-the-report-counts-the-window ()
  (with-wechat ()
    (wechat:note :contact "Lin" :text "moving in March" :kind "plan")
    (wechat:note :contact "Bo" :text "old news" :kind "fact" :at "-90d")
    (let ((answer (wechat-answer (wechat:report 30))))
      (is-carrying answer ("the last 30 day" "the window") ("1 note" "only what is inside it")
                    ("plan 1" "counted by kind") ("Bo" "and Bo named as quiet")))))

(deftest wechat-cell-keep-notes-drops-the-oldest ()
  (with-wechat ("keep_notes" 10)
    (dotimes (i 14)
      (wechat:note :contact (format nil "P~d" i) :text (format nil "note ~d" i)))
    (is (= 10 (wechat-note-count)) "the cap holds")
    (is (find "note 13" (getf (wechat-memory-raw) :notes)
              :key (lambda (note) (getf note :text)) :test #'string=) "the newest stays")
    (is (null (find "note 0" (getf (wechat-memory-raw) :notes)
                    :key (lambda (note) (getf note :text)) :test #'string=)) "the oldest went first")))

(deftest wechat-cell-a-tool-call-goes-through-its-contract ()
  (with-wechat ()
    (let ((fn (third (nle::find-tool "wechat_note"))))
      (is-carrying (result (funcall fn (wechat::decode-wechat-note
                                       (nlk:make-json-object "contact" "Lin" "text" "moving in March"
                                                              "kind" "plan"))))
                   ("noted #1 on Lin" "the body answers what the verb does"))
      (is (signals-error nlk:contract-refusal
                         (wechat::decode-wechat-note (nlk:make-json-object "text" "no name"))) "a call without a contact is refused at the boundary")
      (is (signals-error nlk:contract-refusal
                         (wechat::decode-wechat-note (nlk:make-json-object "contact" "x"
                                                                        "text" "y" "kind" "gift"))) "and a kind outside the enum"))))
