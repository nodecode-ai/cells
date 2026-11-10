;;;; cell-test.lisp --- the lifecycle, the detector, the scrub, the store, /ichor, the Hub.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring over a real store and real
;;;; session facts: that the start installs the observer, the command and the
;;;; manual and the stop takes them back; that a turn which rejects the one
;;;; before it becomes a pending pair, and a re-sent message a retry pair;
;;;; that secrets, emails and home paths never reach the file; that
;;;; review/keep/discard/export answer and write the TRL file; that
;;;; /ichor forget-all deletes; and that /ichor share reaches nothing but the
;;;; Hugging Face Hub it is told to -- the one network call, stubbed here.

(in-package #:nodecode.test)

(define-test-slice "ichor" "ICHOR-CELL-" :start nodecode-ichor:start-cell)

(define-cell-lifecycle-tests "ichor"
  (:hooks :frame)
  (:help :ichor)
  (:command "ichor")
  (:running (is (equal nodecode-ichor::+primer+ (nle:help :ichor)) "the manual is the primer")
            (is (eq t (nodecode-ichor::setting :capture)) "capture is on by default")
            (is (eq t (nodecode-ichor::setting :hf-private)) "sharing is private by default")
            (is (equal "ichor-pairs" (nodecode-ichor::setting :hf-repo))))
  (:refused ("dir" 42) ("hf_repo" 42))
  (:idle ichor:ichor-error (ichor:status)))

;;; --- fixtures ---------------------------------------------------------------------

(defmacro with-ichor ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, its own folder when a form names one, and stopped on unwind."
  `(with-cell-stop ((ichor-start ,@pairs)) ,@body))

(defun ichor-last-turn (session)
  "The id of SESSION's newest admitted turn."
  (nlk:events :session-id session :kind nlk::+kind-turn-started+ :order :newest :limit 1
              :as :value :columns '("turn_id")))

(defun ichor-complete (session input answer)
  "Seed SESSION with a completed turn of INPUT and ANSWER, and answer its turn id."
  (seed-completed-turn session input answer)
  (ichor-last-turn session))

(defun ichor-observe (session turn)
  "Fire the cell's :FRAME observer with TURN's completion, as a shell's frame does."
  (cell-publish "nodecode-ichor" (cell-frame-op "turn.completed" :session session :turn turn)))

;;; --- the detector -----------------------------------------------------------------

(deftest ichor-cell-a-rejection-opener-is-a-correction ()
  (multiple-value-bind (kind confidence) (nodecode-ichor::correction "show disk usage" "du -sh ."
                                                                      "no, make it human readable")
    (is (equal "correction" kind) "an opener rejects the answer")
    (is (equal "medium" confidence) "with medium confidence"))
  (is (null (nodecode-ichor::correction "show disk usage" "du -sh ." "thanks, perfect")) "praise is not a correction")
  (is (null (nodecode-ichor::correction "want me to deploy?" "Done. Want me to deploy it?" "no")) "a bare no to a yes/no question is an answer")
  (is (null (nodecode-ichor::correction "show disk usage" "du -sh ."
                                        "how do I sort that by size instead of this")) "a question asks for help, it does not reject"))

(deftest ichor-cell-a-resend-is-a-retry ()
  (multiple-value-bind (kind) (nodecode-ichor::detect "run the tests" "3 passing"
                                                       "run the tests" "1 failing")
    (is (equal "retry" kind) "the same message twice is a retry"))
  (multiple-value-bind (kind) (nodecode-ichor::detect "run the tests" "3 passing"
                                                       "run the tests" "3 passing")
    (is (null kind) "the same answer twice proves nothing was rejected")))

;;; --- the scrub --------------------------------------------------------------------

(deftest ichor-cell-secrets-never-reach-the-file ()
  (with-temp-store ()
    (with-temp-directory (root "ichor")
      (with-ichor ("dir" (namestring root))
        (ichor-complete "s-scrub" "mail me the plan" "Sent the plan to bob@example.com from /home/alice/plan.md")
        (let ((turn (ichor-complete "s-scrub" "no, send it to carol@example.com, not bob@example.com"
                                    "Sent to carol@example.com")))
          (ichor-observe "s-scrub" turn))
        (let ((pair (first (nodecode-ichor::pairs))))
          (is pair "one pair was captured")
          (is (search "[REDACTED:email]" (nodecode-ichor::field pair "rejected")) "the email is redacted")
          (is (search "~" (nodecode-ichor::field pair "rejected")) "the home path is a tilde")
          (is (null (search "alice" (nodecode-ichor::field pair "rejected"))) "the user name is gone")
          (is (null (search "bob@example.com" (nodecode-ichor::field pair "accepted"))) "and in the accepted side too")
          (is (equal "1 path · 0 secrets" (nodecode-ichor::summarize-scrub
                                       '(("home_path" . 1)))) "the card counts a redacted path")
          (is (string= "0 paths · 2 secrets" (nodecode-ichor::summarize-scrub '(("openai_key" . 2)))) "and the secrets apart"))))))

;;; --- the pair, end to end ---------------------------------------------------------

(deftest ichor-cell-a-correction-is-a-pending-pair ()
  (with-temp-store ()
    (with-temp-directory (root "ichor")
      (with-ichor ("dir" (namestring root))
        (ichor-complete "s-pair" "show disk usage of this folder" "du -sh .")
        (let ((turn (ichor-complete "s-pair" "no, make it human readable and sorted"
                                    "du -sh * | sort -h")))
          (ichor-observe "s-pair" turn)
          (ichor-observe "s-pair" turn))       ; a frame may be seen twice
        (let ((pair (first (nodecode-ichor::pairs))))
          (is (= 1 (length (nodecode-ichor::pairs))) "one pair, never two")
          (is (equal "correction" (nodecode-ichor::field pair "kind")))
          (is (equal "pending" (nodecode-ichor::field pair "status")))
          (is (equal "du -sh ." (nodecode-ichor::field pair "rejected")))
          (is (equal "du -sh * | sort -h" (nodecode-ichor::field pair "accepted")))
          (is (equal "no, make it human readable and sorted" (nodecode-ichor::field pair "feedback")))
          (is (equal "missing option" (nodecode-ichor::field pair "category")) "the local label says what was missing")
          (is (search "captured pair #1" (ichor:review)) "review shows the card")
          (is (search "you said:" (ichor:review))))))))

(deftest ichor-cell-a-retry-keeps-the-answer-that-replaced-it ()
  (with-temp-store ()
    (with-temp-directory (root "ichor")
      (with-ichor ("dir" (namestring root))
        (ichor-complete "s-retry" "run the test suite" "3 passing")
        (let ((turn (ichor-complete "s-retry" "run the test suite" "1 failing, 3 passing")))
          (ichor-observe "s-retry" turn))
        (let ((pair (first (nodecode-ichor::pairs))))
          (is (equal "retry" (nodecode-ichor::field pair "kind")))
          (is (equal "3 passing" (nodecode-ichor::field pair "rejected")))
          (is (equal "1 failing, 3 passing" (nodecode-ichor::field pair "accepted")))
          (is (equal "retried answer" (nodecode-ichor::field pair "category"))))))))

(deftest ichor-cell-a-first-turn-captures-nothing ()
  (with-temp-store ()
    (with-temp-directory (root "ichor")
      (with-ichor ("dir" (namestring root))
        (let ((turn (ichor-complete "s-one" "no, that is wrong" "fixed")))
          (ichor-observe "s-one" turn))
        (is (null (nodecode-ichor::pairs)) "nothing to compare a first turn with")))))

;;; --- the verbs and /ichor ---------------------------------------------------------

(deftest ichor-cell-keep-export-and-forget-answer ()
  (with-temp-store ()
    (with-temp-directory (root "ichor")
      (with-ichor ("dir" (namestring root))
        (ichor-complete "s-verbs" "show disk usage" "du -sh .")
        (let ((turn (ichor-complete "s-verbs" "no, make it human readable" "du -sh * | sort -h")))
          (ichor-observe "s-verbs" turn))
        (is (search "captured pair #1" (cell-entry "nodecode-ichor" "ichor" "review")) "a pending pair is there to review")
        (is (search "captured pair #1" (cell-entry "nodecode-ichor" "ichor" "review")))
        (is (search "kept" (cell-entry "nodecode-ichor" "ichor" "keep 1")) "keep settles it")
        (is (null (search "captured" (cell-entry "nodecode-ichor" "ichor" "review"))) "and it is no longer pending")
        (is (search "1 kept" (cell-entry "nodecode-ichor" "ichor" "status")))
        (is (search "usage: /ichor keep <id>" (nodecode-ichor::run-command "keep" "s-verbs")) "a verb without an id says how to use it")
        (multiple-value-bind (count folder) (nodecode-ichor::write-export
                                              (nodecode-ichor::exportable) (pathname (namestring root)))
          (is (= 1 count) "one pair exports")
          (let* ((line (first (nlk:read-json-lines
                                (merge-pathnames "dpo.jsonl" folder))))
                 (prompt (nlk:json-value line :array "prompt"))
                 (chosen (nlk:json-value line :array "chosen")))
            (is (equal "show disk usage" (nlk:json-value (aref prompt 0) :string "content")) "the prompt is what was asked")
            (is (equal "du -sh * | sort -h" (nlk:json-value (aref chosen 0) :string "content")) "the chosen side is the answer that replaced it")))
        (is (search "exported 1 pair" (cell-entry "nodecode-ichor" "ichor" "export")))
        (is (probe-file (merge-pathnames "export/atropos_env/pairs.jsonl" (pathname (namestring root)))) "the Atropos pairs file is written too")
        (is (search "deletes all 1" (cell-entry "nodecode-ichor" "ichor" "forget-all")) "forget-all asks first")
        (is (search "deleted 1 pair" (cell-entry "nodecode-ichor" "ichor" "forget-all yes")))
        (is (null (nodecode-ichor::pairs)) "and the file holds nothing")))))

(deftest ichor-cell-off-pauses-capture-here ()
  (with-temp-store ()
    (with-temp-directory (root "ichor")
      (with-ichor ("dir" (namestring root))
        (is (search "off for this session" (cell-entry "nodecode-ichor" "ichor" "off" "s-off")))
        (ichor-complete "s-off" "show disk usage" "du -sh .")
        (let ((turn (ichor-complete "s-off" "no, make it human readable" "du -sh *")))
          (ichor-observe "s-off" turn))
        (is (null (nodecode-ichor::pairs)) "a paused session records nothing")
        (is (search "on for this session" (cell-entry "nodecode-ichor" "ichor" "on" "s-off")))
        (let ((turn (ichor-complete "s-off" "no, make it readable and sorted" "du -sh * | sort -h")))
          (ichor-observe "s-off" turn))
        (is (= 1 (length (nodecode-ichor::pairs))) "and resumes")))))

;;; --- the one network call ---------------------------------------------------------

(defun ichor-stub-hub (box)
  "A stub for the Hub: whoami, create, commit, each recorded on BOX."
  (lambda (method url &rest arguments)
    (declare (ignore arguments))
    (push (list method url) (car box))
    (cond ((search "whoami" url) (values "{\"name\":\"tester\"}" 200 nil))
          ((search "repos/create" url) (values "{}" 201 nil))
          ((search "commit" url) (values "{}" 200 nil))
          (t (values "{}" 404 nil)))))

(deftest ichor-cell-share-reaches-only-the-hub-it-was-told ()
  (with-temp-store ()
    (with-temp-directory (root "ichor")
      (with-ichor ("dir" (namestring root))
      (ichor-complete "s-share" "show disk usage" "du -sh .")
      (let ((turn (ichor-complete "s-share" "no, make it human readable" "du -sh * | sort -h")))
        (ichor-observe "s-share" turn))
      (is (search "queued to share" (cell-entry "nodecode-ichor" "ichor" "share 1")))
      (let* ((box (list nil))
             (nodecode-ichor::*http* (ichor-stub-hub box))
             (nodecode-ichor::*hf-token* "hf_test"))
        (let ((answer (cell-entry "nodecode-ichor" "ichor" "share")))
          (is (search "shared 1 pair (private repo)" answer) "the upload says what went")
          (is (search "https://huggingface.co/datasets/tester/ichor-pairs" answer) "and where it landed"))
        (is (= 3 (length (car box))) (format nil "~d calls" (length (car box))))
        (is (every (lambda (call) (search "https://huggingface.co/" (second call))) (car box)) "every call is the Hub")
        (is (equal "https://huggingface.co/datasets/tester/ichor-pairs"
                   (nodecode-ichor::field (first (nodecode-ichor::pairs)) "share_url")) "the pair remembers where it went"))
      (let ((nodecode-ichor::*hf-token* nil))
        ;; the scratch environment holds no token, so the refusal is the real one
        (is (search "sharing needs a Hugging Face token"
                    (cell-entry "nodecode-ichor" "ichor" "share")) "without a token the refusal names it"))))))
