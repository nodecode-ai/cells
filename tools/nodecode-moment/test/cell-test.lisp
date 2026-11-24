;;;; cell-test.lisp --- the fingerprint, the receipt, the report and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the cell starts and stops cleanly, its tool and its
;;;; command answer for the files of the scratch home, a file that changed
;;;; under a running turn puts one receipt in that turn's next request and is
;;;; not reported twice, a file that appears and one that goes read as each,
;;;; and an unset state_file refuses. Every file lived in the scratch home, so
;;;; no network is touched at all.

(in-package #:nodecode.test)

(define-test-slice "moment" "MOMENT-CELL-" :start nodecode-moment:start-cell)

;;; --- helpers --------------------------------------------------------------------

(defvar *moment-calls* 0
  "How many tool calls this test file made, for call ids.")

(defun moment-state-file ()
  "The state file this test's scratch home holds."
  (namestring (nlk:home "moment/state.json")))

(defun moment-house (name text)
  "Write TEXT to NAME under the scratch home; answer its path as a string."
  (let ((path (nlk:home name)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :external-format :utf-8)
      (write-string text out))
    (namestring path)))

(defmacro with-moment ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS over the section, the state file always set."
  `(with-cell-stop ((moment-start "state_file" (moment-state-file) ,@pairs))
     ,@body))

(defun moment-tool-text (&rest arguments)
  "The text the moment_check tool answers, called as the provider's call is."
  (princ-to-string
   (nle::message-content
    (execute-wire-call (format nil "moment-call-~d" (incf *moment-calls*)) "moment_check"
                        (if arguments
                            (nlk:encode-json-object (apply #'nlk:make-json-object arguments))
                            "{}")))))

(defun moment-receipts (context)
  "The moment receipts one compiled request carries."
  (loop for message in (coerce (nle::compiled-turn-context-messages context) 'list)
        for text = (nle::content-text (gethash "content" message))
        when (and (stringp text) (search "[moment] DRIFT RECEIPT" text))
          collect text))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "moment"
  (:config "state_file" (moment-state-file))
  (:hooks :round)
  (:help :moment)
  (:command "moment")
  (:running (is (string= (moment-state-file) (namestring (nodecode-moment::state-path))))
            (is (= 2000 (nodecode-moment::setting :max-chars)))
            (is (= 50 (nodecode-moment::setting :keep-sessions))))
  (:refused ("max_chars" 10) ("keep_sessions" 0))
  (:idle nodecode-moment::moment-error (nodecode-moment::run-slash "" nil)))

;;; --- the tool -------------------------------------------------------------------

(deftest moment-cell-the-tool-reports-a-change ()
  (with-moment ()
    (moment-house "SOUL.md" "Be terse.
")
    (nodecode-moment::observe "moment-tool")
    (is-carrying (unchanged (moment-tool-text))
                 "\"moment_check\"" ("unchanged since session start"))
    (is-carrying (narrowed (moment-tool-text "file" "SOUL.md"))
                 ("SOUL.md") (:absent "AGENTS.md"))
    (moment-house "SOUL.md" "Be terse.
Prefer plain words.
")
    (is-carrying (drifted (moment-tool-text))
                 ("CHANGED since session start") ("+Prefer plain words."))))

;;; --- every round -----------------------------------------------------------------

(deftest moment-cell-a-change-under-a-turn-reaches-it-once ()
  (with-session-store ("moment-drift")
    (with-moment ()
      (moment-house "SOUL.md" "Be terse.
")
      (ensure-durable-session "moment-drift")
      (with-provider-stub (nil (wire-message "assistant" :content "ok"))
        (nle:run "hello" :session (nle:intern-session "moment-drift") :command-id "moment-1"))
      (moment-house "SOUL.md" "Be terse.
Prefer plain words.
")
      (let ((asked '()))
        (with-provider-stub (context
                             (push (moment-receipts context) asked)
                             (wire-message "assistant" :content "ok"))
          (nle:run "again" :session (nle:intern-session "moment-drift") :command-id "moment-2"))
        (is (= 1 (length (first asked))) "the changed file is reported once")
        (is-carrying (first-receipt (first (first asked)))
                     ("[moment] DRIFT RECEIPT") ("SOUL.md")
                     ("Changed lines (session start -> now):") ("+Prefer plain words."))
        (is (null (nodecode-moment::observe "moment-drift")) "the same change is not reported twice")))))

;;; --- the operator's way in ---------------------------------------------------------

(deftest moment-cell-the-command-reports-and-resets ()
  (with-moment ()
    (moment-house "AGENTS.md" "Answer in English.
")
    (nodecode-moment::observe "moment-cmd")
    (is-carrying (steady (nodecode-moment::run-slash "" "moment-cmd"))
                 ("unchanged since session start") ("AGENTS.md"))
    (moment-house "AGENTS.md" "Answer in English.
Never guess.
")
    (is-carrying (moved (nodecode-moment::run-slash "" "moment-cmd"))
                 ("CHANGED since session start") ("+Never guess."))
    (is-carrying (reset (nodecode-moment::run-slash "reset" "moment-cmd"))
                 ("baseline taken again"))
    (is-carrying (steady-again (nodecode-moment::run-slash "" "moment-cmd"))
                 ("unchanged since session start") (:absent "CHANGED"))))

(deftest moment-cell-a-file-that-appears-and-one-that-goes ()
  (let ((notes (namestring (nlk:home "notes/extra.md"))))
    (with-moment ("watch" (vector notes))
      (nodecode-moment::observe "moment-appear")
      (moment-house "notes/extra.md" "hello
")
      (is-carrying (appeared (nodecode-moment::observe "moment-appear"))
                   ("did not exist when this session started") ("hello"))
      (delete-file (nlk:home "notes/extra.md"))
      (is-carrying (vanished (nodecode-moment::observe "moment-appear"))
                   ("existed when this session started; it is gone now")))))

;;; --- without a state file ------------------------------------------------------------

(deftest moment-cell-without-a-state-file-it-refuses ()
  (with-cell-stop ((moment-start))
    (is (nle::find-tool "moment_check") "the tool is there")
    (is-carrying (answer (moment-tool-text))
                 "ERROR" ("moment.state_file is not set"))
    (is-carrying (refusal (handler-case (nodecode-moment::run-slash "" nil)
                             (nodecode-moment::moment-error (condition)
                               (princ-to-string condition))))
                 ("moment.state_file is not set"))))
