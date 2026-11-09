;;;; cell-test.lisp --- the limit, the one line, the handoff file, the reset.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's work over real sessions and a real
;;;; store: that a session past its limit is told once and its operator gets
;;;; one standing notice naming the path; that the reasons are the ones
;;;; actually over their limit; that the handoff tool files a dated markdown
;;;; page under the home and that the next request says where it is; that
;;;; handoff mode asks again at each turn's first round; that /handydandy
;;;; reset forgets and the session is told again after it; and that the
;;;; section refuses a limit below zero.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "handydandy"
  (:hooks :round)
  (:help :handydandy)
  (:command "handydandy")
  (:running (is (equal nodecode-handydandy::+primer+ (nle:help :handydandy)))
            (is (equal "advise" (nodecode-handydandy::setting :mode)) "advise is the default mode")
            (is (= 100000 (nodecode-handydandy::setting :max-input-tokens)) "and its token limit")
            (is (= 50 (nodecode-handydandy::setting :context-percent)) "half the model's window"))
  (:refused ("max_input_tokens" -1) ("context_percent" -1) ("max_rounds" -1))
  (:idle handydandy:handydandy-error (handydandy:status)))

;;; --- the limits ------------------------------------------------------------------------

(deftest handydandy-cell-the-reasons-are-the-ones-over-their-limit ()
  (with-handydandy ()
    (:config ("max_input_tokens" 1000) ("context_percent" 50) ("max_rounds" 4))
    (is (equal '("1,200 input tokens, limit 1,000")
               (nodecode-handydandy::reasons-over 1200 nil 1)) "the token limit alone")
    (is (equal '("90% of the model's 1,000-token context")
               (nodecode-handydandy::reasons-over 900 1000 1)) "and the window's share")
    (is (equal '("4 rounds, limit 4") (nodecode-handydandy::reasons-over 10 2000 4)) "and the rounds")
    (is (null (nodecode-handydandy::reasons-over 10 2000 1)) "a session under its limits is fine")
    (is (null (nodecode-handydandy::reasons-over 10 nil 3)) "an unknown window checks nothing")))

;;; --- the one line ----------------------------------------------------------------------

(deftest handydandy-cell-a-session-past-its-limit-is-told-once ()
  (with-handydandy ("a")
    (:config ("max_rounds" 1))
    (handydandy-turn "a")
    (let ((told (handydandy-told "a")))
      (is (= 1 (length told)) "one line at that request")
      (is-carrying (line (first told))
        ("[handydandy] This session is getting long")
        ("1 round, limit 1" "the reason it was told")
        ("`handoff` tool" "and what to do about it")))
    (is-present (notice (cell-notice "nodecode-handydandy"))
      "the operator gets one standing notice"
      (is (search "handoffs" (second notice)) "naming where the handoff goes"))
    (is (nodecode-handydandy::armed-at "a") "the session is armed")
    (handydandy-turn "a")
    (is (= 1 (length (handydandy-told "a"))) "a second turn is told nothing more")))

;;; --- the handoff file ---------------------------------------------------------------------

(deftest handydandy-cell-the-tool-files-the-handoff-and-the-next-request-says-where ()
  (with-handydandy ("a")
    (:config ("max_rounds" 100)
             ("session_log_csv" (namestring (nlk:home "handoffs.csv"))))
    (let ((answer (on-session "a" (nodecode-handydandy::write-handoff (handydandy-brief)))))
      (is (search "handoff written to" answer) answer))
    (let ((path (nodecode-handydandy::handoff-written "a")))
      (is path "the path is held in the session's state")
      (is (probe-file path) "and the file is on disk")
      (is-carrying (body (uiop:read-file-string path))
        ("# Handoff")
        ("## Standing orders and approvals still in force")
        ("## State of each workstream")
        ("the cell is written; its tests are next" "the model's own words")
        ("## Next action")
        ("run the cell's tests")
        ("## What was not touched")
        ("(nothing to say)" "an unsaid section says so")))
    (is-carrying (log (uiop:read-file-string (nlk:home "handoffs.csv")))
      ("timestamp,session_id,handoff" "the header, once")
      ("\"a\"" "the session's row"))
    (handydandy-turn "a")
    (let ((told (handydandy-told "a")))
      (is (= 1 (length told)) "the request after it is told where the handoff is")
      (is-carrying (line (first told)) ("[handydandy] The handoff is at")))))

;;; --- where the file goes ----------------------------------------------------------------------

(deftest handydandy-cell-a-handoff-is-dated-named-for-its-session-and-under-the-home ()
  (with-handydandy ("ab/cd")
    (:config ("max_rounds" 0))
    (let* ((path (nodecode-handydandy::handoff-path "ab/cd"))
           (name (file-namestring path)))
      (is (uiop:string-prefix-p "HANDOFF-" name) name)
      (is (search (subseq (nlk:iso-now) 0 10) name) "dated today")
      (is (search "ab-cd.md" name) "named for the session, its slash a dash")
      (is (equal (namestring (nlk:home "handoffs/"))
                 (namestring (uiop:pathname-directory-pathname path))) "under the home's handoffs folder")))
  (let ((mine (namestring (nlk:home "mine/"))))
    (with-handydandy ()
      (:config ("handoff_dir" mine))
      (is (equal mine
                 (namestring (uiop:pathname-directory-pathname
                              (nodecode-handydandy::handoff-path "a")))) "and handoff_dir moves it"))))

;;; --- the operator's reset ---------------------------------------------------------------------

(deftest handydandy-cell-reset-forgets-and-the-session-is-told-again ()
  (with-handydandy ("a")
    (:config ("max_rounds" 1))
    (handydandy-turn "a")
    (is (= 1 (length (handydandy-told "a"))))
    (is-carrying (answer (cell-entry "nodecode-handydandy" "handydandy" "reset" "a"))
      ("forgotten" "the answer says what it did"))
    (is (null (nodecode-handydandy::armed-at "a")) "the session is no longer armed")
    (is (null (cell-notice "nodecode-handydandy")) "and the operator's notice is gone")
    (handydandy-turn "a")
    (is (= 2 (length (handydandy-told "a"))) "so the next request nudges again")))

;;; --- handoff mode ---------------------------------------------------------------------------------

(deftest handydandy-cell-handoff-mode-asks-for-the-file-at-each-turn-s-first-round ()
  (with-handydandy ("a")
    (:config ("mode" "handoff") ("max_rounds" 1))
    (handydandy-turn "a")
    (handydandy-turn "a")
    (let ((told (handydandy-told "a")))
      (is (= 2 (length told)) "the ask stands at each turn's first round")
      (is-carrying (line (first told))
        ("[handydandy] This session has passed its size limit")
        ("write a handoff for a fresh session")
        ("never copy a secret value"))
      (is (search (namestring (nodecode-handydandy::handoff-path "a")) (first told)) "and names the file it goes to")))
  (with-handydandy ("b")
    (:config ("handoff_dir" (namestring (nlk:home "elsewhere/"))))
    (is (search "elsewhere" (nodecode-handydandy::status-line "b")) "the status names where a handoff would go")))

