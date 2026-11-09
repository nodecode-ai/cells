;;;; cell-test.lisp --- bot chat's lifecycle, its refusals and its one style.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own behaviour: nothing is refused or
;;;; said until /bot-chat on names the session, each of the rules refuses the
;;;; call it describes and lets the rest run, off puts everything back, a
;;;; second question in one turn does not wait, and the style rides exactly
;;;; the requests of the sessions the mode is on for.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "bot-chat"
  (:hooks :tool 'nle::read-harness-sections 'nle:ask-operator)
  (:command "bot-chat")
  (:help :bot-chat)
  (:refused ("scope" 42)))

;;; --- nothing is on until the operator says so ------------------------------

(deftest bot-chat-cell-is-off-until-turned-on
  (with-cell-stop ((bot-chat-start))
    (is (eq :allowed (bot-chat-eval-verdict "(sh \"rm -rf build\")")) "a delete runs while the mode is off")
    (is (eq :allowed (bot-chat-eval-verdict "(read-file \".env\")")) "and so does reading a credential file")
    (is (search "off for this session" (bot-chat-command "status")) "status says the mode is off")
    (is (search "on for this session" (bot-chat-command "on")) "on turns it on for this session")
    (is (stringp (bot-chat-eval-verdict "(sh \"rm -rf build\")")) "and the same delete is refused")))

;;; --- the rules -------------------------------------------------------------

(deftest bot-chat-cell-refuses-the-six-tool-rules
  (with-cell-stop ((bot-chat-start))
    (bot-chat-command "on")
    (is (search "secrets" (bot-chat-eval-verdict "(read-file \".env\")")) "a credential file is refused")
    (is (search "secrets" (bot-chat-eval-verdict "(sh \"cat ~/.ssh/id_rsa\")")) "a key in a command is refused")
    (is (eq :allowed (bot-chat-eval-verdict "(read-file \".env.example\")")) "but a documented placeholder is not")
    (is (search "secrets" (bot-chat-eval-verdict "(sh \"printenv\")")) "printing the environment is refused")
    (is (search "deletes" (bot-chat-eval-verdict "(sh \"rm -rf build\")")) "a delete is refused")
    (is (search "destructive git" (bot-chat-eval-verdict "(sh \"git reset --hard HEAD~1\")")) "a hard reset is refused")
    (is (search "protected branch" (bot-chat-eval-verdict "(sh \"git push origin main\")")) "a push to main is refused")
    (is (eq :allowed (bot-chat-eval-verdict "(sh \"git push origin feature\")")) "a push to a feature branch runs")
    (is (eq :allowed (bot-chat-eval-verdict "(write-file \"notes.md\" \"rm -rf is what a delete looks like\")")) "a write whose text mentions a delete is not a delete")
    (is (eq :allowed (bot-chat-eval-verdict "(sh \"ls\")")) "and an ordinary command runs")))

(deftest bot-chat-cell-refuses-a-send-an-expensive-model-and-a-delete-tool
  (with-cell-stop ((bot-chat-start))
    (bot-chat-command "on")
    (is (search "outside world" (bot-chat-tool-verdict "send_email" "to" "a@b")) "a send is refused")
    (is (search "outside world" (bot-chat-tool-verdict "post-message" "text" "hi")) "a dashed send is refused")
    (is (search "expensive" (bot-chat-tool-verdict "model-router" "model" "claude-opus-4")) "an expensive model is refused")
    (is (eq :allowed (bot-chat-tool-verdict "web_search" "query" "cheap")) "a search runs")
    (is (search "deletes" (bot-chat-tool-verdict "delete-file" "path" "build")) "a delete tool is refused")))

(deftest bot-chat-cell-relaxed-keeps-three-rules
  (with-cell-stop ((bot-chat-start "preset" "relaxed"))
    (bot-chat-command "on")
    (is (eq :allowed (bot-chat-eval-verdict "(sh \"rm -rf build\")")) "relaxed lets a delete run")
    (is (stringp (bot-chat-eval-verdict "(read-file \".env\")")) "but not a credential file")
    (is (stringp (bot-chat-eval-verdict "(sh \"git push origin main\")")) "nor a push to a protected branch")
    (is (stringp (bot-chat-eval-verdict "(sh \"git reset --hard HEAD~1\")")) "while a destructive reset still waits for approval")))

(deftest bot-chat-cell-scope-all-covers-every-session
  (with-cell-stop ((bot-chat-start "scope" "all"))
    (is (stringp (bot-chat-eval-verdict "(sh \"rm -rf build\")" "another")) "nothing has to be turned on")
    (is-present (entry (assoc "bot-chat" (bot-chat-section "another") :test #'equal))
      "the style rides a session nobody named"
      (is (search "Act, don't narrate" (cdr entry)) "and it is the style contract"))))

;;; --- the style, and the one question --------------------------------------

(deftest bot-chat-cell-rides-the-style-where-it-is-on
  (with-cell-stop ((bot-chat-start))
    (is (notany (lambda (entry) (equal "bot-chat" (car entry))) (bot-chat-section "s1")) "nothing rides a request while it is off")
    (bot-chat-command "on")
    (let ((sections (bot-chat-section "s1")))
      (is-present (entry (assoc "bot-chat" sections :test #'equal))
        "the style rides the session it was turned on in"
        (is (search "Act, don't narrate" (cdr entry)) "it is the style contract")
        (is (search "nothing here" (cdr (assoc "index" sections :test #'equal))) "the next reader's own entries stay beneath it"))
      (is (notany (lambda (entry) (equal "bot-chat" (car entry))) (bot-chat-section "s2")) "and never another session's"))
    (bot-chat-command "off")
    (is (notany (lambda (entry) (equal "bot-chat" (car entry))) (bot-chat-section "s1")) "off takes it back")))

(deftest bot-chat-cell-asks-one-question-a-turn
  (with-cell-stop ((bot-chat-start))
    (bot-chat-command "on")
    (is (eq :asked (bot-chat-ask "the first" "s1" "t1")) "the first question of a turn is shown")
    (is (eq :one-question (nth-value 1 (bot-chat-ask "the second" "s1" "t1"))) "a second one in the same turn is not")
    (is (eq :asked (bot-chat-ask "the next turn's" "s1" "t2")) "the next turn asks again")
    (bot-chat-command "off")
    (is (eq :asked (bot-chat-ask "one" "s1" "t3")) "a session with the mode off is not limited"
        )))

;;; --- the operator's side ---------------------------------------------------

(deftest bot-chat-cell-status-and-rules-say-what-is-on
  (with-cell-stop ((bot-chat-start))
    (is (search "preset: default" (bot-chat-command "status")) "status names the preset")
    (is (search "secrets" (bot-chat-command "rules")) "rules lists what is refused")
    (is (search "main, master" (bot-chat-command "rules")) "and the protected branches")
    (is (search "Unknown" (bot-chat-command "nonsense")) "and an unknown argument is named")))
