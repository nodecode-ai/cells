;;;; cell-test.lisp --- lifecycle, the round's blocks, the write it records,
;;;; and /neuralmind.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a stubbed program: that
;;;; a start installs the two hooks, the manual and /neuralmind, and a stop
;;;; takes every one of them back; that a section with no program refuses;
;;;; that a session's first turn carries the recap and then what its prompt
;;;; recalls, in that order, in the payloads the plugin sent; that a later
;;;; round of the same turn adds nothing and the next turn carries its own
;;;; recall only; that a turn in a project nothing has built runs nothing;
;;;; that a cron run recalls nothing; that an eval that wrote files records
;;;; each of them; and that /neuralmind prints the state and asks by hand.
;;;; Nothing here needs NeuralMind installed, and none of it touches a network.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "neuralmind"
  (:hooks :round :tool)
  (:help :neuralmind)
  (:command "neuralmind")
  (:running (is (= 8 (nodecode-neuralmind::setting :timeout)) "eight seconds a call by default")
            (is (eq t (nodecode-neuralmind::setting :recall)) "recall on by default")
            (is (eq t (nodecode-neuralmind::setting :recap)) "and the recap too"))
  (:refused ("command" "definitely-no-such-program-xyz") ("command" "") ("timeout" 0)))

(deftest neuralmind-cell-a-section-with-no-program-refuses ()
  (is (signals-error nlk:config-refusal
        (nodecode-neuralmind:start-cell (nlk:json-object "neuralmind" (nlk:json-object))))
      "an empty section names no program, so the cell refuses at start"))

;;; --- what a reply adds --------------------------------------------------------------

(deftest neuralmind-cell-a-reply-s-context-is-the-block-and-nothing-else ()
  (is (equal "recall body"
             (nodecode-neuralmind::context-of (json-context "recall body")))
      "additionalContext is the block, as the plugin read it")
  (is (equal "" (nodecode-neuralmind::context-of "")) "an empty reply adds nothing")
  (is (equal "" (nodecode-neuralmind::context-of "not JSON at all"))
      "and neither does a reply outside the hook protocol")
  (is (equal "" (nodecode-neuralmind::context-of "{\"hookSpecificOutput\":{}}"))
      "nor a reply that carries no context"))

;;; --- the turn's blocks -------------------------------------------------------------

(deftest neuralmind-cell-a-sessions-first-turn-carries-the-recap-then-the-recall ()
  (with-temp-store ()
    (ensure-durable-session "n1")
    (with-cell-stop ((neuralmind-start "project" (namestring (neuralmind-project))))
      (with-run-stub ()
        (let ((turn (neuralmind-turn "n1" :input "where is the discount code applied")))
          (neuralmind-round "n1" turn 0)
          (is-present (text (first (neuralmind-told "n1")))
            "the turn's first request carried one block"
            (is (search "the previous session in this project" text) "the recap")
            (is (search "add_discount" text) "and what the prompt matched")
            (is (< (search "the previous session" text) (search "add_discount" text))
                "the recap first, as the plugin ordered them"))
          (let ((start (neuralmind-payload "session-start"))
                (prompt (neuralmind-payload "prompt-submit")))
            (is-present (entry start) "session-start was asked"
              (is (search "neuralmind-project" (gethash "cwd" entry)) "in the project")
              (is (equal "n1" (gethash "session_id" entry)) "for the session")
              (is (equal "startup" (gethash "source" entry)) "as a startup"))
            (is-present (entry prompt) "prompt-submit was asked"
              (is (equal "where is the discount code applied" (gethash "prompt" entry))
                  "with the turn's own words, as tool_input the plugin sent")
              (is (equal "n1" (gethash "session_id" entry)))))
          (neuralmind-round "n1" turn 1)
          (is (= 1 (length (neuralmind-told "n1"))) "a later round of the same turn adds nothing")
          (nlk:complete-turn turn nil)
          (neuralmind-round "n1" (neuralmind-turn "n1" :input "and the tax") 0)
          (let ((told (neuralmind-told "n1")))
            (is (= 2 (length told)) "the next turn carries its own match")
            (is (search "add_discount" (second told)))
            (is (not (search "the previous session" (second told))) "the recap is given once a session")
            (is (equal "and the tax" (gethash "prompt" (neuralmind-payload "prompt-submit")))
                "with that turn's words")))))))

(deftest neuralmind-cell-a-project-nothing-has-built-carries-nothing ()
  (with-temp-store ()
    (ensure-durable-session "n2")
    (with-cell-stop ((neuralmind-start "project" (namestring (neuralmind-unbuilt))))
      (with-run-stub ()
        (neuralmind-round "n2" (neuralmind-turn "n2" :input "anything at all") 0)
        (is (null (neuralmind-told "n2")) "the turn is told nothing")
        (is (null *calls*) "and the program was not even asked")))))

(deftest neuralmind-cell-a-cron-run-recalls-nothing-on-its-own ()
  (with-temp-store ()
    (ensure-durable-session "n3")
    (with-cell-stop ((neuralmind-start "project" (namestring (neuralmind-project))))
      (with-run-stub ()
        (neuralmind-round "n3" (neuralmind-turn "n3" :source "cron") 0)
        (is (null (neuralmind-told "n3")) "a cron run is told nothing")
        (is (null *calls*) "and nothing was run for it")))))

(deftest neuralmind-cell-the-two-halves-switch-off-one-at-a-time ()
  (with-temp-store ()
    (ensure-durable-session "n4")
    (with-cell-stop ((neuralmind-start "project" (namestring (neuralmind-project))
                                      "recap" nil))
      (with-run-stub ()
        (neuralmind-round "n4" (neuralmind-turn "n4" :input "the discount code") 0)
        (is (null (neuralmind-call "session-start")) "recap off asks for no recap")
        (is-present (text (first (neuralmind-told "n4")))
          "the recall still rides the turn"
          (is (search "add_discount" text)))))))

;;; --- the write a turn made ---------------------------------------------------------

(deftest neuralmind-cell-an-eval-that-wrote-files-records-each-of-them ()
  (with-cell-stop ((neuralmind-start "project" (namestring (neuralmind-project))
                                    "record_edits" t))
    (let ((nodecode-neuralmind::*session* "n5"))
      (with-run-stub ()
        (let ((answer (nodecode-neuralmind::note-tool
                       (list :name "eval" :call-id "c1"
                             :arguments (nlk:make-json-object
                                         "form"
                                         "(write-file \"src/a.lisp\" ($ \"x\")) (edit \"src/b.lisp\" ($ \"o\") ($ \"n\"))"))
                       (lambda (op) (declare (ignore op)) "wrote 2 files"))))
          (is (equal "wrote 2 files" answer) "the tool's answer passes through")
          (let ((recorded (reverse (remove "edit-activity" *calls* :key #'first :test-not #'equal))))
            (is (= 2 (length recorded)) "one record per written file")
            (is-present (entry (first recorded)) "the first write was recorded"
              (is (search "src/a.lisp" (second entry)) "the file the write named")
              (is (search "n5" (second entry)) "stamped with the session")
              (is (search "neuralmind-project" (second entry)) "in the project"))
            (is-present (entry (second recorded)) "and the second"
              (is (search "src/b.lisp" (second entry)) "the file the edit named"))))))))

(deftest neuralmind-cell-a-failed-eval-and-an-eval-that-wrote-nothing-record-nothing ()
  (with-cell-stop ((neuralmind-start "project" (namestring (neuralmind-project))
                                    "record_edits" t))
    (let ((nodecode-neuralmind::*session* "n6"))
      (with-run-stub ()
        (nodecode-neuralmind::note-tool
         (list :name "eval" :call-id "c2"
               :arguments (nlk:make-json-object "form" "(write-file \"src/a.lisp\" ($ \"x\"))"))
         (lambda (op) (declare (ignore op)) (nle:failure "ERROR: eval refused the form")))
        (is (null *calls*) "a failed call records nothing")
        (nodecode-neuralmind::note-tool
         (list :name "eval" :call-id "c3"
               :arguments (nlk:make-json-object "form" "(+ 1 2)"))
         (lambda (op) (declare (ignore op)) "3"))
        (is (null *calls*) "and neither does one that wrote no file")))))

;;; --- /neuralmind -------------------------------------------------------------------

(deftest neuralmind-cell-the-command-prints-the-state-and-asks-by-hand ()
  (with-cell-stop ((neuralmind-start "project" (namestring (neuralmind-project))))
    (with-run-stub ()
      (let ((line (nodecode-neuralmind::run-slash "" "n7")))
        (is (search "project neuralmind-project" line) "the project it serves")
        (is (search "recall on" line) "the switches")
        (is (null *calls*) "status runs nothing"))
      (let ((answer (nodecode-neuralmind::run-slash "recall discount code" "n7")))
        (is (search "add_discount" answer) "a query answers with what matched")
        (is (equal "discount code" (gethash "prompt" (neuralmind-payload "prompt-submit")))
            "asking the program for the words typed"))
      (let ((answer (nodecode-neuralmind::run-slash "recap" "n7")))
        (is (search "the previous session" answer) "recap answers with the recap")
        (is-present (entry (neuralmind-payload "session-start")) "asked as a startup"
          (is (equal "n7" (gethash "session_id" entry))))))))

(deftest neuralmind-cell-the-verbs-say-so-where-nothing-is-built ()
  (with-cell-stop ((neuralmind-start "project" (namestring (neuralmind-unbuilt))))
    (with-run-stub ()
      (is (equal nodecode-neuralmind:*no-project* (neuralmind:recall "anything"))
          "a query with no built project names the fix")
      (is (equal nodecode-neuralmind:*no-project* (neuralmind:recap))
          "and so does the recap")
      (is (null *calls*) "without running the program"))))
