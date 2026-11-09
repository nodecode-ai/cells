;;;; cell-test.lisp --- the four tools, the command, the labels, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here runs Rex: the CLI is a Lisp function the tests rebind, and
;;;; what it is asked (argv, timeout, session, pane) is recorded. What is
;;;; proved is the cell's own doing -- the argv each verb builds, the mark on
;;;; a label and the label kept under it, the pane read back, where this
;;;; session is -- and that every way it can fail says why.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "rex"
  (:help :rex)
  (:command "rex")
  (:running (is (= 3 (nodecode-rex::setting :timeout-seconds)) "three seconds a call")
            (is (= 4 (nodecode-rex::setting :label-words)) "four words a label")
            (is (equal "below" (nodecode-rex::setting :split)) "a new pane goes below")
            (is-present (entry (nle::find-tool "rex_run")) "the tools are registered"
              (is (hash-table-p (nlk:contract-schema (second entry))) "rex_run against a declared contract"))
            (is-present (entry (nle::find-tool "rex_capture")) "rex_capture")
            (is-present (entry (nle::find-tool "rex_panes")) "rex_panes")
            (is-present (entry (nle::find-tool "rex_label")) "rex_label"))
  (:refused ("binary" "definitely-no-such-rex-xyz") ("split" "left")
            ("timeout_seconds" 0) ("label_words" 0))
  (:idle rex:rex-error (rex:panes) (rex:label "fix-login")))

;;; --- the tools ---------------------------------------------------------------------

(deftest rex-cell-the-run-tool-opens-a-pane-with-the-plugin-s-argv ()
  (with-cell-stop ((rex-start))
    (with-rex-stub ((rex-answers (("run") 0 +opened+)))
      (let ((answer (rex-tool "rex_run" "command" "npm run dev" "label" "Dev Server")))
        (is (search "npm run dev" answer) "the answer names the command")
        (is (search "b-2" answer) "and the pane rex opened")
        (let ((want '("--autostart=false" "run" "-s" "s-1" "-b" "b-1"
                      "--split=below" "--ratio" "35" "--focus=false" "--label" "dev-server"
                      "--json" "--keep-open" "--" "npm run dev")))
          (is (equal want (last-argv)) "the argv is the plugin's, beside this pane"))
        (is (equal "b-1" (last-pane)) "the call carries this session's REX_BLOCK")
        (is (search "dev-server" answer) "the label is a slug")
        (is (not (search "focus=true" answer)) "a pane never takes focus")))
    (with-rex-stub ((rex-answers (("run") 0 +opened+)))
      (rex:run "npm test" :label "tests" :split "right" :keep-open nil)
      (is (member "--split=right" (last-argv) :test #'equal) "a split to the right")
      (is (not (member "--keep-open" (last-argv) :test #'equal)) "a pane that closes when the command exits"))))

(deftest rex-cell-a-command-without-words-or-a-direction-is-refused ()
  (with-cell-stop ((rex-start))
    (with-rex-stub ((rex-answers))
      (is (search "no command to run" (rex-refusal #'rex:run "   ")) "an empty command is named")
      (is (search "no split direction" (rex-refusal #'rex:run "ls" :split "beside")) "an unknown split is named")
      (is (search "no word" (rex-refusal #'rex:label "!!!")) "a label needs a word")
      (is (null *runs*) "and nothing was run"))))

(deftest rex-cell-the-capture-tool-reads-the-pane-s-newest-lines ()
  (with-cell-stop ((rex-start))
    (with-rex-stub ((rex-answers (("capture" "-b" "b-1" "--trim") 0
                                   (format nil "one~%two~%three~%four~%five"))))
      (let ((answer (rex-tool "rex_capture" "lines" 2)))
        (let ((want '("--autostart=false" "capture" "-b" "b-1" "--trim")))
          (is (equal want (last-argv)) "the pane's output, trimmed, as the plugin read it"))
        (is (search "four" answer) "the newest lines are the answer")
        (is (search "five" answer))
        (is (not (search "one" answer)) "with the oldest left out")
        (is (search "3 earlier lines not shown" answer) "and the cut said out loud")))
    (with-rex-stub ((rex-answers (("capture" "-b" "b-9" "--trim") 0 "ready")))
      (is (search "ready" (rex-tool "rex_capture" "pane" "b-9")) "a named pane is read instead"))
    (with-rex-stub ((rex-answers (("capture" "-b" "b-1" "--trim") 0 "")))
      (is (search "shows nothing yet" (rex-tool "rex_capture")) "silence is said, not answered with nothing"))))

(deftest rex-cell-the-panes-tool-says-where-this-session-is ()
  (with-cell-stop ((rex-start))
    (with-rex-stub ((rex-answers (("ls" "--json") 0 +listing+)
                                  (("window" "list") 0 +windows+)))
      (let ((answer (rex-tool "rex_panes")))
        (is (search "session s-1" answer) "the session it acts on")
        (is (search "ops" answer) "every session Rex has")
        (is (search "w-2" answer) "and this session's windows")
        (is (search "<- this one" answer) "with its own session marked")
        (is (search "<- this pane" answer) "and its own pane marked")
        (is (search "b-9" answer) "the focused pane of each window")))))

(deftest rex-cell-the-label-tool-renames-and-marks-without-touching-the-rest ()
  (with-cell-stop ((rex-start))
    (with-rex-stub ((rex-answers (("window" "inspect" "--json") 0 +window+)
                                  (("window" "rename") 0 "")))
      (is (search "fix-login" (rex:label "Fix Login Redirect Now Please")) "the label Rex now has")
      (is (ran-p (rex-rename "fix-login-redirect-now")) "four words, hyphens, as the plugin named them"))
    (with-rex-stub ((rex-answers (("window" "inspect" "--json") 0 +window+)
                                  (("window" "rename") 0 "")))
      (rex:label "fix-login" :mark "done")
      (is (ran-p (rex-rename "✓ fix-login")) "a mark is one glyph and a space, in front"))
    (with-rex-stub ((rex-answers (("window" "inspect" "--json") 0
                                   "{\"window_id\":\"w-1\",\"label\":\"● fix-login\"}")
                                  (("window" "rename") 0 "")))
      (rex:label "fix-login" :mark "clear")
      (is (ran-p (rex-rename "fix-login")) "clear takes the glyph off and keeps the label"))
    (with-rex-stub ((rex-answers (("window" "inspect" "--json") 0 +window+)
                                  (("window" "rename") 0 "")))
      (is (search "no status mark" (rex-refusal #'rex:label "x" :mark "sleepy")) "an unknown mark is named"))))

;;; --- /rex --------------------------------------------------------------------------

(deftest rex-cell-the-command-answers-where-it-is-with-a-row-for-each-verb ()
  (with-cell-stop ((rex-start))
    (with-rex-stub ((rex-answers (("window" "inspect" "--json") 0 +window+)))
      (multiple-value-bind (text dialog) (cell-entry "nodecode-rex" "rex" "")
        (is (search "session s-1" text) "the text says which session")
        (is (search "fix-login" text) "and what the window is called")
        (is-present (rows (rex-dialog-rows dialog))
          "typed alone it answers a panel"
          (is (plusp (length rows)) "with a row per verb"))))))

(deftest rex-cell-the-command-runs-each-verb-and-a-bare-label-is-a-rename ()
  (with-cell-stop ((rex-start))
    (with-rex-stub ((rex-answers (("run") 0 +opened+)))
      (is (search "npm test" (cell-entry "nodecode-rex" "rex" "run npm test")) "run opens a pane"))
    (with-rex-stub ((rex-answers (("ls" "--json") 0 +listing+)
                                  (("window" "list") 0 +windows+)))
      (is (search "w-2" (cell-entry "nodecode-rex" "rex" "panes")) "panes lists them"))
    (with-rex-stub ((rex-answers (("capture" "-b" "b-1" "--trim") 0 (format nil "a~%b~%c~%d"))))
      (is (equal "d" (rex-capture-answer)) "capture takes how many lines"))
    (with-rex-stub ((rex-answers (("window" "inspect" "--json") 0 +window+)
                                  (("window" "rename") 0 "")))
      (cell-entry "nodecode-rex" "rex" "rename Fix Login")
      (is (ran-p (rex-rename "fix-login")) "rename sets the label")
      (cell-entry "nodecode-rex" "rex" "Fix Login Again")
      (is (ran-p (rex-rename "fix-login-again")) "and a bare word does what /title does"))))

;;; --- every refusal -----------------------------------------------------------------

(deftest rex-cell-outside-rex-every-verb-says-what-to-set ()
  (with-cell-stop ((rex-start "session" "" "block" ""))
    (with-rex-stub ((rex-answers))
      (is (search "no Rex session" (rex-refusal #'rex:panes)) "there is no pane to list")
      (is (search "REX_SESSION" (rex-refusal #'rex:run "ls")) "and the refusal says which name is missing")
      (is (search "REX_BLOCK" (rex-refusal #'rex:capture)) "a pane is named the same way")
      (is (search "not in a Rex pane" (cell-entry "nodecode-rex" "rex" "")) "the shell is told the same")
      (is (null *runs*) "and nothing was run"))))

(deftest rex-cell-a-failing-rex-is-one-refusal-naming-what-it-said ()
  (with-cell-stop ((rex-start))
    (with-rex-stub ((rex-answers (("capture") 2 "" "no such pane b-9")))
      (let ((text (rex-refusal #'rex:capture :pane "b-9")))
        (is (search "exited 2" text) "the exit is named")
        (is (search "no such pane b-9" text) "and what rex said of it")))))
