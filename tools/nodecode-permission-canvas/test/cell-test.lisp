;;;; cell-test.lisp --- lifecycle; the levels; one approval; what is withheld.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the start installs the three tools, /permission-canvas
;;;; and the manual, and the stop takes all three back; a section with a wrong
;;;; shape refuses; a control's level comes from the most restrictive rule that
;;;; matches and every uncertain case is the restrictive one; an approval is for
;;;; one exact action and is used once; a hidden control is out of the
;;;; observation, its rectangle is masked and its label is never named; a rule
;;;; typed at the shell decides; a selected window withholds every other one
;;;; with fixed text; an unreadable policy and a missing folder each refuse
;;;; naming what to fix.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "permission-canvas"
  (:config "dir" (pc-dir))
  (:help :permission-canvas)
  (:command "permission-canvas")
  (:running
   (is (string= (pc-dir) (namestring (nodecode-permission-canvas::data-directory))) "the section's folder is the one the cell reads")
   (is (= 120 (nodecode-permission-canvas::setting :approval-ttl-seconds)) "two minutes by default"))
  (:refused ("default_level" "sideways")
            ("approval_ttl_seconds" 0)))

(deftest permission-canvas-each-level-decides-as-the-rules-say ()
  (with-cell-stop ((permission-canvas-start "dir" (pc-dir)))
    (pc-clear)
    (pc-write "policy.json" +policy+)
    (let ((green (pc-decide "click" (pc-save-button))))
      (is (equal "allow" (getf green :decision)) "a green rule allows")
      (is (string= "green" (getf green :level)) "at its own level"))
    (let ((blue (pc-decide "click" (pc-search-field))))
      (is (equal "deny" (getf blue :decision)) "a blue rule refuses the operation")
      (is-carrying (text (getf blue :text)) ("read only")))
    (let ((hidden (pc-decide "click" (nodecode-permission-canvas:control
                                       3 :app "notes.exe" :role "StaticText" :label "API key"
                                       :pid 9001 :window-id 31 :bounds '(10 90 200 20)))))
      (is (equal "deny" (getf hidden :decision)) "a black rule refuses")
      (is-carrying (text (getf hidden :text)) ("hidden") (:absent "API key")))
    (let ((unknown (pc-decide "click" (nodecode-permission-canvas:control
                                        4 :app "notes.exe" :role "Button" :label "Sync"
                                        :pid 9001 :window-id 31 :bounds '(10 130 80 24)))))
      (is (equal "approval_required" (getf unknown :decision)) "a control no rule matches asks first")
      (is (stringp (getf unknown :id)) "and carries one id"))
    (let ((unnamed (pc-decide "click" (nodecode-permission-canvas:control
                                        5 :app "notes.exe" :role "Button" :label ""
                                        :pid 9001 :window-id 31 :bounds '(10 170 80 24)))))
      (is (equal "approval_required" (getf unnamed :decision)) "an unnamed control in a restricted window asks first"))
    (let ((elsewhere (pc-decide "click" (nodecode-permission-canvas:control
                                          6 :app "elsewhere.exe" :role "Button" :label "Export"
                                          :pid 9002 :window-id 44 :bounds '(10 10 80 24)))))
      (is (equal "approval_required" (getf elsewhere :decision)) "a green rule only widens one app"))))

(deftest permission-canvas-one-approval-is-used-once-and-only-for-its-action ()
  (with-cell-stop ((permission-canvas-start "dir" (pc-dir)))
    (pc-clear)
    (pc-write "policy.json" +policy+)
    (let* ((asked (pc-decide "click" (nodecode-permission-canvas:control
                                       7 :app "notes.exe" :role "Button" :label "Sync"
                                       :pid 9001 :window-id 31 :bounds '(10 10 80 24))))
           (id (getf asked :id)))
      (is (equal "approval_required" (getf asked :decision)) "the first ask waits for the person")
      (let ((pending (text (cell-entry "nodecode-permission-canvas" "permission-canvas" "pending" nil))))
        (is (search id pending) "the person is shown the pending id")
        (is (search "Sync" pending) "and which control it is for"))
      (let ((approve (text (cell-entry "nodecode-permission-canvas" "permission-canvas"
                                       (format nil "approve ~a" id) nil))))
        (is (search "approved" approve) "the person approves that id"))
      (let ((allowed (pc-decide "click" (nodecode-permission-canvas:control
                                          7 :app "notes.exe" :role "Button" :label "Sync"
                                          :pid 9001 :window-id 31 :bounds '(10 10 80 24)))))
        (is (equal "allow" (getf allowed :decision)) "the identical action is allowed once")
        (is (getf allowed :approved) "and the answer says it was the approval"))
      (let ((again (pc-decide "click" (nodecode-permission-canvas:control
                                        7 :app "notes.exe" :role "Button" :label "Sync"
                                        :pid 9001 :window-id 31 :bounds '(10 10 80 24)))))
        (is (equal "approval_required" (getf again :decision)) "a second identical action asks again")
        (is (not (equal id (getf again :id))) "under a new id"))
      (let ((other (pc-decide "double_click" (nodecode-permission-canvas:control
                                               7 :app "notes.exe" :role "Button" :label "Sync"
                                               :pid 9001 :window-id 31 :bounds '(10 10 80 24)))))
        (is (equal "approval_required" (getf other :decision)) "an approval is one action, not a control")))))

(deftest permission-canvas-an-observation-withholds-hidden-controls-and-masks-them ()
  (with-cell-stop ((permission-canvas-start "dir" (pc-dir)))
    (pc-clear)
    (pc-write "policy.json" +policy+)
    (multiple-value-bind (visible withheld rectangles)
        (nodecode-permission-canvas:observation
         (pc-policy)
         (pc-index '((1 :app "notes.exe" :role "StaticText" :label "API key" :pid 9001 :window-id 31 :bounds (10 10 200 20))
                     (2 :app "notes.exe" :role "Edit" :label "seed-1234" :pid 9001 :window-id 31 :parent 1 :bounds (10 40 200 20))
                     (3 :app "notes.exe" :role "Button" :label "Save" :pid 9001 :window-id 31 :bounds (300 300 60 24))
                     (4 :app "notes.exe" :role "Button" :label "Copy" :pid 9001 :window-id 31 :bounds (20 15 40 20)))) "Notes")
      (is (equal '(3) (mapcar (lambda (element) (getf element :index)) visible)) "only the control clear of the hidden content is visible")
      (is (equal '(1 2 4) (sort (mapcar (lambda (element) (getf element :index)) withheld) #'<)) "the hidden control, its descendant and what overlaps it are withheld")
      (is (equal '((10 10 200 20) (10 40 200 20)) rectangles) "and the rectangles to mask are the hidden control's and its descendant's"))
    (let ((answer (text (execute-wire-call
                         "pc-observe-1" "permission_canvas_observe"
                         "{\"elements\":[{\"index\":1,\"app\":\"notes.exe\",\"role\":\"StaticText\",\"label\":\"API key\",\"pid\":9001,\"window_id\":31,\"bounds\":[10,10,200,20]},{\"index\":3,\"app\":\"notes.exe\",\"role\":\"Button\",\"label\":\"Save\",\"pid\":9001,\"window_id\":31,\"bounds\":[300,300,60,24]}]}"))))
      (is-carrying answer ("1 of 2 controls withheld") ("(10,10,200,20)") ("3 Button \"Save\"")
                    (:absent "API key")))))

(deftest permission-canvas-a-rule-typed-at-the-shell-decides ()
  (with-cell-stop ((permission-canvas-start "dir" (pc-dir)))
    (pc-clear)
    (pc-write "policy.json" +policy+)
    (let ((typed (text (cell-entry "nodecode-permission-canvas" "permission-canvas"
                                   "rule hide role=Button label=\"Delete account\"" nil))))
      (is (search "is black on" typed) "the person's rule is written")
      (is-carrying (text (cell-entry "nodecode-permission-canvas" "permission-canvas" "rules" nil))
                   ("role=\"Button\"") ("Delete account")))
    (let ((element (nodecode-permission-canvas:control
                    9 :app "notes.exe" :role "Button" :label "Delete account"
                    :pid 9001 :window-id 31 :bounds '(10 200 120 24))))
      (is (equal "deny" (getf (pc-decide "click" element) :decision)) "the control that rule names is refused")
      (is (equal "deny" (getf (pc-decide "observe" element) :decision)) "and observation is refused too"))))

(deftest permission-canvas-a-tool-call-and-the-pause ()
  (with-cell-stop ((permission-canvas-start "dir" (pc-dir)))
    (pc-clear)
    (pc-write "policy.json" +policy+)
    (let ((answer (nle:message-content (execute-wire-call "pc-1" "permission_canvas_check"
                                           "{\"action\":\"click\",\"element\":{\"index\":1,\"app\":\"notes.exe\",\"role\":\"Button\",\"label\":\"Save\",\"pid\":9001,\"window_id\":31}}"))))
      (is-carrying answer ("permission-canvas: click on element 1 allowed (green)")))
    (is (search "paused" (text (cell-entry "nodecode-permission-canvas" "permission-canvas" "pause" nil))) "the person pauses the desktop")
    (let ((refused (nle:message-content (execute-wire-call "pc-2" "permission_canvas_check"
                                            "{\"action\":\"click\",\"element\":{\"index\":1,\"app\":\"notes.exe\",\"role\":\"Button\",\"label\":\"Save\",\"pid\":9001,\"window_id\":31}}"))))
      (is (search "paused" refused) "every check is refused while paused"))
    (is (search "running" (text (cell-entry "nodecode-permission-canvas" "permission-canvas" "resume" nil))) "and resume puts it back")
    (let ((answer (nlk:encode-json-object (execute-wire-call "pc-3" "permission_canvas_policy" "{}"))))
      (is-carrying answer ("default yellow") ("r1 green")))))

(deftest permission-canvas-a-selected-window-withholds-the-rest-in-fixed-text ()
  (with-cell-stop ((permission-canvas-start "dir" (pc-dir)))
    (pc-clear)
    (pc-write "policy.json" +policy+)
    (is (search "only pid 9001" (text (cell-entry "nodecode-permission-canvas" "permission-canvas"
                                                  "target 9001 31 notes.exe Notes" nil))) "the person selects one window")
    (let ((outside (pc-decide "click" (nodecode-permission-canvas:control
                                        6 :app "elsewhere.exe" :role "Button" :label "Export"
                                        :pid 9002 :window-id 44 :bounds '(10 10 80 24)))))
      (is (equal "outside_selected_window" (getf outside :code)) "another window is outside the selection")
      (is-carrying (text (getf outside :text)) ("capture withheld") (:absent "elsewhere.exe")))
    (let ((inside (pc-decide "click" (pc-save-button))))
      (is (equal "allow" (getf inside :decision)) "the selected window is decided as its rules say"))))

(deftest permission-canvas-an-unreadable-policy-and-a-missing-folder-refuse ()
  (with-cell-stop ((permission-canvas-start "dir" (pc-dir)))
    (pc-clear)
    (pc-write "policy.json" "{\"default_level\":\"purple\",\"rules\":[]}")
    (let ((condition (signals-error nodecode-permission-canvas:permission-canvas-error
                        (pc-policy))))
      (is condition "a level nobody defined refuses")
      (when condition
        (is-carrying (text (princ-to-string condition)) ("default_level"))))
    (pc-clear)
    (pc-write "policy.json" "{\"default_level\":\"yellow\",\"rules\":[{\"id\":\"a\",\"level\":\"green\",\"label_regex\":\"([\"}]}")
    (let ((condition (signals-error nodecode-permission-canvas:permission-canvas-error
                        (pc-policy))))
      (is condition "a pattern that does not compile refuses")
      (when condition (is-carrying (text (princ-to-string condition)) ("rule a")))))
  (with-cell-stop ((permission-canvas-start))
    (let ((condition (signals-error nodecode-permission-canvas:permission-canvas-error
                        (nodecode-permission-canvas::data-directory))))
      (is condition "with no folder named, the folder refuses")
      (when condition
        (is-carrying (text (princ-to-string condition)) ("dir"))))))
