;;;; cell-test.lisp --- the lifecycle; windows and apps; a frame and the coordinate
;;;; mapping; the target refusal; the keys; focus; the section; /hyprland.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own work over a stubbed compositor: that
;;;; the tool answers the actions the original backend did, that a capture of a
;;;; named window makes it the target and keeps the frame's origin and scale, that
;;;; a click is mapped from the frame's pixels back to native ones and refused
;;;; outside the target, that text starting with a dash and a key combination
;;;; become the argv the original helper was given, that a missing required
;;;; setting is reported by the section, and that /hyprland prints lines.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "hyprland"
  (:config "pointer_bin" "hypr-vinput-not-built")
  (:help :hyprland)
  (:command "hyprland")
  (:running (is (= 1600 (nodecode-hyprland::setting :capture-width)) "a capture is 1600 px wide by default")
            (is (eq t (nodecode-hyprland::setting :grid)) "the ruler is on by default")
            (is (eq t (nodecode-hyprland::setting :enforce-target)) "input is held to the target by default")
            (is (equal "screen" (nodecode-hyprland::setting :scope)) "a capture without a window is the screen"))
  (:refused ("capture_width" 100) ("ruler_step" 0))
  (:idle hyprland:hyprland-error (hyprland:status) (hyprland:windows)))

;;; --- the desktop the model reads ------------------------------------------------

(deftest hyprland-cell-windows-are-the-compositors-own-list ()
  (with-stub ()
    (let ((answer (hyprland-tool "{\"action\": \"windows\"}")))
      (is (eq t (answer-value answer :boolean "ok")) "ok is true")
      (is (= 2 (answer-value answer :integer "count")) "two windows")
      (let ((rows (entries answer "windows")))
        (is (equal '("firefox" "kitty") (mapcar (lambda (row) (entry-value row :string "app")) rows)) "each row names its class")
        (is (equal "0x55a1b2c3" (entry-value (first rows) :string "window_id")) "and its address")
        (is (eq t (entry-value (first rows) :boolean "focused")) "the focused one says so")
        (is (equal '(100 50 800 600)
                   (coerce (nlk:json-value (first rows) :array "bounds") 'list)) "and carries the native geometry"))
      (is (search "2 windows" (answer-value answer :string "note")) "the note counts them"))))

(deftest hyprland-cell-apps-group-the-windows ()
  (with-stub ()
    (let ((answer (hyprland-tool "{\"action\": \"apps\"}")))
      (is (= 2 (answer-value answer :integer "count")) "two classes")
      (is (equal '("firefox" "kitty")
                 (mapcar (lambda (row) (entry-value row :string "app"))
                         (entries answer "apps"))) "one row per class, sorted"))))

;;; --- a frame, and where a click lands ------------------------------------------

(deftest hyprland-cell-a-capture-of-a-window-is-the-window-and-the-target ()
  (with-stub ()
    (let ((answer (hyprland-tool "{\"action\": \"capture\", \"app\": \"firefox\"}")))
      (is (eq t (answer-value answer :boolean "ok")))
      (is (= 800 (answer-value answer :integer "width")) "the window's own width")
      (is (= 600 (answer-value answer :integer "height")))
      (let ((path (answer-value answer :string "path")))
        (is (probe-file path) "the frame is a file")
        (is (string= (uiop:native-namestring (uiop:pathname-directory-pathname path))
                     (uiop:native-namestring
                      (uiop:ensure-directory-pathname (nlk:home *capture-dir*)))) "written where the section says"))
      (is (equal "firefox" (entry-value (answer-value answer :object "window") :string "app")) "the answer names the window")
      (is (equal "firefox" (nodecode-hyprland::window-app nodecode-hyprland:*target*)) "and the cell holds it as the target")
      (is (equal '(100 50) (list (getf nodecode-hyprland::*frame* :x)
                                  (getf nodecode-hyprland::*frame* :y))) "with the frame's origin")
      (is (equal '(800 600) (list (getf nodecode-hyprland::*frame* :width)
                                   (getf nodecode-hyprland::*frame* :height))))
      (is (equal '("-g" "100,50 800x600") (subseq (arguments-of "grim") 0 2)) "grim is asked for that window's rectangle, and nothing else"))))

(deftest hyprland-cell-a-click-is-mapped-back-to-native-pixels ()
  (with-stub ()
    (with-hyprland ()
      (hyprland:capture :app "firefox")
      (is (search "clicked (110, 60)" (hyprland:click 10 10)) "the frame's pixel plus its origin")
      (is (equal '("hitat" "110" "60" "1920" "1080" "left") (arguments-of "hypr-vinput")) "the pointer helper gets the native point and the layout extent"))))

(deftest hyprland-cell-a-click-outside-the-target-is-refused-in-words ()
  (with-stub ()
    (with-hyprland ()
      (hyprland:capture :app "firefox")
      (let ((refusal (refusal-text hyprland:hyprland-error (hyprland:click 900 700))))
        (is (search "outside the captured window firefox" refusal) "the refusal names the window")
        (is (search "100,50 800x600" refusal) "and its native box"))
      (is (null (called "hypr-vinput")) "nothing was sent")
      (with-hyprland ("enforce_target" nil)
        (hyprland:capture :app "firefox")
        (is (search "clicked" (hyprland:click 900 700)) "and allowed once enforcement is off")))))

(deftest hyprland-cell-a-drag-and-a-wheel-become-the-helpers-argv ()
  (with-stub ()
    (with-hyprland ()
      (hyprland:capture :app "firefox")
      (is (search "dragged (110, 60) to (210, 160)" (hyprland:drag 10 10 :to-x 110 :to-y 110)))
      (is (equal '("drag" "110" "60" "210" "160" "1920" "1080" "left")
                 (arguments-of "hypr-vinput")))
      (is (equal "scrolled down x3" (hyprland:scroll "down")) "no coordinate scrolls the target")
      (is (equal '("scrollat" "400" "300" "1920" "1080" "3") (arguments-of "hypr-vinput")) "at the middle of the captured window, in frame pixels"))))

;;; --- the keyboard -----------------------------------------------------------------

(deftest hyprland-cell-keys-and-text-are-the-argv-wtype-takes ()
  (with-stub ()
    (with-hyprland ()
      (is (equal "pressed ctrl+Return" (hyprland:press-key "ctrl+Return")))
      (is (equal '("-M" "ctrl" "-k" "Return" "-m" "ctrl") (arguments-of "wtype")) "one modifier held around one key")
      (is (search "typed 5 characters" (hyprland:type-text "hello")))
      (is (equal '("hello") (arguments-of "wtype")) "plain text is one wtype argument")
      (is (search "pasted" (hyprland:type-text "-dashed")) "a leading dash goes through the clipboard")
      (is (equal "-dashed" (cdr (find "wl-copy" *inputs* :key #'car :test #'string=))) "the text is the clipboard's input")
      (is (equal '("-M" "ctrl" "-k" "v" "-m" "ctrl") (arguments-of "wtype")) "and ctrl+v pastes it"))))

(deftest hyprland-cell-super-is-logo-to-wtype ()
  (with-stub ()
    (with-hyprland ()
      (hyprland:press-key "super+q")
      (is (equal '("-M" "logo" "-k" "q" "-m" "logo") (arguments-of "wtype")) "wtype names the Windows key logo"))))

;;; --- focus, and what the machine is missing --------------------------------------

(deftest hyprland-cell-focus-goes-through-the-compositor ()
  (with-stub ()
    (with-hyprland ()
      (is (search "focused firefox" (hyprland:focus "firefox")))
      (is (search "address:0x55a1b2c3" (format nil "~{~a~^ ~}" (arguments-of "hyprctl"))) "the focus expression names the window's address")
      (is (equal "firefox" (nodecode-hyprland::window-app nodecode-hyprland:*target*)) "and the window becomes the input target")
      (is (search "no window matches" (refusal-text hyprland:hyprland-error (hyprland:focus "thunderbird"))) "a window that is not open refuses by name"))))

(deftest hyprland-cell-the-section-reports-the-pointer-helper-it-needs ()
  (is (search "pointer_bin"
              (format nil "~{~a~^; ~}"
                      (nlk:section-problems (nlk:find-section '("hyprland")) nil))) "a cell with no pointer helper is unconfigured, and says which setting")
  (with-stub ()
    (with-hyprland ("pointer_bin" "not-a-pointer-helper")
      (is (search "pointer_bin names"
                  (refusal-text hyprland:hyprland-error (hyprland:click 10 10))) "and a pointer_bin pointing at nothing refuses when it is used"))))

(deftest hyprland-cell-status-and-the-slash-command-print-lines ()
  (with-stub ()
    (with-hyprland ()
      (is (search "windows: 2" (hyprland:status)) "the doctor counts the windows")
      (is (search "[ok] grim: /usr/bin/grim" (hyprland:status)) "and names the programs it finds")
      (is (search "firefox" (cell-entry "nodecode-hyprland" "hyprland" "windows")) "/hyprland windows lists them")
      (is (search "capture:" (cell-entry "nodecode-hyprland" "hyprland" nil)) "/hyprland alone is the doctor"))))
