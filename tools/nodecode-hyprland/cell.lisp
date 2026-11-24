;;;; cell.lisp --- one desktop, one tool: a frame, a coordinate, a key, a window.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin was a Hermes computer-use backend: Hermes's own
;;;; computer_use tool called capture, click, drag, scroll, type, key and
;;;; focus_app on it, and window metadata came from hyprctl. Here the same
;;;; nine actions are one contract on one tool, so a model that learned the
;;;; plugin's vocabulary finds every verb, and the same nine are Lisp verbs
;;;; for eval -- they read this file's answers, MACHINE.LISP runs the CLIs.
;;;;
;;;; The coordinate contract is the original's: every click is in the space
;;;; of the last capture, and the backend maps it back to native pixels. A
;;;; capture of a named window makes that window the target, and input
;;;; outside it is refused in words instead of landing in the tile next door.

(in-package #:nodecode-hyprland)

;;; --- the call a model makes --------------------------------------------------------

(nlk:define-contract hyprland-action
  (:doc "Drive a Hyprland (Wayland) desktop. windows lists the open windows;
capture takes a PNG of the screen or of one window and returns its path;
click, drag, scroll, type and key act on the focused surface -- call focus
first, or pass bring_to_front -- with x and y in the pixels of the last
capture. Pixel-only: there is no element tree, so act on coordinates read
from the frame.")
  ("action"    :string :required t :options '("windows" "apps" "capture" "click" "drag" "scroll"
                                              "type" "key" "focus")
                :doc "what to do: windows or apps to read the desktop, capture for a
frame, click/drag/scroll/type/key to act on it, focus to raise a window")
  ("app"       :string :doc "a window's class, as windows names it, e.g. firefox; capture
focuses it, crops to it and makes it the target")
  ("pid"       :integer :doc "a window's process id, instead of app")
  ("window_id" :string :doc "a window's id from windows (hex address, or its decimal value)")
  ("x"         :integer :doc "column in the last capture's pixels; without a capture, native pixels")
  ("y"         :integer :doc "row in the last capture's pixels; without a capture, native pixels")
  ("to_x"      :integer :doc "where a drag ends, in the same pixels as x")
  ("to_y"      :integer :doc "where a drag ends, in the same pixels as y")
  ("button"    :string :options '("left" "right" "middle") :default "left" :doc "the pointer button")
  ("count"     :integer :default 1 :slot clicks :doc "clicks to send: 2 is a double click")
  ("direction" :string :options '("up" "down") :doc "which way scroll turns the wheel")
  ("amount"    :integer :default 3 :doc "wheel ticks")
  ("text"      :text :doc "what type types, into the focused surface")
  ("keys"      :text :doc "a combo for key, as Key names it: ctrl+Return, super+q, ctrl+shift+t")
  ("scope"     :string :options '("screen" "window") :doc "capture the whole screen or the focused window")
  ("bring_to_front" :boolean :default nil :doc "focus the target window before typing or clicking"))

(in-package #:nodecode-hyprland)

;;; --- windows as answers ------------------------------------------------------------

(defun window-json (row)
  "ROW as the JSON object a tool answer carries, or NIL."
  (and row
       (nlk:make-json-object
        "window_id" (getf row :address)
        "app" (window-app row)
        "title" (getf row :title)
        "pid" (getf row :pid)
        "bounds" (vector (getf row :x) (getf row :y) (getf row :width) (getf row :height))
        "workspace" (getf row :workspace)
        "focused" (and (getf row :focused) t))))

(defun window-line (row)
  "ROW as the one line a person reads."
  (format nil "~a~@[ \"~a\"~] at ~d,~d ~dx~d, ~a, pid ~d~:[~;, focused~]"
          (window-app row) (getf row :title)
          (getf row :x) (getf row :y) (getf row :width) (getf row :height)
          (or (getf row :workspace) "no workspace") (getf row :pid) (getf row :focused)))

(defun app-rows ()
  "One row per window class: its windows and their pids."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (row (window-list))
      (let ((entry (gethash (window-app row) groups)))
        (unless entry
          (setf entry (setf (gethash (window-app row) groups)
                            (list :app (window-app row) :windows 0 :pids '()))))
        (incf (getf entry :windows))
        (pushnew (getf row :pid) (getf entry :pids))))
    (sort (loop for entry being the hash-values of groups collect entry)
          #'string< :key (lambda (entry) (getf entry :app)))))

;;; --- the frame, and where a coordinate lands ---------------------------------------

(defun to-native (x y)
  "(X Y) in the last capture's pixels as native pixels; the frame's own when
no capture has been taken."
  (if *frame*
      (list (round (+ (getf *frame* :x) (* x (getf *frame* :scale-x))))
            (round (+ (getf *frame* :y) (* y (getf *frame* :scale-y)))))
      (list (round x) (round y))))

(defun outside-note (x y)
  "Why native (X Y) is outside the captured window, or NIL when it is inside."
  (when (and (setting :enforce-target) *target*)
    (let ((left (getf *target* :x)) (top (getf *target* :y))
          (width (getf *target* :width)) (height (getf *target* :height)))
      (unless (and (<= left x) (< x (+ left width)) (<= top y) (< y (+ top height)))
        (format nil "coordinate (~d, ~d) native is outside the captured window ~a at ~d,~d ~dx~d; ~
click inside that window, or capture the app you mean first"
                x y (window-app *target*) left top width height)))))

(defun focus-target ()
  "Focus the captured window before an event, the original's foreground delivery."
  (when *target* (focus-address (getf *target* :address))))

(defun deliver (call)
  "Focus the target first when CALL asks for foreground delivery."
  (when (hyprland-action-bring-to-front call) (focus-target)))

(defun do-capture (call)
  "A frame of the screen or of one window, as the answer's plist."
  (let* ((layout (layout-box))
         (window-id (hyprland-action-window-id call))
         (explicit (or (hyprland-action-app call) (hyprland-action-pid call)
                       (and window-id (plusp (length window-id)))))
         (subject (and explicit (resolve-window :app (hyprland-action-app call)
                                                :pid (hyprland-action-pid call)
                                                :window-id window-id)))
         (scope (string-downcase (or (hyprland-action-scope call) (setting :scope) "screen")))
         (window-scope (and subject (or explicit (string= scope "window"))))
         (region layout)
         (note ""))
    (when window-scope
      (let* ((left (max (getf subject :x) (first layout)))
             (top (max (getf subject :y) (second layout)))
             (right (min (+ (getf subject :x) (getf subject :width)) (+ (first layout) (third layout))))
             (bottom (min (+ (getf subject :y) (getf subject :height)) (+ (second layout) (fourth layout)))))
        (if (and (> (- right left) 40) (> (- bottom top) 40))
            (setf region (list left top (- right left) (- bottom top)))
            (setf note (format nil "that window is too small to crop to; this frame is the whole ~
screen. ")))))
    (destructuring-bind (left top width height) region
      (let* ((raw (fresh-path "screen"))
             (fell-back nil))
        (handler-case (grab raw left top width height)
          (hyprland-error (condition)
            (declare (ignore condition))
            (when window-scope
              (destructuring-bind (sx sy sw sh) layout
                (setf left sx top sy width sw height sh)
                (grab raw sx sy sw sh)
                (setf fell-back t)))))
        (multiple-value-bind (path out-width out-height scale-x scale-y)
            (render raw width height)
          (setf *frame* (list :x left :y top :scale-x scale-x :scale-y scale-y
                              :width out-width :height out-height)
                *target* (unless fell-back subject))
          (list :ok t :action "capture"
                :path (uiop:native-namestring path)
                :width out-width :height out-height
                :window subject
                :note (format nil "~apixel-only frame of ~a at native ~d,~d ~dx~d; click coordinates ~
are this frame's pixels (~dx~d), mapped back by the cell~@[, and the target window is ~a~].~@[ ~a~]"
                              note
                              (if (and window-scope (not fell-back))
                                  (format nil "window ~a" (window-app subject))
                                  "the screen")
                              left top width height out-width out-height
                              (and (not fell-back) subject (window-app subject))
                              (when explicit
                                (unless subject
                                  (format nil "The requested window was not found; this frame is ~
the focused window instead."))))))))))

;;; --- the pointer and the keyboard --------------------------------------------------

(defun do-click (call)
  "CLICK at the call's coordinate, COUNT times."
  (let* ((x (hyprland-action-x call)) (y (hyprland-action-y call)))
    (unless (and x y) (fail "click needs x and y, in the last capture's pixels"))
    (destructuring-bind (native-x native-y) (to-native x y)
      (let* ((outside (outside-note native-x native-y))
             (button (string-downcase (or (hyprland-action-button call) "left")))
             (count (max 1 (or (hyprland-action-clicks call) 1)))
             (layout (layout-box)))
        (when outside (fail "~a" outside))
        (deliver call)
        (dotimes (index count)
          (pointer "hitat" (princ-to-string native-x) (princ-to-string native-y)
                   (princ-to-string (third layout)) (princ-to-string (fourth layout)) button)
          (when (< index (1- count)) (sleep 0.06)))
        (list :ok t :action "click"
              :note (format nil "clicked (~d, ~d) native ~a~@[ x~d~]"
                            native-x native-y button (and (> count 1) count)))))))

(defun do-drag (call)
  "DRAG from the call's coordinate to to_x and to_y."
  (let ((x (hyprland-action-x call)) (y (hyprland-action-y call))
        (to-x (hyprland-action-to-x call)) (to-y (hyprland-action-to-y call)))
    (unless (and x y to-x to-y) (fail "drag needs x, y, to_x and to_y"))
    (destructuring-bind (from-x from-y) (to-native x y)
      (destructuring-bind (to-native-x to-native-y) (to-native to-x to-y)
        (dolist (point (list (list from-x from-y) (list to-native-x to-native-y)))
          (let ((outside (outside-note (first point) (second point))))
            (when outside (fail "~a" outside))))
        (deliver call)
        (let ((layout (layout-box)))
          (pointer "drag" (princ-to-string from-x) (princ-to-string from-y)
                   (princ-to-string to-native-x) (princ-to-string to-native-y)
                   (princ-to-string (third layout)) (princ-to-string (fourth layout))
                   (string-downcase (or (hyprland-action-button call) "left"))))
        (list :ok t :action "drag"
              :note (format nil "dragged (~d, ~d) to (~d, ~d) native"
                            from-x from-y to-native-x to-native-y))))))

(defun do-scroll (call)
  "SCROLL the wheel, at the call's coordinate or in the captured window's middle."
  (let* ((direction (string-downcase (or (hyprland-action-direction call) "down")))
         (ticks (* (max 1 (or (hyprland-action-amount call) 3))
                   (if (string= direction "up") -1 1)))
         (x (hyprland-action-x call)) (y (hyprland-action-y call)))
    (deliver call)
    (cond
      ((and x y)
       (destructuring-bind (native-x native-y) (to-native x y)
         (let ((outside (outside-note native-x native-y)))
           (when outside (fail "~a" outside))
           (let ((layout (layout-box)))
             (pointer "scrollat" (princ-to-string native-x) (princ-to-string native-y)
                      (princ-to-string (third layout)) (princ-to-string (fourth layout))
                      (princ-to-string ticks))))))
      (*target*
       ;; No coordinate: scroll the captured window, not whatever the pointer is over.
       (let ((middle-x (/ (- (+ (getf *target* :x) (/ (getf *target* :width) 2))
                             (if *frame* (getf *frame* :x) 0))
                          (if *frame* (getf *frame* :scale-x) 1)))
             (middle-y (/ (- (+ (getf *target* :y) (/ (getf *target* :height) 2))
                             (if *frame* (getf *frame* :y) 0))
                          (if *frame* (getf *frame* :scale-y) 1)))
             (layout (layout-box)))
         (pointer "scrollat" (princ-to-string (round middle-x)) (princ-to-string (round middle-y))
                  (princ-to-string (third layout)) (princ-to-string (fourth layout))
                  (princ-to-string ticks))))
      (t (pointer "scroll" (princ-to-string ticks))))
    (list :ok t :action "scroll" :note (format nil "scrolled ~a x~d" direction (abs ticks)))))

(defun do-focus (call)
  "FOCUS the window the call names, and make it the input target."
  (let ((row (resolve-window :app (hyprland-action-app call) :pid (hyprland-action-pid call)
                             :window-id (hyprland-action-window-id call))))
    (unless row
      (fail "no window matches ~a" (or (hyprland-action-app call)
                                        (hyprland-action-window-id call)
                                        (hyprland-action-pid call)
                                        "that id")))
    (focus-address (getf row :address))
    (setf *target* row)
    (list :ok t :action "focus" :window row
          :note (format nil "focused ~a" (window-line row)))))

(defun do-windows ()
  (let ((rows (window-list)))
    (list :ok t :action "windows" :count (length rows)
          :windows (map 'vector #'window-json rows)
          :note (if rows (format nil "~d window~:p" (length rows))
                     "no windows: the compositor lists none, which is what an empty workspace is"))))

(defun do-apps ()
  (let ((rows (app-rows)))
    (list :ok t :action "apps" :count (length rows)
          :apps (map 'vector (lambda (row)
                               (nlk:make-json-object
                                "app" (getf row :app) "windows" (getf row :windows)
                                "pids" (coerce (reverse (getf row :pids)) 'vector)))
                     rows)
          :note (format nil "~d application~:p" (length rows)))))

(defun dispatch (call)
  "CALL's action, answered as the plist its JSON is built from."
  (let ((action (string-downcase (hyprland-action-action call))))
    (answer-json
     (cond ((string= action "windows") (do-windows))
           ((string= action "apps") (do-apps))
           ((string= action "capture") (do-capture call))
           ((string= action "click") (do-click call))
           ((string= action "drag") (do-drag call))
           ((string= action "scroll") (do-scroll call))
           ((string= action "focus") (do-focus call))
           ((string= action "type")
            (let ((text (hyprland-action-text call)))
              (unless text (fail "type needs text"))
              (deliver call)
              (list :ok t :action "type" :note (wtype-text text))))
           ((string= action "key")
            (let ((keys (hyprland-action-keys call)))
              (unless keys (fail "key needs keys, as in ctrl+Return"))
              (deliver call)
              (list :ok t :action "key" :note (press keys))))
           (t (fail "~s is not an action: windows, apps, capture, click, drag, scroll, type, ~
key or focus" action))))))

(defun act (call)
  "The hyprland tool's one entry: CALL's action, answered as the JSON string a
model reads, its refusal included."
  (handler-case (nlk:encode-json-object (dispatch call))
    (hyprland-error (condition)
      (nlk:encode-json-object
       (nlk:make-json-object "ok" nil
                              "action" (hyprland-action-action call)
                              "error" (hyprland-error-detail condition))))))

(defun answer-json (result)
  "RESULT, a plist of the answer's fields, as the JSON object the tool returns."
  (apply #'nlk:make-json-object
         (loop for (key value) on result by #'cddr
               append (list (string-downcase (substitute #\_ #\- (symbol-name key)))
                            (if (eq key :window) (window-json value) value)))))

;;; --- the same nine verbs, for eval ------------------------------------------------

(define-verb status ()
  "What this cell would drive, and what the machine has: the doctor the original plugin shipped."
  (let ((checks (list (cons "WAYLAND_DISPLAY" (or (uiop:getenv "WAYLAND_DISPLAY") "missing"))
                      (cons "HYPRLAND_INSTANCE_SIGNATURE"
                            (if (uiop:getenv "HYPRLAND_INSTANCE_SIGNATURE") "set" "missing"))
                      (cons "hyprctl" (or (find-program "hyprctl") "missing"))
                      (cons "grim" (or (find-program "grim") "missing"))
                      (cons "wtype" (or (find-program "wtype") "missing"))
                      (cons "wl-copy" (or (find-program "wl-copy") "optional, missing"))
                      (cons "magick" (or (find-program "magick") "optional, missing"))
                      (cons "pointer_bin" (or (and (plusp (length (nlk:trimmed (or (setting :pointer-bin) ""))))
                                                    (nlk:trimmed (setting :pointer-bin)))
                                               (find-program "hypr-vinput") "missing"))))
        (lines '()))
    (dolist (check checks)
      (push (format nil "  [~a] ~a: ~a" (if (string= (cdr check) "missing") "!!" "ok")
                    (car check) (cdr check))
            lines))
    (push (format nil "  capture: ~d px wide, ruler ~:[off~;on~], scope ~a, target enforced ~:[no~;yes~]"
                  (setting :capture-width) (setting :grid) (setting :scope)
                  (setting :enforce-target))
          lines)
    (let ((count (ignore-errors (length (window-list)))))
      (push (format nil "  windows: ~a" (or count "hyprctl did not answer")) lines))
    (format nil "hyprland computer use~%~{~a~%~}" (nreverse lines))))

(define-verb windows ()
  "Every window the compositor holds: class, title, geometry, workspace, pid."
  (let ((rows (window-list)))
    (if rows
        (format nil "~d window~:p:~%~{~a~%~}" (length rows) (mapcar #'window-line rows))
        "no windows: the compositor lists none, which is what an empty workspace is")))

(define-verb apps ()
  "The window classes on this desktop, with how many windows each holds."
  (let ((rows (app-rows)))
    (if rows
        (format nil "~{~a~%~}"
                (mapcar (lambda (row)
                          (format nil "~a: ~d window~:p, pid ~{~a~^, ~}"
                                  (getf row :app) (getf row :windows) (reverse (getf row :pids))))
                        rows))
        "no applications: no window is open")))

(define-verb capture (&key app pid window-id scope)
  "A frame of the screen or of one window, saved as a PNG; (look PATH) shows it."
  (let ((result (do-capture (make-hyprland-action :action "capture" :app app :pid pid
                                                   :window-id window-id :scope scope))))
    (format nil "~a~%~a" (getf result :path) (getf result :note))))

(define-verb click (x y &key (button "left") (count 1))
  "Click at X and Y, in the last capture's pixels."
  (getf (do-click (make-hyprland-action :action "click" :x x :y y :button button :clicks count))
        :note))

(define-verb drag (x y &key to-x to-y)
  "Drag from X, Y to TO-X, TO-Y: press, motion, release in one helper call."
  (getf (do-drag (make-hyprland-action :action "drag" :x x :y y :to-x to-x :to-y to-y)) :note))

(define-verb scroll (direction &key (amount 3) x y)
  "Turn the wheel DIRECTION (up or down) AMOUNT ticks, at X, Y when given."
  (getf (do-scroll (make-hyprland-action :action "scroll" :direction direction
                                          :amount amount :x x :y y))
        :note))

(define-verb type-text (text)
  "Type TEXT into the focused surface."
  (wtype-text text))

(define-verb press-key (keys)
  "Press a combo as wtype names it: ctrl+Return, super+q."
  (press keys))

(define-verb focus (app)
  "Focus the window whose class is APP, and make it the input target."
  (getf (do-focus (make-hyprland-action :action "focus" :app app)) :note))

;;; --- the manual -------------------------------------------------------------------

(defparameter +primer+
  "Hyprland computer use, pixel-only: the hyprland tool, or the same verbs in
Lisp through eval. Coordinates are the last capture's pixels, mapped back to
native ones by the cell; a capture of a named window makes it the target and
input outside it is refused. Everything is real input on the live session.
  {'action': 'windows'}                the open windows: class, title, bounds, pid
  {'action': 'apps'}                   window classes and how many windows each holds
  {'action': 'capture', 'app': 'firefox'}   a PNG of that window (or the screen), saved to a file
  {'action': 'click', 'x': 640, 'y': 320, 'count': 2}
  {'action': 'drag', 'x': 400, 'y': 300, 'to_x': 900, 'to_y': 700}
  {'action': 'scroll', 'direction': 'down', 'amount': 3}
  {'action': 'type', 'text': 'hello'}    into the focused surface
  {'action': 'key', 'keys': 'ctrl+Return'}
  {'action': 'focus', 'app': 'firefox'}  raise it and make it the target
Eval: (hyprland:capture :app 'firefox') (look PATH) shows the frame;
(hyprland:windows) (hyprland:click 640 320) (hyprland:press-key 'ctrl+Return')
(hyprland:scroll 'down') (hyprland:status) says what this machine is missing.
A capture is an image of the screen going to the model, as any computer-use
backend sends one. Text starting with - is pasted through wl-copy, which
replaces your clipboard and leaves that text in it. Modifier+click and
modifier+drag need /dev/uinput and are not supported: press the key, then
click. /hyprland windows lists the windows."
  "What (help :hyprland) answers while the cell runs.")

;;; --- the slash command ------------------------------------------------------------

(defun slash-hyprland (args session-id)
  "One /hyprland invocation: the doctor, or the window list."
  (declare (ignore session-id))
  (let ((argument (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member argument '("windows" "list" "w") :test #'string=) (windows))
          ((member argument '("apps" "a") :test #'string=) (apps))
          ((member argument '("capture" "screen") :test #'string=) (capture))
          (t (status)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell hyprland
  (:section ("hyprland")
    (:guide "install grim, wtype and wl-clipboard, build the pointer helper from the original ~
repository's pointer/ folder with make, and name the binary in pointer_bin; nothing else is needed")
    ("pointer_bin" :path :required t
                   :doc "the hypr-vinput pointer helper built from the original repository's pointer/ ~
folder; a hypr-vinput on PATH is used when this is empty")
    ("capture_width" :integer :default 1600 :min 320
                      :doc "how wide a capture is, in pixels; click coordinates are read in this space")
    ("grid" :boolean :default t :doc "draw the coordinate ruler on a capture")
    ("ruler_step" :integer :default 200 :min 50 :doc "image pixels between ruler labels")
    ("scope" :choice :options '("screen" "window") :default "screen"
             :doc "what a capture without a named window covers: the whole screen, or the focused window")
    ("enforce_target" :boolean :default t
                       :doc "refuse a click outside the captured window instead of letting it land in the ~
tile next door; turn it off for a drag between two windows")
    ("capture_dir" :directory :default "hyprland-captures/" :doc "where a capture is saved"))
  (:tool "hyprland" (nlk:contract 'hyprland-action) #'act)
  (:help :hyprland
         "hyprland: drive a Hyprland desktop - capture, click, drag, scroll, type, key, focus"
         +primer+)
  (:command "hyprland" 'slash-hyprland
            :catches 'hyprland-error
            :description "Hyprland: the windows on this desktop, the doctor, or a capture"
            :argument-hint "windows|apps|status")
  (:start (lambda ()
            (setf *frame* nil *target* nil)
            (nle:notice (format nil "hyprland: pointer helper ~a"
                                (or (ignore-errors (uiop:native-namestring (pointer-program)))
                                    "missing -- build it and set pointer_bin"))
                        :key "nodecode-hyprland"))))
