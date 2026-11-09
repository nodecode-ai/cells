;;;; cell.lisp --- teach_screen: look at a live window, then paint the lesson on it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The flow the original plugin taught, kept whole: capture the window the
;;;; user is working in, let the model look at that picture, and paint
;;;; numbered marks and a step card over the controls on the real screen. The
;;;; model never copies a coordinate by hand: a draw reuses the capture's
;;;; frame size and window bounds, and refuses when the window it came from was
;;;; resized underneath it.

(in-package #:nodecode-hud-teach)

;;; --- the boundary --------------------------------------------------------------

(nlk:define-contract teach-arguments
  (:doc "Look at a live Mac window, then paint numbered marks and a step card on the
real UI. Flow: action=capture (optional target app or window name) -> (look PATH)
the image it answers -> action=draw with shapes in that image's pixel
coordinates; frame size and bounds default to the last capture. For a lesson
pass title, steps and current_step: the card lists them and highlights the one
to do now. After the user does a step and the screen changes, capture again and
draw the next one with earlier steps marked done. A new draw replaces the old
marks. Run action=check when capture or draw fails.")
  ("action" :text :required t :options '("capture" "draw" "clear" "list" "status" "check")
   :doc "capture, draw, clear (take the marks off), list (visible windows), status, or check (what this Mac is missing).")
  ("target" :string :doc "capture: an app or window-title substring (Chess, Safari, Photoshop). Defaults to the frontmost app's largest window.")
  ("shapes" :any :doc "draw: up to 24 marks in the capture's pixels. circle: x, y, radius. rect: x, y, width, height. arrow/line: fromX, fromY, toX, toY (an arrow points at its to). polyline: points. label: x, y, text. Each takes step (1-12), color, label (a short caption) and dashed; a rect takes fill.")
  ("title" :string :doc "draw: the lesson title on the step card, at most 80 characters.")
  ("steps" :any :doc "draw: up to 12 short instructions for the card, in order, each a string or {text, done}.")
  ("current_step" :integer :doc "draw: the step the user should do now; it is highlighted on the card.")
  ("legend_position" :string :doc "draw: where the card sits: auto (the corner the marks leave free), top-left, top-right, bottom-left, bottom-right.")
  ("capture_id" :string :doc "draw: the id the capture answered; a draw refuses when a newer capture replaced it.")
  ("frame_width" :number :doc "draw: override the capture's pixel width (normally omitted).")
  ("frame_height" :number :doc "draw: override the capture's pixel height (normally omitted).")
  ("bounds" :any :doc "draw: override the window rect in screen points, x y width height (normally omitted).")
  ("ttl_seconds" :number :doc "draw: how long the marks stay up: 180 by default, 900 at most."))

;;; --- the answers --------------------------------------------------------------

(defun window-text (windows)
  "WINDOWS as the lines a model reads."
  (if (null windows)
      "no visible window (Accessibility may be missing; run action=check)"
      (format nil "~d window~:p visible:~{~%~a~}"
              (length windows)
              (mapcar (lambda (window)
                        (format nil "  ~a~@[ - ~a~]  (~d,~d ~dx~d)~:[~;  frontmost~]"
                                (getf window :app)
                                (let ((title (getf window :title)))
                                  (and title (plusp (length title)) title))
                                (getf window :x) (getf window :y)
                                (getf window :width) (getf window :height)
                                (getf window :frontmost)))
                      windows))))

(defun short-title (title)
  "TITLE when it says something, else NIL."
  (and title (plusp (length title)) (not (string= title "main display")) title))

(defun status-text ()
  "The overlay's state, then the capture a draw would use."
  (let ((status (overlay-status))
        (last (load-capture)))
    (format nil "overlay: ~:[not running~;running, ~:[no marks up~;marks up~]~]~@[, heartbeat ~ds ago~]~%~
last capture: ~:[none~;~a~@[ - ~a~], ~ds old, ~a~]"
            (getf status :running) (getf status :visible) (getf status :age)
            last (and last (getf last :app)) (and last (short-title (getf last :title)))
            (and last (capture-age last)) (and last (getf last :file)))))

(defun check-text ()
  "What a first run needs, each line with the one fix for a miss."
  (let* ((entries
           (append
            (list (list :name "macos" :ok (macos-p)
                        :detail (string-downcase (funcall *platform*))
                        :fix (unless (macos-p)
                               "teach_screen photographs and draws on macOS only")))
            (handler-case
                (list (list :name "tcl" :ok t
                            :detail (format nil "~a answers Tcl/Tk" (tcl-interpreter))))
              (hud-teach-error (condition)
                (list (list :name "tcl" :ok nil
                            :detail (hud-teach-error-detail condition)
                            :fix "install Tk, or name its wish in the cell's tcl setting"))))
            (when (macos-p) (check-permissions))
            (list (list :name "capture_dir"
                        :ok (and (setting :capture-dir) t)
                        :detail (namestring (or (setting :capture-dir) (state-dir)))
                        :fix (unless (setting :capture-dir)
                               "name the folder screenshots are written to in /settings")))))
         (ready (every (lambda (entry) (getf entry :ok)) entries)))
    (format nil "~:[not ready~;ready~]~{~%~a ~a~@[ - ~a~]~@[ -- fix: ~a~]~}"
            ready
            (loop for entry in entries
                  append (list (if (getf entry :ok) "ok  " "miss")
                               (getf entry :name)
                               (getf entry :detail)
                               (getf entry :fix))))))

;;; --- draw ------------------------------------------------------------------

(defun bounds-plist (bounds)
  "BOUNDS, a decoded JSON object, as (x y width height), or NIL when it
carries no positive size."
  (when (hash-table-p bounds)
    (let ((width (nlk:json-value bounds :number "width"))
          (height (nlk:json-value bounds :number "height")))
      (when (and width height (plusp width) (plusp height))
        (list (truncate (or (nlk:json-value bounds :number "x") 0))
              (truncate (or (nlk:json-value bounds :number "y") 0))
              (truncate width) (truncate height))))))

(defun resolve-geometry (capture-id frame-width frame-height bounds)
  "The frame size and window rect a draw paints on: the capture's, unless the
call overrode them. => (values FRAME-WIDTH FRAME-HEIGHT BOUNDS FROM-CAPTURE APP TITLE)"
  (let* ((last (load-capture))
         (explicit (or bounds frame-width frame-height)))
    (when (and capture-id (or (null last) (not (equal capture-id (getf last :id)))))
      (fail "capture_id ~a is not the latest capture: capture again and draw on what the new screenshot shows"
            capture-id))
    (unless (or last explicit)
      (fail "nothing captured yet: call teach_screen action=capture first, then draw"))
    (let ((width (or frame-width (getf last :frame-width)))
          (height (or frame-height (getf last :frame-height)))
          (rect (or (bounds-plist bounds) (and (null bounds) (getf last :bounds)))))
      (unless rect
        (fail "draw needs bounds from the capture you just took (x, y, width, height)"))
      (unless (and width height (plusp width) (plusp height))
        (fail "draw needs frame_width and frame_height from the screenshot just captured"))
      (values width height rect
              (and (null bounds) (not (null last)))
              (and (null bounds) (getf last :app))
              (and (null bounds) (getf last :title))))))

(defun follow-window (rect from-capture app title)
  "Where the captured window is now: it may have moved, and the marks follow
it; a window that changed size reflowed, so the old coordinates point at
nothing. => (values BOUNDS NOTES), or a refusal."
  (unless (and from-capture app (plusp (length app)) (not (string= app "display")))
    (return-from follow-window (values rect nil)))
  (multiple-value-bind (windows error) (safe-windows)
    (when (or (null windows) error)
      (return-from follow-window
        (values rect (list "could not re-check the window's position; drawing where it was captured"))))
    (let ((now (find-window windows app (or title ""))))
      (unless now
        (return-from follow-window
          (values rect (list (format nil "~a was not found on screen; drawing where it was captured" app)))))
      (when (or (> (abs (- (getf now :width) (third rect))) +resize-tolerance+)
                (> (abs (- (getf now :height) (fourth rect))) +resize-tolerance+))
        (fail "~a was resized since the capture (~dx~d -> ~dx~d), so the marks would miss: capture again"
              app (third rect) (fourth rect) (getf now :width) (getf now :height)))
      (values (list (getf now :x) (getf now :y) (getf now :width) (getf now :height))
              (when (or (/= (getf now :x) (first rect)) (/= (getf now :y) (second rect)))
                (list (format nil "~a moved since the capture; the marks follow it" app)))))))

(defun draw-answer (drawn app title notes)
  "What a draw reports: how many marks, where, and what to know about them."
  (format nil "drew ~d mark~:p~@[ over ~a~@[ - ~a~]~] on the screen, up for ~ds~@[; card: ~a~]~@[; ~{~a~^; ~}~]. \
A new draw replaces them; action=clear takes them off."
          (getf drawn :drawn) app (short-title title)
          (getf drawn :ttl)
          (and (getf drawn :steps) (plusp (getf drawn :steps))
               (format nil "~d step~:p, step ~@[~d ~]now" (getf drawn :steps) (getf drawn :current-step)))
          notes))

(defun draw-marks (arguments)
  "The draw action: validate, place, paint. => the answer's text."
  (let* ((shapes (shape-list (teach-arguments-shapes arguments)))
         (steps (normalized-steps (teach-arguments-steps arguments)))
         (raw-title (teach-arguments-title arguments))
         (title (and raw-title (plusp (length (string-trim '(#\Space) raw-title)))
                     (string-trim '(#\Space) raw-title)))
         (current (teach-arguments-current-step arguments))
         (legend (or (teach-arguments-legend-position arguments) "auto"))
         (ttl (or (teach-arguments-ttl-seconds arguments) +default-ttl+))
         (capture-id (teach-arguments-capture-id arguments)))
    (let ((problem (validate-lesson title steps current legend)))
      (when problem (fail "~a" problem)))
    (unless (and (numberp ttl) (plusp ttl) (<= ttl +max-ttl+))
      (fail "ttl_seconds must be between 0 and ~d." +max-ttl+))
    (multiple-value-bind (width height rect from-capture app title-of)
        (resolve-geometry capture-id (teach-arguments-frame-width arguments)
                          (teach-arguments-frame-height arguments)
                          (teach-arguments-bounds arguments))
      (let ((problem (validate-draw-payload shapes width height)))
        (when problem (fail "~a" problem)))
      (multiple-value-bind (bounds notes)
          (follow-window rect from-capture app title-of)
        (let* ((last (load-capture))
               (age (and last from-capture (capture-age last)))
               (corner (pick-legend-corner shapes width height legend))
               (drawn (draw bounds width height shapes title steps current corner ttl)))
          (draw-answer drawn (and from-capture app) title
                        (append notes
                                (when (and age (> age +stale-after+))
                                  (list (format nil "the capture is ~d minutes old; capture again if the screen changed"
                                                (floor age 60)))))))))))

;;; --- the actions --------------------------------------------------------------

(defun act (arguments)
  "One teach_screen call, whatever it asks for."
  (let ((action (string-downcase (teach-arguments-action arguments))))
    (cond
      ((string= action "check") (check-text))
      ((string= action "status") (status-text))
      ((string= action "list")
       (unless (macos-p)
         (fail "the window list is macOS's; run action=check for what this system has"))
       (window-text (list-windows)))
      ((string= action "capture")
       (capture-text (capture-window (teach-arguments-target arguments))))
      ((string= action "clear")
       (if (clear-overlay "clear")
           "cleared the marks from the screen"
           "no marks were up: the overlay was not running"))
      ((string= action "draw") (draw-marks arguments))
      (t (fail "action ~a is not one of capture, draw, clear, list, status, check" action)))))

(defun teach-screen (arguments)
  "The tool body: a refusal answers text the model can act on, because a
missing permission is the ordinary first answer on a Mac."
  (handler-case (act arguments)
    (hud-teach-error (condition)
      (format nil "ERROR: ~a" (hud-teach-error-detail condition)))))

;;; --- /teach -----------------------------------------------------------------

(defun demo (&optional target)
  "A sample lesson on the window TARGET names (the frontmost by default): the
window's outline and a ring in the middle, so a first run proves the whole
path from capture to paint."
  (let* ((record (capture-window target))
         (width (getf record :frame-width))
         (height (getf record :frame-height))
         (shapes (list (nlk:make-json-object
                        "kind" "rect" "x" 4 "y" 4
                        "width" (- width 8) "height" (- height 8)
                        "color" "green" "label" "this is the window")
                       (nlk:make-json-object
                        "kind" "circle" "x" (round width 2) "y" (round height 2)
                        "radius" (min 90 (round (min width height) 4))
                        "color" "yellow" "label" "a sample mark"))))
    (draw (getf record :bounds) width height shapes "HUD Teach demo"
          (normalized-steps '("the window is captured")) 1
          (pick-legend-corner shapes width height "auto") +default-ttl+)
    (format nil "demo drawn over ~a; the screenshot is ~a: (look ~s) to see what the model would"
            (getf record :app) (getf record :file) (getf record :file))))

(defun run-slash (args session-id)
  "/teach check, demo [app], status, clear or stop: the actions the tool has,
from the operator's side. => the answer's text."
  (declare (ignore session-id))
  (let* ((text (string-trim '(#\Space #\Tab) (or args "")))
         (space (position #\Space text))
         (head (string-downcase (subseq text 0 space)))
         (rest (and space (string-trim '(#\Space #\Tab) (subseq text space)))))
    (handler-case
        (cond ((or (zerop (length text)) (string= head "status")) (status-text))
              ((string= head "check") (check-text))
              ((string= head "clear") (if (clear-overlay "clear")
                                          "cleared the marks from the screen"
                                          "no marks were up: the overlay was not running"))
              ((string= head "stop") (if (stop-overlay) "overlay stopped" "the overlay was not running"))
              ((string= head "demo")
               (unless (macos-p) (fail "the demo paints on macOS only; /teach check says what is missing"))
               (demo rest))
              (t (fail "~s is not one of check, demo, status, clear, stop" text)))
      (hud-teach-error (condition)
        (format nil "ERROR: ~a" (hud-teach-error-detail condition))))))

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "HUD Teach paints on the real Mac window the user is working in: numbered
marks on the controls and a step card with the lesson. One tool, teach_screen,
and /teach.
  action=capture [target]  photograph the window (an app or title substring;
      the frontmost app's largest by default). It answers the screenshot's path
      and pixel size: call (look PATH) to see it. A capture also records the
      window's bounds and frame size, so a draw needs no numbers.
  action=draw shapes ...  paint up to 24 marks in the capture's pixels (circle
      x y radius; rect x y width height; arrow or line fromX fromY toX toY;
      polyline points; label x y text), each with step 1-12, a colour, an
      optional caption and dashed; a rect takes fill. For a lesson pass title,
      steps (text, or {text, done}) and current_step: the card lists them and
      highlights the one to do now.
  action=clear takes the marks off; action=list answers the visible windows
  with their position; action=status the overlay and the last capture;
  action=check every permission and program a first run needs, with its fix.
The loop: capture, look, draw the next step only; when the user does it,
capture again and draw with the earlier steps marked done. A new draw replaces
the old marks, they stay up to ttl_seconds (180 by default, 900 at most), and
a click on them takes them off. The user keeps the mouse: a mark shows where a
control is, it does not press it. Screen Recording and Accessibility are
needed; capture and draw are macOS only."
  "What (help :teach) answers while the cell runs.")

;;; --- the declaration ----------------------------------------------------------

(nle:define-cell hud-teach
  (:section ("hud-teach")
    (:guide "capture_dir is the one required setting: the folder screenshots are written to, which the model reads with (look); everything else has a working default. A first run wants Screen Recording and Accessibility for the app running Nodecode, which /teach check reports")
    ("capture_dir" :path :required t
                   :doc "the folder screenshots are written to, for the model to look at")
    ("directory" :path
                 :doc "where the cell keeps its own files: the overlay script, its heartbeat, the last capture. Defaults to a folder of the home")
    ("tcl" :string :default "wish"
           :doc "the Tcl/Tk interpreter the overlay runs on; wish, wish8.6 and wish8.5 are tried when it is absent")
    ("max_edge" :integer :default 1280 :min 320
                :doc "the longest edge of a capture, in pixels: smaller is cheaper for the model and still enough to find a control")
    ("ttl_seconds" :integer :default 180 :min 1
                   :doc "seconds marks stay up when a draw names no time")
    ("idle_exit_seconds" :integer :default 1800 :min 0
                          :doc "seconds the overlay waits with nothing on screen before it quits; the next draw starts it again"))
  (:tool "teach_screen" (nlk:contract 'teach-arguments) 'teach-screen)
  (:help :teach "teach_screen and /teach: numbered marks and a step card on the live Mac window, so a lesson is shown on the app itself" +primer+)
  (:command "teach" 'run-slash
            :description "HUD Teach: check the setup, run a demo, show status, or clear the marks"
            :argument-hint "<check|demo [app]|status|clear|stop>")
  (:start (lambda ()
            (setf *tcl-interpreter* nil)
            (ignore-errors (delete-file (heartbeat-file)))
            (ignore-errors (delete-file (revision-file)))
            (nle:on-stop (lambda ()
                           (ignore-errors (stop-overlay))
                           (setf *tcl-interpreter* nil)
                           (ignore-errors (delete-file (heartbeat-file)))
                           (ignore-errors (delete-file (pid-file))))))))
