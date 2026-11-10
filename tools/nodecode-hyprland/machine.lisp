;;;; machine.lisp --- the CLIs this cell drives, and the one seam a test replaces.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hyprland is Wayland-only: there is no XTEST and no X11 grab, so the
;;;; desktop is reached the way the compositor's own clients reach it --
;;;; hyprctl for window metadata and focus, grim for a frame, wtype for text
;;;; and keys, wl-copy for the paste fallback, and the pointer helper built
;;;; from the original repository's pointer/ folder for clicks, drags and
;;;; wheel. Every call is a fixed argv: no shell is spawned, so a window
;;;; title with a quote in it stays data.
;;;;
;;;; The one seam is *RUN* (and *WHICH* under it): a test answers argv
;;;; itself and no process runs, which is what keeps this cell's tests
;;;; offline and independent of the session they run in.

(in-package #:nodecode-hyprland)

(defvar *run* nil
  "NIL runs ARGV through UIOP; a function of (ARGV &key INPUT) answering
\(values OUTPUT STDERR EXIT) is what a test or a layer substitutes.")

(defvar *which* nil
  "NIL looks a program up on PATH; a function of NAME answering its path or
NIL is what a test substitutes on a machine with no Wayland session.")

(defvar *timeout-seconds* 30
  "How long one command this cell runs may take before it is killed.")

(defvar *frame* nil
  "The last capture's mapping to native pixels: (:x :y :scale :width :height),
and NIL before the first one. Every coordinate the model sends is read in it.")

(defvar *target* nil
  "The window row the last capture or focus selected, or NIL for the screen.")

(defvar *capture-counter* 0
  "A number in a capture's file name, so two in one second do not collide.")

;;; --- processes ---------------------------------------------------------------------

(defun find-program (name)
  "NAME's absolute path when this machine has it, else NIL."
  (if *which*
      (funcall *which* name)
      (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
            when (plusp (length directory))
              do (let ((found (probe-file (merge-pathnames name (uiop:ensure-directory-pathname directory)))))
                   (when found (return (uiop:native-namestring found)))))))

(defun need-program (name &optional (hint ""))
  "NAME's path, or a refusal naming what to install."
  (or (find-program name)
      (fail "~a is not on PATH~@[ -- ~a~]" name (and (plusp (length hint)) hint))))

(defun real-run (argv input)
  "ARGV through UIOP, as (values OUTPUT STDERR EXIT); a failure to start is
an exit of -1 and the reason in stderr, never a condition the caller must catch."
  (handler-case
      (multiple-value-bind (output error exit)
          (uiop:run-program argv
                            :input (and input (make-string-input-stream input))
                            :output :string :error-output :string
                            :ignore-error-status t :timeout *timeout-seconds*)
        (values (or output "") (or error "") (or exit 0)))
    (error (condition) (values "" (princ-to-string condition) -1))))

(defun run (argv &key input)
  "ARGV as (values OUTPUT STDERR EXIT), through *RUN* when a test set it."
  (if *run* (funcall *run* argv :input input) (real-run argv input)))

(defun explain (error output)
  "The one line a refusal carries: what the command said, or what it printed."
  (nlk:trimmed (if (plusp (length error)) error output)))

;;; --- hyprctl ----------------------------------------------------------------------

(defun hyprctl-json (&rest arguments)
  "Hyprctl ARGUMENTS as JSON: a hash table for one object, a vector for a list."
  (multiple-value-bind (output error exit)
      (run (append (list "hyprctl") arguments (list "-j")))
    (unless (zerop exit)
      (fail "hyprctl ~{~a~^ ~} failed: ~a" arguments (explain error output)))
    (handler-case (nlk:decode-json output)
      (error ()
        (fail "hyprctl ~{~a~^ ~} did not answer JSON: ~a" arguments (nlk:trimmed output))))))

(defun hyprctl-text (&rest arguments)
  "Hyprctl ARGUMENTS as text, refused when the compositor answers an error."
  (multiple-value-bind (output error exit) (run (append (list "hyprctl") arguments))
    (let ((text (nlk:trimmed (format nil "~a ~a" output error))))
      (when (or (not (zerop exit)) (search "error:" (string-downcase text)))
        (fail "hyprctl ~{~a~^ ~} refused: ~a" arguments text))
      text)))

;;; --- the desktop's windows ---------------------------------------------------------

(defun json-text (object key)
  "KEY of OBJECT as a string, never NIL."
  (or (nlk:json-value object :string key) ""))

(defun json-number (object key)
  "KEY of OBJECT as an integer, 0 when it has none."
  (let ((value (nlk:json-value object :integer key))) (or value 0)))

(defun json-pair (object key)
  "KEY of OBJECT as two numbers, (0 0) when it has none."
  (let ((array (nlk:json-value object :array key)))
    (if (and array (>= (length array) 2))
        (list (or (aref array 0) 0) (or (aref array 1) 0))
        (list 0 0))))

(defun window-row (client)
  "One CLIENT of hyprctl clients -j as the row this cell carries: a plist."
  (destructuring-bind (x y) (json-pair client "at")
    (destructuring-bind (width height) (json-pair client "size")
      (let ((workspace (nlk:json-value client :object "workspace")))
        (list :address (json-text client "address")
              :app (or (let ((class (json-text client "class")))
                         (if (plusp (length class)) class (json-text client "initialClass")))
                       "")
              :title (json-text client "title")
              :pid (json-number client "pid")
              :x x :y y :width width :height height
              :workspace (and workspace (json-text workspace "name"))
              :focused (eql 0 (json-number client "focusHistoryID")))))))

(defun window-list ()
  "Every window the compositor holds, the focused one first."
  (let ((answer (hyprctl-json "clients")))
    (if (vectorp answer)
        (stable-sort (loop for client across answer collect (window-row client))
                     #'string< :key (lambda (row) (getf row :app)))
        '())))

(defun window-app (row)
  "ROW's class, as a person would name the app."
  (or (and row (getf row :app)) ""))

(defun resolve-window (&key app pid window-id)
  "The window APP, PID or WINDOW-ID names, or NIL; an exact class wins over a
substring of a class and title, so an app called hermes is not another
window's title."
  (let ((windows (window-list)))
    (cond
      (window-id
       (let* ((wanted (string-downcase (nlk:trimmed (princ-to-string window-id))))
              (decimal (ignore-errors (parse-integer wanted))))
         (find-if (lambda (row)
                    (let ((address (string-downcase (getf row :address))))
                      (or (string= wanted address)
                          (and decimal
                               (= decimal (or (ignore-errors
                                               (parse-integer address :start 2 :radix 16))
                                              -1))))))
                  windows)))
      (pid (find-if (lambda (row) (eql (getf row :pid) pid)) windows))
      (app
       (let ((wanted (string-downcase (nlk:trimmed (princ-to-string app)))))
         (and (plusp (length wanted))
              (or (find wanted windows :test #'string= :key (lambda (row) (string-downcase (window-app row))))
                  (find-if (lambda (row)
                             (search wanted (string-downcase
                                              (format nil "~a ~a" (window-app row) (getf row :title)))))
                           windows)))))
      (t nil))))

(defun focus-address (address &key raise)
  "Focus the window at ADDRESS through the compositor's own dispatcher."
  (unless (plusp (length address))
    (fail "that window has no address to focus"))
  (let ((expression (format nil "hl.dispatch(hl.dsp.focus({window=\"address:~a\"}))" address)))
    (handler-case (hyprctl-text "repl" expression)
      (hyprland-error ()
        ;; Hyprland before the Lua REPL answers `hyprctl dispatch' and nothing else.
        (hyprctl-text "dispatch" "focuswindow" (format nil "address:~a" address)))))
  (when raise
    (handler-case
        (hyprctl-text "repl" (format nil "hl.dispatch(hl.dsp.window.bring_to_top({window=\"address:~a\"}))"
                                      address))
      (hyprland-error () (ignore-errors (hyprctl-text "dispatch" "bringactivetotop")))))
  (sleep 0.12))

(defun layout-box ()
  "(X Y WIDTH HEIGHT) of the whole monitor layout, negative origins included."
  (let ((monitors (hyprctl-json "monitors")))
    (unless (and (vectorp monitors) (plusp (length monitors)))
      (fail "hyprctl monitors returned no outputs"))
    (let ((left most-positive-fixnum) (top most-positive-fixnum)
          (right most-negative-fixnum) (bottom most-negative-fixnum))
      (loop for monitor across monitors
            for x = (json-number monitor "x") for y = (json-number monitor "y")
            do (setf left (min left x) top (min top y)
                     right (max right (+ x (json-number monitor "width")))
                     bottom (max bottom (+ y (json-number monitor "height")))))
      (list left top (- right left) (- bottom top)))))

;;; --- a frame ---------------------------------------------------------------------

(defun capture-directory ()
  "Where a capture lands, from the section's capture_dir, which the section
reads as a pathname."
  (let* ((declared (setting :capture-dir))
         (directory (etypecase declared
                      (pathname (uiop:native-namestring declared))
                      (string declared))))
    (uiop:ensure-directory-pathname
     (cond ((zerop (length directory)) (nlk:home "hyprland-captures/"))
           ((char= (char directory 0) #\/) directory)
           (t (nlk:home directory))))))

(defun fresh-path (prefix &optional (type "png"))
  "A new file in the capture directory, named PREFIX plus the second and a counter."
  (let ((directory (capture-directory)))
    (ensure-directories-exist directory)
    (merge-pathnames (format nil "~a-~d-~d.~a" prefix (get-universal-time)
                              (incf *capture-counter*) type)
                     directory)))

(defun head-bytes (path count)
  "The first COUNT bytes of PATH, or NIL when it cannot be read."
  (ignore-errors
    (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
      (let ((buffer (make-array count :element-type '(unsigned-byte 8))))
        (subseq buffer 0 (read-sequence buffer in))))))

(defun png-size (path)
  "(values WIDTH HEIGHT) of the PNG at PATH, or NIL when it is not one."
  (let ((bytes (head-bytes path 24)))
    (when (and bytes (>= (length bytes) 24)
               (equalp (subseq bytes 0 8) #(137 80 78 71 13 10 26 10)))
      (flet ((number (from)
               (+ (* (aref bytes from) 16777216) (* (aref bytes (1+ from)) 65536)
                  (* (aref bytes (+ from 2)) 256) (aref bytes (+ from 3)))))
        (values (number 16) (number 20))))))

(defun grab (path left top width height)
  "Grim's frame of (LEFT TOP WIDTH HEIGHT) written to PATH."
  (multiple-value-bind (output error exit)
      (run (list (need-program "grim" "install grim for Wayland screen capture")
                 "-g" (format nil "~d,~d ~dx~d" left top width height)
                 (uiop:native-namestring path)))
    (unless (and (zerop exit) (probe-file path))
      (fail "grim captured nothing: ~a" (explain error output)))))

(defun ruler-arguments (out-width out-height)
  "The ImageMagick draw arguments that put the coordinate ruler on a frame."
  (let* ((label-step (setting :ruler-step))
         (line-step (max 50 (floor label-step 2)))
         (lines '()))
    (loop for x from line-step below out-width by line-step
          do (push (format nil "line ~d,0 ~d,~d" x x out-height) lines))
    (loop for y from line-step below out-height by line-step
          do (push (format nil "line 0,~d ~d,~d" y out-width y) lines))
    (append (when lines
              (list "-stroke" "rgba(99,102,241,0.10)" "-strokewidth" "1" "-fill" "none"
                    "-draw" (format nil "~{~a~^ ~}" (nreverse lines))))
            (list "-stroke" "none" "-pointsize" "13")
            (loop for x from label-step below out-width by label-step
                  append (list "-fill" "rgba(255,255,255,0.82)"
                               "-draw" (format nil "rectangle ~d,0 ~d,15" x (+ x (1+ (* 8 (length (princ-to-string x))))))
                               "-fill" "#1e1e1e"
                               "-draw" (format nil "text ~d,12 '~d'" (1+ x) x)))
            (loop for y from label-step below out-height by label-step
                  append (list "-fill" "rgba(255,255,255,0.82)"
                               "-draw" (format nil "rectangle 0,~d 34,~d" y (+ y 14))
                               "-fill" "#1e1e1e"
                               "-draw" (format nil "text 2,~d '~d'" (+ y 12) y))))))

(defun render (raw native-width native-height)
  "RAW downscaled to the section's capture width with the ruler drawn on it,
when ImageMagick is there; else the frame as grim wrote it. =>
\(values PATH WIDTH HEIGHT SCALE), SCALE image pixels to native ones."
  (let* ((out-width (min native-width (setting :capture-width)))
         (out-height (max 1 (round (* native-height out-width) native-width)))
         (target (fresh-path "frame")))
    (when (find-program "magick")
      (let ((arguments (append (list (find-program "magick") (uiop:native-namestring raw)
                                     "-resize" (format nil "~dx>" out-width))
                               (when (setting :grid) (ruler-arguments out-width out-height))
                               (list (uiop:native-namestring target)))))
        (multiple-value-bind (output error exit) (run arguments)
          (declare (ignore output error))
          (when (zerop exit)
            (multiple-value-bind (width height) (png-size target)
              (when (and width (plusp width))
                (return-from render
                  (values target width height (/ (float native-width) width)
                          (/ (float native-height) height)))))))))
    (uiop:copy-file raw target)
    (values target native-width native-height 1.0 1.0)))

;;; --- the pointer -------------------------------------------------------------------

(defun pointer-program ()
  "The wlr-virtual-pointer helper: the section's pointer_bin, else one on PATH."
  (let ((configured (nlk:trimmed (or (setting :pointer-bin) ""))))
    (cond ((plusp (length configured))
           (or (probe-file (uiop:parse-unix-namestring (expand-home configured)))
               (fail "pointer_bin names ~a, which is not a file" configured)))
          ((find-program "hypr-vinput"))
          (t (fail "the pointer helper hypr-vinput is missing: build it from the original ~
repository's pointer/ folder (make -C pointer) and name it in pointer_bin")))))

(defun pointer (&rest arguments)
  "The pointer helper called with ARGUMENTS; a non-zero exit is a refusal."
  (multiple-value-bind (output error exit)
      (run (cons (uiop:native-namestring (pointer-program)) arguments))
    (unless (zerop exit)
      (fail "hypr-vinput ~{~a~^ ~} failed: ~a" arguments (explain error output)))
    (nlk:trimmed output)))

;;; --- keyboard ----------------------------------------------------------------------

(defparameter +key-names+
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair '(("enter" . "Return") ("return" . "Return") ("esc" . "Escape")
                    ("escape" . "Escape") ("tab" . "Tab") ("space" . "space")
                    ("backspace" . "BackSpace") ("delete" . "Delete") ("up" . "Up")
                    ("down" . "Down") ("left" . "Left") ("right" . "Right")
                    ("home" . "Home") ("end" . "End") ("pageup" . "Prior")
                    ("pagedown" . "Next")))
      (setf (gethash (car pair) table) (cdr pair)))
    table)
  "The key names a person types, as wtype spells them.")

(defun key-name (name)
  "NAME as wtype takes it; the Windows key is logo there, not super."
  (let ((lower (string-downcase name)))
    (or (gethash lower +key-names+)
        (if (member lower '("super" "meta" "win" "cmd" "command") :test #'string=)
            "logo"
            name))))

(defun press (keys)
  "KEYS, a combo such as ctrl+Return, held through one wtype call."
  (let ((parts (remove "" (mapcar #'nlk:trimmed (uiop:split-string (substitute #\+ #\- keys)
                                                                    :separator "+"))
                       :test #'string=)))
    (when (null parts) (fail "no key to press"))
    (let* ((modifiers (mapcar #'key-name (butlast parts)))
           (key (key-name (car (last parts))))
           (arguments (list (need-program "wtype" "install wtype for key input"))))
      (dolist (modifier modifiers) (setf arguments (append arguments (list "-M" modifier))))
      (setf arguments (append arguments (list "-k" key)))
      (dolist (modifier (reverse modifiers)) (setf arguments (append arguments (list "-m" modifier))))
      (multiple-value-bind (output error exit) (run arguments)
        (unless (zerop exit)
          (fail "wtype refused ~a: ~a" keys (explain error output))))
      (format nil "pressed ~a" (format nil "~{~a~^+~}" parts)))))

(defun paste (text)
  "TEXT through the clipboard: wl-copy, then ctrl+v, for text wtype cannot take."
  (let ((copy (need-program "wl-copy" "install wl-clipboard for the paste fallback")))
    (multiple-value-bind (output error exit) (run (list copy) :input text)
      (unless (zerop exit)
        (fail "wl-copy refused the text: ~a" (explain error output))))
    (sleep 0.05)
    (press "ctrl+v")
    (format nil "pasted ~d characters through the clipboard, which replaced its contents"
            (length text))))

(defun expand-home (path)
  "PATH with a leading ~ taken as this user's home directory."
  (let ((path (nlk:trimmed (or path ""))))
    (cond ((zerop (length path)) path)
          ((and (> (length path) 1) (char= (char path 0) #\~) (char= (char path 1) #\/))
           (format nil "~a~a" (uiop:getenv "HOME") (subseq path 1)))
          (t path))))

(defun wtype-text (text)
  "TEXT into the focused surface; leading dashes or a refusal go through the clipboard."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) text)))
    (cond ((zerop (length text)) "nothing to type")
          ((and (plusp (length trimmed)) (char= (char trimmed 0) #\-)) (paste text))
          (t (multiple-value-bind (output error exit)
                 (run (list (need-program "wtype" "install wtype for text input") text))
               (declare (ignore output error))
               (if (zerop exit)
                   (format nil "typed ~d characters" (length text))
                   (paste text)))))))
