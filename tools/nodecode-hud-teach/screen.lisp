;;;; screen.lisp --- the window list, one screenshot, and the geometry a draw reuses.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three of macOS's own programs, and nothing else: osascript reads the
;;;; window list through System Events (Accessibility), screencapture takes the
;;;; picture (Screen Recording), sips shrinks it to a long edge of 1280 px as a
;;;; JPEG. No network, no key, no permission beyond those two.
;;;;
;;;; The picture is handed to the model as a file path, the way a browser
;;;; cell hands over a screenshot: it calls (look PATH) to see it. What a
;;;; capture remembers here -- its app, title, bounds and pixel size -- is what
;;;; the next draw places marks on.

(in-package #:nodecode-hud-teach)

;;; --- where the cell keeps its own files -------------------------------------------

(defun state-dir ()
  "The cell's own runtime folder: the overlay script, its heartbeat, the last
capture's geometry. DIR is the section's `directory', the home's hud-teach/
by default."
  (let ((dir (uiop:ensure-directory-pathname
              (namestring (or (setting :directory) (nlk:home "hud-teach/"))))))
    (ensure-directories-exist (merge-pathnames "keep" dir))
    dir))

(defun capture-dir ()
  "Where screenshots are written for the model to look at, refusing when the
section names none: a path the model cannot read is no capture at all."
  (let ((dir (setting :capture-dir)))
    (unless (and dir (plusp (length (namestring dir))))
      (fail "hud-teach has no capture_dir: name the folder screenshots are written to in /settings"))
    (uiop:ensure-directory-pathname (namestring dir))))

;;; --- the window list ---------------------------------------------------------------

(defparameter +list-windows-js+
  "function run() {
  var se = Application(\"System Events\");
  var skip = [\"loginwindow\", \"window server\", \"systemuiserver\", \"notification center\",
              \"control center\", \"screenshot\", \"screencaptureui\", \"dock\",
              \"nodecode teach hud\"];
  var out = [];
  se.processes.whose({backgroundOnly: false})().forEach(function (proc) {
    var app = \"\";
    try { app = String(proc.name()); } catch (e) { return; }
    if (skip.indexOf(app.toLowerCase()) !== -1) return;
    var front = false;
    try { front = Boolean(proc.frontmost()); } catch (e1) { front = false; }
    try {
      proc.windows().forEach(function (win) {
        var title = \"\";
        try { title = String(win.name() || \"\"); } catch (e2) { title = \"\"; }
        if (title === \"Nodecode Teach HUD\") return;
        var pos = win.position();
        var size = win.size();
        if (!pos || !size) return;
        var width = Number(size[0]);
        var height = Number(size[1]);
        if (width < 80 || height < 80) return;
        out.push({app: app, title: title, frontmost: front,
                  x: Number(pos[0]), y: Number(pos[1]),
                  width: width, height: height});
      });
    } catch (e3) {}
  });
  return JSON.stringify(out);
}"
  "The JavaScript the window list runs: every on-screen window of a regular
app, with its app name, title, position and size in screen points. The
overlay's own window and the chrome every Mac has are left out.")

(defparameter +accessibility-hint+
  "Grant Accessibility to the app running Nodecode, in System Settings -> Privacy & Security -> Accessibility."
  "What a missing window list means, and the one fix.")

(defparameter +screen-recording-hint+
  "Grant Screen Recording to the app running Nodecode, in System Settings -> Privacy & Security -> Screen Recording, then restart it."
  "What a failed capture means, and the one fix.")

(defun line-number (text marker)
  "The integer after MARKER's colon on its line of TEXT, or NIL."
  (let ((start (search marker text)))
    (when start
      (let* ((colon (position #\: text :start start))
             (rest (and colon (string-trim '(#\Space #\Tab #\Return)
                                           (subseq text (1+ colon)
                                                   (or (position #\Newline text :start (1+ colon))
                                                       (length text)))))))
        (ignore-errors (parse-integer rest :junk-allowed t))))))

(defun window-row (row)
  "ROW, one decoded JSON object of the window list, as a plist; NIL when it
carries no usable geometry."
  (let ((width (nlk:json-value row :number "width"))
        (height (nlk:json-value row :number "height")))
    (and width height (plusp width) (plusp height)
         (list :app (or (nlk:json-value row :string "app") "")
               :title (or (nlk:json-value row :string "title") "")
               :frontmost (nlk:json-value row :boolean "frontmost")
               :x (truncate (or (nlk:json-value row :number "x") 0))
               :y (truncate (or (nlk:json-value row :number "y") 0))
               :width (truncate width)
               :height (truncate height)))))

(defun decode-windows (text)
  "The window list a script answered, as plists, oldest first."
  (let ((decoded (ignore-errors (nlk:decode-json (string-trim '(#\Space #\Newline #\Return) text)))))
    (if (and decoded (vectorp decoded))
        (loop for row across decoded
              for window = (and (hash-table-p row) (window-row row))
              when window collect window)
        '())))

(defun list-windows ()
  "Every visible window the Accessibility permission lets the cell see."
  (multiple-value-bind (out err status)
      (run (list "osascript" "-l" "JavaScript" "-e" +list-windows-js+))
    (unless (zerop status)
      (fail "could not list windows: ~a ~a" +accessibility-hint+
            (string-trim '(#\Space #\Newline) err)))
    (decode-windows out)))

(defun safe-windows ()
  "LIST-WINDOWS, or nothing and the reason: a capture with no window list
still photographs the main display."
  (handler-case (values (list-windows) nil)
    (hud-teach-error (condition) (values nil (hud-teach-error-detail condition)))))

(defun window-area (window)
  "How much of the screen WINDOW covers, so the biggest of a match wins."
  (* (getf window :width) (getf window :height)))

(defun visible-apps (windows)
  "The app names WINDOWS holds, deduplicated and sorted, for a refusal that
says what *is* on screen."
  (let ((names (remove-duplicates (remove "" (mapcar (lambda (w) (getf w :app)) windows)
                                         :test #'string=)
                                  :test #'string=)))
    (if names (format nil "~{~a~^, ~}" (sort names #'string<))
        "none (Accessibility may be missing)")))

(defun pick-window (windows target)
  "The window TARGET names -- its app exactly, else app or title containing
it -- or the frontmost app's largest, else the largest on screen. A TARGET
that matches nothing refuses rather than teaching on another app."
  (let ((needle (string-downcase (string-trim '(#\Space #\Tab) (or target "")))))
    (when (and (plusp (length needle)) (null windows))
      (fail "no visible window matches ~s; visible apps: ~a" target (visible-apps windows)))
    (if (plusp (length needle))
        (let* ((exact (remove-if-not (lambda (w) (string= needle (string-downcase (getf w :app))))
                                      windows))
               (hits (or exact
                         (remove-if-not (lambda (w)
                                          (or (search needle (string-downcase (getf w :app)))
                                              (search needle (string-downcase (getf w :title)))))
                                        windows))))
          (or (first (sort (copy-list hits) #'> :key #'window-area))
              (fail "no visible window matches ~s; visible apps: ~a" target (visible-apps windows))))
        (let ((front (remove-if-not (lambda (w) (getf w :frontmost)) windows)))
          (first (sort (copy-list (or front windows)) #'> :key #'window-area))))))

(defun find-window (windows app title)
  "The window a capture came from, matched by APP and TITLE: the same title
first, the one window of that app when it is the only one, else NIL."
  (let* ((same-app (remove-if-not (lambda (w) (string= app (getf w :app))) windows))
         (same-title (remove-if-not (lambda (w) (string= title (getf w :title))) same-app)))
    (cond ((plusp (length same-title)) (first (sort (copy-list same-title) #'> :key #'window-area)))
          ((= 1 (length same-app)) (first same-app))
          (t nil))))

;;; --- one screenshot ------------------------------------------------------------------

(defun capture-id ()
  "A short id for one capture, so a draw can say which picture it is about."
  (subseq (nlk:sha256-text (format nil "~a/~a/~a" (funcall *now*) (random 1000000) (gensym)))
          7 15))

(defun file-size (path)
  "PATH's length in bytes, or 0 when it is not there."
  (or (ignore-errors (with-open-file (in path :element-type '(unsigned-byte 8))
                       (file-length in)))
      0))

(defun shoot (region png)
  "Screenshot REGION (x y width height) -- the main display when it is NIL
-- into PNG; refuse with the permission fix when the file does not appear."
  (let ((argv (if region
                  (list "screencapture" "-x" "-R"
                        (format nil "~d,~d,~d,~d" (first region) (second region)
                                (third region) (fourth region))
                        (namestring png))
                  (list "screencapture" "-x" "-m" (namestring png)))))
    (multiple-value-bind (out err status) (run argv)
      (declare (ignore out))
      (unless (and (zerop status) (>= (file-size png) 32))
        (fail "screenshot failed: ~a ~a" +screen-recording-hint+
              (string-trim '(#\Space #\Newline) err))))))

(defun shrink (png jpg)
  "PNG as a JPEG at JPG, long edge at most `max_edge' pixels, and its pixel
size. The picture is only ever as big as a model needs to find a control."
  (multiple-value-bind (out err status)
      (run (list "sips" "-Z" (princ-to-string (setting :max-edge)) "-s" "format" "jpeg"
                 (namestring png) "--out" (namestring jpg)))
    (declare (ignore out))
    (unless (and (zerop status) (probe-file jpg))
      (fail "could not compress the screenshot: ~a" (string-trim '(#\Space #\Newline) err))))
  (ignore-errors (delete-file png))
  (multiple-value-bind (out err status)
      (run (list "sips" "-g" "pixelWidth" "-g" "pixelHeight" (namestring jpg)))
    (declare (ignore err))
    (let ((width (and (zerop status) (line-number out "pixelWidth")))
          (height (and (zerop status) (line-number out "pixelHeight"))))
      (unless (and width height (plusp width) (plusp height))
        (fail "could not read the screenshot's pixel size"))
      (values width height))))

(defun capture-file ()
  "Where the last capture's geometry is kept."
  (merge-pathnames "last-capture.sexp" (state-dir)))

(defun save-capture (record)
  "Keep RECORD for the next draw; a draw without geometry of its own places
its marks on this capture."
  (write-value (capture-file) record))

(defun load-capture ()
  "The last capture's record, or NIL when nothing was captured yet."
  (let ((record (read-value (capture-file))))
    (and (listp record) (getf record :id) record)))

(defun capture-age (record)
  "How many seconds ago RECORD was taken."
  (max 0 (- (funcall *now*) (or (getf record :at) 0))))

(defun capture-window (&optional target)
  "Photograph the window TARGET names, or the frontmost app's largest, and
answer its record: id, app, title, bounds, pixel size, display flag, file."
  (unless (macos-p)
    (fail "teach_screen photographs and draws on macOS only; run action=check for what this system has"))
  (multiple-value-bind (windows window-error) (safe-windows)
    (when (and target (stringp target) (plusp (length (string-trim '(#\Space) target)))
               (null windows) window-error)
      (fail "cannot find ~s without the window list: ~a" target window-error))
    (let* ((chosen (pick-window windows target))
           (id (capture-id))
           (png (merge-pathnames (format nil "shot-~a.png" id) (state-dir)))
           (jpg (merge-pathnames (format nil "hud-teach-~a.jpg" id) (capture-dir))))
      (unwind-protect
           (progn
             (if chosen
                 (progn
                   (shoot (list (getf chosen :x) (getf chosen :y)
                                (getf chosen :width) (getf chosen :height))
                          png))
                 (shoot nil png))
             (multiple-value-bind (width height) (shrink png jpg)
               (save-capture
                (list :id id
                      :app (if chosen (getf chosen :app) "display")
                      :title (if chosen (getf chosen :title) "main display")
                      :bounds (if chosen
                                  (list (getf chosen :x) (getf chosen :y)
                                        (getf chosen :width) (getf chosen :height))
                                  (list 0 0 0 0))
                      :frame-width width :frame-height height
                      :display (not chosen)
                      :window-error window-error
                      :file (namestring jpg)
                      :at (funcall *now*)))))
        (ignore-errors (delete-file png))))))

(defun capture-text (record)
  "RECORD as the model reads it: where the picture is, and what to draw on."
  (format nil "captured ~a~@[ - ~a~] at ~dx~d px, capture ~a.~%The screenshot is ~a: (look ~s) to see it.~%\
Draw with teach_screen action=draw; frame size and bounds default to this capture, \
and shape coordinates are pixels of this image, (0,0) at its top left.~@[~%~a~]"
          (getf record :app)
          (let ((title (getf record :title)))
            (and title (plusp (length title)) (not (string= title "main display")) title))
          (getf record :frame-width) (getf record :frame-height) (getf record :id)
          (getf record :file) (getf record :file)
          (getf record :window-error)))

;;; --- what a first run is missing -------------------------------------------------------

(defun check-permissions ()
  "Probe Accessibility with a window list and Screen Recording with a 1x1
capture, each answer carrying the one fix when it fails."
  (let ((report '()))
    (handler-case
        (push (list :name "accessibility" :ok t
                    :detail (format nil "~d window~:p visible" (length (list-windows))))
              report)
      (hud-teach-error (condition)
        (push (list :name "accessibility" :ok nil :fix +accessibility-hint+
                    :detail (hud-teach-error-detail condition))
              report)))
    (let ((probe (merge-pathnames (format nil "probe-~a.png" (capture-id)) (state-dir))))
      (unwind-protect
           (multiple-value-bind (out err status)
               (run (list "screencapture" "-x" "-R" "0,0,1,1" (namestring probe)))
             (declare (ignore out))
             (if (and (zerop status) (>= (file-size probe) 32))
                 (push (list :name "screen-recording" :ok t
                             :detail "a 1x1 capture came back; a picture of only the wallpaper still means this is off")
                       report)
                 (push (list :name "screen-recording" :ok nil :fix +screen-recording-hint+
                             :detail (string-trim '(#\Space #\Newline) err))
                       report)))
        (ignore-errors (delete-file probe))))
    (nreverse report)))
