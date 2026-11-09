;;;; cell-test.lisp --- the window list, a capture, a draw and every refusal,
;;;; with every program stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here runs osascript, screencapture, sips or wish: *RUN* answers
;;;; what each one would, writing the two files the capture path really reads.
;;;; What is pinned is what a user gets -- a screenshot file with its own
;;;; pixel size and the window it came from, marks placed on it without the
;;;; model copying a number, a step card -- and what a refused draw never
;;;; costs: a mark off the picture, a window that was resized, a folder the
;;;; section never named.

(in-package #:nodecode.test)

(define-test-slice "hud-teach" "HUD-TEACH-" :start nodecode-hud-teach:start-cell)

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "hud-teach"
  (:config "capture_dir" (ht-capture-dir) "directory" (ht-directory))
  (:help :teach)
  (:command "teach")
  (:running (let ((entry (nle::find-tool "teach_screen")))
              (is-present entry "the tool is registered under the plugin's name"
                (is (equal "teach_screen" (first entry))))
              (is (eq (third entry) 'nodecode-hud-teach::teach-screen) "the tool body is the cell's own function")))
  (:stopped (is (null (nle::find-tool "teach_screen")) "stop takes the tool back out"))
  (:refused ("max_edge" 10)))

;;; --- fixtures -----------------------------------------------------------------

(defvar *ht-frame* '(1050 812)
  "The pixel size a stubbed sips reports, so a draw has a frame to check against.")

(defvar *ht-window* (list 40 60 860 620 "Chess" "New Game")
  "The one window a stubbed osascript lists: x y width height app title.")

(defvar *ht-accessibility* t
  "Whether the stubbed window list answers, or refuses like a Mac without the permission.")

(defvar *ht-asked* '()
  "Every argv the cell ran, newest first.")

(defvar *ht-dirs* '()
  "The two folders a started test made, deleted when it ends.")

(defun ht-directory ()
  "A state folder of this test's own under the scratch home."
  (namestring (nlk:home (format nil "hud-teach-~12,'0x/" (random (expt 16 10))))))

(defun ht-capture-dir ()
  "A screenshot folder of this test's own under the scratch home."
  (namestring (nlk:home (format nil "hud-teach-shots-~12,'0x/" (random (expt 16 10))))))

(defun ht-pairs (&rest pairs)
  "PAIRS over the section a healthy start reads, one value per key."
  (let ((alist (list (cons "capture_dir" (ht-capture-dir))
                      (cons "directory" (ht-directory))
                      (cons "max_edge" 1280))))
    (loop for (key value) on pairs by #'cddr
          do (let ((cell (assoc key alist :test #'string=)))
               (if cell (setf (cdr cell) value) (setf alist (append alist (list (cons key value)))))))
    ;; A key given NIL is left out: absent is what an unset setting is.
    (loop for (key . value) in alist when value append (list key value))))

(defun ht-start (&rest pairs)
  "The cell started on PAIRS, over the defaults a healthy section has."
  (let ((started (apply #'hud-teach-start (apply #'ht-pairs pairs))))
    (setf *ht-dirs* (list (nodecode-hud-teach::state-dir)
                           (ignore-errors (nodecode-hud-teach::capture-dir))))
    started))

(defun ht-cleanup ()
  "Delete what the test made: its two folders, nothing else."
  (dolist (dir (remove nil *ht-dirs*))
    (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))
  (setf *ht-dirs* '()
        nodecode-hud-teach::*tcl-interpreter* nil
        *ht-accessibility* t
        *ht-window* (list 40 60 860 620 "Chess" "New Game")
        *ht-frame* '(1050 812)))

(defmacro with-hud-teach ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS and every program, clock and launcher stubbed."
  `(let ((nodecode-hud-teach::*run* #'ht-run)
         (nodecode-hud-teach::*platform* (lambda () "Darwin"))
         (nodecode-hud-teach::*launch* #'ht-launch)
         (nodecode-hud-teach::*sleep* (lambda (seconds) (declare (ignore seconds)) nil))
         (*ht-asked* '()))
     (with-cell-stop ((apply #'ht-start (list ,@pairs)))
       (nlk:with-cleanup ((ht-cleanup))
         ,@body))))

;;; --- the stubs -------------------------------------------------------------------

(defun ht-write-bytes (path count)
  "COUNT zero bytes at PATH, as screencapture and sips would leave."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :element-type '(unsigned-byte 8))
    (loop repeat count do (write-byte 0 out)))
  path)

(defun ht-windows-json ()
  "The window list the stubbed System Events answers."
  (destructuring-bind (x y width height app title) *ht-window*
    (format nil "[{\"app\": ~s, \"title\": ~s, \"frontmost\": true, \"x\": ~d, \"y\": ~d, \
\"width\": ~d, \"height\": ~d}]" app title x y width height)))

(defun ht-run (argv)
  "Every program the cell runs, answered: the window list, a screenshot file,
the size sips reports, a Tcl version, a kill that always works."
  (push argv *ht-asked*)
  (let ((program (first argv)))
    (cond
      ((string= program "osascript")
       (if *ht-accessibility*
           (values (ht-windows-json) "" 0)
           (values "" "not authorized to send Apple events" 1)))
      ((string= program "screencapture") (ht-write-bytes (car (last argv)) 64) (values "" "" 0))
      ((string= program "sips")
       (if (member "-Z" argv :test #'string=)
           (progn (ht-write-bytes (car (last argv)) 4096) (values "" "" 0))
           (values (format nil "~a~%  pixelWidth: ~d~%  pixelHeight: ~d"
                           (car (last argv)) (first *ht-frame*) (second *ht-frame*))
                   "" 0)))
      ((string= program "wish") (values "8.6.13" "" 0))
      ((string= program "kill") (values "" "" 0))
      (t (values "" "no such program" 127)))))

(defun ht-launch ()
  "Starting the overlay, as a Mac with Tk does: the daemon's first heartbeat."
  (nodecode-hud-teach::write-text (nodecode-hud-teach::heartbeat-file)
                                   (princ-to-string (get-universal-time)))
  :stub-process)

(defun ht-tool (&rest pairs)
  "The tool as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool "teach_screen")))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun ht-scene ()
  "The scene script the cell last wrote."
  (uiop:read-file-string (nodecode-hud-teach::scene-file)))

;;; --- one window, one picture -------------------------------------------------------

(deftest hud-teach-lists-the-windows-it-may-paint-on ()
  (with-hud-teach ()
    (let ((answer (ht-tool "action" "list")))
      (is (search "Chess" answer) "the app is named")
      (is (search "New Game" answer) "with its window title")
      (is (search "860x620" answer) "and where it is on screen"))
    (setf *ht-accessibility* nil)
    (is (search "ERROR:" (ht-tool "action" "list")) "a window list without the permission is a refusal the model can read")))

(deftest hud-teach-capture-answers-the-picture-and-its-geometry ()
  (with-hud-teach ()
    (let* ((answer (ht-tool "action" "capture"))
           (record (nodecode-hud-teach::load-capture)))
      (is (search "Chess" answer) "the capture names the app")
      (is (search "1050x812" answer) "and the picture's own pixel size")
      (is (search "look" answer) "and says to look at it, which is how the model sees")
      (is (equal (getf record :app) "Chess") "the capture is remembered for the next draw")
      (is (equal (getf record :bounds) '(40 60 860 620)) "with the window's own rect")
      (is (probe-file (getf record :file)) "and the JPEG is really written")
      (is (member "screencapture" *ht-asked* :key #'first :test #'string=) "through screencapture")
      (is (member "sips" *ht-asked* :key #'first :test #'string=) "shrunk by sips, as the original did"))))

(deftest hud-teach-capture-refuses-when-the-folder-is-unset ()
  (with-hud-teach ("capture_dir" nil)
    (let ((answer (ht-tool "action" "capture")))
      (is (search "ERROR:" answer) "a section with no capture_dir refuses")
      (is (search "capture_dir" answer) "naming the setting the user must fill")
      (is (null (nodecode-hud-teach::load-capture)) "and nothing was captured"))))

;;; --- the marks -------------------------------------------------------------------

(deftest hud-teach-draws-on-the-last-capture-without-a-number-from-the-model ()
  (with-hud-teach ()
    (ht-tool "action" "capture")
    (let* ((answer (ht-tool "action" "draw"
                             "shapes" (vector (nlk:make-json-object
                                               "kind" "circle" "x" 400 "y" 300 "radius" 40
                                               "step" 2 "color" "yellow" "label" "press here")
                                               (nlk:make-json-object
                                                "kind" "rect" "x" 10 "y" 10
                                                "width" 200 "height" 40))
                             "title" "Export as PNG"
                             "steps" (vector (nlk:make-json-object "text" "Open the File menu" "done" t)
                                             (nlk:make-json-object "text" "Choose Export As"))
                             "current_step" 2))
           (scene (or (ignore-errors (ht-scene)) "")))
      (is (search "drew 2 marks" answer) "both marks are painted")
      (is (search "step 2" answer) "and the card is told which step is now")
      (is (search "create oval" scene) "the circle is an oval in the scene")
      (is (search "create rectangle" scene) "the rect is a rectangle")
      (is (search "Open the File menu" scene) "the lesson is on the card")
      (is (search "860x620+40+60" scene) "the overlay sits on the captured window")
      (is (search "1050" scene) "scaled from the capture's own pixel size")
      (is (search "after 180000" scene) "and clears itself after the default 180 seconds")
      (is (search "press here" scene) "a caption is painted")
      (is (search "Export as PNG" scene) "and the lesson title"))))

(deftest hud-teach-refuses-a-mark-off-the-picture ()
  (with-hud-teach ()
    (ht-tool "action" "capture")
    (let ((answer (ht-tool "action" "draw"
                            "shapes" (vector (nlk:make-json-object
                                              "kind" "circle" "x" 5000 "y" 300 "radius" 40)))))
      (is (search "ERROR:" answer) "a coordinate from another picture is refused")
      (is (search "outside the 1050x812 capture" answer) "naming the frame it missed"))))

(deftest hud-teach-refuses-a-window-that-was-resized ()
  (with-hud-teach ()
    (ht-tool "action" "capture")
    (setf *ht-window* (list 40 60 700 500 "Chess" "New Game"))
    (let ((answer (ht-tool "action" "draw"
                            "shapes" (vector (nlk:make-json-object
                                              "kind" "circle" "x" 400 "y" 300 "radius" 40)))))
      (is (search "resized" answer) "a reflowed window refuses rather than pointing at nothing")
      (is (search "capture again" answer) "and says what to do about it"))))

(deftest hud-teach-follows-a-window-that-moved ()
  (with-hud-teach ()
    (ht-tool "action" "capture")
    (setf *ht-window* (list 120 200 860 620 "Chess" "New Game"))
    (let ((answer (ht-tool "action" "draw"
                            "shapes" (vector (nlk:make-json-object
                                              "kind" "circle" "x" 400 "y" 300 "radius" 40)))))
      (is (search "moved" answer) "the marks follow the window and say so")
      (is (search "860x620+120+200" (ht-scene)) "and are painted on its new rect"))))

(deftest hud-teach-clears-and-reports ()
  (with-hud-teach ()
    (ht-tool "action" "capture")
    (ht-tool "action" "draw"
             "shapes" (vector (nlk:make-json-object "kind" "rect" "x" 10 "y" 10
                                                     "width" 100 "height" 30)))
    (is (search "cleared the marks" (ht-tool "action" "clear")) "clear takes them off")
    (is (search "visible 0" (ht-scene)) "and the scene says the screen is clear")
    (is (search "running" (ht-tool "action" "status")) "status answers the overlay's state")
    (ignore-errors (delete-file (nodecode-hud-teach::heartbeat-file)))
    (is (search "no marks were up" (ht-tool "action" "clear")) "with no daemon, clear says nothing was up rather than starting one")))

;;; --- the first run, and the operator's side -------------------------------------------

(deftest hud-teach-check-names-what-a-first-run-needs ()
  (with-hud-teach ()
    (let ((answer (ht-tool "action" "check")))
      (is (search "ready" answer) "on a Mac with Tk, a window list and a 1x1 capture, it is ready")
      (is (search "screen-recording" answer) "the screen recording probe is reported")
      (is (search "accessibility" answer) "so is accessibility")
      (is (search "tcl" answer) "and the interpreter the overlay will paint with"))
    (setf *ht-accessibility* nil)
    (is (search "fix:" (ht-tool "action" "check")) "a miss carries its one fix")))

(deftest hud-teach-slash-reaches-the-same-actions ()
  (with-hud-teach ()
    (is (search "overlay:" (nodecode-hud-teach::run-slash "" nil)) "bare /teach is status")
    (is (search "ready" (nodecode-hud-teach::run-slash "check" nil)) "/teach check runs the probes")
    (is (search "not one of" (nodecode-hud-teach::run-slash "paint" nil)) "an unknown word says what the words are")
    (let ((answer (nodecode-hud-teach::run-slash "demo" nil)))
      (is (search "demo drawn" answer) "/teach demo captures and paints in one word")
      (is (search "create oval" (ht-scene)) "its sample ring is on screen"))))

(deftest hud-teach-renders-a-tcl-safe-caption ()
  (is (equal (nodecode-hud-teach::tcl-string "a \"b\" $c \\[d\\]")
             "\"a \\\"b\\\" \\$c \\\\\\[d\\\\\\]\"") "a caption cannot end the Tcl string, interpolate a variable or run a command")
  (is (equal (nodecode-hud-teach::tcl-string "plain") "\"plain\"") "a plain caption is one word"))
