;;;; cell-test.lisp --- the lifecycle, the three tools, /klipper, the watch's diff.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Offline: NLK:HTTP is stubbed with a Moonraker that answers the objects
;;;; query, the file metadata, the move and the webcam list from canned
;;;; answers, so the run asserts on what the cell asked and what it made of
;;;; the answer. Nothing dials.

(in-package #:nodecode.test)

(define-test-slice "klipper" "KLIPPER-CELL-" :start nodecode-klipper:start-cell)

;;; --- a Moonraker that is not there ------------------------------------------------

(defvar *requests* nil
  "Every exchange the cell asked for, newest first: (METHOD URL CONTENT).")

(defvar *moonraker* nil
  "A function (METHOD URL CONTENT) => (values OCTETS STATUS HEADERS TRUNCATED).")

(defun moonraker-stub (method url &rest arguments)
  (push (list method url (getf arguments :content)) *requests*)
  (funcall *moonraker* method url (getf arguments :content)))

(defun octet-answer (object &optional (status 200) (truncated nil))
  (values (sb-ext:string-to-octets (if (stringp object) object (nlk:encode-json-object object)))
          status nil truncated))

(defun objects-answer (&key (state "printing") (filename "part.gcode")
                             (progress 0.5) (klippy "ready"))
  (octet-answer
   (nlk:make-json-object
    "result"
    (nlk:make-json-object
     "status"
     (nlk:make-json-object
      "print_stats" (nlk:make-json-object "state" state "filename" filename "message" "")
      "virtual_sdcard" (nlk:make-json-object "progress" progress "file_position" 120.0
                                               "file_size" 900.0)
      "display_status" (nlk:make-json-object "progress" progress)
      "extruder" (nlk:make-json-object "temperature" 210.5 "target" 210.0)
      "heater_bed" (nlk:make-json-object "temperature" 60.0 "target" 60.0)
      "webhooks" (nlk:make-json-object "state" klippy "state_message" ""))))))

(defun state-answer (state)
  (octet-answer
   (nlk:make-json-object
    "result" (nlk:make-json-object
              "status" (nlk:make-json-object
                        "print_stats" (nlk:make-json-object "state" state))))))

(defmacro with-klipper ((&rest pairs) &body body)
  "BODY over a stubbed Moonraker and the cell started on PAIRS, stopped on unwind."
  `(let ((*requests* '()) (*moonraker* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest arguments)
                                 (apply #'moonraker-stub method url arguments))
       (with-cell-stop ((klipper-start ,@pairs))
         ,@body))))

(defun call-tool (name &optional (arguments "{}"))
  "NAME called as a model calls it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun asked () (mapcar #'second *requests*))

(defun asked-p (needle) (find needle (asked) :test #'search))

(defun run-command (name &optional (argument ""))
  "NAME driven the way the shell drives a slash command; => its text."
  (let ((entry (nle::find-registered-command name)))
    (and entry (funcall (nle::slash-command-headless entry) argument "klipper-session"))))

;;; --- the lifecycle, the manual, the refusals ------------------------------------------

(define-cell-lifecycle-tests "klipper"
  (:config "url" "http://printer.test:7125")
  (:help :klipper)
  (:command "klipper")
  (:running (is (zerop (nodecode-klipper::setting :watch-minutes)) "no watch by default"))
  (:refused ("stall_minutes" 0) ("watch_minutes" -5))
  (:idle nodecode-klipper::klipper-error
         (nodecode-klipper::run-klipper "status" nil)
         (nodecode-klipper::watch-once)))

;;; --- reading the printer ------------------------------------------------------------

(deftest klipper-cell-status-answers-with-progress-temperatures-and-the-estimate ()
  (with-klipper ("url" "http://printer.test:7125")
    (setf *moonraker*
          (lambda (method url content)
            (declare (ignore method content))
            (cond ((search "/server/files/metadata" url)
                   (octet-answer (nlk:make-json-object
                                   "result" (nlk:make-json-object "estimated_time" 3600.0))))
                  ((search "/printer/objects/query" url) (objects-answer))
                  (t (octet-answer "{}" 404)))))
    (let ((text (call-tool "klipper_status")))
      (is-carrying text
        ("\"state\":\"printing\"" "the print state")
        ("\"progress\":0.500" "the fraction, as a plain number")
        ("210.5" "the hotend temperature")
        ("1800" "the slicer estimate times what is left")
        ("ready")
        (:absent "ERROR"))
      (let ((query (find "/printer/objects/query" *requests* :key #'second :test #'search)))
        (is (search "\"print_stats\":null" (third query)) "the object query asked for the block")))))

(deftest klipper-cell-a-stopped-klipper-is-named-as-such ()
  (with-klipper ("url" "http://printer.test:7125")
    (setf *moonraker*
          (lambda (method url content)
            (declare (ignore method content))
            (if (search "/server/files/metadata" url)
                (octet-answer (nlk:make-json-object "result" (nlk:make-json-object)))
                (objects-answer :state "error" :klippy "shutdown"))))
    (let ((text (call-tool "klipper_status")))
      (is-carrying text
        ("Klipper has stopped" "the printer's own state, not the print's")
        ("not reliable until it restarts")))))

(deftest klipper-cell-a-missing-url-answers-with-what-to-do ()
  (with-klipper ("url" "")
    (let ((text (call-tool "klipper_status")))
      (is-carrying text ("ERROR missing_url" "set it"))
      (is (null *requests*) "nothing was dialled"))))

;;; --- moving the printer, twice -------------------------------------------------------

(deftest klipper-cell-control-previews-and-then-confirms ()
  (with-klipper ("url" "http://printer.test:7125")
    (let ((state "printing"))
      (setf *moonraker*
            (lambda (method url content)
              (declare (ignore method content))
              (cond ((search "/printer/print/pause" url)
                     (setf state "paused")
                     (octet-answer (nlk:make-json-object "result" "ok")))
                    ((search "/printer/objects/query" url) (state-answer state))
                    (t (octet-answer "{}" 404)))))
      (let ((preview (call-tool "klipper_control" "{\"action\": \"pause\"}")))
        (is-carrying preview
          ("Nothing was sent" "the preview sends nothing")
          ("printing" "and says what the state is now")))
      (is (null (asked-p "/printer/print/pause")) "no POST on the preview")
      (let ((confirmed (call-tool "klipper_control"
                                  "{\"action\": \"pause\", \"confirm\": true}")))
        (is-carrying confirmed
          ("\"moved\":true" "the move is confirmed by a second read")
          ("printing to paused")))
      (is (asked-p "/printer/print/pause") "and only then was it posted"))))

(deftest klipper-cell-control-sends-nothing-when-the-state-is-already-there ()
  (with-klipper ("url" "http://printer.test:7125")
    (setf *moonraker*
          (lambda (method url content)
            (declare (ignore method content))
            (if (search "/printer/objects/query" url)
                (state-answer "paused")
                (octet-answer "{}" 404))))
    (let ((text (call-tool "klipper_control"
                            "{\"action\": \"pause\", \"confirm\": true}")))
      (is-carrying text ("already paused" "nothing was sent")))
    (is (null (asked-p "/printer/print/pause")))))

;;; --- the watch ----------------------------------------------------------------------

(deftest klipper-cell-watch-records-a-baseline-and-then-reports-the-finish ()
  (with-klipper ("url" "http://printer.test:7125")
    (setf *moonraker*
          (lambda (method url content)
            (declare (ignore method content))
            (if (search "/printer/objects/query" url)
                (objects-answer :state "printing" :progress 0.5)
                (octet-answer "{}" 404))))
    (let ((first (call-tool "klipper_watch")))
      (is-carrying first ("No earlier sample" "the first check only records")))
    (setf *moonraker*
          (lambda (method url content)
            (declare (ignore method content))
            (if (search "/printer/objects/query" url)
                (objects-answer :state "complete" :progress 1.0)
                (octet-answer "{}" 404))))
    (let ((finished (call-tool "klipper_watch")))
      (is-carrying finished
        ("Print finished: part.gcode" "the change is the event")
        ("\"notify\":true")))
    (let ((repeat (call-tool "klipper_watch")))
      (is-carrying repeat ("No new print event" "and a repeat is silent")))))

;;; --- the operator's reading ----------------------------------------------------------

(deftest klipper-cell-the-command-reads-and-never-moves ()
  (with-klipper ("url" "http://printer.test:7125")
    (setf *moonraker*
          (lambda (method url content)
            (declare (ignore method content))
            (if (search "/server/files/metadata" url)
                (octet-answer (nlk:make-json-object "result" (nlk:make-json-object)))
                (objects-answer))))
    (let ((status (run-command "klipper" "status")))
      (is-carrying status
        ("Printer state printing")
        ("Extruder 210.5/210.0 C")))
    (let ((pause (run-command "klipper" "pause")))
      (is-carrying pause ("was not sent" "klipper_control")))
    (is (null (asked-p "/printer/print/")) "the command never posted a move")))

;;; --- one webcam still ----------------------------------------------------------------

(deftest klipper-cell-a-snapshot-is-saved-inside-the-cells-own-folder ()
  (with-klipper ("url" "http://printer.test:7125")
    (setf *moonraker*
          (lambda (method url content)
            (declare (ignore method content))
            (cond ((search "/server/webcams/list" url)
                   (octet-answer (nlk:make-json-object
                                   "result" (nlk:make-json-object
                                             "webcams" (vector (nlk:make-json-object
                                                                 "snapshot_url" "/webcam/shot.jpg"))))))
                  ((search "/webcam/shot.jpg" url)
                   (values (coerce #(255 216 255 224 1 2 3 4) 'nlk:octets) 200 nil nil))
                  (t (objects-answer)))))
    (let ((text (call-tool "klipper_status" "{\"include_snapshot\": true}")))
      (is-carrying text ("Saved the webcam still" "the still was kept")))
    (let ((files (uiop:directory-files (nodecode-klipper::snapshot-directory))))
      (is (= 1 (length files)) "one file in the cell's own folder")
      (is (string= "jpg" (pathname-type (first files))) "of the kind the magic bytes say"))
    (is (find "/webcam/shot.jpg" (asked) :test #'search) "from Moonraker's own list")))
