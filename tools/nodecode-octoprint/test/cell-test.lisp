;;;; cell-test.lisp --- the lifecycle, the three tools, /octoprint, the watch's diff.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Offline: NLK:HTTP is stubbed with an OctoPrint that answers the
;;;; connection, the job, the printer, the move and the webcam from canned
;;;; answers, so the run asserts on what the cell asked and what it made of
;;;; the answer. Nothing dials.

(in-package #:nodecode.test)

(define-test-slice "octoprint" "OCTOPRINT-CELL-" :start nodecode-octoprint:start-cell)

;;; --- an OctoPrint that is not there ------------------------------------------------

(defvar *requests* nil
  "Every exchange the cell asked for, newest first: (METHOD PATH CONTENT).")

(defvar *printer* nil
  "A function (METHOD PATH CONTENT) => (values OCTETS STATUS HEADERS TRUNCATED).")

(defun path-of (url)
  (or (quri:uri-path (quri:uri url)) url))

(defun octoprint-stub (method url &rest arguments)
  (let ((path (path-of url)))
    (push (list method path (getf arguments :content)) *requests*)
    (funcall *printer* method path (getf arguments :content))))

(defun octet-answer (object &optional (status 200) (truncated nil))
  (values (sb-ext:string-to-octets (if (stringp object) object (nlk:encode-json-object object)))
          status nil truncated))

(defun connection-answer (&key (state "Operational"))
  (octet-answer (nlk:make-json-object "current" (nlk:make-json-object "state" state))))

(defun job-answer (&key (state "Printing") (filename "part.gcode") (completion 0.5)
                        (filepos 120.0) (left 1800.0) (error ""))
  (octet-answer
   (nlk:make-json-object
    "job" (nlk:make-json-object "file" (nlk:make-json-object "name" filename))
    "progress" (nlk:make-json-object "completion" completion "filepos" filepos
                                      "printTimeLeft" left)
    "state" state "error" error)))

(defun printer-answer ()
  (octet-answer
   (nlk:make-json-object
    "temperature" (nlk:make-json-object
                   "tool0" (nlk:make-json-object "actual" 210.5 "target" 210.0)
                   "bed" (nlk:make-json-object "actual" 60.0 "target" 60.0)))))

(defun printer-fn (&key (state "Printing") (connection "Operational") (completion 0.5)
                        (temperatures t) (move nil))
  "An OctoPrint answering these states; MOVE names what a POST /api/job sets."
  (lambda (method path content)
    (declare (ignore content))
    (cond ((search "/api/connection" path) (connection-answer :state connection))
          ((search "/api/job" path)
           (if (eq method :post)
               (progn (when move (setf state move))
                      (values (sb-ext:string-to-octets "") 204 nil nil))
               (job-answer :state state :completion completion)))
          ((search "/api/printer" path)
           (if temperatures (printer-answer)
               (values (sb-ext:string-to-octets "{\"error\":\"Printer is not operational\"}") 409 nil nil)))
          ((search "/webcam/shot.jpg" path)
           (values (coerce #(255 216 255 224 1 2 3 4) 'nlk:octets) 200 nil nil))
          (t (octet-answer "{}" 404)))))

(defmacro with-octoprint ((&rest pairs) &body body)
  "BODY over a stubbed OctoPrint and the cell started on PAIRS, stopped on unwind."
  `(let ((*requests* '()) (*printer* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest arguments)
                                 (apply #'octoprint-stub method url arguments))
       (with-cell-stop ((octoprint-start ,@pairs))
         ,@body))))

(defun call-tool (name &optional (arguments "{}"))
  "NAME called as a model calls it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun asked () (mapcar #'second *requests*))

(defun asked-p (needle) (find needle (asked) :test #'search))

(defun posted-p ()
  "The first exchange the cell sent as a POST, or NIL."
  (find-if (lambda (request) (eq (first request) :post)) *requests*))

(defun run-command (name &optional (argument ""))
  "NAME driven the way the shell drives a slash command; => its text."
  (let ((entry (nle::find-registered-command name)))
    (and entry (funcall (nle::slash-command-headless entry) argument "octoprint-session"))))

;;; --- the lifecycle, the manual, the refusals ------------------------------------------

(define-cell-lifecycle-tests "octoprint"
  (:config "url" "http://printer.test:5000" "api_key" "K")
  (:help :octoprint)
  (:command "octoprint")
  (:running (is (zerop (nodecode-octoprint::setting :watch-minutes)) "no watch by default"))
  (:refused ("stall_minutes" 0) ("snapshot_max_bytes" 0) ("url" 5))
  (:idle nodecode-octoprint::octoprint-error
         (nodecode-octoprint::run-octoprint "status" nil)
         (nodecode-octoprint::watch-once)))

;;; --- reading the printer ------------------------------------------------------------

(deftest octoprint-cell-status-answers-with-progress-temperatures-and-the-estimate ()
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "K")
    (setf *printer* (printer-fn :completion 50.0))
    (let ((text (call-tool "octoprint_status")))
      (is-carrying text
        ("\"job_state\":\"Printing\"" "the job state")
        ("\"kind\":\"printing\"")
        ("completion 50.0%" "the percentage OctoPrint sent")
        ("210.5" "the hotend temperature")
        ("1800" "printTimeLeft")
        ("part.gcode")
        (:absent "ERROR")))))

(deftest octoprint-cell-a-disconnected-printer-is-not-a-pause ()
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "K")
    (setf *printer* (printer-fn :state "Paused" :connection "Closed" :temperatures nil))
    (let ((text (call-tool "octoprint_status")))
      (is-carrying text
        ("The printer is disconnected" "the connection state wins over a leftover Paused")
        ("This is not a pause")))))

(deftest octoprint-cell-a-missing-url-or-key-answers-with-what-to-do ()
  (with-octoprint ("url" "" "api_key" "K")
    (setf *printer* (printer-fn))
    (let ((text (call-tool "octoprint_status")))
      (is-carrying text ("ERROR missing_url" "the url it lacks") ("Point it at one OctoPrint origin" text)))
    (is (null *requests*) "nothing was dialled"))
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "")
    (setf *printer* (printer-fn))
    (let ((text (call-tool "octoprint_status")))
      (is-carrying text ("ERROR no_key" "the key it lacks") ("Set the API key" text))
      (is (null *requests*) "nothing was dialled without a key"))))

(deftest octoprint-cell-a-printer-that-is-not-operational-has-no-temperatures ()
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "K")
    (setf *printer* (printer-fn :temperatures nil))
    (let ((text (call-tool "octoprint_status")))
      (is-carrying text
        ("\"job_state\":\"Printing\"" "the 409 from /api/printer is not a state")
        (:absent "ERROR"))
      (is (search "\"temperatures_text\":\"Hotend unknown, bed unknown.\"" text) "the temperatures line says it read none"))))

;;; --- moving the printer, twice -------------------------------------------------------

(deftest octoprint-cell-control-previews-and-then-confirms ()
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "K")
    (setf *printer* (printer-fn :state "Printing" :move "Paused"))
    (let ((preview (call-tool "octoprint_control" "{\"action\": \"pause\"}")))
      (is-carrying preview
        ("Nothing was sent" "the preview sends nothing")
        ("POST /api/job" "and says what would be sent")
        ("Printing")))
    (is (null (posted-p)) "no POST on the preview")
    (let ((confirmed (call-tool "octoprint_control"
                                "{\"action\": \"pause\", \"confirm\": true}")))
      (is-carrying confirmed
        ("\"moved\":true" "the move is confirmed by a second read")
        ("Printing to Paused")))
    (let ((post (first (remove-if-not (lambda (request) (eq (first request) :post))
                                      (reverse *requests*)))))
      (is-present post "the move was posted"
        (is (search "\"command\":\"pause\"" (third post)) "with the pause command")
        (is (search "\"action\":\"pause\"" (third post)) "and the action pause, never toggle")))))

(deftest octoprint-cell-control-sends-nothing-when-the-job-is-already-there ()
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "K")
    (setf *printer* (printer-fn :state "Paused"))
    (let ((text (call-tool "octoprint_control"
                            "{\"action\": \"pause\", \"confirm\": true}")))
      (is-carrying text ("Job state is Paused, so pause was not sent" "nothing was sent")))
    (is (null (posted-p)))))

;;; --- the watch ----------------------------------------------------------------------

(deftest octoprint-cell-watch-records-a-baseline-and-then-reports-the-finish ()
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "K")
    (setf *printer* (printer-fn :state "Printing" :completion 50.0))
    (let ((first (call-tool "octoprint_watch")))
      (is-carrying first ("No earlier sample" first)))
    (setf *printer* (printer-fn :state "Operational" :completion 100.0))
    (let ((finished (call-tool "octoprint_watch")))
      (is-carrying finished
        ("Print finished: part.gcode." finished)
        ("\"notify\":true" finished)))
    (let ((repeat (call-tool "octoprint_watch")))
      (is-carrying repeat ("Nothing new to report" repeat)))))

(deftest octoprint-cell-watch-reports-a-failure-once-then-once-a-day ()
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "K")
    (setf *printer* (lambda (method path content)
                      (declare (ignore method path content))
                      (error "connection refused")))
    (let ((first (call-tool "octoprint_watch")))
      (is-carrying first
        ("Could not reach OctoPrint" first)
        ("\"notify\":true" first)))
    (let ((repeat (call-tool "octoprint_watch")))
      (is-carrying repeat ("The same failure was already reported" repeat)))
    (setf *printer* (printer-fn :state "Paused"))
    (let ((recovered (call-tool "octoprint_watch")))
      (is-carrying recovered
        ("The printer can be checked again" recovered)
        ("\"notify\":true" recovered)))))

;;; --- the operator's reading ----------------------------------------------------------

(deftest octoprint-cell-the-command-reads-and-never-moves ()
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "K")
    (setf *printer* (printer-fn))
    (let ((status (run-command "octoprint" "status")))
      (is-carrying status
        ("Job state Printing")
        ("Hotend 210.5/210.0 C")
        ("bed 60.0/60.0 C")))
    (let ((pause (run-command "octoprint" "pause")))
      (is-carrying pause ("was not sent" "octoprint_control")))
    (is (null (posted-p)) "the command never posted a move")))

;;; --- one webcam still ----------------------------------------------------------------

(deftest octoprint-cell-a-still-is-saved-inside-the-cells-own-folder ()
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "K"
                   "snapshot_url" "http://printer.test:8080/webcam/shot.jpg")
    (setf *printer* (printer-fn))
    (let ((text (call-tool "octoprint_status" "{\"include_snapshot\": true}")))
      (is-carrying text ("Saved the webcam still" "the still was kept")))
    (let ((files (uiop:directory-files (nodecode-octoprint::still-directory))))
      (is (= 1 (length files)) "one file in the cell's own folder")
      (is (string= "jpg" (pathname-type (first files))) "of the kind the magic bytes say"))))

(deftest octoprint-cell-a-still-from-another-host-is-refused ()
  (with-octoprint ("url" "http://printer.test:5000" "api_key" "K"
                   "snapshot_url" "http://camera.test/webcam/shot.jpg")
    (setf *printer* (printer-fn))
    (let ((text (call-tool "octoprint_status" "{\"include_snapshot\": true}")))
      (is-carrying text ("not on the printer's host" "the still stays near the printer")))
    (is (null (asked-p "/webcam/shot.jpg")) "and was not asked for")))
