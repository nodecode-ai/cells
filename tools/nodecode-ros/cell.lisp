;;;; cell.lisp --- drive and watch a robot: nine tools, a safety interlock, /ros.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's nine tools are the nine verbs here, against the same two
;;;; backends (robot.lisp): the built-in 2D sim, which needs no ROS, and a
;;;; real robot or simulator behind a rosbridge websocket. What the plugin
;;;; enforced from a pre_tool_call hook -- a speed and duration ceiling, an
;;;; optional geofence, a write allowlist, a latching e-stop and an audit of
;;;; every blocked call -- is enforced inside the tools that write, because a
;;;; cell owns its tools: nothing reaches a robot without passing the checks,
;;;; and ros_stop is never blocked, so stopping always works.
;;;;
;;;; The e-stop latch is a file under this home, so a restart keeps it
;;;; latched; only a human clears it, from /ros clear.

(in-package #:nodecode-ros)

;;; --- the words ---------------------------------------------------------------------

(defparameter *primer*
  "ros is a robot link: nine tools that drive and observe a ROS or ROS 2 robot
over rosbridge, behind a safety interlock. Discover with ros_list, read with
ros_echo and ros_get_pose, move with ros_drive (short moves; check the pose
after each), publish with ros_publish, call a service with ros_call_service,
see with ros_camera_snapshot (then vision_analyze its path and put the MEDIA:
line in your reply), and stop with ros_stop -- estop=true latches the e-stop,
which blocks all motion and service calls until a human types /ros clear.
Every write is checked: speeds and durations above the limits, a path out of
the geofence, a topic or service not on the write allowlist, or a latched
e-stop answers a refusal naming the rule -- do not retry it, adjust the call
or tell the user. The `ros' section picks the backend: mock (a 2D sim that
needs no ROS) or rosbridge (host and port of a robot). (help :ros) is this
text."
  "What (help :ros) answers while the cell runs.")

(defparameter *blocked-kept* 200
  "How many blocked calls the ring keeps; the file the plugin wrote held 500.")

;;; --- state -------------------------------------------------------------------------

(defvar *backend* nil "The live backend: the sim, or the rosbridge link.")
(defvar *estop* nil "The latch, a plist, or NIL when movement is allowed.")
(defvar *blocked* '() "Blocked calls, newest first.")
(defvar *counters* '(:allowed 0 :blocked 0 :modified 0) "What the interlock let pass and stopped.")

(defun estop-file () (nlk:home "ros/estop.json"))
(defun snapshot-dir () (nlk:home "ros/snapshots/"))
(defun latched-p () (and *estop* t))

(defun estop-record (reason source)
  "One latch record."
  (nlk:make-json-object "latched" t "reason" reason "source" source
                         "at" (get-universal-time)))

(defun write-estop (record)
  "RECORD kept in the home, so a restart still finds the robot latched."
  (ignore-errors
    (let ((path (estop-file)))
      (nlk:write-file-atomically path (nlk:encode-json-object record)))))

(defun load-estop ()
  "Read the latch the last process left; an unreadable file fails closed."
  (let ((path (estop-file)))
    (when (probe-file path)
      (let ((record (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))
        (when (and (hash-table-p record) (eq t (gethash "latched" record)))
          (setf *estop* record))))))

(defun engage-estop (reason source)
  "Latch the e-stop until a human clears it."
  (let ((record (estop-record (if (and reason (plusp (length reason))) reason "e-stop engaged") source)))
    (setf *estop* record)
    (write-estop record)
    record))

(defun clear-estop (source)
  "Clear the latch, answering whether it was latched."
  (let ((was (latched-p)))
    (setf *estop* nil)
    (write-estop (nlk:make-json-object "latched" :null "cleared_by" source "at" (get-universal-time)))
    was))

(defun estop-status ()
  (if *estop* *estop* (nlk:make-json-object "latched" :null)))

(defun record-block (tool rule reason)
  "One refused call, in the ring and in the counters."
  (push (nlk:make-json-object "at" (get-universal-time) "tool" tool "rule" rule "reason" reason)
        *blocked*)
  (when (> (length *blocked*) *blocked-kept*)
    (setf *blocked* (subseq *blocked* 0 *blocked-kept*)))
  (incf (getf *counters* :blocked))
  (car *blocked*))

(defun blocked (rule reason)
  "Refuse: record it, then signal the refusal the model reads."
  (record-block *current-tool* rule reason)
  (fail "interlock BLOCKED [~a]: ~a" rule reason))

(defvar *current-tool* "ros" "The tool whose call is running, for the audit line.")

(defun backend ()
  "The live backend, made on first use from the section."
  (or *backend* (setf *backend* (make-backend))))

(defun make-backend ()
  (let ((kind (string-downcase (setting :backend))))
    (cond ((equal kind "mock")
           (make-sim :half-x (car (setting :arena)) :half-y (cdr (setting :arena))
                     :cones (setting :cones)))
          ((equal kind "rosbridge")
           (let ((host (setting :host)) (port (setting :port)))
             (make-link :host host :port port
                        :url (format nil "ws://~a:~a" host port)
                        :timeout (setting :timeout-seconds)
                        :replies '() :messages '()
                        :advertised (make-hash-table :test #'equal))))
          (t (fail "unknown backend ~s: it is mock or rosbridge" kind)))))

(defun close-backend ()
  (when *backend* (ignore-errors (rb-close *backend*)))
  (setf *backend* nil))

;;; --- the interlock ------------------------------------------------------------------

(defun allowlisted-p (kind name)
  (matches-any name (if (equal kind "topic") (setting :allow-topics) (setting :allow-services))))

(defun check-allowlist (kind name)
  (unless (allowlisted-p kind name)
    (blocked (format nil "~a_not_allowlisted" kind)
             (format nil "~a ~s is not on the write allowlist (~{~a~^, ~}); a human can add it under ros.allow_~as"
                     kind name
                     (if (equal kind "topic") (setting :allow-topics) (setting :allow-services))
                     kind))))

(defun clamp-mode-p () (equal (string-downcase (setting :over-limit)) "clamp"))

(defun motion-topic-p (topic)
  (matches-any topic (setting :motion-patterns)))

(defun project-path (pose linear angular duration)
  "The commanded path dead-reckoned from POSE, for the geofence."
  (let* ((steps (max 1 (ceiling duration 0.1)))
         (step (/ duration steps))
         (x (coerce (nlk:json-value pose :number "x") 'double-float))
         (y (coerce (nlk:json-value pose :number "y") 'double-float))
         (yaw (coerce (or (ignore-errors (nlk:json-value pose :number "yaw_rad")) 0) 'double-float))
         (points (list (cons x y))))
    (dotimes (index steps)
      (declare (ignore index))
      (setf x (+ x (* linear (cos yaw) step))
            y (+ y (* linear (sin yaw) step))
            yaw (+ yaw (* angular step)))
      (push (cons x y) points))
    (nreverse points)))

(defun check-geofence (linear angular duration)
  "The geofence, when one is set: the projected path must stay inside it."
  (let ((fence (setting :geofence)))
    (when fence
      (destructuring-bind (x-min x-max y-min y-max) fence
        (let ((pose (ignore-errors (rb-latest-pose (backend)))))
          (unless pose
            (blocked "geofence_no_odom"
                     "the geofence is on and no odometry is available; refusing to move blind"))
          (let ((age (or (ignore-errors (nlk:json-value pose :number "age_s")) 0)))
            (when (> age (setting :max-odom-age))
              (blocked "geofence_odom_stale"
                       (format nil "the newest odometry is ~,1f s old (max ~,1f s); refusing to move on stale data"
                               age (setting :max-odom-age)))))
          (let ((margin (setting :margin)))
            (dolist (point (project-path pose linear angular duration))
              (unless (and (<= (+ x-min margin) (car point) (- x-max margin))
                           (<= (+ y-min margin) (cdr point) (- y-max margin)))
                (blocked "geofence"
                         (format nil "the commanded path leaves the geofence x[~,1f,~,1f] y[~,1f,~,1f] ~
(margin ~,2f) near (~,2f, ~,2f); the robot is at (~,2f, ~,2f). Choose a shorter move."
                                 x-min x-max y-min y-max margin (car point) (cdr point)
                                 (nlk:json-value pose :number "x") (nlk:json-value pose :number "y")))))
            (format nil "geofence ok (ends near (~,2f, ~,2f))"
                    (car (car (last (project-path pose linear angular duration))))
                    (cdr (car (last (project-path pose linear angular duration)))))))))))

(defun vet-motion (linear angular duration given-p)
  "Check one velocity command; answer (VALUES LINEAR ANGULAR DURATION NOTES).
A zero command is a stop and is always let through; a latched e-stop refuses
everything else."
  (let ((notes '())
        (linear (coerce linear 'double-float))
        (angular (coerce angular 'double-float))
        (duration (and duration (coerce duration 'double-float))))
    (if (and (zerop linear) (zerop angular))
        (values linear angular (or duration (setting :default-duration))
                (list "zero-velocity command (stop) always allowed"))
        (progn
          (when (latched-p)
            (blocked "estop_latched"
                     (format nil "the e-stop is latched (~a); motion is blocked until a human clears it with /ros clear"
                             (nlk:json-value *estop* :string "reason"))))
          (let ((max-linear (setting :max-linear))
                (max-angular (setting :max-angular)))
            (when (> (abs linear) (+ max-linear 1d-9))
              (if (clamp-mode-p)
                  (progn (push (format nil "linear speed clamped ~,2f -> ~,2f m/s" (abs linear) max-linear) notes)
                         (setf linear (* max-linear (signum linear))))
                  (blocked "over_speed_linear"
                           (format nil "linear speed ~,2f m/s exceeds the limit of ~,2f m/s"
                                   (abs linear) max-linear))))
            (when (> (abs angular) (+ max-angular 1d-9))
              (if (clamp-mode-p)
                  (progn (push (format nil "angular speed clamped ~,2f -> ~,2f rad/s" (abs angular) max-angular) notes)
                         (setf angular (* max-angular (signum angular))))
                  (blocked "over_speed_angular"
                           (format nil "angular speed ~,2f rad/s exceeds the limit of ~,2f rad/s"
                                   (abs angular) max-angular))))
            (let ((max-duration (setting :max-duration)))
              (cond ((or (null duration) (not given-p))
                     (setf duration (setting :default-duration))
                     (push (format nil "no duration given; defaulted to ~,1f s" duration) notes))
                    ((<= duration 0)
                     (blocked "invalid_args" "duration_s must be > 0 for a moving command"))
                    ((> duration (+ max-duration 1d-9))
                     (if (clamp-mode-p)
                         (progn (push (format nil "duration clamped ~,1f s -> ~,1f s" duration max-duration) notes)
                                (setf duration max-duration))
                         (blocked "over_duration"
                                  (format nil "duration ~,1f s exceeds the maximum of ~,1f s"
                                          duration max-duration))))))
            (let ((note (check-geofence linear angular duration)))
              (when note (push note notes)))
            (values linear angular duration (nreverse notes)))))))

(defun extract-twist (message)
  "MESSAGE as (VALUES LINEAR ANGULAR), or NIL when it is no Twist."
  (when (hash-table-p message)
    (let ((twist (or (and (hash-table-p (gethash "twist" message)) (gethash "twist" message)) message)))
      (when (and (hash-table-p twist)
                 (or (gethash "linear" twist) (gethash "angular" twist)))
        (values (coerce (or (ignore-errors (nlk:json-value twist :number "linear" "x")) 0) 'double-float)
                (coerce (or (ignore-errors (nlk:json-value twist :number "angular" "z")) 0) 'double-float))))))

(defun twist-message (linear angular)
  "A geometry_msgs/Twist, or the TwistStamped the section asks for."
  (let ((twist (nlk:make-json-object
                "linear" (nlk:make-json-object "x" linear "y" 0d0 "z" 0d0)
                "angular" (nlk:make-json-object "x" 0d0 "y" 0d0 "z" angular))))
    (if (setting :stamped)
        (nlk:make-json-object "header" (nlk:make-json-object "frame_id" "base_link") "twist" twist)
        twist)))

(defun send-stop (topic msg-type &optional (times 3))
  "A burst of zero velocities: stopping is never blocked."
  (dotimes (index times)
    (ignore-errors (rb-publish (backend) topic msg-type (twist-message 0d0 0d0)))))

(defun run-timed (topic msg-type message duration rate)
  "Publish MESSAGE at RATE for DURATION seconds, aborting on a latch or a
geofence breach, then send the stop. => a plist of what happened."
  (let ((sent 0) (elapsed 0d0) (reason "completed")
        (period (/ 1d0 (coerce rate 'double-float)))
        (fence (setting :geofence)))
    (loop while (< elapsed (- (coerce duration 'double-float) 1d-9))
          do (cond ((latched-p) (setf reason "aborted: e-stop latched") (return))
                   ((and fence
                         (let ((pose (ignore-errors (rb-latest-pose (backend)))))
                           (and pose
                                (destructuring-bind (x-min x-max y-min y-max) fence
                                  (let ((x (nlk:json-value pose :number "x"))
                                        (y (nlk:json-value pose :number "y")))
                                    (not (and (<= x-min x x-max) (<= y-min y y-max))))))))
                    (record-block *current-tool* "geofence_runtime" "aborted: robot left the geofence")
                    (setf reason "aborted: robot left the geofence")
                    (return))
                   (t
                    (rb-publish (backend) topic msg-type message)
                    (incf sent)
                    (let ((step (min period (- (coerce duration 'double-float) elapsed))))
                      (rb-sleep (backend) step)
                      (incf elapsed step)))))
    (send-stop topic msg-type)
    (list :published sent :ran-for (round-to elapsed 2) :end-reason reason)))

;;; --- one pose, and the JSON the tools answer ----------------------------------------

(defun pose-lines (pose)
  "POSE as the one line a model reads."
  (when pose
    (format nil "x=~,3f y=~,3f yaw=~,1f° (~,3f rad)~@[, collided: ~a~]"
            (nlk:json-value pose :number "x") (nlk:json-value pose :number "y")
            (or (ignore-errors (nlk:json-value pose :number "yaw_deg")) 0)
            (or (ignore-errors (nlk:json-value pose :number "yaw_rad")) 0)
            (let ((collided (gethash "collided" pose)))
              (and (stringp collided) collided)))))

(defun summarize (value &optional (depth 0))
  "VALUE shrunk to what fits in a model's context: long strings, big arrays."
  (cond ((hash-table-p value)
         (let ((out (nlk:make-json-object)))
           (maphash (lambda (key item) (setf (gethash key out) (summarize item (1+ depth)))) value)
           out))
        ((and (vectorp value) (not (stringp value)))
         (let ((items (coerce value 'list)))
           (if (> (length items) 16)
               (coerce (append (mapcar (lambda (item) (summarize item (1+ depth))) (subseq items 0 16))
                               (list (format nil "… (+~d more)" (- (length items) 16))))
                       'vector)
               (coerce (mapcar (lambda (item) (summarize item (1+ depth))) items) 'vector))))
        ((and (stringp value) (> (length value) 160))
         (format nil "~a… (~d chars)" (subseq value 0 160) (length value)))
        ((floatp value) (round-to value 5))
        (t value)))

(defun answer (object) (nlk:encode-json-object object))

(defun refusal (condition)
  "CONDITION as the JSON a tool answers, the plugin's shape."
  (answer (nlk:make-json-object "ok" :null "error" (format nil "~a" condition))))

(defun tool-answer (thunk)
  "Run THUNK, answering its JSON string; a refusal is JSON too, never a signal."
  (handler-case (funcall thunk)
    (ros-error (condition) (refusal condition))
    (error (condition)
      (refusal (format nil "~a: ~a" (type-of condition) condition)))))

(defmacro with-tool ((name) &body body)
  "BODY as tool NAME: the audit line names it, and a refusal is JSON."
  `(let ((*current-tool* ,name))
     (tool-answer (lambda () ,@body))))

;;; --- the nine verbs -----------------------------------------------------------------

(define-verb topics (&optional (kind "all") (filter ""))
  "What the robot exposes: topics with their types, services and/or nodes.
=> a JSON string. Read-only."
  (let* ((be (backend))
         (kind (string-downcase (or kind "all")))
         (filter (string-downcase (or filter "")))
         (wanted (lambda (text) (or (zerop (length filter)) (search filter (string-downcase text)))))
         (out (nlk:make-json-object "ok" t "backend" (string-downcase (format nil "~a" (setting :backend)))
                                     "via" (rb-description be))))
    (flet ((kept (items &key typed)
             (coerce
              (loop for item in items
                    when (if typed
                             (funcall wanted (format nil "~a ~a"
                                                     (nlk:json-value item :string "name")
                                                     (or (nlk:json-value item :string "type") "")))
                             (funcall wanted (princ-to-string item)))
                      collect (if typed item (princ-to-string item)))
              'vector)))
      (when (member kind '("topics" "all") :test #'equal)
        (setf (gethash "topics" out) (kept (rb-topics be) :typed t)))
      (when (member kind '("services" "all") :test #'equal)
        (setf (gethash "services" out) (kept (rb-services be) :typed t)))
      (when (member kind '("nodes" "all") :test #'equal)
        (setf (gethash "nodes" out) (kept (rb-nodes be)))))
    (answer out)))

(define-verb echo (topic &optional msg-type (timeout 5))
  "Read ONE message from a topic. => a JSON string, big arrays summarised."
  (let ((topic (norm-topic topic)))
    (answer (nlk:make-json-object "ok" t "topic" topic
                                   "message" (summarize (rb-echo (backend) topic msg-type timeout))))))

(define-verb drive (linear angular &optional duration topic)
  "Drive: publish a velocity for DURATION seconds, then stop, and answer the
pose before and after. Prefer this to PUBLISH for movement."
  (let* ((given (not (null duration)))
         (topic (or-topic topic (setting :cmd-vel)))
         (msg-type (if (setting :stamped) "geometry_msgs/TwistStamped" "geometry_msgs/Twist"))
         (before (ignore-errors (rb-latest-pose (backend)))))
    (check-allowlist "topic" topic)
    (multiple-value-bind (linear angular duration notes)
        (vet-motion (or linear 0) (or angular 0) duration given)
      (let ((run (run-timed topic msg-type (twist-message linear angular) duration 10))
            (after (ignore-errors (rb-latest-pose (backend)))))
        (answer (nlk:make-json-object
                 "ok" t "topic" topic
                 "command" (nlk:make-json-object "linear" linear "angular" angular "duration_s" duration)
                 "published" (getf run :published) "ran_for_s" (getf run :ran-for)
                 "end_reason" (getf run :end-reason)
                 "interlock" (coerce notes 'vector)
                 "pose_before" before "pose_after" after
                 "displacement_m" (if (and before after)
                                       (round-to (sqrt (+ (expt (- (nlk:json-value after :number "x")
                                                                    (nlk:json-value before :number "x")) 2)
                                                          (expt (- (nlk:json-value after :number "y")
                                                                   (nlk:json-value before :number "y")) 2)))
                                                  3)
                                       :null)))))))

(define-verb publish (topic msg-type message &optional duration rate)
  "Publish a message to an allowlisted topic, optionally repeated. A Twist is
checked as a motion command; anything else is checked as a write."
  (let* ((topic (norm-topic topic))
         (rate (max 0.1d0 (min (coerce (or rate 10) 'double-float) 30d0)))
         (message (if (hash-table-p message) message (nlk:make-json-object))))
    (check-allowlist "topic" topic)
    (multiple-value-bind (linear angular) (extract-twist message)
      (cond
        ((null linear)
         (when (or (motion-topic-p topic) (search "Twist" (string msg-type)))
           (blocked "unparseable_motion_message"
                    (format nil "~a looks like a velocity topic but the message is not a readable Twist; ~
refusing because its speed cannot be verified" topic)))
         (when (latched-p)
           (blocked "estop_latched"
                    (format nil "the e-stop is latched (~a); writes are blocked until a human clears it with /ros clear"
                            (nlk:json-value *estop* :string "reason"))))
         (if duration
             (let ((run (run-timed topic msg-type message (coerce duration 'double-float) rate)))
               (answer (nlk:make-json-object "ok" t "topic" topic
                                              "published" (getf run :published)
                                              "ran_for_s" (getf run :ran-for)
                                              "end_reason" (getf run :end-reason))))
             (progn (rb-publish (backend) topic msg-type message)
                    (answer (nlk:make-json-object "ok" t "topic" topic "published" 1)))))
        (t
         (multiple-value-bind (linear angular duration notes)
             (vet-motion linear angular duration (not (null duration)))
           (let ((run (run-timed topic msg-type message duration rate)))
             (answer (nlk:make-json-object "ok" t "topic" topic
                                            "published" (getf run :published)
                                            "ran_for_s" (getf run :ran-for)
                                            "end_reason" (getf run :end-reason)
                                            "interlock" (coerce notes 'vector))))))))))

(define-verb call (service &optional srv-type request timeout)
  "Call a service on the allowlist. => its response, summarised."
  (let ((service (norm-topic service)))
    (check-allowlist "service" service)
    (when (latched-p)
      (blocked "estop_latched"
               (format nil "the e-stop is latched (~a); service calls are blocked until a human clears it with /ros clear"
                       (nlk:json-value *estop* :string "reason"))))
    (answer (nlk:make-json-object
             "ok" t "service" service
             "response" (summarize (rb-call (backend) service srv-type
                                             (if (hash-table-p request) request (nlk:make-json-object))
                                             timeout))))))

(define-verb pose (&optional topic (timeout 5))
  "The robot's pose from odometry: x, y, yaw, velocities and the collision flag."
  (answer (nlk:make-json-object "ok" t "pose" (rb-pose (backend) (or-topic topic (setting :odom)) timeout))))

(define-verb snapshot (&optional topic (timeout 6))
  "One camera frame, saved as a PNG or JPEG in this home. => its path, and the
MEDIA: line that shows it to the user."
  (multiple-value-bind (octets source)
      (rb-camera (backend) (or-topic topic (setting :camera)) timeout)
    (let* ((png (and (>= (length octets) 8) (equal (subseq octets 0 8) #(137 80 78 71 13 10 26 10))))
           (ext (if png "png" "jpg"))
           (mime (if png "image/png" "image/jpeg"))
           (dir (snapshot-dir))
           (stamp (now-stamp))
           (path (merge-pathnames (format nil "ros-snapshot-~a.~a" stamp ext) dir)))
      (ignore-errors (ensure-directories-exist path))
      (with-open-file (out path :direction :output :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
        (write-sequence octets out))
      (prune-snapshots dir (setting :snapshots-kept))
      (answer (nlk:make-json-object
               "ok" t "path" (namestring path) "mime" mime "bytes" (length octets) "source" source
               "media_tag" (format nil "MEDIA:~a" (namestring path))
               "next_step" (format nil "Call vision_analyze with image_url='~a' to describe what the robot sees; ~
to show the picture, put the line MEDIA:~a in your reply."
                                    (namestring path) (namestring path)))))))

(defun now-stamp ()
  "This moment as the name a snapshot carries."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month day hour minute second)))

(defun prune-snapshots (dir keep)
  "Leave the newest KEEP snapshots in DIR."
  (ignore-errors
    (let ((files (sort (directory (merge-pathnames "ros-snapshot-*" dir))
                       #'string< :key #'file-write-date)))
      (dolist (file (butlast files (max 1 keep)))
        (ignore-errors (delete-file file))))))

(define-verb stop (&optional estop reason)
  "Stop the robot now (a burst of zero velocities); with ESTOP also latch the
e-stop, which only a human clears. Never blocked by the interlock."
  (let* ((be (ignore-errors (backend)))
         (topic (norm-topic (setting :cmd-vel)))
         (msg-type (if (setting :stamped) "geometry_msgs/TwistStamped" "geometry_msgs/Twist"))
         (error-text nil))
    (when be
      (handler-case (send-stop topic msg-type 5)
        (error (condition) (setf error-text (format nil "~a" condition)))))
    (let ((latched (and estop (engage-estop reason "model"))))
      (answer (nlk:make-json-object
               "ok" (if error-text :null t)
               "stop_sent_to" topic
               "estop" (or latched (estop-status))
               "note" (cond (latched "e-stop latched: only a human can clear it with /ros clear")
                            (t "velocity zeroed (no latch)")))))))

(define-verb status ()
  "The link and the safety state: backend, limits, geofence, allowlists,
e-stop latch, counters and the most recent blocked calls."
  (let ((be (ignore-errors (rb-status (backend)))))
    (answer (nlk:make-json-object
             "ok" t
             "backend" (or be (nlk:make-json-object))
             "estop" (estop-status)
             "limits" (nlk:make-json-object "max_linear_mps" (setting :max-linear)
                                             "max_angular_rps" (setting :max-angular)
                                             "max_duration_s" (setting :max-duration)
                                             "over_limit_action" (setting :over-limit))
             "geofence" (let ((fence (setting :geofence)))
                          (if fence
                              (destructuring-bind (x-min x-max y-min y-max) fence
                                (nlk:make-json-object "enabled" t "x_min" x-min "x_max" x-max
                                                       "y_min" y-min "y_max" y-max
                                                       "margin_m" (setting :margin)))
                              (nlk:make-json-object "enabled" :null)))
             "write_allowlist" (nlk:make-json-object
                                 "topics" (coerce (setting :allow-topics) 'vector)
                                 "services" (coerce (setting :allow-services) 'vector))
             "counters" (nlk:make-json-object "allowed" (getf *counters* :allowed)
                                               "blocked" (getf *counters* :blocked)
                                               "modified" (getf *counters* :modified))
             "recent_blocked" (coerce (subseq *blocked* 0 (min 5 (length *blocked*))) 'vector)))))

;;; --- /ros ---------------------------------------------------------------------------

(defparameter *command-help*
  "ros -- robot link and safety controls
  /ros status          backend, limits, geofence, allowlists, e-stop
  /ros estop [reason]  latch the e-stop now (blocks all motion)
  /ros clear           clear the e-stop latch (human-only)
  /ros log [n]         the last n blocked calls with their reasons
  /ros snapshot        grab a camera frame
  /ros stop            a zero-velocity burst (no latch)"
  "What /ros answers with no subcommand, and on help.")

(define-verb run-slash (&optional args session-id)
  "The /ros command: status, estop [REASON], clear, log [N], snapshot or stop -
the operator's own view of the link and the interlock."
  (when (null session-id) (values session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (character) (member character '(#\Space #\Tab))) text))
         (sub (string-downcase (if space (subseq text 0 space) text)))
         (rest (nlk:trimmed (if space (subseq text space) ""))))
    (cond ((zerop (length text)) (status-lines))
          ((equal sub "status") (status-lines))
          ((member sub '("help" "-h" "--help") :test #'equal) *command-help*)
          ((equal sub "estop")
           (let ((record (engage-estop rest "human")))
             (ignore-errors (send-stop (norm-topic (setting :cmd-vel))
                                       (if (setting :stamped) "geometry_msgs/TwistStamped" "geometry_msgs/Twist")))
             (format nil "E-STOP LATCHED (~a). All motion and service calls are blocked. Clear with /ros clear."
                     (nlk:json-value record :string "reason"))))
          ((equal sub "clear")
           (if (clear-estop "human")
               "E-stop cleared; motion is allowed again within the safety limits."
               "E-stop was not latched."))
          ((equal sub "stop")
           (send-stop (norm-topic (setting :cmd-vel))
                      (if (setting :stamped) "geometry_msgs/TwistStamped" "geometry_msgs/Twist") 5)
           (format nil "zero velocity sent to ~a (no latch)" (norm-topic (setting :cmd-vel))))
          ((equal sub "log")
           (let* ((count (or (ignore-errors (parse-integer rest)) 10))
                  (rows (subseq *blocked* 0 (min count (length *blocked*)))))
             (if (null rows)
                 "No blocked calls recorded since this cell started."
                 (format nil "~{~a~%~}" (mapcar (lambda (row)
                                                   (format nil "~a  [~a]  ~a"
                                                           (nlk:json-value row :string "tool")
                                                           (nlk:json-value row :string "rule")
                                                           (nlk:json-value row :string "reason")))
                                                 (reverse rows))))))
          ((equal sub "snapshot")
           (let* ((text (snapshot))
                  (object (ignore-errors (nlk:decode-json text))))
             (if (and (hash-table-p object) (gethash "path" object))
                 (format nil "camera frame saved: ~a~%~a"
                         (gethash "path" object) (gethash "media_tag" object))
                 (format nil "snapshot failed: ~a" text))))
          (t (format nil "~s is not a subcommand.~%~a" sub *command-help*)))))

(defun status-lines ()
  "The operator's own view of the link and the interlock."
  (let* ((be (ignore-errors (rb-status (backend))))
         (fence (setting :geofence)))
    (format nil "~{~a~%~}"
            (list (format nil "backend: ~a~@[ (~a)~]"
                          (setting :backend)
                          (and be (or (ignore-errors (nlk:json-value be :string "url"))
                                      (ignore-errors (nlk:json-value be :string "backend")))))
                  (format nil "e-stop: ~a"
                          (if *estop*
                              (format nil "LATCHED -- ~a" (nlk:json-value *estop* :string "reason"))
                              "clear"))
                  (format nil "limits: <=~,2f m/s, <=~,2f rad/s, <=~,1f s (~a when exceeded)"
                          (setting :max-linear) (setting :max-angular) (setting :max-duration)
                          (setting :over-limit))
                  (format nil "geofence: ~a"
                          (if fence
                              (destructuring-bind (x-min x-max y-min y-max) fence
                                (format nil "x[~,1f,~,1f] y[~,1f,~,1f]" x-min x-max y-min y-max))
                              "off"))
                  (format nil "write allowlist: topics=~{~a~^,~} services=~{~a~^,~}"
                          (setting :allow-topics) (setting :allow-services))
                  (format nil "counters: allowed ~d, blocked ~d, modified ~d"
                          (getf *counters* :allowed) (getf *counters* :blocked)
                          (getf *counters* :modified))))))

;;; --- the boundaries -------------------------------------------------------------

(nlk:define-contract ros-topics
  (:doc "List what the connected robot exposes over ROS: topics with their message types, services and nodes. Start here to find the velocity, odometry and camera topics. Read-only.")
  ("kind" :string :options '("topics" "services" "nodes" "all") :default "all"
          :doc "what to list")
  ("filter" :string :default ""
            :doc "case-insensitive substring to filter names and types by, e.g. cmd_vel or camera"))

(nlk:define-contract ros-echo
  (:doc "Read ONE message from a ROS topic and return it; large arrays and image data are summarised. Read-only.")
  ("topic" :text :required t :doc "the topic to read once, e.g. /odom")
  ("msg_type" :string :default "" :doc "the message type; auto-detected when empty")
  ("timeout_s" :integer :default 5 :doc "how long to wait for a message, in seconds"))

(nlk:define-contract ros-drive
  (:doc "Drive the robot: publish a velocity (forward metres per second, turn radians per second, positive is left) for a bounded duration, then stop, and answer the pose before and after. Use small, short moves and check the pose or a snapshot after each.")
  ("linear" :number :default 0 :doc "forward speed in m/s; negative reverses")
  ("angular" :number :default 0 :doc "turn rate in rad/s; positive turns left")
  ("duration_s" :number :default 1 :doc "how long to move, in seconds; a short default when left out")
  ("topic" :string :default "" :doc "the velocity topic; the section's cmd_vel when empty"))

(nlk:define-contract ros-publish
  (:doc "Publish a ROS message to an allowlisted topic, optionally repeated for a duration. For robot motion prefer ros_drive. A Twist is checked as a motion command.")
  ("topic" :text :required t :doc "the topic to publish to; it must be on the write allowlist")
  ("msg_type" :text :required t :doc "the message type, e.g. geometry_msgs/Twist or std_msgs/String")
  ("message" :object :required t :doc "the message body as a JSON object")
  ("duration_s" :number :doc "repeat the message for this long; a Twist is followed by a zero-velocity stop")
  ("rate_hz" :number :default 10 :doc "how often to repeat it, at most 30 Hz"))

(nlk:define-contract ros-call-service
  (:doc "Call a ROS service on the write allowlist, e.g. a reset or a teleport, and answer its response.")
  ("service" :text :required t :doc "the service to call; it must be on the service allowlist")
  ("srv_type" :string :default "" :doc "the service type; auto-detected when empty")
  ("request" :object :doc "the request fields as a JSON object")
  ("timeout_s" :integer :default 8 :doc "how long to wait for the answer, in seconds"))

(nlk:define-contract ros-get-pose
  (:doc "The robot's pose from odometry: x and y in metres, yaw in radians and degrees, velocities, frame, data age and the collision flag. Read-only.")
  ("topic" :string :default "" :doc "the odometry topic; the section's odom when empty")
  ("timeout_s" :integer :default 5 :doc "how long to wait for a pose, in seconds"))

(nlk:define-contract ros-camera-snapshot
  (:doc "Grab ONE frame from the robot's camera, save it as a PNG or JPEG file under this home, and answer its path. Then call vision_analyze on that path and put the MEDIA: line in your reply to show the picture. Read-only.")
  ("topic" :string :default "" :doc "the camera topic; the section's camera when empty")
  ("timeout_s" :integer :default 6 :doc "how long to wait for a frame, in seconds"))

(nlk:define-contract ros-stop
  (:doc "STOP the robot now: a burst of zero-velocity commands. With estop also LATCH the e-stop, after which all motion and service calls are blocked until a human clears it with /ros clear. Never blocked by the interlock. Use estop when something looks unsafe, or the user says stop.")
  ("estop" :boolean :default nil :doc "latch the e-stop, which only a human clears")
  ("reason" :string :default "" :doc "why, in one sentence; it is kept with the latch"))

(defun topics-tool (call) (with-tool ("ros_list") (topics (ros-topics-kind call) (ros-topics-filter call))))
(defun echo-tool (call)
  (with-tool ("ros_echo") (echo (ros-echo-topic call) (ros-echo-msg-type call) (ros-echo-timeout-s call))))
(defun drive-tool (call)
  (with-tool ("ros_drive")
    (drive (ros-drive-linear call) (ros-drive-angular call) (ros-drive-duration-s call)
           (ros-drive-topic call))))
(defun publish-tool (call)
  (with-tool ("ros_publish")
    (publish (ros-publish-topic call) (ros-publish-msg-type call) (ros-publish-message call)
             (ros-publish-duration-s call) (ros-publish-rate-hz call))))
(defun call-tool (call)
  (with-tool ("ros_call_service")
    (call (ros-call-service-service call) (ros-call-service-srv-type call)
          (ros-call-service-request call) (ros-call-service-timeout-s call))))
(defun pose-tool (call)
  (with-tool ("ros_get_pose") (pose (ros-get-pose-topic call) (ros-get-pose-timeout-s call))))
(defun snapshot-tool (call)
  (with-tool ("ros_camera_snapshot")
    (snapshot (ros-camera-snapshot-topic call) (ros-camera-snapshot-timeout-s call))))
(defun stop-tool (call)
  (with-tool ("ros_stop") (stop (ros-stop-estop call) (ros-stop-reason call))))
(defun status-tool (call)
  (declare (ignore call))
  (with-tool ("ros_status") (status)))

;;; --- the declaration ---------------------------------------------------------------

(defun safety-number (table key default)
  "One safety knob, from the section's `safety' object, or its default."
  (let* ((safety (and (hash-table-p table) (gethash "safety" table)))
         (value (and (hash-table-p safety) (gethash key safety))))
    (if (numberp value) (coerce value 'double-float) (coerce default 'double-float))))

(defun safety-mode (table)
  "BLOCK or CLAMP, from the section's `safety' object."
  (let* ((safety (and (hash-table-p table) (gethash "safety" table)))
         (value (and (hash-table-p safety) (gethash "over_limit_action" safety))))
    (if (equal value "clamp") "clamp" "block")))

(defun number-list (value fallback)
  "VALUE as a list of numbers, or FALLBACK when it is empty or unreadable."
  (let* ((items (string-list value))
         (numbers (loop for item in items
                        for number = (cond ((numberp item) item)
                                           ((stringp item) (ignore-errors (read-from-string item)))
                                           (t nil))
                        when (numberp number) collect (coerce number 'double-float))))
    (if (= (length numbers) (length items)) numbers
        (if items numbers fallback))))

(defun parse-fence (value)
  "A geofence as (X-MIN X-MAX Y-MIN Y-MAX), or NIL when it is off."
  (let ((numbers (number-list value '())))
    (when (= (length numbers) 4) numbers)))

(defun parse-arena (value)
  "The mock arena's half sizes, as (HALF-X . HALF-Y)."
  (let ((numbers (number-list value '(5d0 5d0))))
    (if (= (length numbers) 2) (cons (first numbers) (second numbers)) (cons 5d0 5d0))))

(defun parse-cones (value)
  "The mock cones, as a list of (X . Y)."
  (let ((numbers (number-list value '(3.5d0 0.8d0))))
    (loop for (x y) on numbers by #'cddr
          when y collect (cons x y))))

(nle:define-cell ros
  (:section ("ros")
    (:guide "backend mock needs nothing; rosbridge needs the host and port of a rosbridge_server; writes are refused outside the allowlists and above the speed, duration and geofence limits")
    ("backend" :choice :options '("mock" "rosbridge") :default "mock"
               :doc "mock = the built-in 2D sim (no ROS needed); rosbridge = a robot or simulator behind a rosbridge websocket")
    ("host" :string :default "localhost"
            :doc "the rosbridge machine (used when backend is rosbridge)")
    ("port" :integer :default 9090 :min 1
            :doc "the rosbridge websocket port")
    ("timeout_seconds" :integer :default 8 :min 1
                       :doc "how long one service call or socket connect may take, in seconds")
    ("cmd_vel" :string :default "/cmd_vel"
               :doc "the velocity topic ros_drive publishes to")
    ("odom" :string :default "/odom"
            :doc "the odometry topic ros_get_pose and the geofence read")
    ("camera" :string :default "/camera/image_raw"
              :doc "the camera topic ros_camera_snapshot reads")
    ("stamped" :boolean :default :false
               :doc "wrap the velocity in TwistStamped (some bases want it)")
    ("allow_topics" :list
                    :doc "topics the tools may write to, as globs; /cmd_vel when empty")
    ("allow_services" :list
                      :doc "services the tools may call, as globs; /mock/* when empty")
    ("geofence" :list
                :doc "x_min, x_max, y_min, y_max in metres; empty leaves the geofence off")
    ("mock_arena" :list
                  :doc "half_x, half_y of the mock arena; 5,5 when empty")
    ("mock_cones" :list
                  :doc "cones as x,y,x,y in metres; one at 3.5,0.8 when empty")
    ("snapshots_kept" :integer :default 20 :min 1
                      :doc "how many camera snapshots this home keeps"))
  (:settings
   (lambda (values table)
     (let ((backend (getf values :backend)))
       (when (and (equal backend "rosbridge")
                  (let ((host (getf values :host)))
                    (not (and (stringp host) (plusp (length (nlk:trimmed host)))))))
         (nlk:config-error "ros.host names the machine rosbridge runs on; set it (localhost when it runs here)"))
       (when (and (equal backend "rosbridge")
                  (let ((host (getf values :host)))
                    (and (stringp host)
                         (or (search "://" host) (find #\/ host) (find #\Space host)))))
         (nlk:config-error "ros.host is a machine name, not a url: rosbridge is reached at ws://<host>:<port>, so ~
drop the scheme and the port (they are host and port)")))
     (let ((topics (string-list (getf values :allow-topics)))
           (services (string-list (getf values :allow-services))))
       (list* :backend (or (getf values :backend) "mock")
              :host (or (getf values :host) "localhost")
              :port (or (getf values :port) 9090)
              :timeout-seconds (or (getf values :timeout-seconds) 8)
              :cmd-vel (norm-topic (or (getf values :cmd-vel) "/cmd_vel"))
              :odom (norm-topic (or (getf values :odom) "/odom"))
              :camera (norm-topic (or (getf values :camera) "/camera/image_raw"))
              :stamped (and (getf values :stamped) t)
              :allow-topics (if topics topics '("/cmd_vel"))
              :allow-services (if services services '("/mock/*"))
              :geofence (parse-fence (getf values :geofence))
              :arena (parse-arena (getf values :mock-arena))
              :cones (parse-cones (getf values :mock-cones))
              :snapshots-kept (or (getf values :snapshots-kept) 20)
              :max-linear (safety-number table "max_linear_mps" 0.5d0)
              :max-angular (safety-number table "max_angular_rps" 1d0)
              :max-duration (safety-number table "max_duration_s" 5d0)
              :default-duration (safety-number table "default_duration_s" 1d0)
              :over-limit (safety-mode table)
              :margin (safety-number table "margin_m" 0.25d0)
              :max-odom-age (safety-number table "max_odom_age_s" 5d0)
              :motion-patterns (list "*cmd_vel*")))))
  (:tool "ros_list" (nlk:contract 'ros-topics) 'topics-tool)
  (:tool "ros_echo" (nlk:contract 'ros-echo) 'echo-tool)
  (:tool "ros_drive" (nlk:contract 'ros-drive) 'drive-tool)
  (:tool "ros_publish" (nlk:contract 'ros-publish) 'publish-tool)
  (:tool "ros_call_service" (nlk:contract 'ros-call-service) 'call-tool)
  (:tool "ros_get_pose" (nlk:contract 'ros-get-pose) 'pose-tool)
  (:tool "ros_camera_snapshot" (nlk:contract 'ros-camera-snapshot) 'snapshot-tool)
  (:tool "ros_stop" (nlk:contract 'ros-stop) 'stop-tool)
  (:tool "ros_status" (list :description "The robot link and the safety state: backend, limits, geofence, write allowlists, the e-stop latch and the most recent blocked calls. Read-only; call it before the first move of a session."
                              :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)))
            'status-tool)
  (:command "ros" 'run-slash
            :catches 'ros-error
            :description "Robot link and safety: status, estop, clear, log, snapshot, stop"
            :argument-hint "[status|estop|clear|log|snapshot|stop]")
  (:help :ros "ros: a ROS robot behind a safety interlock - (ros:status)" *primer*)
  (:start (lambda ()
            (setf *estop* nil *blocked* '() *counters* '(:allowed 0 :blocked 0 :modified 0))
            (load-estop)
            (nle:on-stop (lambda ()
                           (close-backend)
                           (setf *estop* nil *blocked* '()))))))
