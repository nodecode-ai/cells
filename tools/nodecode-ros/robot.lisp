;;;; robot.lisp --- the two robots behind the same nine verbs: the mock sim, and rosbridge.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every verb in cell.lisp talks to one of these through the RB- generics,
;;;; so the tools and the safety interlock behave identically against the
;;;; built-in 2D sim and against a real robot. The mock is the plugin's: a
;;;; unicycle in a rectangular arena, a velocity command that expires 0.5 s
;;;; after the last publish, walls and cones that stop it dead, odometry, three
;;;; services and a rendered camera. The rosbridge half speaks the same JSON
;;;; protocol the plugin's small client did (call_service, subscribe,
;;;; advertise/publish) over the image's own websocket-driver.
;;;;
;;;; One seam opens a socket, *CONNECT*, so a test drives the whole rosbridge
;;;; path without a network; nothing else in the cell does any I/O.

(in-package #:nodecode-ros)

;;; --- the socket seam --------------------------------------------------------------

(defstruct (socket (:conc-name socket-))
  "One open websocket as this cell sees it: SEND TEXT, CLOSE it, and HANDLER,
the function the peer's text arrives at."
  (send #'identity)
  (close #'identity)
  (handler nil)
  (open t))

(defun socket-deliver (socket text)
  "Hand TEXT to SOCKET's handler, as the ws lap does when a frame lands."
  (when (socket-open socket)
    (funcall (socket-handler socket) text)))

(defun ws-connect (url)
  "Open URL through websocket-driver and answer it as a SOCKET. The driver's
own thread calls back into SOCKET-DELIVER; nothing here blocks."
  (let* ((ws (wsd:make-client url))
         (socket (make-socket :send (lambda (text) (wsd:send ws text))
                              :close (lambda () (ignore-errors (nlk:sever-websocket ws))))))
    (wsd:on :message ws (lambda (message) (socket-deliver socket message)))
    (wsd:on :close ws (lambda (&key &allow-other-keys) (setf (socket-open socket) nil)))
    (wsd:start-connection ws)
    socket))

(defvar *connect* #'ws-connect
  "The one seam that opens a websocket: (URL) => SOCKET. Tests replace it.")

;;; --- the backend generics ---------------------------------------------------------

(defgeneric rb-description (backend))
(defgeneric rb-topics (backend))
(defgeneric rb-services (backend))
(defgeneric rb-nodes (backend))
(defgeneric rb-echo (backend topic msg-type timeout))
(defgeneric rb-publish (backend topic msg-type message))
(defgeneric rb-call (backend service srv-type request timeout))
(defgeneric rb-pose (backend topic timeout))
(defgeneric rb-latest-pose (backend))
(defgeneric rb-camera (backend topic timeout))
(defgeneric rb-status (backend))
(defgeneric rb-sleep (backend seconds))
(defgeneric rb-close (backend))

;;; --- the mock robot ---------------------------------------------------------------

(defparameter +robot-radius+ 0.2d0)
(defparameter +cone-radius+ 0.2d0)
(defparameter +command-timeout+ 0.5d0
  "Like a real base controller, a velocity command expires this long after the last publish.")
(defparameter +sim-dt+ 0.02d0)

(defparameter +mock-topics+
  '(("/cmd_vel" "geometry_msgs/Twist")
    ("/odom" "nav_msgs/Odometry")
    ("/camera/image_raw" "sensor_msgs/Image")
    ("/rosout" "rcl_interfaces/Log"))
  "What the mock robot says it publishes, as (NAME TYPE).")

(defparameter +mock-services+
  '(("/mock/reset" "std_srvs/Trigger")
    ("/mock/teleport" "mock_msgs/Teleport")
    ("/mock/get_cones" "mock_msgs/GetCones")))

(defparameter +mock-nodes+ '("/mock_robot" "/mock_camera" "/rosbridge_websocket"))

(defstruct (sim (:conc-name sim-))
  (half-x 5d0) (half-y 5d0) (cones '()) (x 0d0) (y 0d0) (yaw 0d0) (v 0d0) (w 0d0)
  (cmd-lin 0d0) (cmd-ang 0d0) (cmd-at -1d0) (time 0d0)
  (collided nil) (published '()) (trail '()))

(defun wrap-angle (angle)
  "ANGLE in (-pi, pi]."
  (- (mod (+ angle pi) (* 2 pi)) pi))

(defun sim-blocked-at (sim nx ny &optional (margin 0d0))
  "What stops the robot at (NX, NY): \"wall\", \"cone\", or NIL."
  (cond ((or (> (abs nx) (- (sim-half-x sim) +robot-radius+ margin))
             (> (abs ny) (- (sim-half-y sim) +robot-radius+ margin)))
         "wall")
        ((some (lambda (cone)
                 (< (sqrt (+ (expt (- nx (car cone)) 2) (expt (- ny (cdr cone)) 2)))
                    (+ +robot-radius+ +cone-radius+ margin)))
               (sim-cones sim))
         "cone")
        (t nil)))

(defun sim-step (sim dt)
  "Advance the sim DT seconds under the live command, stopping at a wall or a cone."
  (let* ((live (<= (- (sim-time sim) (sim-cmd-at sim)) +command-timeout+))
         (lin (if live (sim-cmd-lin sim) 0d0))
         (ang (if live (sim-cmd-ang sim) 0d0))
         (nx (+ (sim-x sim) (* lin (cos (sim-yaw sim)) dt)))
         (ny (+ (sim-y sim) (* lin (sin (sim-yaw sim)) dt)))
         (hit (sim-blocked-at sim nx ny)))
    (setf (sim-w sim) ang)
    (if hit
        (setf (sim-collided sim) hit
              (sim-v sim) 0d0)
        (progn (unless (sim-blocked-at sim nx ny 0.02d0) (setf (sim-collided sim) nil))
               (setf (sim-x sim) nx (sim-y sim) ny (sim-v sim) lin)))
    (setf (sim-yaw sim) (wrap-angle (+ (sim-yaw sim) (* ang dt)))
          (sim-time sim) (+ (sim-time sim) dt))
    sim))

(defun sim-advance (sim seconds)
  "Advance the sim SECONDS of its own time, in SIM-DT steps."
  (loop with remaining = (max 0d0 seconds)
        while (> remaining 1d-6)
        for dt = (min +sim-dt+ remaining)
        do (sim-step sim dt)
           (decf remaining dt))
  (unless (and (sim-trail sim)
               (< (sqrt (+ (expt (- (sim-x sim) (caar (sim-trail sim))) 2)
                           (expt (- (sim-y sim) (cadar (sim-trail sim))) 2)))
                  0.03d0))
    (setf (sim-trail sim) (cons (list (sim-x sim) (sim-y sim)) (sim-trail sim)))))

(defun sim-odom (sim)
  "The sim's pose as a nav_msgs/Odometry message."
  (let ((yaw (sim-yaw sim)))
    (nlk:make-json-object
     "header" (nlk:make-json-object "frame_id" "odom")
     "child_frame_id" "base_link"
     "pose" (nlk:make-json-object
             "pose" (nlk:make-json-object
                     "position" (nlk:make-json-object "x" (sim-x sim) "y" (sim-y sim) "z" 0)
                     "orientation" (nlk:make-json-object "x" 0 "y" 0
                                                          "z" (sin (/ yaw 2)) "w" (cos (/ yaw 2))))
             "covariance" (make-array 36 :initial-element 0))
     "twist" (nlk:make-json-object
              "twist" (nlk:make-json-object
                       "linear" (nlk:make-json-object "x" (sim-v sim) "y" 0 "z" 0)
                       "angular" (nlk:make-json-object "x" 0 "y" 0 "z" (sim-w sim)))
              "covariance" (make-array 36 :initial-element 0)))))

;;; The mock's camera: a small top-down picture of the arena, drawn here and
;;; written as a PNG by the image's own encoder. It is what a snapshot tool
;;; hands the model when no robot is at hand.

(defun mock-camera (sim)
  "The arena, the cones and the robot, as a PNG file's bytes."
  (let* ((width 96) (height 72)
         (pixels (nle::blank-pixels width height 3))
         (data (nle::pixels-data pixels))
         (span-x (* 2 (sim-half-x sim)))
         (span-y (* 2 (sim-half-y sim))))
    (labels ((put (col row red green blue)
             (when (and (< -1 col width) (< -1 row height))
               (let ((at (* 3 (+ col (* row width)))))
                 (setf (aref data at) red
                       (aref data (1+ at)) green
                       (aref data (+ at 2)) blue))))
           (plot (x y red green blue)
             (put (floor (+ (* (/ (+ x (sim-half-x sim)) span-x) (1- width)) 0.5))
                  (floor (+ (* (/ (- (sim-half-y sim) y) span-y) (1- height)) 0.5))
                  red green blue)))
      ;; the floor, then the four walls
      (loop for row below height
            do (loop for col below width
                     do (if (or (zerop row) (zerop col) (= row (1- height)) (= col (1- width)))
                            (put col row 210 60 60)
                            (put col row 32 40 48))))
      ;; the cones, three pixels wide
      (dolist (cone (sim-cones sim))
        (loop for dx from -1 to 1
              do (loop for dy from -1 to 1
                       do (plot (+ (car cone) (* dx 0.08d0)) (+ (cdr cone) (* dy 0.08d0)) 220 90 40))))
      ;; the robot, and a line along its heading
      (loop for step from 0 to 4
            do (plot (+ (sim-x sim) (* step 0.1d0 (cos (sim-yaw sim))))
                     (+ (sim-y sim) (* step 0.1d0 (sin (sim-yaw sim))))
                     120 200 255))
      (loop for dx from -1 to 1
            do (loop for dy from -1 to 1
                     do (plot (+ (sim-x sim) (* dx 0.08d0)) (+ (sim-y sim) (* dy 0.08d0)) 70 130 240))))
    (nle::pixels-png pixels)))

(defmethod rb-description ((sim sim)) "built-in 2D kinematic sim (no ROS)")
(defmethod rb-topics ((sim sim))
  (declare (ignore sim))
  (mapcar (lambda (row) (nlk:make-json-object "name" (first row) "type" (second row))) +mock-topics+))
(defmethod rb-services ((sim sim))
  (declare (ignore sim))
  (mapcar (lambda (row) (nlk:make-json-object "name" (first row) "type" (second row))) +mock-services+))
(defmethod rb-nodes ((sim sim)) (declare (ignore sim)) (copy-list +mock-nodes+))

(defmethod rb-echo ((sim sim) topic msg-type timeout)
  (declare (ignore msg-type timeout))
  (let ((topic (norm-topic topic)))
    (cond ((equal topic "/odom") (sim-odom sim))
          ((equal topic "/cmd_vel")
           (nlk:make-json-object
            "linear" (nlk:make-json-object "x" (sim-cmd-lin sim) "y" 0 "z" 0)
            "angular" (nlk:make-json-object "x" 0 "y" 0 "z" (sim-cmd-ang sim))))
          ((equal topic "/camera/image_raw")
           (nlk:make-json-object "header" (nlk:make-json-object "frame_id" "camera_link")
                                  "format" "png" "data" (format nil "<~d bytes PNG>" (length (mock-camera sim)))))
          ((equal topic "/rosout")
           (nlk:make-json-object "level" 20 "name" "mock_robot"
                                  "msg" (format nil "collided=~a" (sim-collided sim))))
          (t (fail "no publisher on ~s (the mock publishes ~{~a~^, ~})"
                   topic (mapcar #'first +mock-topics+))))))

(defmethod rb-publish ((sim sim) topic msg-type message)
  (declare (ignore msg-type))
  (push (list topic (sim-time sim)) (sim-published sim))
  (when (equal (norm-topic topic) "/cmd_vel")
    (let* ((twist (or (and (hash-table-p message) (gethash "twist" message)) message))
           (lin (or (ignore-errors (nlk:json-value twist :number "linear" "x")) 0d0))
           (ang (or (ignore-errors (nlk:json-value twist :number "angular" "z")) 0d0)))
      (setf (sim-cmd-lin sim) (coerce lin 'double-float)
            (sim-cmd-ang sim) (coerce ang 'double-float)
            (sim-cmd-at sim) (sim-time sim))))
  nil)

(defmethod rb-call ((sim sim) service srv-type request timeout)
  (declare (ignore srv-type timeout))
  (let ((service (norm-topic service)))
    (cond ((equal service "/mock/reset")
           (setf (sim-x sim) 0d0 (sim-y sim) 0d0 (sim-yaw sim) 0d0
                 (sim-v sim) 0d0 (sim-w sim) 0d0 (sim-cmd-lin sim) 0d0 (sim-cmd-ang sim) 0d0
                 (sim-cmd-at sim) -1d0 (sim-collided sim) nil (sim-trail sim) '())
           (nlk:make-json-object "success" t "message" "robot reset to origin"))
          ((equal service "/mock/teleport")
           (let ((nx (coerce (or (ignore-errors (nlk:json-value request :number "x")) 0) 'double-float))
                 (ny (coerce (or (ignore-errors (nlk:json-value request :number "y")) 0) 'double-float))
                 (yaw (coerce (or (ignore-errors (nlk:json-value request :number "yaw")) 0) 'double-float)))
             (if (sim-blocked-at sim nx ny)
                 (nlk:make-json-object "success" nil "message" "target is inside a wall or cone")
                 (progn (setf (sim-x sim) nx (sim-y sim) ny (sim-yaw sim) (wrap-angle yaw))
                        (nlk:make-json-object "success" t
                                               "message" (format nil "teleported to (~,2f, ~,2f)" nx ny))))))
          ((equal service "/mock/get_cones")
           (nlk:make-json-object
            "cones" (coerce (mapcar (lambda (cone) (nlk:make-json-object "x" (car cone) "y" (cdr cone)))
                                     (sim-cones sim))
                            'vector)))
          (t (fail "unknown service ~s (the mock answers ~{~a~^, ~})"
                   service (mapcar #'first +mock-services+))))))

(defun sim-pose (sim)
  "The sim's pose as the cell's one pose object."
  (nlk:make-json-object
   "x" (round-to (sim-x sim) 4) "y" (round-to (sim-y sim) 4)
   "yaw_rad" (round-to (sim-yaw sim) 4) "yaw_deg" (round-to (* (/ 180 pi) (sim-yaw sim)) 1)
   "linear_vel" (round-to (sim-v sim) 4) "angular_vel" (round-to (sim-w sim) 4)
   "frame_id" "odom" "child_frame_id" "base_link" "age_s" 0d0
   "collided" (or (sim-collided sim) :null) "source" "mock:/odom"))

(defmethod rb-pose ((sim sim) topic timeout)
  (declare (ignore topic timeout))
  (sim-pose sim))
(defmethod rb-latest-pose ((sim sim)) (sim-pose sim))
(defmethod rb-camera ((sim sim) topic timeout)
  (declare (ignore topic timeout))
  (values (mock-camera sim) "mock camera (top-down, 96x72)"))
(defmethod rb-status ((sim sim))
  (nlk:make-json-object
   "backend" "mock" "t" (round-to (sim-time sim) 2) "pose" (sim-pose sim)
   "arena" (nlk:make-json-object "half_x" (sim-half-x sim) "half_y" (sim-half-y sim))))
(defmethod rb-sleep ((sim sim) seconds) (sim-advance sim (coerce seconds 'double-float)))
(defmethod rb-close ((sim sim)) (declare (ignore sim)) nil)

;;; --- rosbridge ---------------------------------------------------------------------

(defstruct (link (:conc-name link-))
  (host "localhost") (port 9090) (url "") (socket nil) (timeout 8)
  (id 0) (replies nil) (messages nil) (advertised nil) (open nil))

(defun link-send (link message)
  "One protocol message, as JSON, to the link's socket."
  (unless (link-open link)
    (fail "not connected to ~a" (link-url link)))
  (funcall (socket-send (link-socket link)) (nlk:encode-json-object message)))

(defun link-receive (link text)
  "One protocol message that landed: a service response, or a published message."
  (let ((message (ignore-errors (nlk:decode-json text))))
    (when (hash-table-p message)
      (let ((op (nlk:json-value message :string "op")))
        (cond ((equal op "service_response")
               (let ((id (nlk:json-value message :string "id")))
                 (when id (push (cons id message) (link-replies link)))))
              ((equal op "publish")
               (let ((topic (nlk:json-value message :string "topic")))
                 (when topic
                   (push (cons topic (gethash "msg" message)) (link-messages link))))))))))

(defun link-connect (link)
  "Open the websocket when it is not open, and answer LINK."
  (unless (link-open link)
    (let ((socket (handler-case (funcall *connect* (link-url link))
                    (error (condition)
                      (fail "cannot reach rosbridge at ~a (~a). Is rosbridge_server running, and is ~
the host and port in the `ros' section right?"
                            (link-url link) condition)))))
      (setf (socket-handler socket) (lambda (text) (link-receive link text))
            (link-socket link) socket
            (link-open link) t))
    link))

(defun link-close (link)
  (when (link-open link)
    (setf (link-open link) nil)
    (ignore-errors (funcall (socket-close (link-socket link)))))
  (setf (link-socket link) nil (link-replies link) nil (link-messages link) nil
        (link-advertised link) nil)
  nil)

(defun link-call-service (link service srv-type request timeout)
  "Call SERVICE and answer its response object."
  (link-connect link)
  (let* ((id (format nil "call_service:~a:~d" service (incf (link-id link))))
         (message (nlk:make-json-object "op" "call_service" "id" id "service" service
                                         "args" (or request (nlk:make-json-object))))
         (deadline (+ (get-internal-real-time) (* (or timeout (link-timeout link)) internal-time-units-per-second))))
    (when srv-type (setf (gethash "type" message) srv-type))
    (link-send link message)
    (loop for reply = (cdr (assoc id (link-replies link) :test #'equal))
          do (cond (reply
                    (setf (link-replies link) (remove id (link-replies link) :key #'car :test #'equal))
                    (unless (eq t (gethash "result" reply))
                      (fail "service ~a reported failure: ~a" service
                            (nlk:encode-json-object (gethash "values" reply))))
                    (let ((values (gethash "values" reply)))
                      (return (if (hash-table-p values) values (nlk:make-json-object)))))
                   ((> (get-internal-real-time) deadline)
                    (fail "no response from ~a within ~a s" service timeout))
                   (t (sleep 0.005))))))

(defmethod rb-description ((link link)) (format nil "rosbridge ~a" (link-url link)))
(defmethod rb-call ((link link) service srv-type request timeout)
  (link-call-service link (norm-topic service) srv-type request timeout))

(defmethod rb-topics ((link link))
  (let ((answer (link-call-service link "/rosapi/topics_and_raw_types" "rosapi/TopicsAndRawTypes"
                                   (nlk:make-json-object) (link-timeout link))))
    (let ((names (coerce (or (gethash "topics" answer) #()) 'list))
          (types (coerce (or (gethash "types" answer) #()) 'list)))
      (if (= (length names) (length types))
          (mapcar (lambda (name type) (nlk:make-json-object "name" name "type" type)) names types)
          (mapcar (lambda (name) (nlk:make-json-object "name" name "type" "")) names)))))

(defmethod rb-services ((link link))
  (let ((answer (link-call-service link "/rosapi/services" "rosapi/Services"
                                   (nlk:make-json-object) (link-timeout link))))
    (mapcar (lambda (name) (nlk:make-json-object "name" name "type" ""))
            (coerce (or (gethash "services" answer) #()) 'list))))

(defmethod rb-nodes ((link link))
  (let ((answer (link-call-service link "/rosapi/nodes" "rosapi/Nodes"
                                   (nlk:make-json-object) (link-timeout link))))
    (coerce (or (gethash "nodes" answer) #()) 'list)))

(defun link-subscribe (link topic msg-type)
  "Subscribe to TOPIC, answering the subscription id."
  (link-connect link)
  (let* ((id (format nil "subscribe:~a:~d" topic (incf (link-id link))))
         (message (nlk:make-json-object "op" "subscribe" "id" id "topic" (norm-topic topic))))
    (when msg-type (setf (gethash "type" message) msg-type))
    (link-send link message)
    id))

(defun link-unsubscribe (link topic id)
  (when (link-open link)
    (ignore-errors
      (link-send link (nlk:make-json-object "op" "unsubscribe" "id" id "topic" (norm-topic topic))))))

(defun link-wait-message (link topic timeout)
  "The next message on TOPIC within TIMEOUT seconds."
  (let ((deadline (+ (get-internal-real-time) (* (or timeout 5) internal-time-units-per-second)))
        (topic (norm-topic topic)))
    (loop for entry = (assoc topic (link-messages link) :test #'equal)
          do (cond (entry
                    (setf (link-messages link) (remove topic (link-messages link) :key #'car :test #'equal))
                    (return (cdr entry)))
                   ((> (get-internal-real-time) deadline)
                    (fail "nothing arrived on ~a within ~a s" topic timeout))
                   (t (sleep 0.005))))))

(defmethod rb-echo ((link link) topic msg-type timeout)
  (let ((id (link-subscribe link topic msg-type)))
    (unwind-protect (link-wait-message link topic timeout)
      (link-unsubscribe link topic id))))

(defmethod rb-publish ((link link) topic msg-type message)
  (link-connect link)
  (let ((topic (norm-topic topic))
        (msg-type (or msg-type "geometry_msgs/Twist")))
    (unless (equal (gethash topic (link-advertised link)) msg-type)
      (link-send link (nlk:make-json-object "op" "advertise" "id" (format nil "advertise:~a" topic)
                                             "topic" topic "type" msg-type))
      (setf (gethash topic (link-advertised link)) msg-type))
    (link-send link (nlk:make-json-object "op" "publish" "id" (format nil "publish:~a:~d" topic (incf (link-id link)))
                                           "topic" topic "msg" message))
    nil))

(defun quaternion-yaw (orientation)
  "The yaw of a quaternion object, as an odometry or pose message carries it."
  (let ((x (coerce (or (ignore-errors (nlk:json-value orientation :number "x")) 0) 'double-float))
        (y (coerce (or (ignore-errors (nlk:json-value orientation :number "y")) 0) 'double-float))
        (z (coerce (or (ignore-errors (nlk:json-value orientation :number "z")) 0) 'double-float))
        (w (coerce (or (ignore-errors (nlk:json-value orientation :number "w")) 1) 'double-float)))
    (atan (* 2d0 (+ (* w z) (* x y)))
          (- 1d0 (* 2d0 (+ (* y y) (* z z)))))))

(defun parse-pose (message)
  "MESSAGE as the cell's one pose object: nav_msgs/Odometry, a PoseStamped,
or turtlesim/Pose."
  (cond ((and (hash-table-p message) (hash-table-p (gethash "pose" message)))
         (let* ((pose (gethash "pose" message))
                (body (if (hash-table-p (gethash "pose" pose)) (gethash "pose" pose) pose))
                (position (and (hash-table-p body) (gethash "position" body)))
                (twist (or (and (hash-table-p (gethash "twist" pose)) (gethash "twist" pose))
                           (and (hash-table-p (gethash "twist" message)) (gethash "twist" message))))
                (yaw (quaternion-yaw (and (hash-table-p body) (gethash "orientation" body)))))
           (unless (and (hash-table-p position) (gethash "x" position))
             (fail "that message carries no pose (expected nav_msgs/Odometry, a PoseStamped or turtlesim/Pose)"))
           (pose-object (nlk:json-value position :number "x") (nlk:json-value position :number "y") yaw
                         (ignore-errors (nlk:json-value twist :number "linear" "x"))
                         (ignore-errors (nlk:json-value twist :number "angular" "z")))))
        ((and (hash-table-p message) (gethash "theta" message) (gethash "x" message))
         (pose-object (nlk:json-value message :number "x") (nlk:json-value message :number "y")
                      (nlk:json-value message :number "theta")
                      (ignore-errors (nlk:json-value message :number "linear_velocity"))
                      (ignore-errors (nlk:json-value message :number "angular_velocity"))))
        (t (fail "that message carries no pose (expected nav_msgs/Odometry, a PoseStamped or turtlesim/Pose)"))))

(defun pose-object (x y yaw &optional linear angular)
  "One pose object, whatever message it was read off."
  (nlk:make-json-object
   "x" (round-to x 4) "y" (round-to y 4) "yaw_rad" (round-to yaw 4)
   "yaw_deg" (round-to (* (/ 180 pi) yaw) 1)
   "linear_vel" (round-to (or linear 0) 4) "angular_vel" (round-to (or angular 0) 4)
   "frame_id" "odom" "age_s" 0d0 "collided" :null "source" "rosbridge"))

(defmethod rb-pose ((link link) topic timeout)
  (parse-pose (rb-echo link (or topic (setting :odom)) "nav_msgs/Odometry" timeout)))

(defmethod rb-latest-pose ((link link))
  (when (link-open link)
    (let ((entry (assoc (norm-topic (setting :odom)) (link-messages link) :test #'equal)))
      (when entry (ignore-errors (parse-pose (cdr entry)))))))

(defun decode-camera (message)
  "MESSAGE as (VALUES OCTETS DESCRIPTION): a CompressedImage's bytes as they
are, an Image's samples written out as a PNG."
  (let* ((data (and (hash-table-p message) (gethash "data" message)))
         (format (and (hash-table-p message) (gethash "format" message)))
         (encoding (and (hash-table-p message) (gethash "encoding" message)))
         (octets (and (stringp data) (cl-base64:base64-string-to-usb8-array data))))
    (unless octets (fail "the camera message carried no data"))
    (cond ((and (>= (length octets) 8) (equal (subseq octets 0 8) #(137 80 78 71 13 10 26 10)))
           (values octets "camera frame, PNG"))
          ((and (>= (length octets) 3) (= (aref octets 0) 255) (= (aref octets 1) 216))
           (values octets (format nil "camera frame, JPEG~@[ (~a)~]" format)))
          (t
           ;; a raw sensor_msgs/Image: the samples run red first, or blue first
           (let* ((width (or (ignore-errors (nlk:json-value message :integer "width")) 0))
                  (height (or (ignore-errors (nlk:json-value message :integer "height")) 0))
                  (channels (cond ((member encoding '("rgb8" "bgr8") :test #'equal) 3)
                                  ((member encoding '("rgba8" "bgra8") :test #'equal) 4)
                                  (t 1))))
             (unless (and (plusp width) (plusp height)
                          (>= (length octets) (* width height channels)))
               (fail "the camera frame is ~a bytes of ~a, which is not a picture this cell can write"
                     (length octets) (or encoding "an unknown encoding")))
             (let ((pixels (nle::blank-pixels width height (if (= channels 4) 4 3))))
               (let ((target (nle::pixels-data pixels))
                     (blue-first (member encoding '("bgr8" "bgra8") :test #'equal)))
                 (dotimes (index (* width height))
                   (let ((from (* index channels))
                         (to (* index (if (= channels 4) 4 3))))
                     (if blue-first
                         (progn (setf (aref target to) (aref octets (+ from 2))
                                      (aref target (1+ to)) (aref octets (1+ from))
                                      (aref target (+ to 2)) (aref octets from))
                                (when (= channels 4) (setf (aref target (+ to 3)) (aref octets (+ from 3)))))
                         (loop for k below (if (= channels 4) 4 3)
                               do (setf (aref target (+ to k)) (aref octets (+ from k)))))))
                 (values (nle::pixels-png pixels)
                         (format nil "camera frame, ~ax~a ~a" width height (or encoding "gray"))))))))))

(defmethod rb-camera ((link link) topic timeout)
  (decode-camera (rb-echo link (or topic (setting :camera)) "sensor_msgs/Image" timeout)))

(defmethod rb-status ((link link))
  (nlk:make-json-object "backend" "rosbridge" "url" (link-url link) "connected" (if (link-open link) t :null)))

(defmethod rb-sleep ((link link) seconds)
  (declare (ignore link))
  (sleep seconds))

(defmethod rb-close ((link link)) (link-close link))
