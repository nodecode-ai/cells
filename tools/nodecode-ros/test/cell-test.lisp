;;;; cell-test.lisp --- the nine tools, the interlock, the command, both backends.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is what the plugin gave a user: the robot's topics,
;;;; services and nodes are listed; a drive moves the robot and stops it; an
;;;; over-speed or over-long command is refused with the rule named, or
;;;; clamped when the section says clamp; a latched e-stop blocks motion and
;;;; service calls until a human clears it; a write outside the allowlist is
;;;; refused; a snapshot is a PNG under the home; and the rosbridge backend
;;;; speaks advertise/publish/subscribe over the socket seam. The mock sim is
;;;; real Lisp, so nothing here is stubbed but the socket.

(in-package #:nodecode.test)

;;; The verbs signal ROS-ERROR on a refusal -- the tool wrappers are what turn
;;; one into the JSON a model reads -- so a refusal is asserted through the
;;; text the model would be given.

(defmacro ros-blocked (form)
  "What FORM refuses with, as the model reads it."
  `(refusal-text nodecode-ros:ros-error ,form))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "ros"
  (:help :ros)
  (:command "ros")
  (:running (is (equal "mock" (nodecode-ros::setting :backend)) "the sim is the default backend")
            (is (= 9090 (nodecode-ros::setting :port)) "and 9090 the rosbridge port"))
  (:refused ("backend" "rosbridge" "host" "ws://robot.example:9090")))

;;; --- discovery ---------------------------------------------------------------------

(deftest ros-cell-the-mock-robot-lists-what-it-exposes ()
  (with-ros ()
    (let ((listing (ros:topics "topics" "")))
      (is (eq t (ros-field listing "ok")) "the listing answered")
      (is (member "/cmd_vel" (ros-names listing "topics") :test #'equal) "the velocity topic is there")
      (is (member "/odom" (ros-names listing "topics") :test #'equal) "and odometry"))
    (let ((services (ros-names (ros:topics "services" "") "services"))
          (nodes (ros-names (ros:topics "nodes" "") "nodes"))
          (camera (ros-names (ros:topics "topics" "camera") "topics")))
      (is (member "/mock/teleport" services :test #'equal) "the mock services are listed")
      (is (member "/mock_robot" nodes :test #'equal) "and its nodes")
      (is (equal '("/camera/image_raw") camera) "a filter keeps only what matches"))
    (is (search "mock" (ros:status)) "status names the backend")))

(deftest ros-cell-the-tools-answer-through-their-contracts ()
  (with-ros ()
    (let ((listing (nodecode-ros::topics-tool
                    (nlk:contract-decode (nlk:contract 'nodecode-ros::ros-topics)
                                         (nlk:make-json-object "kind" "topics"))))
          (pose (nodecode-ros::pose-tool
                 (nlk:contract-decode (nlk:contract 'nodecode-ros::ros-get-pose)
                                      (nlk:make-json-object))))
          (refused (nodecode-ros::drive-tool
                    (nlk:contract-decode (nlk:contract 'nodecode-ros::ros-drive)
                                         (nlk:make-json-object "linear" 9)))))
      (is (eq t (ros-field listing "ok")) "ros_list answers a contract record")
      (is (search "yaw_rad" pose) "ros_get_pose answers the pose object")
      (is (search "limits" (nodecode-ros::status-tool nil)) "ros_status answers the safety state")
      (is (search "over_speed_linear" (ros-field refused "error")) "a refused drive answers JSON, naming the rule"))))

;;; --- motion ------------------------------------------------------------------------

(deftest ros-cell-a-drive-moves-the-robot-and-stops-it ()
  (with-ros ()
    (let ((answer (ros:drive 0.3 0 1)))
      (is (eq t (ros-field answer "ok")) "the drive ran")
      (is (search "completed" answer) "and finished its duration")
      (is (> (ros-field answer "displacement_m") 0.2) "the robot moved")
      (is (search "yaw_rad" (ros:pose)) "the pose is readable afterwards"))
    (is (search "over_speed_linear" (ros-blocked (ros:drive 2 0 1))) "too fast is refused, naming the rule")
    (is (search "over_duration" (ros-blocked (ros:drive 0.2 0 30))) "too long is refused")
    (is (search "over_speed_linear" (nodecode-ros::run-slash "log" "s1")) "and the audit keeps it")))

(deftest ros-cell-clamp-mode-caps-instead-of-refusing ()
  (with-ros ("safety" (nlk:make-json-object "over_limit_action" "clamp"))
    (let ((answer (ros:drive 2 0 0.2)))
      (is (eq t (ros-field answer "ok")) "the drive ran")
      (is (search "linear speed clamped" answer) "and says what it clamped"))))

(deftest ros-cell-the-estop-latches-until-a-human-clears-it ()
  (with-ros ()
    (is (search "e-stop latched" (ros:stop t "something looks unsafe")) "the model latched it")
    (is (search "estop_latched" (ros-blocked (ros:drive 0.2 0 1))) "and motion is refused")
    (is (search "estop_latched" (ros-blocked (ros:call "/mock/reset"))) "and service calls with it")
    (is (search "E-stop cleared" (nodecode-ros::run-slash "clear" "s1")) "a human clears it")
    (is (eq t (ros-field (ros:drive 0.2 0 0.2) "ok")) "and the robot moves again")))

;;; --- the allowlist ------------------------------------------------------------------

(deftest ros-cell-writes-are-confined-to-the-allowlist ()
  (with-ros ()
    (let ((topic-refusal (ros-blocked (ros:publish "/sprayer" "std_msgs/Bool"
                                                   (nlk:make-json-object "data" t))))
          (service-refusal (ros-blocked (ros:call "/system/shutdown"))))
      (is (search "topic_not_allowlisted" topic-refusal) "a topic nobody allowlisted is refused")
      (is (search "service_not_allowlisted" service-refusal) "and so is a service"))
    (is (eq t (ros-field (ros:call "/mock/reset") "ok")) "the mock services are allowed")
    (let ((answer (ros:publish "/cmd_vel" "geometry_msgs/Twist"
                               (nlk:make-json-object
                                "linear" (nlk:make-json-object "x" 0.2 "y" 0 "z" 0)
                                "angular" (nlk:make-json-object "x" 0 "y" 0 "z" 0))
                               0.3 10)))
      (is (eq t (ros-field answer "ok")) "an allowlisted twist publishes")
      (is (plusp (ros-field answer "published")) "and is repeated for its duration"))))

;;; --- the camera ---------------------------------------------------------------------

(defun ros-octets (path &optional (count 8))
  "The first COUNT bytes of PATH."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array count :element-type '(unsigned-byte 8))))
      (read-sequence buffer in)
      buffer)))

(deftest ros-cell-a-snapshot-is-a-png-under-the-home ()
  (with-ros ()
    (let* ((answer (ros:snapshot))
           (path (ros-field answer "path")))
      (is (stringp path) "the tool answered a path")
      (is (probe-file path) "and the file is there")
      (is (equalp #(137 80 78 71 13 10 26 10) (ros-octets path)) "written as a PNG")
      (is (search "MEDIA:" answer) "with the line that shows it to the operator"))))

;;; --- the rosbridge half -------------------------------------------------------------

(deftest ros-cell-the-rosbridge-path-speaks-the-protocol ()
  (with-fake-rosbridge
      (("/rosapi/topics_and_raw_types"
        (nlk:make-json-object "topics" #("/cmd_vel") "types" #("geometry_msgs/Twist")))
       ("/rosapi/services" (nlk:make-json-object "services" #("/robot/reset") "types" #())))
    (with-ros ("backend" "rosbridge" "host" "robot.example" "port" 9091)
      (is (member "/cmd_vel" (ros-names (ros:topics "topics" "") "topics") :test #'equal) "the robot's topics come back through /rosapi")
      (is (equal "ws://robot.example:9091" *connected-url*) "the configured url is opened")
      (ros:drive 0.2 0 0.2)
      (is (= 1 (length (ros-protocol "advertise" "/cmd_vel"))) "the velocity topic is advertised once")
      (is (plusp (length (ros-protocol "publish" "/cmd_vel"))) "and the twist is published")
      (is (search "yaw_rad" (ros:pose)) "a subscribed odometry message becomes a pose"))))

;;; --- the command, and the guard -----------------------------------------------------

(deftest ros-cell-the-slash-command-answers-the-operator ()
  (with-ros ()
    (is (search "backend: mock" (cell-entry "nodecode-ros" "ros" "" "s1")) "bare /ros shows the link")
    (is (search "limits:" (cell-entry "nodecode-ros" "ros" "status" "s1")) "and its limits")
    (let ((latched (cell-entry "nodecode-ros" "ros" "estop the test tripped it" "s1"))
          (unknown (cell-entry "nodecode-ros" "ros" "wat" "s1")))
      (is (search "LATCHED" latched) "the operator can latch the e-stop")
      (is (search "not a subcommand" unknown) "an unknown subcommand says so"))))

(deftest ros-cell-the-verbs-refuse-while-not-running ()
  (is (search "not running" (refusal-text nodecode-ros:ros-error (nodecode-ros::setting :port))) "a verb with nothing started names the cell and the way back"))
