;;;; support.lisp --- ros test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; ros tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a ROS-CELL- name
;;;; prefix; RUN-ROS-TESTS runs exactly that slice.
;;;;
;;;; The cell's one outside thing is a websocket, so that is what the helpers
;;;; stub: *CONNECT* is bound to FAKE-CONNECT, which answers the rosbridge
;;;; protocol from a table of canned replies, so a rosbridge test drives the
;;;; whole path without a socket. Everything else runs for real: the mock sim
;;;; is pure Lisp, and a snapshot is a PNG written under this run's home.

(in-package #:nodecode.test)

(define-test-slice "ros" "ROS-CELL-" :start nodecode-ros:start-cell
  :defaults (("backend" "mock")))

(defmacro with-ros ((&rest config) &body body)
  "BODY with the cell started on CONFIG -- mock backend by default -- and
stopped afterwards, and no e-stop latch left from an earlier test."
  `(progn (ignore-errors (delete-file (nodecode-ros::estop-file)))
          (with-cell-stop ((ros-start ,@config))
            ,@body)))

(defun ros-json (text)
  "TEXT as the object a tool answered, or NIL."
  (let ((object (ignore-errors (shasht:read-json (princ-to-string text)))))
    (and (hash-table-p object) object)))

(defun ros-field (text key)
  (let ((object (ros-json text)))
    (and object (gethash key object))))

(defun ros-names (text key)
  "The names under KEY of what a listing answered, as a list of strings."
  (let ((items (ros-field text key)))
    (if (vectorp items)
        (loop for item across items
              collect (if (hash-table-p item) (gethash "name" item) (princ-to-string item)))
        '())))

;;; --- the rosbridge seam ------------------------------------------------------------

(defvar *sent* '() "Every protocol message the fake socket was handed, newest first.")
(defvar *connected-url* nil "The url the cell asked to open.")
(defvar *replies* (make-hash-table :test #'equal)
  "Canned service answers, by service name, as the values object.")

(defun fake-reply (service values)
  "Answer SERVICE with VALUES while the fake socket is in use."
  (setf (gethash service *replies*) values))

(defun fake-answer (text)
  "What the peer answers to one protocol message, or NIL."
  (let ((message (ignore-errors (shasht:read-json text))))
    (when (hash-table-p message)
      (let ((op (gethash "op" message))
            (id (gethash "id" message)))
        (cond ((and (equal op "call_service") (gethash (gethash "service" message) *replies*))
               (list :service-response id (gethash (gethash "service" message) *replies*)))
              ((and (equal op "subscribe") (equal (gethash "topic" message) "/odom"))
               (list :publish "/odom" (nlk:make-json-object
                                       "pose" (nlk:make-json-object
                                               "pose" (nlk:make-json-object
                                                       "position" (nlk:make-json-object "x" 1 "y" 2 "z" 0)
                                                       "orientation" (nlk:make-json-object "x" 0 "y" 0 "z" 0 "w" 1)))
                                       "twist" (nlk:make-json-object
                                                "twist" (nlk:make-json-object
                                                         "linear" (nlk:make-json-object "x" 0)
                                                         "angular" (nlk:make-json-object "z" 0))))))
              (t nil))))))

(defun fake-connect (url)
  "The *CONNECT* seam: a socket that records what it is sent and answers from
*REPLIES* synchronously, as a peer would a moment later."
  (setf *connected-url* url)
  (let ((socket nil))
    (setf socket
          (nodecode-ros::make-socket
           :send (lambda (text)
                   (push text *sent*)
                   (let ((answer (fake-answer text)))
                     (when answer
                       (nodecode-ros::socket-deliver
                        socket
                        (ecase (first answer)
                          (:service-response
                           (nlk:encode-json-object
                            (nlk:make-json-object "op" "service_response" "id" (second answer)
                                                   "result" t "values" (third answer))))
                          (:publish
                           (nlk:encode-json-object
                            (nlk:make-json-object "op" "publish" "topic" (second answer)
                                                   "msg" (third answer)))))))))
           :close (lambda () (setf (nodecode-ros::socket-open socket) nil))))
    socket))

(defmacro with-fake-rosbridge ((&rest canned) &body body)
  "BODY with the rosbridge seam faked: CANNED pairs are (SERVICE VALUES)."
  `(let ((*sent* '())
         (*connected-url* nil)
         (*replies* (make-hash-table :test #'equal))
         (nodecode-ros::*connect* #'fake-connect))
     ,@(loop for (service values) in canned
             collect `(fake-reply ,service ,values))
     ,@body))

(defun ros-protocol (op &optional topic)
  "The protocol messages of OP the fake socket was sent, oldest first."
  (loop for text in (reverse *sent*)
        for message = (ignore-errors (shasht:read-json text))
        when (and (hash-table-p message) (equal op (gethash "op" message))
                  (or (null topic) (equal topic (gethash "topic" message))))
          collect message))
