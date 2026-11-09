;;;; package.lisp --- the ROS package: nine verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed ROS so an eval form reads (ros:pose). Every
;;;; exported verb answers a JSON string, the shape the plugin's tools
;;;; answered, and every refusal is ROS-ERROR, rendered by the eval snippet as
;;;; "ERROR: ROS-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `ros' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :PORT).

(defpackage #:nodecode-ros
  (:use #:cl)
  (:nicknames #:ros)
  (:export
   ;; the nine verbs, each one a JSON string
   #:topics #:echo #:drive #:publish #:call #:pose #:snapshot #:stop #:status
   ;; what /ros runs
   #:run-slash
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:ros-error))

(in-package #:nodecode-ros)

(nlk:define-peripheral ros :not-running t)

;;; --- the small words every file uses ------------------------------------------------

(defun round-to (value places)
  "VALUE rounded to PLACES after the point."
  (let ((scale (expt 10 places)))
    (/ (coerce (round (* (coerce value 'double-float) scale)) 'double-float) scale)))

(defun norm-topic (name)
  "NAME as a ROS name: trimmed, one leading slash, never empty."
  (let ((name (nlk:trimmed (or name ""))))
    (cond ((zerop (length name)) "/")
          ((char= (char name 0) #\/) name)
          (t (concatenate 'string "/" name)))))

(defun or-topic (topic default)
  "TOPIC as a ROS name, or DEFAULT when TOPIC is absent or empty."
  (norm-topic (if (and topic (plusp (length (nlk:trimmed (princ-to-string topic)))))
                   topic
                   default)))

(defun glob-match-p (text pattern)
  "Whether TEXT matches PATTERN: * is any run, ? any one character."
  (labels ((walk (i j)
             (cond ((= j (length pattern)) (= i (length text)))
                   ((char= (char pattern j) #\*)
                    (or (walk i (1+ j))
                        (and (< i (length text)) (walk (1+ i) j))))
                   ((and (< i (length text))
                         (or (char= (char pattern j) #\?)
                             (char-equal (char pattern j) (char text i))))
                    (walk (1+ i) (1+ j)))
                   (t nil))))
    (walk 0 0)))

(defun matches-any (name patterns)
  "Whether NAME matches one of PATTERNS, each read as a ROS name first."
  (some (lambda (pattern) (glob-match-p name (norm-topic pattern))) patterns))

(defun string-list (value)
  "VALUE as a list of strings: a config list arrives as a vector."
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) value)
        ((stringp value) (list value))
        (t '())))
