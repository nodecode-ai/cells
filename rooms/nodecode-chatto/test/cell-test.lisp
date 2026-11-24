;;;; cell-test.lisp --- the Chatto lane against a scripted executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call is a scripted plan executor's; nothing reaches a server, and
;;;; the password is a file in the run's own temporary folder.

(in-package #:nodecode.test)

(define-test-slice "chatto" "CHATTO-CELL-")

(defun chatto-password-file ()
  "A password file of this run's own, holding one fixed password."
  (let ((path (merge-pathnames "chatto-cell-password" (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "hunter2~%"))
    (namestring path)))

(defun chatto-section (&rest pairs)
  "A channels.chatto section pointed at a make-believe server, PAIRS over it."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "url" section "https://chat.example.com")
    (alexandria:ensure-gethash "login" section "bot")
    (alexandria:ensure-gethash "password_file" section (chatto-password-file))
    section))

(defun chatto-message (id body &key (actor "u1") (login "kim") thread)
  "One room event carrying a message, as GetRoomEvents spells it."
  (let ((msg (nlk:json-object "id" id "body" body "actorId" actor "actorLogin" login))
        (event (nlk:json-object "id" (format nil "e-~a" id))))
    (when thread (setf (gethash "thread" msg) (nlk:json-object "threadRootEventId" thread)))
    (setf (gethash "messagePosted" event) (nlk:json-object "message" msg))
    event))

(defun chatto-rooms-answer (&rest rooms)
  (nlk:json-object "rooms" (coerce rooms 'vector)))

(defun chatto-room (id &key (name "general") (kind "ROOM_KIND_GROUP") (member t))
  (nlk:json-object "room" (nlk:json-object "id" id "name" name "kind" kind)
                    "viewerState" (nlk:json-object "isMember" member)))

(defun chatto-events-answer (&rest events)
  (nlk:json-object "page" (nlk:json-object "events" (coerce events 'vector))))

;;; --- the section -----------------------------------------------------------------

(deftest chatto-cell-section-refuses-without-url ()
  (let ((problems (nlk:section-problems (nlk:find-section '("channels" "chatto"))
                                        (nlk:json-object "login" "bot"))))
    (is (plusp (length problems)) "an unset url refuses")
    (is (search "url" (format nil "~{~a~^ ~}" problems)) "and the refusal names the key")))

;;; --- what a message becomes -------------------------------------------------------

(deftest chatto-cell-candidate-carries-the-ask ()
  (let ((candidate (nodecode-chatto:chatto-message-candidate
                     (chatto-message "$1" "@bot why is the build red") "R1" :login "bot")))
    (is (equal "@bot why is the build red" (nlk:json-value candidate :string "text")))
    (is (equal "R1" (nlk:json-value candidate :string "source" "channel_id")))
    (is (eq t (nlk:json-value candidate :boolean "source" "addressed")) "a mention addresses the line")
    (is (equal "channel" (nlk:json-value candidate :string "source" "chat_kind")))
    (is (equal "kim" (nlk:json-value candidate :string "source" "user_name")))))

(deftest chatto-cell-unaddressed-room-line-is-not-taken ()
  (let ((candidate (nodecode-chatto:chatto-message-candidate
                     (chatto-message "$2" "lunch?") "R1" :login "bot")))
    (is (null (nlk:json-value candidate :boolean "source" "addressed")))
    (let ((direct (nodecode-chatto:chatto-message-candidate
                   (chatto-message "$3" "lunch?") "D1" :login "bot" :dm-p t)))
      (is (eq t (nlk:json-value direct :boolean "source" "addressed")) "a direct message is addressed")
      (is (equal "direct_message" (nlk:json-value direct :string "source" "chat_kind"))))))

(deftest chatto-cell-mention-is-stripped ()
  (is (equal "deploy it" (nodecode-chatto:chatto-strip-mention "@bot: deploy it" "bot")))
  (is (eq t (nodecode-chatto:chatto-mention-p "hi @bot" "bot")))
  (is (null (nodecode-chatto:chatto-mention-p "hi robot" "bot"))))

;;; --- the plans --------------------------------------------------------------------

(deftest chatto-cell-message-plan-posts-to-create-message ()
  (let* ((adapter (nodecode-chatto::make-chatto-adapter :login "bot" :token "t0k"
                                                        :base-url "https://chat.example.com"))
         (plan (nodecode-chatto:chatto-message-plan adapter (list :channel-id "R1") "hello"
                                                    :reply-to "e-1")))
    (is (search "CreateMessage" (nck::request-plan-path plan)) "the answer is a CreateMessage")
    (is (equal "R1" (nlk:json-value (nck::request-plan-body plan) :string "roomId")))
    (is (equal "e-1" (nlk:json-value (nck::request-plan-body plan) :string "threadRootEventId")))
    (is (equal "Bearer t0k" (cdr (assoc "authorization" (nck::request-plan-headers plan)
                                          :test #'string=))) "the token rides the plan, never the body")))

;;; --- the lap ----------------------------------------------------------------------

(deftest chatto-cell-lap-marks-and-then-answers ()
  (let* ((executor
           (nck:make-recording-executor
            :responses
            (list (nck:make-scripted-response 200 (chatto-rooms-answer (chatto-room "R1")))
                  (nck:make-scripted-response 200 (chatto-events-answer (chatto-message "$old" "before")))
                  (nck:make-scripted-response 200 (chatto-rooms-answer (chatto-room "R1")))
                  (nck:make-scripted-response
                   200 (chatto-events-answer (chatto-message "$old" "before")
                                             (chatto-message "$new" "@bot hi"))))))
         (adapter (nodecode-chatto::make-chatto-adapter :login "bot" :token "t0k"
                                                        :base-url "https://chat.example.com"))
         (host (nck:make-host-from-section (chatto-section)
                                           (nodecode-chatto:chatto-platform :adapter adapter)
                                           :executor executor
                                           :policy (nodecode-chatto:chatto-policy adapter '())
                                           :owners '("kim"))))
    (setf (nodecode-chatto::chatto-adapter-host adapter) host)
    (is (integerp (nodecode-chatto:chatto-lap adapter (constantly nil))) "the first lap polls")
    (is (null (nodecode-chatto::chatto-adapter-first-lap adapter)) "and marks where the room stood")
    (is (integerp (nodecode-chatto:chatto-lap adapter (constantly nil))) "the second lap polls too")
    (is (nck:channel-status "chatto") "the lap left the channel reporting a state")))

;;; --- start and stop ---------------------------------------------------------------

(deftest chatto-cell-starts-and-stops ()
  (let ((executor (nck:make-recording-executor
                   :responses
                   (list
                    (nck:make-scripted-response 200 (nlk:json-object "success" t "token" "t0k"))))))
    (let ((stop (nodecode-chatto:start-channel
                 (chatto-section "allowed_users" (vector "kim") "owner" (vector "kim"))
                 :executor executor)))
      (is (functionp stop) "the lane started")
      (is (eq t (and (nck:channel-status "chatto") t)) "the channel reports a state")
      (is (eq t (funcall stop)) "and stops")
      (is (null nodecode-chatto::*chatto-adapter*) "with no lane left behind"))))

;;; --- a refused start --------------------------------------------------------------

(deftest chatto-cell-start-refuses-without-an-allowlist ()
  (let ((refused
          (handler-case
              (progn
                (nodecode-chatto:start-channel (chatto-section) :executor (nck:make-recording-executor))
                nil)
            (error (condition) (princ-to-string condition)))))
    (is (and refused (search "refuses to start open" refused)) "no allowlist, no lane")))
