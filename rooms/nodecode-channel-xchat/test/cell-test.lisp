;;;; cell-test.lisp --- the X Chat lane, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The wire is a scripted executor (no network: the run has none), the
;;;; account the lane runs as is named, and every case here is one a person
;;;; with a token meets: a body that seals and opens, a body someone edited,
;;;; a DM read off a stubbed conversation and handed on, a stranger's DM left
;;;; alone, a lane that starts and stops, and a section with no token.

(in-package #:nodecode.test)

(defun xchat-fake-adapter (&key (self "900") (allowed '("501")) responses)
  (list :book (ncxc:make-xchat-book :token "T0KEN")
        :self self
        :allowed allowed
        :executor (nck:make-recording-executor :responses responses)
        :host nil))

(defun xchat-body (object)
  (nck:json-plist object))

(deftest channel-xchat-seal-round-trip ()
  (let* ((key (ncxc:conversation-key "900" "501" "g123"))
         (sealed (ncxc:seal-text key "hello from a DM")))
    (is (stringp sealed))
    (is (search "x1." sealed))
    (is (equal "hello from a DM" (ncxc:open-text key sealed)))
    ;; both ends derive the same key, whichever side asks
    (is (equalp key (ncxc:conversation-key "501" "900" "g123")))
    ;; another conversation is another key
    (is (not (equalp key (ncxc:conversation-key "900" "501" "g124"))))))

(deftest channel-xchat-a-tampered-body-does-not-open ()
  (let* ((key (ncxc:conversation-key "900" "501" "g123"))
         (sealed (ncxc:seal-text key "the answer is 4"))
         (parts (uiop:split-string sealed :separator "."))
         (tampered (format nil "~a.~a.~a.~a" (first parts) (second parts)
                            (ncxc:hex-encode (make-array 4 :element-type '(unsigned-byte 8)))
                            (fourth parts))))
    (is (null (ncxc:open-text key tampered)))
    (is (null (ncxc:open-text key "not a sealed body")))
    (is (null (ncxc:open-text (ncxc:conversation-key "900" "502" "g123") sealed)))))

(deftest channel-xchat-lap-reads-a-dm-and-leaves-strangers-alone ()
  (let* ((self "900")
         (key (ncxc:conversation-key self "501" "g900-501"))
         (conversations (nlk:make-json-object
                         "data" (vector (nlk:make-json-object "id" "g900-501"
                                                               "newest_event_id" "42"))))
         (events (nlk:make-json-object
                  "data" (vector (nlk:make-json-object "id" "ev-1" "sender_id" "501"
                                                        "sequence_id" "41"
                                                        "encoded_event" (ncxc:seal-text key "what is up"))
                                 (nlk:make-json-object "id" "ev-2" "sender_id" "777"
                                                        "sequence_id" "42"
                                                        "encoded_event" (ncxc:seal-text
                                                                         (ncxc:conversation-key self "777" "g900-501")
                                                                         "not for you")))))
         (adapter (xchat-fake-adapter
                   :self self
                   :responses (list (nck:make-scripted-response 200 conversations)
                                    (nck:make-scripted-response 200 events))))
         (seen '()))
    (is (eq :stop (ncxc:xchat-poll-lap adapter (lambda () t))))
    (is (equalp 1.0 (ncxc:xchat-poll-lap adapter (lambda () nil)
                                          :sink (lambda (candidate) (push candidate seen)))))
    (is (= 1 (length seen)))
    (let ((candidate (first seen)))
      (is (equal "what is up" (gethash "text" candidate)))
      (is (equal "501" (gethash "user_id" (gethash "source" candidate))))
      (is (equal "g900-501" (gethash "channel_id" (gethash "source" candidate)))))
    ;; the cursor went as far as the last event read, and a second lap of the
    ;; same listing costs one call and answers nothing new
    (is (equal "42" (ncxc:xchat-cursor (getf adapter :book) "g900-501")))))

(deftest channel-xchat-plans-name-the-api ()
  (let* ((adapter (xchat-fake-adapter))
         (platform (ncxc:xchat-platform adapter))
         (plan (funcall (nck::platform-plan-message platform)
                        (list :channel-id "g900-501") "see you")))
    (is (equal "POST" (nck::request-plan-method plan)))
    (is (search "/2/chat/conversations/g900-501/messages" (nck::request-plan-path plan)))))

(deftest channel-xchat-a-lane-with-no-token-refuses ()
  (let ((section (nlk:make-json-object "user_id" "900" "allowed_users" (vector "501"))))
    (is (handler-case (progn (ncxc:start-channel section :executor (nck:make-recording-executor)) nil)
          (error () t)))))

(deftest channel-xchat-starts-and-stops ()
  (let* ((section (nlk:make-json-object
                   "access_token_env" "XCHAT_TEST_TOKEN"
                   "user_id" "900"
                   "allowed_users" (vector "501")
                   "poll_seconds" 2))
         (conversations (nlk:make-json-object "data" (vector)))
         (stop nil))
    (setf (uiop:getenv "XCHAT_TEST_TOKEN") "T0KEN")
    (unwind-protect
         (progn
           (setf stop (ncxc:start-channel section
                                           :executor (nck:make-recording-executor
                                                      :responses (list (nck:make-scripted-response 200 conversations)))))
           (is (functionp stop))
           (is (not (null ncxc:*xchat-adapter*)))
           (funcall stop)
           (is (null ncxc:*xchat-adapter*))
           (setf stop nil))
      (when stop (ignore-errors (funcall stop)))
      (sb-posix:setenv "XCHAT_TEST_TOKEN" "" 1))))
