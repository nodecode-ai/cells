;;;; cell-test.lisp --- the MQTT cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a socket: the publisher, the reader and the prober
;;;; are bound to scripted answers, so "what would have gone on the wire" and
;;;; "what the tool answered" are facts a test reads. The gates -- a literal
;;;; publish topic, the two allowlists, the secret scan, the size cap, a read
;;;; no wider than what it was given -- are the original plugin's, and each
;;;; refusal is asserted to reach no wire at all.

(in-package #:nodecode.test)

(define-test-slice "mqtt" "MQTT-CELL-" :start nodecode-mqtt:start-cell)

(defparameter +url+ "tcp://127.0.0.1:1883"
  "A broker a test points the cell at: nothing dials it.")

(defparameter +topics+ (vector "demo/#"))

(defun has-all (text &rest needles)
  "Whether TEXT carries every NEEDLE: one assertion for a whole answer."
  (and (stringp text) (every (lambda (needle) (search needle text)) needles)))

(defmacro refused (&body body)
  "BODY's refusal, as the text a caller reads."
  `(handler-case (progn ,@body "no refusal")
     (mqtt:mqtt-error (e) (princ-to-string e))))

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "mqtt"
  (:config "url" +url+ "publish_allowed" +topics+ "subscribe_allowed" +topics+)
  (:help :mqtt)
  (:command "mqtt")
  (:running (is (equal "nodecode" (nodecode-mqtt::setting :username)) "the username defaults")
            (is (= 32768 (nodecode-mqtt::setting :max-payload-bytes)) "and the payload cap")
            (is (equal '("demo/#") (nodecode-mqtt::allowlist 'publish)) "the allowlist reads back"))
  (:refused ("timeout_seconds" 0) ("max_payload_bytes" 1)))

(deftest mqtt-cell-a-section-that-names-no-broker-refuses ()
  ;; url is the one required member: without it there is nothing to talk to.
  (with-cell-stop ((mqtt-start "publish_allowed" +topics+))
    (is (has-all (refused (mqtt:publish "demo/x" "hi"))
                 "no MQTT broker is set" "url to the mqtt section") "a section that names no broker refuses every verb")))

(defmacro with-mqtt (() &body body)
  "BODY with the cell started on a broker nothing dials and demo/# allowed."
  `(with-cell-stop ((mqtt-start "url" +url+
                                 "publish_allowed" +topics+
                                 "subscribe_allowed" +topics+))
     ,@body))

;;; --- publishing ----------------------------------------------------------------

(deftest mqtt-cell-publish-wraps-the-event-and-is-allowlisted ()
  (with-mqtt ()
    (let* ((sent '())
           (nodecode-mqtt:*publisher*
             (lambda (settings topic body &key retain)
               (declare (ignore settings retain))
               (push (list topic body) sent)
               (list :bytes (length body)))))
      (is (has-all (mqtt:publish "demo/x" '(:ok t) :kind "reading")
                   "Published" "demo/x" "QoS 1" "acked") "the ack is the answer")
      (is (= 1 (length sent)) "one publish reached the wire")
      (let ((topic (first (first sent))) (body (second (first sent))))
        (is (equal "demo/x" topic) "the literal topic")
        (is (has-all body "nodecode-mqtt.v1" "reading" "nonce" "data") "the envelope: schema, kind, nonce, and the data"))
      (is (has-all (refused (mqtt:publish "other/x" "hi"))
                   "not in mqtt.publish_allowed" "deny by default") "a topic outside the allowlist is refused")
      (is (has-all (refused (mqtt:publish "demo/#" "hi")) "literal" "no + or #") "a publish topic may not be widened")
      (is (has-all (refused (mqtt:publish "demo/x" "ghp_aaaaaaaaaaaaaaaaaaaaaaaaaa"))
                   "looks like a credential") "a payload shaped like a token is refused")
      (is (= 1 (length sent)) "a refused publish never reaches the wire"))))

(deftest mqtt-cell-publish-refuses-more-bytes-than-the-cap ()
  (with-cell-stop ((mqtt-start "url" +url+ "publish_allowed" +topics+
                                "subscribe_allowed" +topics+ "max_payload_bytes" 64))
    (let ((nodecode-mqtt:*publisher* (lambda (settings topic body &key retain)
                                       (declare (ignore settings topic body retain))
                                       (list :bytes 1))))
      (is (has-all (refused (mqtt:publish "demo/x" (make-string 200 :initial-element #\a)))
                   "max_payload_bytes is 64") "the size cap is the section's"))))

;;; --- reading --------------------------------------------------------------------

(deftest mqtt-cell-read-collects-a-topic-and-answers-it ()
  (with-mqtt ()
    (let* ((asked nil)
           (nodecode-mqtt:*reader*
             (lambda (settings filter &key count timeout)
               (declare (ignore settings))
               (setf asked (list filter count timeout))
               (list (list :topic "demo/a" :payload "21" :retained nil)
                     (list :topic "demo/b" :payload "22" :retained t)))))
      (is (has-all (mqtt:read-topics "demo/#" :count 5)
                   "2 messages on demo/#" "demo/a: 21" "demo/b (retained): 22") "both messages come back")
      (is (equal '("demo/#" 5 10) asked) "the filter, the count and the section's timeout")
      (is (has-all (refused (mqtt:read-topics "other/#"))
                   "not in mqtt.subscribe_allowed") "a filter outside the allowlist is refused"))
    (let ((nodecode-mqtt:*reader* (lambda (settings filter &key count timeout)
                                    (declare (ignore settings filter count timeout))
                                    '())))
      (is (has-all (mqtt:read-topics "demo/none" :timeout 2)
                   "No messages on demo/none within 2s") "an empty read says so"))))

(deftest mqtt-cell-read-will-not-widen-the-filter ()
  ;; the allowlist is one level; # would reach past it.
  (with-cell-stop ((mqtt-start "url" +url+ "publish_allowed" +topics+
                                "subscribe_allowed" (vector "demo/+")))
    (let ((nodecode-mqtt:*reader* (lambda (settings filter &key count timeout)
                                    (declare (ignore settings filter count timeout))
                                    '())))
      (is (has-all (refused (mqtt:read-topics "demo/#")) "not in mqtt.subscribe_allowed") "a filter wider than the allowlist is refused")
      (is (has-all (mqtt:read-topics "demo/temp" :timeout 1) "No messages on demo/temp") "a narrower one is read"))))

;;; --- /mqtt -----------------------------------------------------------------------

(deftest mqtt-cell-the-command-says-where-the-broker-is ()
  (with-mqtt ()
    (is (has-all (cell-entry "nodecode-mqtt" "mqtt" "" "s1")
                 "mqtt: tcp://127.0.0.1:1883" "as nodecode"
                 "publish: demo/#" "subscribe: demo/#") "the command reports the section")
    (let ((nodecode-mqtt:*prober*
            (lambda (settings)
              (declare (ignore settings))
              "the MQTT broker at tcp://127.0.0.1:1883 accepted the connection")))
      (is (has-all (cell-entry "nodecode-mqtt" "mqtt" "ping" "s1")
                   "accepted the connection") "/mqtt ping probes"))
    (let ((nodecode-mqtt:*prober*
            (lambda (settings)
              (declare (ignore settings))
              (nodecode-mqtt::fail "cannot reach the MQTT broker at ~a: refused"
                                    "tcp://127.0.0.1:1883"))))
      (is (has-all (cell-entry "nodecode-mqtt" "mqtt" "ping" "s1") "no answer:") "a probe that cannot connect answers, it does not throw"))))
