;;;; cell-test.lisp --- the NATS cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a socket: the publisher, the fetcher and the prober
;;;; are bound to scripted answers, so "what would have gone on the wire" and
;;;; "what the tool answered" are facts a test reads. The gates -- a literal
;;;; subject, the two allowlists, the secret scan, the size cap -- are the
;;;; original plugin's, and each refusal is asserted to reach no wire at all.

(in-package #:nodecode.test)

(define-test-slice "nats" "NATS-CELL-" :start nodecode-nats:start-cell)

(defparameter +servers+ (vector "nats://127.0.0.1:4222")
  "A server a test points the cell at: nothing dials it.")

(defparameter +demo+ (vector "demo.>"))

(defun has-all (text &rest needles)
  "Whether TEXT carries every NEEDLE: one assertion for a whole answer."
  (and (stringp text) (every (lambda (needle) (search needle text)) needles)))

(defmacro refused (&body body)
  "BODY's refusal, as the text a caller reads."
  `(handler-case (progn ,@body "no refusal")
     (nats:nats-error (e) (princ-to-string e))))

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "nats"
  (:config "servers" +servers+ "publish_allowed" +demo+ "subscribe_allowed" +demo+)
  (:help :nats)
  (:command "nats")
  (:running (is (equal "hermes" (nodecode-nats::setting :stream)) "the stream defaults")
            (is (= 32768 (nodecode-nats::setting :max-payload-bytes)) "and the payload cap")
            (is (equal '("demo.>") (nodecode-nats::allowlist 'publish)) "the allowlist reads back"))
  (:refused ("timeout_seconds" 0) ("max_payload_bytes" 1)))

(deftest nats-cell-a-section-that-names-no-server-refuses ()
  ;; servers is the one required member: without it there is nothing to talk to.
  (with-cell-stop ((nats-start "stream" "hermes" "publish_allowed" +demo+))
    (is (has-all (refused (nats:publish "demo.x" "hi"))
                 "no NATS server is set" "servers to the nats section")
        "a section that names no server refuses every verb")))

(defmacro with-nats (() &body body)
  "BODY with the cell started on a server nothing dials and demo.> allowed."
  `(with-cell-stop ((nats-start "servers" +servers+
                                 "publish_allowed" +demo+
                                 "subscribe_allowed" +demo+))
     ,@body))

;;; --- publishing ----------------------------------------------------------------

(deftest nats-cell-publish-wraps-the-event-and-is-allowlisted ()
  (with-nats ()
    (let* ((sent '())
           (nodecode-nats:*publisher*
             (lambda (settings subject body)
               (declare (ignore settings))
               (push (list subject body) sent)
               (list :stream "hermes" :seq 7))))
      (is (has-all (nats:publish "demo.build" '(:ok t) :kind "ci-result")
                   "Published" "demo.build" "stream=hermes" "seq=7")
          "the ack is the answer")
      (is (= 1 (length sent)) "one publish reached the wire")
      (let ((subject (first (first sent))) (body (second (first sent))))
        (is (equal "demo.build" subject) "the literal subject")
        (is (has-all body "nodecode-nats.v1" "ci-result" "nonce" "data")
            "the envelope: schema, kind, nonce, and the data"))
      (is (has-all (refused (nats:publish "other.x" "hi"))
                   "not in nats.publish_allowed" "deny by default")
          "a subject outside the allowlist is refused")
      (is (has-all (refused (nats:publish "demo.>" "hi")) "literal" "no * or >")
          "a publish subject may not be widened")
      (is (has-all (refused (nats:publish "demo.x" "ghp_aaaaaaaaaaaaaaaaaaaaaaaaaa"))
                   "looks like a credential")
          "a payload shaped like a token is refused")
      (is (= 1 (length sent)) "a refused publish never reaches the wire"))))

(deftest nats-cell-publish-refuses-more-bytes-than-the-cap ()
  (with-cell-stop ((nats-start "servers" +servers+ "publish_allowed" +demo+
                                "subscribe_allowed" +demo+ "max_payload_bytes" 64))
    (let ((nodecode-nats:*publisher* (lambda (s subj body)
                                       (declare (ignore s subj body))
                                       (list :stream "hermes" :seq 1))))
      (is (has-all (refused (nats:publish "demo.x" (make-string 200 :initial-element #\a)))
                   "max_payload_bytes is 64")
          "the size cap is the section's"))))

;;; --- fetching --------------------------------------------------------------------

(deftest nats-cell-fetch-pulls-a-batch-and-answers-it ()
  (with-nats ()
    (let* ((asked nil)
           (nodecode-nats:*fetcher*
             (lambda (settings subject &key batch timeout)
               (declare (ignore settings))
               (setf asked (list subject batch timeout))
               (list (list :payload "{\"a\":1}") (list :payload "{\"a\":2}")))))
      (is (has-all (nats:fetch "demo.>" :batch 5)
                   "2 events on demo.>" "{\"a\":1}" "{\"a\":2}")
          "both events come back")
      (is (equal '("demo.>" 5 10) asked) "the subject, the batch and the section's timeout")
      (is (has-all (refused (nats:fetch "other.>")) "not in nats.subscribe_allowed")
          "a subject outside the allowlist is refused"))
    (let ((nodecode-nats:*fetcher* (lambda (s subj &key batch timeout)
                                     (declare (ignore s subj batch timeout))
                                     '())))
      (is (has-all (nats:fetch "demo.none" :timeout 2)
                   "No events on demo.none within 2s")
          "an empty pull says so"))))

;;; --- /nats -----------------------------------------------------------------------

(deftest nats-cell-the-command-says-where-the-bus-is ()
  (with-nats ()
    (is (has-all (cell-entry "nodecode-nats" "nats" "" "s1")
                 "nats: nats://127.0.0.1:4222" "stream hermes"
                 "publish: demo.>" "subscribe: demo.>")
        "the command reports the section")
    (let ((nodecode-nats:*prober* (lambda (settings)
                                    (declare (ignore settings))
                                    "reached the NATS server at nats://127.0.0.1:4222")))
      (is (has-all (cell-entry "nodecode-nats" "nats" "ping" "s1")
                   "reached the NATS server")
          "/nats ping probes"))
    (let ((nodecode-nats:*prober* (lambda (settings)
                                    (declare (ignore settings))
                                    (nodecode-nats::fail "cannot reach NATS at ~a: refused"
                                                          "nats://127.0.0.1:4222"))))
      (is (has-all (cell-entry "nodecode-nats" "nats" "ping" "s1") "no answer:")
          "a probe that cannot connect answers, it does not throw"))))
