;;;; cell-test.lisp --- the clack cell against a stubbed relay.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every exchange is a stubbed NLK:HTTP and every pin check a stubbed
;;;; VERIFY-RELAY, except the one test that proves the pin check itself
;;;; against a real RSA-2048 signature made in the test. No relay is dialled.

(in-package #:nodecode.test)

(defparameter +relay-modulus+ (make-string 512 :initial-element #\f)
  "A 2048-bit odd modulus: the section's shape, never a real relay key.")

(defparameter +relay-pin+
  (format nil "{\"n\":\"~a\",\"e\":\"10001\"}" +relay-modulus+))

(define-test-slice "clack" "CLACK-CELL-" :start nodecode-clack:start-cell
  :defaults (("relay_pubkey" +relay-pin+)))

;;; --- fixtures --------------------------------------------------------------------

(defvar *calls* nil "Every stubbed exchange, oldest first, as (PATH BODY).")

(defmacro with-clack-state (&body body)
  "BODY with the cell's globals bound to a scratch directory."
  `(let ((nodecode-clack::*data-dir*
           (merge-pathnames (format nil "clack-test-~a/" (random 1000000))
                            (uiop:temporary-directory)))
         (nodecode-clack::*relay* "https://relay.test")
         (nodecode-clack::*relay-key* +relay-pin+)
         (nodecode-clack::*relay-timeout* 5)
         (*calls* '()))
     (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree
                                         nodecode-clack::*data-dir* :validate t)))
       ,@body)))

(defmacro with-stub-relay (handler &body body)
  "BODY with the relay's pin check answered yes and every exchange answered by
HANDLER, a function of the path and the decoded body => (values TEXT STATUS)."
  `(with-stubbed-fdefinition (nodecode-clack::verify-relay () t)
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                 (declare (ignore method))
                                 (let ((path (quri:uri-path (quri:uri url)))
                                       (body (and (getf options :content)
                                                  (nlk:decode-json (getf options :content)))))
                                   (push (list path body) *calls*)
                                   (funcall ,handler path body)))
       ,@body)))

(defun enroll (seed name)
  "Keep an enrolled identity for SEED under NAME, as a finished join would."
  (nodecode-clack::write-state "identity.key" (nodecode-clack::b64u-encode seed))
  (nodecode-clack::save-identity
   (nlk:make-json-object "relay_url" nodecode-clack::*relay*
                          "identity_pubkey" (nodecode-clack::b64u-encode
                                              (nodecode-clack::ed25519-public seed))
                          "peer_name" name
                          "service_token" "service-token")))

(defun call-tool (function arguments)
  "FUNCTION's answer to an ARGUMENTS JSON object, as the model reads it."
  (funcall function (nlk:decode-json arguments)))

(defun call-object (function arguments)
  (nlk:decode-json (call-tool function arguments)))

(defun body-answer (text) (values text 200))

;;; --- the lifecycle, from the one declaration --------------------------------------

(define-cell-lifecycle-tests "clack"
  (:config)
  (:help :clack)
  (:command "clack")
  (:refused ("relay_pubkey" "") ("relay_url" "http://example.com") ("timeout" 0)))

(deftest clack-cell-refuses-without-a-pinned-relay-key ()
  (with-clack-state
    (let ((condition (signals-error nlk:config-refusal (clack-start "relay_pubkey" ""))))
      (is (search "relay_pubkey" (princ-to-string condition)) "the refusal names the key")
      (is (null (nle::find-tool "clack_send")) "and installs no tool"))))

;;; --- the cryptography, without a relay --------------------------------------------

(deftest clack-cell-envelope-round-trip ()
  (let* ((sender "peer-a") (recipient "peer-b")
         (recipient-seed (ironclad:random-data 32))
         (recipient-pub (nodecode-clack::ed25519-public-to-x25519
                         (nodecode-clack::ed25519-public recipient-seed)))
         (envelope (nodecode-clack::encrypt-envelope sender recipient recipient-pub
                                                     (nodecode-clack::utf8 "the deploy is live"))))
    (is (equal "the deploy is live" (nodecode-clack::as-text (nodecode-clack::decrypt-envelope recipient-seed envelope sender recipient))) "the peer's key opens the body")
    (is (equal "x25519-chacha20poly1305" (gethash "alg" envelope)) "and the envelope says how")
    (let ((wrong (ironclad:random-data 32)))
      (signals-error nodecode-clack::clack-error
        (nodecode-clack::decrypt-envelope wrong envelope sender recipient)))
    (let ((tampered (nlk:decode-json (nlk:encode-json-object envelope))))
      (setf (gethash "ct" tampered)
            (nodecode-clack::b64u-encode (make-array 40 :element-type '(unsigned-byte 8)
                                                         :initial-element 7)))
      (signals-error nodecode-clack::clack-error
        (nodecode-clack::decrypt-envelope recipient-seed tampered sender recipient)))))

(deftest clack-cell-relay-pin-accepts-a-real-signature ()
  (with-clack-state
    (multiple-value-bind (private public) (ironclad:generate-key-pair :rsa :num-bits 2048)
      (declare (ignore public))
      (let* ((parts (ironclad:destructure-private-key private))
             (n (getf parts :n))
             (p (getf parts :p))
             (q (getf parts :q))
             (e 65537)
             (d (nodecode-clack::mod-inverse e (* (1- p) (1- q))))
             (pin (format nil "{\"n\":\"~(~x~)\",\"e\":\"~(~x~)\"}" n e)))
        (let ((nodecode-clack::*relay-key* pin))
          (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                      (declare (ignore method options))
                                      (let* ((nonce (subseq url (+ 6 (search "nonce=" url))))
                                             (signature (nodecode-clack::int-be
                                                          (nodecode-clack::mod-expt
                                                           (nodecode-clack::le-int-be
                                                            (nodecode-clack::rsa-pkcs1-em
                                                             (nodecode-clack::octets-of-hex nonce)))
                                                           d n)
                                                          256)))
                                        (values (nlk:encode-json-object
                                                 (nlk:make-json-object
                                                  "nonce" nonce
                                                  "algorithm" "rsassa-pkcs1-v1_5-sha256"
                                                  "public_key" (nlk:make-json-object
                                                                "n" (format nil "~(~x~)" n)
                                                                "e" (format nil "~(~x~)" e))
                                                  "signature" (nodecode-clack::b64u-encode signature)))
                                                200)))
            (nodecode-clack::verify-relay)
            (is (probe-file (nodecode-clack::state-path "relay-pin.json")) "the proof verified and the pin was kept")
            (let ((nodecode-clack::*relay-key* +relay-pin+))
              (is (search "pin" (princ-to-string (signals-error nodecode-clack::clack-error (nodecode-clack::verify-relay)))) "a different configured key is refused"))))))))

;;; --- the verbs, over a stubbed relay ----------------------------------------------

(deftest clack-cell-whoami-before-enrolling-dials-nothing ()
  (with-clack-state
    (with-stub-relay (lambda (path body) (declare (ignore path body)) (error "no exchange"))
      (let ((object (call-object #'nodecode-clack::whoami "{}")))
        (is (null (nlk:json-value object :boolean "enrolled")) "nothing is enrolled yet")
        (is (equal "https://relay.test" (nlk:json-value object :string "relay_url")))
        (is (null *calls*) "and nothing was dialled")))))

(deftest clack-cell-send-encrypts-and-clears-the-outbox ()
  (with-clack-state
    (let* ((their-seed (ironclad:random-data 32))
           (their-pub (nodecode-clack::b64u-encode (nodecode-clack::ed25519-public their-seed)))
           (sent-id nil)
           (receipt-read nil))
      (enroll (ironclad:random-data 32) "peer-a")
      (with-stub-relay
          (lambda (path body)
            (cond ((search "/v1/handshakes" path)
                   (body-answer (format nil "{\"handshakes\":[{\"peer\":\"peer-b\",\"status\":\"active\",\"enc_capable\":1,\"peer_identity\":\"~a\"}]}" their-pub)))
                  ((search "/v1/send" path)
                   (setf sent-id (gethash "id" body))
                   (body-answer (format nil "{\"accepted\":true,\"id\":\"~a\"}" sent-id)))
                  ((search "/v1/receipts" path)
                   (setf receipt-read t)
                   (body-answer (format nil "{\"receipts\":[{\"id\":\"~a\",\"state\":\"delivered\"}]}"
                                        sent-id)))
                  (t (error "unexpected ~a" path))))
        (let* ((object (call-object #'nodecode-clack::send
                                     "{\"to\":\"peer-b\",\"text\":\"the build is green\"}"))
               (mid (nlk:json-value object :string "message_id")))
          (is (eq t (nlk:json-value object :boolean "encrypted")) "the answer says encrypted")
          (is (stringp mid) "and carries a message id")
          (let* ((sent (second (find "/v1/send" *calls* :test #'search :key #'first)))
                 (envelope (gethash "enc" sent)))
            (is (equal "peer-b" (gethash "to" sent)) "to the peer")
            (is (equal "the build is green" (nodecode-clack::as-text (nodecode-clack::decrypt-envelope their-seed envelope "peer-a" "peer-b"))) "and only the recipient's key opens it"))
          (is receipt-read "the receipt was read back")
          (is (equal "null" (nodecode-clack::read-state "outbox.json")) "the outbox is empty once the receipt is in"))))))

(deftest clack-cell-poll-decrypts-and-quarantines-plaintext ()
  (with-clack-state
    (let* ((my-seed (ironclad:random-data 32))
           (their-seed (ironclad:random-data 32))
           (my-pub (nodecode-clack::ed25519-public-to-x25519
                    (nodecode-clack::ed25519-public my-seed)))
           (envelope (nodecode-clack::encrypt-envelope "peer-b" "peer-a" my-pub
                                                       (nodecode-clack::utf8 "hello over the wire"))))
      (enroll my-seed "peer-a")
      (with-stub-relay
          (lambda (path body)
            (declare (ignore path body))
            (body-answer
             (format nil "{\"messages\":[{\"id\":\"11111111-2222-4333-8444-555555555555\",\"from\":\"peer-b\",\"enc_v\":1,\"sent_at\":1,\"text\":~a},{\"id\":\"aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee\",\"from\":\"peer-b\",\"sent_at\":2,\"text\":\"plain text\"}]}"
                     (nlk:encode-json-object envelope))))
        (let* ((object (call-object #'nodecode-clack::poll "{\"limit\":5}"))
               (messages (nlk:json-array object "messages")))
          (is (= 1 (length messages)) (format nil "one message came through: ~a" (nlk:encode-json-object object)))
          (is (equal "hello over the wire" (nlk:json-value (aref messages 0) :string "plaintext")))
          (is (eq t (nlk:json-value object :boolean "content_is_untrusted")) "and it is marked untrusted")
          (is (= 1 (length (nlk:json-array object "rejected"))) "the plaintext row is rejected")
          (is (probe-file (nodecode-clack::state-path "delivered-11111111-2222-4333-8444-555555555555.json")) "the delivery journal kept the plaintext")
          (is (probe-file (nodecode-clack::state-path "dead-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee.json")) "and the refused row is quarantined")
          (is (null (find "/v1/ack" *calls* :test #'search :key #'first)) "nothing was acknowledged"))))))
