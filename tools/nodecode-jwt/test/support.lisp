;;;; support.lisp --- the jwt test runner and the helpers every test reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under a JWT name prefix;
;;;; RUN-JWT-TESTS runs exactly that slice. Nothing here dials anything: the
;;;; decoder reads no file, no environment and no network, and the one seam a
;;;; test reaches for is the cell's own tool, called through the wire the way a
;;;; provider's call calls it.

(in-package #:nodecode.test)

(define-test-slice "jwt" "JWT" :start nodecode-jwt:start-cell)

(defparameter +now+ 1700000000
  "The clock every test pins: 2023-11-14 22:13:20 UTC.")

(defparameter +alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  "The URL-safe alphabet, as a token's parts are written in it.")

(defun jwt-encode (text)
  "TEXT's UTF-8 octets as base64url, unpadded: a part as a token writes one."
  (let ((data (nlk:utf-8-octets text)))
    (with-output-to-string (out)
      (loop for index from 0 below (length data) by 3
            for left = (- (length data) index)
            for bits = (logior (ash (aref data index) 16)
                               (ash (if (> left 1) (aref data (1+ index)) 0) 8)
                               (if (> left 2) (aref data (+ index 2)) 0))
            do (write-char (char +alphabet+ (ldb (byte 6 18) bits)) out)
               (write-char (char +alphabet+ (ldb (byte 6 12) bits)) out)
               (when (> left 1) (write-char (char +alphabet+ (ldb (byte 6 6) bits)) out))
               (when (> left 2) (write-char (char +alphabet+ (ldb (byte 6 0) bits)) out))))))

(defun jwt-token (header payload &optional (signature "a-signature"))
  "HEADER and PAYLOAD, as JSON text, made into the token a service would issue.
SIGNATURE NIL leaves the third part empty."
  (format nil "~a.~a.~a"
          (jwt-encode header) (jwt-encode payload)
          (if signature (jwt-encode signature) "")))

(defparameter +classic+
  (jwt-token "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"
              "{\"sub\":\"1234567890\",\"name\":\"A User\",\"iat\":1516239022}")
  "The token every example writes: HS256, a subject and an issuance time.")

(defun jwt-clock (unix)
  "UNIX seconds written as the description writes a moment: what a row shows."
  (let ((stamp (nlk:iso-time unix :millis nil)))
    (format nil "~a ~a UTC" (subseq stamp 0 10) (subseq stamp 11 19))))

(defun jwt-expand (token &key (now +now+))
  "TOKEN described at NOW, as jwt_decode answers it."
  (nodecode-jwt:expand-token token :now now))

(defun jwt-refusal (token &key (now +now+))
  "The reason TOKEN cannot be read, NIL when it can."
  (handler-case (progn (jwt-expand token :now now) nil)
    (nodecode-jwt:jwt-error (failure) (nodecode-jwt:jwt-error-detail failure))))

(defun jwt-tool (id arguments)
  "jwt_decode called as the provider's call calls it: the answer's text."
  (nle::message-content (execute-wire-call id "jwt_decode" arguments)))

(defun jwt-arguments (token)
  "One jwt_decode call's JSON, built from TOKEN: a token is letters, digits,
dot, dash and underscore, so no escaping is needed."
  (format nil "{\"token\": \"~a\"}" token))
