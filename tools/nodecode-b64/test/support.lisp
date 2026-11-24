;;;; support.lisp --- the b64 test runner and the helpers every test reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under a B64 name prefix;
;;;; RUN-B64-TESTS runs exactly that slice. Nothing here dials anything: the
;;;; decoder reads no file, no environment and no network, and the one seam a
;;;; test reaches for is the cell's own tool, called through the wire the way
;;;; a provider's call calls it.

(in-package #:nodecode.test)

(define-test-slice "b64" "B64" :start nodecode-b64:start-cell)

(defun b64-expand (value &rest args)
  "The description of VALUE, as b64_decode answers it."
  (apply #'nodecode-b64:expand-value value args))

(defun b64-tool (id arguments)
  "b64_decode called as the provider's call calls it: the answer's text."
  (nle::message-content (execute-wire-call id "b64_decode" arguments)))

(defun b64-base64 (octets &key url)
  "OCTETS as base64, with the padding: what a value looks like in the wild."
  (nodecode-b64:base64-string octets (if url nodecode-b64::+base64-url-safe+
                                           nodecode-b64::+base64-standard+)))

(defun b64-text (string)
  "STRING as the value a base64 reference would carry."
  (b64-base64 (nodecode-b64:utf-8-octets string)))

(defun b64-gzip (octets)
  "OCTETS as gzip data: what a compressed log field holds."
  (salza2:compress-data octets 'salza2:gzip-compressor))

(defun b64-refusal (value &rest args)
  "The refusal's text when VALUE cannot be decoded, NIL when it can."
  (handler-case
      (progn (apply #'nodecode-b64:decode-value value args) nil)
    (nodecode-b64:b64-error (failure) (nodecode-b64:b64-error-detail failure))))
