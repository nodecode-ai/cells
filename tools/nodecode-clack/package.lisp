;;;; package.lisp --- one package, one peripheral.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; DEFINE-PERIPHERAL gives this package its CLACK-ERROR/FAIL, the settings
;;;; variable *CLACK*, RUNNING-SETTINGS and the WITH-CLACK-LOCK the store and
;;;; every relay exchange run under: one tool call at a time, so the single
;;;; outbox and the delivery journals cannot interleave.

(defpackage #:nodecode-clack
  (:use #:cl)
  (:export #:start-cell #:status-text #:whoami #:peers #:send #:poll #:reconcile #:ack
           #:invite #:join #:accept))

(in-package #:nodecode-clack)

(nlk:define-peripheral clack :not-running t :lock "nodecode-clack")

(defparameter +default-relay+ "https://relay.tryclack.com"
  "The public Clack relay the original plugin defaults to.")

(defparameter +max-response+ (* 2 1024 1024)
  "The largest relay answer read, as the original bounds it.")

(defparameter +enc-version+ 1)
(defparameter +enc-alg+ "x25519-chacha20poly1305")
(defparameter +hkdf-info-prefix+ "clack-enc-v1"
  "HKDF info = this prefix, then the sender's name, then the recipient's.")
(defparameter +token-info+ "clack-hermes-credential-v1"
  "HKDF info for the key the relay service token is sealed under.")
