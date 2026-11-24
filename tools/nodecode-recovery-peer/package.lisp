;;;; package.lisp --- the RECOVERY-PEER package: its verbs, its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed RECOVERY-PEER so an eval form reads
;;;; (recovery-peer:redact ...). Every verb answers a STRING, and a review
;;;; that cannot be asked says why in one line rather than signalling into a
;;;; turn that is already failing.

(defpackage #:nodecode-recovery-peer
  (:use #:cl)
  (:nicknames #:recovery-peer)
  (:export
   ;; a review, by hand or from the :TOOL hook
   #:review #:watch-call #:ask-tool
   ;; what the operator and a model read
   #:status-text
   ;; the text helpers, so a layer can reword what a peer sees
   #:redact #:peer-target
   ;; the one condition a start refuses with is the core's own
   #:split-peer))

(in-package #:nodecode-recovery-peer)

(nlk:define-peripheral recovery-peer :not-running t)
