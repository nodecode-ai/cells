;;;; package.lisp --- the NODECODE-IRIS-CHAT-CHANNEL package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it. It is meant to be used qualified only, and its
;;;; nickname is the cell's own name so an eval form reads (iris-chat-channel:...).

(defpackage #:nodecode-iris-chat-channel
  (:documentation "Iris Chat as a Nodecode room: one approved contact, by the iris CLI.")
  (:use #:cl)
  (:nicknames #:iris-chat-channel)
  (:export #:start-cell
           #:iris-chat-channel-error #:iris-chat-channel-error-detail
           #:owner-hex #:chunks #:run-command #:tool-send #:status-line))

(in-package #:nodecode-iris-chat-channel)

;;; IRIS-CHAT-CHANNEL-ERROR is the one condition every refusal signals; the
;;; NOT-RUNNING part gives this package *IRIS-CHAT-CHANNEL*, RUNNING-SETTINGS and
;;; FAIL, and the lock serializes the poll's bookkeeping against the command.
(nlk:define-peripheral iris-chat-channel :not-running t :lock "iris-chat-channel")
