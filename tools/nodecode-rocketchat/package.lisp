;;;; package.lisp --- the ROCKETCHAT package: ten tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed ROCKETCHAT so an eval form reads
;;;; (rocketchat:list-channels ...). The ten verbs are tools the model calls;
;;;; a person can call the same functions from eval.

(defpackage #:nodecode-rocketchat
  (:use #:cl)
  (:nicknames #:rocketchat)
  (:export #:start-cell
           #:stop-cell
           #:list-channels #:search-messages #:get-history #:get-thread #:get-permalink
           #:create-channel #:post-message #:send-file #:send-dm #:delegate
           #:rocketchat-error #:rocketchat-error-detail))

(in-package #:nodecode-rocketchat)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and ROCKETCHAT-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral rocketchat :not-running t)
