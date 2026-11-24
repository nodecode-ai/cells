;;;; package.lisp --- the SYSTEM-ONE package: three gates as verbs, two as tools.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed SYSTEM-ONE so an eval form reads
;;;; (system-one:route "..."). It is meant to be used qualified only.

(defpackage #:nodecode-system-one
  (:use #:cl)
  (:nicknames #:system-one)
  (:export #:route #:search-gate #:tier-gate #:notify-gate #:health
           #:system-one-error #:system-one-error-detail))

(in-package #:nodecode-system-one)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; every function below reads the section through it. FAIL and
;;; SYSTEM-ONE-ERROR, which a gate that cannot go on signals, and DEFINE-VERB,
;;; whose functions refuse while the cell is not running, come from here too.
(nlk:define-peripheral system-one :not-running t)

(defparameter +search-path+ "/v1/workflows/search-gate"
  "The sidecar's Search Gate: does the current task need the public Web.")

(defparameter +tier-path+ "/v1/workflows/model-tier-gate"
  "The sidecar's Model Tier Gate: fast or strong, before expensive reasoning.")

(defparameter +notify-path+ "/v1/workflows/notification-gate"
  "The sidecar's Notification Gate: silent, digest or notify_now.")

(defparameter +max-task-chars+ 4000
  "How much of a task one gate is sent: the original's own bound.")

(defparameter +max-context-chars+ 2000
  "How much recent conversation one gate is sent: the original's own bound.")

(defparameter +max-event-chars+ 6000
  "How much of one event the Notification Gate is sent: the original's bound.")

(defparameter +no-web-reasons+ '("bounded_transform_task" "connected_app_data" "local_file_or_repo")
  "The audited hard no-Web rules the original's Canary mode acts on.")

(defparameter +fast-reasons+ '("bounded_transform" "bounded_structured_transform")
  "The audited hard-fast rules the original's Canary mode acts on.")
