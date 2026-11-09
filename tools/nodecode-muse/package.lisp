;;;; package.lisp --- the companion's own names.
;;;;
;;;; SPDX-License-Identifier: MIT
(defpackage #:nodecode-muse
  (:use #:cl)
  (:nicknames #:muse)
  (:export #:manage #:context #:goal #:goal-done #:goals #:feed #:feed-add #:remind #:resolve
           #:run-slash #:muse-error)
  (:documentation
   "The companion: goals, expiring interests, a quiet Feed and due reminders,
kept in this profile's own store and reached by the muse_manage tool, /muse, or
the verbs below through eval."))
