;;;; package.lisp --- the POCKET-WATCH package: the same report a model asks for.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed POCKET-WATCH, so an eval form reads
;;;; (pocket-watch:report "health"): the same lines the pocket_watch tool
;;;; answers, over the same jobs, for the operator who would rather type one.
;;;;
;;;; Every verb answers a STRING. A refusal is POCKET-WATCH-ERROR, which the
;;;; eval snippet and a tool result both render as "ERROR: ...".

(defpackage #:nodecode-pocket-watch
  (:use #:cl)
  (:nicknames #:pocket-watch)
  (:export
   #:report #:status-text #:jobs-text #:health-text #:daily-text #:weekly-text
   #:monthly-text #:run-slash
   #:+views+ #:+statuses+ #:+kinds+ #:+primer+
   #:pocket-watch-error))
