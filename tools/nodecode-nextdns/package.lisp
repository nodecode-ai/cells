;;;; package.lisp --- the NEXTDNS package: the verbs, their refusal, the settings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed NEXTDNS so an eval form reads
;;;; (nextdns:logs ...). The plugin's tools are named nextdns_logs and so on;
;;;; each verb here is one of them, and the command and the manual topic are
;;;; the same word.
;;;;
;;;; NLK:DEFINE-PERIPHERAL is the one refusal this package needs: it declares
;;;; NEXTDNS-ERROR and its detail reader, FAIL (which signals it with formatted
;;;; text), and -- because the cell needs settings -- *NEXTDNS*,
;;;; RUNNING-SETTINGS and SETTING, which the files after this one read. A
;;;; refused start and a verb called before a start are the same condition.

(defpackage #:nodecode-nextdns
  (:use #:cl)
  (:nicknames #:nextdns)
  (:export #:start-cell
           #:stop-cell
           #:logs
           #:blocked
           #:analytics
           #:profile-settings
           #:totals
           #:nextdns-error
           #:nextdns-error-detail
           #:pct-encode
           #:query-string))

(in-package #:nodecode-nextdns)

(nlk:define-peripheral nextdns :not-running t)
