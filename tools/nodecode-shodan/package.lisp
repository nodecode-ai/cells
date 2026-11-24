;;;; package.lisp --- the SHODAN package: twelve tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, nicknamed SHODAN so an eval form
;;;; reads (shodan:host "1.1.1.1"). The tool bodies are the exported verbs; a
;;;; person can call the same functions from eval.

(defpackage #:nodecode-shodan
  (:use #:cl)
  (:nicknames #:shodan)
  (:export #:start-cell
           #:stop-cell
           #:host #:search-hosts #:count-hosts #:dns #:cve #:account #:meta
           #:scan #:alerts #:exploits #:query-directory #:trends
           ;; the argument contracts, so an eval form can build one
           #:host-lookup #:host-search #:host-count #:dns-lookup #:cve-lookup
           #:account-info #:meta-info #:scan-manage #:alert-manage
           #:exploit-search #:trend-history
           #:shodan-error #:shodan-error-detail))

(in-package #:nodecode-shodan)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and SHODAN-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral shodan :not-running t)
