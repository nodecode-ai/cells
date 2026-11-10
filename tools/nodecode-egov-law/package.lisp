;;;; package.lisp --- the NODECODE-EGOV-LAW package: e-Gov's law data, read-only.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed EGOV-LAW so an eval form reads
;;;; (egov-law:article "労基法" "32").

(defpackage #:nodecode-egov-law
  (:use #:cl)
  (:nicknames #:egov-law)
  (:export #:start-cell
           #:stop-cell
           #:running-settings
           #:egov-law-error
           #:egov-law-error-detail
           #:egov_law_search
           #:egov_law_article
           #:egov_law_revisions
           #:search-laws
           #:article
           #:revisions
           #:run-command))

(in-package #:nodecode-egov-law)

;;; EGOV-LAW-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares
;;; the section that fills them in.
(nlk:define-peripheral egov-law :not-running t)
