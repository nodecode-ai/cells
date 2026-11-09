;;;; package.lisp --- the NODECODE-FINANCIAL-DATASETS package: the tools and their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed FINANCIAL-DATASETS so an eval form reads
;;;; (financial-datasets:fetch "fd_get_news" :ticker "AAPL").

(defpackage #:nodecode-financial-datasets
  (:use #:cl)
  (:nicknames #:financial-datasets)
  (:export #:financial-datasets-error #:financial-datasets-error-detail
           #:fetch #:api-key #:base-url #:tool-names))

(in-package #:nodecode-financial-datasets)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last, and so do the tools' registrations. FAIL, which a request
;;; that cannot go on signals, FINANCIAL-DATASETS-ERROR, and the running
;;; settings variable *FINANCIAL-DATASETS* come from here, so api.lisp can
;;; use them before the declaration loads.
(nlk:define-peripheral financial-datasets :not-running t)
