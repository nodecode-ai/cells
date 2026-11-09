;;;; package.lisp --- the cell's one package, and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-eatwise
  (:use #:cl)
  (:nicknames #:eatwise)
  (:export #:start-cell #:stop-cell #:setting
           #:search-foods #:daily-targets #:review-meal #:store-status
           #:configure-store #:save-profile #:stored-profile #:list-meals
           #:day-review #:forget-data
           #:food #:food-id #:food-name #:find-food))
