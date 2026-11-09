;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package NODECODE-WOOF, and every contribution is
;;;; filed under "nodecode-woof".

(defpackage #:nodecode-woof
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:woof-error
           #:doctor #:show-catalog #:check-plan #:launch #:run-status #:show-maps
           #:compare-runs #:report-run #:stop-run #:search-docs #:campaign #:run-score
           #:run-woof #:launch-woof #:signal-pid #:pid-alive-p
           #:runs-root #:run-directory #:read-record #:list-runs))
