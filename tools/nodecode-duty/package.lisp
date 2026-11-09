;;;; package.lisp --- the one package of the duty cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Named after the system: the loader finds START-CELL as the exported
;;;; symbol of package NODECODE-DUTY, and every contribution is filed under
;;;; that name downcased, which is this folder's name.

(defpackage #:nodecode-duty
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:snapshot #:duty-text #:counts-text #:read-duty #:run-slash
           #:+sections+))
