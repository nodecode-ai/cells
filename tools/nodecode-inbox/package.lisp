;;;; package.lisp --- the one package of the inbox cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Named after the system: the loader finds START-CELL as the exported
;;;; symbol of package NODECODE-INBOX, and every contribution is filed
;;;; under that name downcased, which is this folder's name.

(defpackage #:nodecode-inbox
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:snapshot #:inbox-text #:read-inbox #:run-slash
           #:+sections+))
