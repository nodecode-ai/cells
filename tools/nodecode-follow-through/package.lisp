;;;; package.lisp --- the one package of the follow-through cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Named after the system: the loader finds START-CELL as the exported
;;;; symbol of package NODECODE-FOLLOW-THROUGH, and every contribution is
;;;; filed under that name downcased, which is this folder's name.

(defpackage #:nodecode-follow-through
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:promise-sentence #:watch-turn #:watch-prompt
           #:read-follow-through #:run-slash #:status-text
           #:+tag+ #:+nudge-rule+ #:max-nudges #:nudges-given))
