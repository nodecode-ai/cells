;;;; package.lisp --- the one package of the plan-canvas cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; PLAN-CANVAS is the nickname beside it, so the four tools, /plan-canvas and
;;;; the manual's (help :plan-canvas) all spell the same word.

(defpackage #:nodecode-plan-canvas
  (:nicknames #:plan-canvas)
  (:use #:cl)
  (:export #:plan-canvas #:start-cell #:stop-cell #:install
           #:tool-draw #:tool-update #:tool-get #:tool-next
           #:run-command #:observe-frame #:veto-write #:deliver
           #:draw-plan #:apply-op #:load-doc #:save-doc #:doc-of
           #:box-view #:ready-boxes #:card-line #:delegate-goal
           #:glob-match-p #:relativize #:check-command
           #:written-paths #:changed-paths #:note-work #:note-edit
           #:reminder-text #:plan-text #:new-box #:doc-box
           #:+primer+ #:+statuses+ #:+kinds+ #:+op-names+
           #:*plan-canvas* #:plan-canvas-error #:fail #:setting))
