;;;; package.lisp --- the WECHAT package: the memory's verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed WECHAT so an eval form reads
;;;; (wechat:note :contact "Lin" :text "moving in March" :kind "plan").
;;;; Every exported verb answers a STRING and every refusal is
;;;; WECHAT-ERROR, rendered by the eval snippet as
;;;; "ERROR: WECHAT-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `wechat' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :QUIET-DAYS).

(defpackage #:nodecode-wechat-assistant
  (:use #:cl)
  (:nicknames #:wechat)
  (:export
   ;; the memory, every verb a string
   #:note #:contacts #:brief #:followups #:draft #:drafts #:mark #:report
   ;; data, so a layer can reword it
   #:+kinds+ #:+draft-statuses+ #:*primer*
   ;; the one condition
   #:wechat-error))

(in-package #:nodecode-wechat-assistant)

(nlk:define-peripheral wechat :not-running t :lock "wechat" :recursive t)
