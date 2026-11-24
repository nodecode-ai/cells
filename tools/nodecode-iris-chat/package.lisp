;;;; package.lisp --- the IRIS-CHAT package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed IRIS so an eval form reads (iris:chats) and
;;;; (iris:send "chat-id" "hello"). Every exported verb answers a STRING and
;;;; every refusal is IRIS-ERROR, rendered by the eval snippet as
;;;; "ERROR: IRIS-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `iris-chat' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :IRIS).

(defpackage #:nodecode-iris-chat
  (:use #:cl)
  (:nicknames #:iris)
  (:export
   ;; verbs, every one a string
   #:whoami #:chats #:history #:send #:seen #:invite #:join #:service
   ;; data, so a layer can reword it
   #:*primer* #:*standing* #:*framing*
   ;; the one condition
   #:iris-chat-error))

(in-package #:nodecode-iris-chat)

(nlk:define-peripheral iris-chat :not-running t)
