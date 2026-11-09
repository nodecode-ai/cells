;;;; package.lisp --- the FAST-CHAT package: one command, one manual, its data.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, so the loader finds START-CELL as
;;;; NODECODE-FAST-CHAT:START-CELL and every contribution is filed under
;;;; "nodecode-fast-chat". The command and the manual topic are the slug
;;;; itself, "fast-chat".

(defpackage #:nodecode-fast-chat
  (:use #:cl)
  (:export
   ;; the one verb, answering a string
   #:launch
   ;; the reading of a typed argument line, and its parts
   #:parse-arguments #:split-model-spec
   ;; data, so a layer can reword it
   #:+efforts+ #:*usage* #:*manual*
   ;; the one condition
   #:fast-chat-error))
