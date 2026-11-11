;;;; package.lisp --- the BUBBLEWRAP package: the verbs, the data, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed BUBBLEWRAP so a form in eval reads
;;;; (bubblewrap:sandbox "make test"). Every verb answers a STRING and every
;;;; refusal is BUBBLEWRAP-ERROR; a tool call answers it as the call's failure.
;;;;
;;;; The settings this cell runs on derive from the one `bubblewrap' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :root).

(in-package #:cl-user)

(defpackage #:nodecode-bubblewrap
  (:use #:cl)
  (:nicknames #:bubblewrap)
  (:export
   ;; verbs, each answering a string
   #:sandbox #:status
   ;; data, so a layer can reword it
   #:*credential-stores* #:*home-allow* #:*socket-vars*
   ;; seams a test binds
   #:*run* #:*kind* #:*entries* #:*home*
   ;; the one condition
   #:bubblewrap-error))

(in-package #:nodecode-bubblewrap)

(nlk:define-peripheral bubblewrap :not-running t)
