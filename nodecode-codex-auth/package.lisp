;;;; package.lisp --- the NODECODE-CODEX-AUTH package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-ADDON in it (addons.lisp). Nothing is model-facing here: the add-on
;;;; has no verb, no primer and no slash command. It answers the kernel's
;;;; :CREDENTIAL point — see credential.lisp — and that is the whole of it.

(defpackage #:nodecode-codex-auth
  (:use #:cl))

(in-package #:nodecode-codex-auth)

;;; SETTING comes from the NLE:DEFINE-ADDON declaration in addon.lisp, which
;;; loads last; credential.lisp reads the section through it, and the settings
;;; cell it reads is the one this declaration makes.
(nlk:define-peripheral codex-auth :not-running t)
