;;;; package.lisp --- the NODECODE-CLINE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-ADDON in it (addons.lisp). Nothing is model-facing here: the add-on
;;;; has no verb, no primer and no slash command. It advises the credential a
;;;; lane resolves and the listing a lane fetches — see feed.lisp.

(defpackage #:nodecode-cline
  (:use #:cl))

(in-package #:nodecode-cline)

;;; SETTING comes from the NLE:DEFINE-ADDON declaration in addon.lisp, which
;;; loads last; feed.lisp reads the section through it, and the settings cell
;;; it reads is the one this declaration makes.
(nlk:define-peripheral cline :not-running t)
