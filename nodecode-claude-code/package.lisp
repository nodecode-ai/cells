;;;; package.lisp --- the NODECODE-CLAUDE-CODE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-ADDON in it (addons.lisp). Nothing is model-facing here: no tool,
;;;; no primer, no slash command. The add-on is a provider id, claude-code,
;;;; and the request path under it.

(defpackage #:nodecode-claude-code
  (:use #:cl))

(in-package #:nodecode-claude-code)

;;; SETTING comes from the NLE:DEFINE-ADDON declaration in addon.lisp, which
;;; loads last; cli.lisp reads the section through it, and the settings cell
;;; it reads is the one this declaration makes.
(nlk:define-peripheral claude-code :not-running t)

(defparameter +provider+ "claude-code"
  "The provider id, and the name of the lane it rides: what the /models
picker lists and a turn's frozen config names.")
