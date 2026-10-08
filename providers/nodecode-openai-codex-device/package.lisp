;;;; package.lisp --- the NODECODE-OPENAI-CODEX-DEVICE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-openai-codex-device
  (:use #:cl))

(in-package #:nodecode-openai-codex-device)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp and signin.lisp read the section through it.
;;; FAIL and OPENAI-CODEX-DEVICE-ERROR, which a sign-in that cannot go on
;;; signals, come from here too.
(nlk:define-peripheral openai-codex-device :not-running t)

(defparameter +provider+ "openai-codex"
  "The provider this sign-in is for, and the key it is kept under in
auth.json's oauth_tokens: omp's openai-codex-device stores as openai-codex,
so a device sign-in and a browser one are the same subscription.")

(defparameter +lane+ "openai-codex-device"
  "The lane this cell registers: the Responses fold under a name of its own,
so that beside nodecode-openai-codex (whose lane is openai-codex) each cell
shapes only the rounds on its own lane.")

(defparameter +key+ "nodecode-openai-codex-device"
  "The key this cell's notices stand under: the folder's name.")
