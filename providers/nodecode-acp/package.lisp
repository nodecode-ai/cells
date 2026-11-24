;;;; package.lisp --- the NODECODE-ACP package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it (cells.lisp). The cell is a provider id, acp, whose
;;;; models are agents: a turn on acp/<agent> runs in that agent -- Claude
;;;; Code, Codex, Gemini CLI or any other program that speaks the Agent Client
;;;; Protocol on its stdin and stdout -- and the session shows what it does.

(defpackage #:nodecode-acp
  (:use #:cl))

(in-package #:nodecode-acp)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the files before it read the section through it.
(nlk:define-peripheral acp :not-running t)

(defparameter +provider+ "acp"
  "The provider id, and the name of the lane it rides: what /models lists
an agent under (acp/claude-code) and a turn's frozen config names.")

(defvar *input* nil
  "The running turn's input, as RUN was handed it.")

(defparameter +protocol-version+ 1
  "The Agent Client Protocol version this client speaks.")
