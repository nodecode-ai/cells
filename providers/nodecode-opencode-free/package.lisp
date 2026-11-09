;;;; package.lisp --- the NODECODE-OPENCODE-FREE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it. Nothing is model-facing here: no tool, no
;;;; slash command. The cell is a provider id, opencode-free, and the
;;;; request path under it.

(defpackage #:nodecode-opencode-free
  (:use #:cl))

(in-package #:nodecode-opencode-free)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; acp.lisp reads the section through it.
(nlk:define-peripheral opencode-free :not-running t)

(defparameter +provider+ "opencode-free"
  "The provider id, and the name of the lane it rides: what the /models
picker lists and a turn's frozen config names.")

(defparameter +free-models+
  (vector "glm-5.3-flash-free" "deepseek-v4.1-flash-free" "mimo-v2.6-flash-free"
          "nemotron-3.5-super-vl-free" "nemotron-3.5-lightning-free"
          "beam-free" "big-pickle")
  "The curated free model ids, as the OpenCode CLI names them after
their `opencode/' prefix. Not a pricing promise: what a free tier
costs and whether it still exists is the vendor's, checked live.")

(defparameter +acp-prefix+ "opencode/"
  "What the CLI's model config option wants in front of the id.")

(defun model-id (model)
  "The ACP model string for MODEL: the curated id under the CLI's own
prefix, or MODEL itself when it already carries one."
  (if (and (> (length model) (length +acp-prefix+))
           (string= +acp-prefix+ model :end2 (length +acp-prefix+)))
      model
      (concatenate 'string +acp-prefix+ model)))

(defun approved-model-p (model)
  "Whether MODEL is one of the curated free ids, with or without the prefix."
  (let ((bare (if (string= +acp-prefix+ model :end2 (min (length model) (length +acp-prefix+)))
                  (subseq model (length +acp-prefix+))
                  model)))
    (and (find bare +free-models+ :test #'string=) t)))
