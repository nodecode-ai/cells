;;;; nodecode-progress-squash.asd --- a peripheral cell: one advice on SH.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The .asd sits inside its folder: a cell is a directory carrying its
;;;; .asd, installed by putting — or symlinking — that directory under
;;;; ~/.nodecode/cells/. Presence is enabled; nothing in config names it.
;;;;
;;;; A Nodecode rewrite of pierrenode's hermes-progress-squash, whose whole
;;;; plugin is one transform_terminal_output hook. Nodecode has no such point,
;;;; so the same transform sits on NLE:SH, the one function a model's
;;;; (sh "...") call runs through.

(defsystem "nodecode-progress-squash"
  :description "Collapses terminal progress-bar redraws into the final frame, shrinking tqdm and git clone output"
  :license "MIT"
  :author "pierrenode (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-progress-squash/test"))))

(defsystem "nodecode-progress-squash/test"
  :description "Progress-squash tests: the transform, its settings, the command, and the hook's lifecycle."
  :license "MIT"
  :author "pierrenode (original), Nodecode (rewrite)"
  :depends-on ("nodecode-progress-squash" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-progress-squash-tests)))
