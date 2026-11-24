;;;; nodecode-hades-coding-workflows.asd --- three coding-workflow skills, read
;;;; by the index and never loaded.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A knowledge cell, NOT a peripheral: `:properties ((:knowledge . t))' and
;;;; no component tell the folder loader to record it and never load it. The
;;;; skills themselves are the SKILL.md files under skills/, the shape every
;;;; other coding agent writes, read in place as prose skills: the index lists
;;;; them, (help 'writing-plans) reads one whole, and an edit in this folder is
;;;; the skill at the next read.
;;;;
;;;; It is a rewrite of singhguri's Hades Coding Workflows plugin for Hermes
;;;; Agent -- see NOTICE. The /test system is the only thing that loads these
;;;; files, to prove the index really reads them.

;;; The layer system class is NODECODE.KERNEL's, and a cold ASDF finds this
;;; file before nodecode is loaded: read :class only once it exists, which in
;;; a Nodecode image it always does.
(eval-when (:load-toplevel :execute)
  (unless (find-package "NODECODE.KERNEL") (asdf:load-system "nodecode")))

(defsystem "nodecode-hades-coding-workflows"
  :description "Hades coding workflows: plan-writing, subagent-driven execution and a dual-model harness, as three skills"
  :license "MIT"
  :author "singhguri (original), Nodecode (rewrite)"
  :version "0.1.0"
  :class nodecode.kernel:layer
  :depends-on ("nodecode")
  :properties ((:knowledge . t))
  :components nil
  :in-order-to ((test-op (test-op "nodecode-hades-coding-workflows/test"))))

(defsystem "nodecode-hades-coding-workflows/test"
  :description "Reads this cell as the index does: a knowledge folder, three skills, whole prose. RUN-HADES-TESTS filters the shared registry by the HADES-CELL- prefix."
  :license "MIT"
  :author "singhguri (original), Nodecode (rewrite)"
  :version "0.1.0"
  ;; NODECODE/TEST FIRST: it loads nodecode, which defines the layer system
  ;; class this folder's own system is declared with.
  :depends-on ("nodecode/test" "nodecode-hades-coding-workflows")
  :pathname "test/"
  :serial t
  :components ((:file "hades-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hades-tests)))
