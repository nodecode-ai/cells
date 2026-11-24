;;;; nodecode-github-refs.asd --- a public GitHub issue or pull request, attached as text.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `github-refs' section, registers the one tool the original
;;;; plugin gave its harness, puts /gh and the manual topic on, and hands the
;;;; model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: a context-reference provider that
;;;; turned @gh:owner/repo#123 into a GitHub issue or pull request attached to
;;;; the turn. This cell makes the same anonymous GETs through the image's own
;;;; HTTP call -- no Python, nothing fetched at run time, no dependency of its
;;;; own -- and answers the same block as the tool's text.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-github-refs"
  :description "GitHub references: a public issue or pull request, with its comments, as one model tool"
  :license "MIT"
  :version "0.1.0"
  :author "EloquentBrush0x (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "reference")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-github-refs/test"))))

(defsystem "nodecode-github-refs/test"
  :description "GitHub references tests. Registered into the shared nodecode.test registry; RUN-GITHUB-REFS-TESTS filters by the GITHUB-REFS-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "EloquentBrush0x (original), Nodecode (rewrite)"
  :depends-on ("nodecode-github-refs" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-github-refs-tests)))
