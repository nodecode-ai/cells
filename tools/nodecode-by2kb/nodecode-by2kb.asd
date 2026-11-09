;;;; nodecode-by2kb.asd --- a forwarded video becomes a transcript, an abstract and study notes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `by2kb' section, registers the five by2kb_* tools and the
;;;; /by2kb command, puts the standing paragraph in every request, and the
;;;; manual on (help :by2kb).
;;;;
;;;; The one thing outside this folder is the by2kb program itself: a Python
;;;; CLI the user installs (pipx install "by2kb[asr-whisper,youtube]"), which
;;;; fetches Bilibili/YouTube media, transcribes it and keeps the knowledge
;;;; base. See NOTICE: this folder is a rewrite of Charlesmpc/by2kb's Hermes
;;;; plugin.

(defsystem "nodecode-by2kb"
  :description "by2kb: a Bilibili or YouTube video into a transcript, an abstract and study notes"
  :license "MIT"
  :version "0.1.0"
  :author "Charlesmpc (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-by2kb/test"))))

(defsystem "nodecode-by2kb/test"
  :description "by2kb tests. Registered into the shared nodecode.test registry; RUN-BY2KB-TESTS filters by the BY2KB-CELL- name prefix."
  :license "MIT"
  :author "Charlesmpc (original), Nodecode (rewrite)"
  :depends-on ("nodecode-by2kb" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-by2kb-tests)))
