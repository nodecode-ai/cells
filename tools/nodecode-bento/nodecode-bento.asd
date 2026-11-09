;;;; nodecode-bento.asd --- Bento email marketing as fifteen model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `bento' section, registers the fifteen bento_* tools the
;;;; original plugin gave its harness, puts /bento and (help :bento) on, and
;;;; hands the model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: Python, standard library only,
;;;; talking to app.bentonow.com with HTTP Basic. This cell makes the same
;;;; requests through the image's own HTTP call -- no Python, nothing fetched
;;;; at run time, no dependency of its own -- and keeps the original's safety
;;;; rule in Lisp: a dry run first, and a confirm_token bound to the exact
;;;; body before any Tier 2 or Tier 3 call goes out.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-bento"
  :description "Bento email marketing: subscribers, tags, events, broadcasts and transactional email"
  :license "MIT"
  :version "0.1.0"
  :author "shagghiesuperstar (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "safety")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-bento/test"))))

(defsystem "nodecode-bento/test"
  :description "Bento tests. Registered into the shared nodecode.test registry; RUN-BENTO-TESTS filters by the BENTO-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "shagghiesuperstar (original), Nodecode (rewrite)"
  :depends-on ("nodecode-bento" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-bento-tests)))
