;;;; nodecode-cloudflare-sandbox.asd --- commands in a Cloudflare Container, through your Worker.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cloudflare-sandbox' section, registers the cf_sandbox tool,
;;;; /cloudflare-sandbox and (help :cloudflare-sandbox).
;;;;
;;;; A rewrite of kiyo-e/hermes-cloudflare-sandbox (see NOTICE). Its Worker,
;;;; deployed to the operator's own Cloudflare account, is the peer; this
;;;; folder is the client half, in Common Lisp, on the dependencies Nodecode
;;;; already carries (dexador through nlk:http, ppcre, uiop).

(defsystem "nodecode-cloudflare-sandbox"
  :description "Run one command in a Cloudflare Container, through the Worker you deployed"
  :license "MIT"
  :author "kiyo-e (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cloudflare-sandbox/test"))))

(defsystem "nodecode-cloudflare-sandbox/test"
  :description "Cloudflare sandbox tests. Registered into the shared nodecode.test registry; RUN-CLOUDFLARE-SANDBOX-TESTS filters by the CLOUDFLARE-SANDBOX-CELL- name prefix."
  :license "MIT"
  :author "kiyo-e (original), Nodecode (rewrite)"
  :depends-on ("nodecode-cloudflare-sandbox" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cloudflare-sandbox-tests)))
