;;;; nodecode-wslc.asd --- commands in a Linux container on the WSL Containers runtime.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `wslc' section, registers the wslc_exec tool and its two
;;;; siblings, the /wslc command and (help :wslc).
;;;;
;;;; A rewrite of Yicheng-Mark's hermes-wslc-sandbox (see NOTICE). The
;;;; plugin's wslc CLI adapter (wslc.py) and its config shapers (config.py)
;;;; are reimplemented in Common Lisp, on what Nodecode already carries: uiop
;;;; and nlk:run-bounded.

(defsystem "nodecode-wslc"
  :description "Run one command in a Linux container on the WSL Containers runtime"
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cli")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-wslc/test"))))

(defsystem "nodecode-wslc/test"
  :description "WSL Containers sandbox tests. Registered into the shared nodecode.test registry; RUN-WSLC-TESTS filters by the WSLC-CELL- name prefix."
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :depends-on ("nodecode-wslc" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-wslc-tests)))
