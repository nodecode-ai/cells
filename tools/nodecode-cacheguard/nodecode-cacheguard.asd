;;;; nodecode-cacheguard.asd --- a prompt-cache guard cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, not part of the organism core: a cell is a folder
;;;; carrying its .asd, and the gateway calls this system's START-CELL after
;;;; recovery. The cell wraps the one function that makes a provider call, so a
;;;; big prompt after a pause is reported, or stopped before it is sent.

(defsystem "nodecode-cacheguard"
  :description "Warns or blocks a big prompt after a pause, when the cache may have expired"
  :license "MIT"
  :author "AIverflow (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cacheguard/test"))))

(defsystem "nodecode-cacheguard/test"
  :description "Cacheguard tests. Registered into the shared nodecode.test registry; RUN-CACHEGUARD-TESTS filters by the CACHEGUARD- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cacheguard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cacheguard-tests)))
