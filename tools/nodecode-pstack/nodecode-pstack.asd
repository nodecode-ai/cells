;;;; nodecode-pstack.asd --- Poteto's engineering stack as one cell: the
;;;; router, its playbooks and its skills, read one at a time.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Cloeille's pstack for Hermes Agent (Cloeille/pstack, MIT).
;;;; The prose stays the original's, unchanged, under prose/; one `pstack'
;;;; tool and one /pstack command read it on demand. See NOTICE.

(defsystem "nodecode-pstack"
  :description "Poteto's engineering playbooks: routes a task to a playbook and loads it whole"
  :license "MIT"
  :author "Cloeille (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pstack/test"))))

(defsystem "nodecode-pstack/test"
  :description "pstack tests: the catalogue, the reader, its refusals, the command."
  :license "MIT"
  :author "Cloeille (original), Nodecode (rewrite)"
  :depends-on ("nodecode-pstack" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pstack-tests)))
