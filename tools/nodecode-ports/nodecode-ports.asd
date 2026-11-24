;;;; nodecode-ports.asd --- who holds a local port: the TCP/UDP owner tables as a
;;;; tool and a /ports command, on Windows, Linux and macOS.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ports' section and registers the win_port_owners tool, /ports
;;;; and (help :ports).
;;;;
;;;; Rewritten from Yicheng-Mark's hermes-windows-ports (see NOTICE). Every
;;;; dependency rides with nodecode: the tables come from iphlpapi on Windows,
;;;; /proc on Linux and lsof on macOS, and nothing else is installed.

(defsystem "nodecode-ports"
  :description "Who holds a local port: the TCP/UDP owner tables, with PID and process, as a tool and /ports"
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "table")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ports/test"))))

(defsystem "nodecode-ports/test"
  :description "Ports tests. Registered into the shared nodecode.test registry; RUN-PORTS-TESTS filters by the PORTS-CELL- name prefix."
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ports" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ports-tests)))
