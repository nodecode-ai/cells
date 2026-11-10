;;;; nodecode-hotwire.asd --- an analog circuit bench: netlist in, verdict out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when this directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://hotwire' is accepted. It reads the `hotwire' config
;;;; section and contributes eight tools (hotwire_check, hotwire_simulate,
;;;; hotwire_library, hotwire_explain, hotwire_bom, hotwire_tune, hotwire_yield,
;;;; hotwire_export), one command (/hotwire) and one manual topic
;;;; (help :hotwire). Nothing here dials a host, starts a thread or writes a file.

(defsystem "nodecode-hotwire"
  :description "Analog circuit bench: a SPICE netlist simulated, proven against specs, priced and tuned"
  :license "MIT"
  :author "RohanGlitched (original), Nodecode (rewrite)"
  :version "0.2.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "netlist")
               (:file "sim")
               (:file "analysis")
               (:file "parts")
               (:file "tooling")
               (:file "library")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hotwire/test"))))

(defsystem "nodecode-hotwire/test"
  :description "Tests for the hotwire cell; RUN-HOTWIRE-TESTS filters the shared registry by the HOTWIRE- name prefix."
  :license "MIT"
  :depends-on ("nodecode-hotwire" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hotwire-tests)))
