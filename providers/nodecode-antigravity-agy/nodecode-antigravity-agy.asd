;;;; nodecode-antigravity-agy.asd --- Antigravity's agy CLI as a provider cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `antigravity-agy' section, publishes the provider's catalog row
;;;; and serves the loopback relay a round is sent to.
;;;;
;;;; A round on the antigravity-agy provider is answered by the operator's own
;;;; signed-in `agy' CLI: Nodecode builds the chat body as for any
;;;; OpenAI-compatible provider, the cell turns it into one agy prompt, and
;;;; the answer streams back as chat chunks. Tools run in Nodecode, never in
;;;; agy. The shape follows Realtyxxx's hermes-plugin-antigravity-agy (see
;;;; NOTICE).
;;;;
;;;; Every dependency is already in the serving image (usocket rides with
;;;; dexador, uiop with nodecode). The .asd sits at the top of this folder.

(defsystem "nodecode-antigravity-agy"
  :description "Antigravity (agy CLI): your Google Antigravity subscription as a Nodecode provider, answered by agy"
  :license "MIT"
  :author "Realtyxxx (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, and its login is the agy CLI's own
  :properties ((:serves (:provider "antigravity-agy" :name "Antigravity (agy CLI)")))
  :serial t
  :components ((:file "package")
               (:file "agy")
               (:file "relay")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-antigravity-agy/test"))))

(defsystem "nodecode-antigravity-agy/test"
  :description "Antigravity (agy CLI) tests. Registered into the shared nodecode.test registry; RUN-ANTIGRAVITY-AGY-TESTS filters by the ANTIGRAVITY-AGY-CELL- name prefix."
  :license "MIT"
  :author "Realtyxxx (original), Nodecode (rewrite)"
  :depends-on ("nodecode-antigravity-agy" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-antigravity-agy-tests)))
