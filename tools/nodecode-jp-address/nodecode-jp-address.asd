;;;; nodecode-jp-address.asd --- a Japanese address, split into prefecture, city and town.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes plugin `jp-address' by TakeshiTGAL: one tool the
;;;; model calls, `address_split', and one slash command, /jp-address, that
;;;; answer with the prefecture, the municipality, the town and the rest of a
;;;; Japanese address, split against Geolonia's public address data (CC BY
;;;; 4.0, from Japan's Digital Agency address base registry). The only peer is
;;;; the address data host; no key, no file, no subprocess.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting the directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-jp-address"
  :description "Splits a Japanese address into prefecture, city and town, over Geolonia's public address data"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "split")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jp-address/test"))))

(defsystem "nodecode-jp-address/test"
  :description "jp-address tests. They register into the shared nodecode.test registry; RUN-JP-ADDRESS-TESTS runs the JP-ADDRESS-CELL- slice."
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-jp-address" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jp-address-tests)))
