;;;; nodecode-network-xray.asd --- one URL's DNS, TCP, verified TLS and HTTP
;;;; HEAD evidence from this machine, saved and compared: four tools and a
;;;; /network-xray command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `network-xray' section and registers network_xray_probe,
;;;; network_xray_import, network_xray_compare, network_xray_history,
;;;; /network-xray and (help :network-xray).
;;;;
;;;; Rewritten from rusty4444's hermes-network-xray (see NOTICE). Everything
;;;; it needs rides with nodecode: the sockets are usocket's, the TLS is
;;;; cl+ssl's, the JSON is the kernel's and the digest is ironclad's.

(defsystem "nodecode-network-xray"
  :description "X-ray one URL from this machine: DNS, TCP, verified TLS and HTTP HEAD evidence, saved and compared"
  :license "MIT"
  :author "rusty4444 (original), Nodecode (rewrite)"
  :version "0.1.0"
  ;; ironclad's SHA-256 seals a record's content; the kernel names no ironclad
  ;; system, so a cell that digests names the one it uses (as this image's
  ;; other digesting cells do).
  :depends-on ("nodecode" "ironclad/digest/sha256")
  :serial t
  :components ((:file "package")
               (:file "probe")
               (:file "records")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-network-xray/test"))))

(defsystem "nodecode-network-xray/test"
  :description "Network X-ray tests. Registered into the shared nodecode.test registry; RUN-NETWORK-XRAY-TESTS filters by the NETWORK-XRAY-CELL- name prefix."
  :license "MIT"
  :author "rusty4444 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-network-xray" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-network-xray-tests)))
