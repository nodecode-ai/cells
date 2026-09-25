;;;; nodecode-cline.asd --- Cline's API as a lane: its client header and its feed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF add-on, NOT part of the organism core: nothing in
;;;; src/src names this system. The folder loader (kernel addons.lisp) loads it
;;;; at boot when its directory sits under ~/.nodecode/addons/; the gateway
;;;; calls START-ADDON after recovery, which reads the `cline' config section
;;;; and advises two kernel functions.
;;;;
;;;; api.cline.bot serves a key's models in two ways the kernel cannot see:
;;;; its free models answer only a request that names its client in
;;;; X-CLIENT-TYPE, and neither they nor the Pass models appear in its
;;;; /models listing — Cline's own clients read them off a public feed. This
;;;; folder is that knowledge. It names nodecode as the client, never Cline.
;;;;
;;;; Every dependency is already in the serving image (dexador, shasht ride
;;;; with nodecode). Install it from the add-on hub (/setup, Choose), or clone
;;;; this repository into ~/.nodecode/addons/. Presence is enabled; the package
;;;; is named after the system, which is how the loader finds START-ADDON.

(defsystem "nodecode-cline"
  :description "Cline's API: the client header its free models need, and its model feed"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "feed")
               (:file "addon"))
  :in-order-to ((test-op (test-op "nodecode-cline/test"))))

(defsystem "nodecode-cline/test"
  :description "Cline tests. Registered into the shared nodecode.test registry; RUN-CLINE-TESTS filters by the CLINE-ADDON- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cline" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "addon-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cline-tests)))
