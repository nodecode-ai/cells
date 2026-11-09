;;;; nodecode-browser-profiles.asd --- drive a named Chrome, Edge or Brave profile over CDP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `browser_profiles' section, puts (help :browser-profiles) on
;;;; the manual, registers the browser_profile tool and the /browser-profile
;;;; command, and stops the browsers it launched on the way down.
;;;;
;;;; A rewrite of anpicasso/hermes-plugin-chrome-profiles (see NOTICE).

(defsystem "nodecode-browser-profiles"
  :description "Drive a named Chrome, Edge or Brave profile over CDP"
  :license "MIT"
  :author "anpicasso (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-browser-profiles/test"))))

(defsystem "nodecode-browser-profiles/test"
  :description "Browser profile tests. Registered into the shared nodecode.test registry; RUN-BROWSER-PROFILES-TESTS filters by the BROWSER-PROFILES-CELL- name prefix."
  :license "MIT"
  :author "anpicasso (original), Nodecode (rewrite)"
  :depends-on ("nodecode-browser-profiles" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-browser-profiles-tests)))
