;;;; nodecode-security-patches.asd --- the breaches and the fixes that follow them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core. The folder loader
;;;; loads it at boot when this directory sits under ~/.nodecode/cells/, or
;;;; the gateway installs it live when `nodecode add nc://nodecode-security-patches'
;;;; is accepted. Nothing here dials until asked, and the feed it reads is
;;;; refreshed at runtime, so an installed copy never goes stale.

(defsystem "nodecode-security-patches"
  :description "The last two weeks' breaches, each with the fix its vendor shipped"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "feed")
               (:file "detect")
               (:file "check")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-security-patches/test"))))

(defsystem "nodecode-security-patches/test"
  :description "Tests for the security-patches cell; RUN-SECURITY-PATCHES-TESTS filters the shared registry by the SECURITY-PATCHES- prefix."
  :license "MIT"
  :depends-on ("nodecode-security-patches" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "feed-test")
               (:file "detect-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-security-patches-tests)))
