;;;; nodecode-hyprland.asd --- drive a Hyprland desktop: capture, click, drag,
;;;; scroll, type, keys, window focus.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `hyprland' section, registers the hyprland tool, /hyprland and
;;;; (help :hyprland).
;;;;
;;;; Rewritten from mddragon18's hermes hyprland computer-use provider (see
;;;; NOTICE). Everything it drives is a CLI the operator already has --
;;;; hyprctl, grim, wtype, wl-copy and the pointer helper built from the
;;;; original repository's pointer/ folder -- plus optional ImageMagick; no
;;;; Python, no XWayland, no accessibility tree, no /dev/uinput.

(defsystem "nodecode-hyprland"
  :description "Drive a Hyprland desktop: grim capture, pointer clicks and drags, wtype text, window focus"
  :license "MIT"
  :author "mddragon18 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "machine")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hyprland/test"))))

(defsystem "nodecode-hyprland/test"
  :description "Hyprland tests. Registered into the shared nodecode.test registry; RUN-HYPRLAND-TESTS filters by the HYPRLAND-CELL- name prefix."
  :license "MIT"
  :author "mddragon18 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-hyprland" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hyprland-tests)))
