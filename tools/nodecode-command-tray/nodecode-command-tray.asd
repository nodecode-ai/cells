;;;; nodecode-command-tray.asd --- the shell commands this session ran, with verdicts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of aydnOktay's hermes-command-tray (MIT). Optional
;;;; ASDF cell, NOT part of the organism core: nothing in src/ names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `command-tray' section, advises EVAL's SH so every command the
;;;; agent runs lands in the tray, registers the two tools the plugin had,
;;;; puts /commands in the slash catalog and the manual on (help :command-tray).
;;;;
;;;; The .asd sits INSIDE its folder: install by putting - or symlinking -
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-command-tray"
  :description "The shell commands this session ran, with exit codes, as tools and /commands"
  :license "MIT"
  :version "0.1.0"
  :author "aydnOktay (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-command-tray/test"))))

(defsystem "nodecode-command-tray/test"
  :description "Command tray tests. Registered into the shared nodecode.test registry; RUN-COMMAND-TRAY-TESTS filters by the COMMAND-TRAY-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "aydnOktay (original), Nodecode (rewrite)"
  :depends-on ("nodecode-command-tray" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-command-tray-tests)))
