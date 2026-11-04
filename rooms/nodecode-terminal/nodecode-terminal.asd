;;;; nodecode-terminal.asd --- the operator's shell on a pty of the image's own,
;;;; drawn in any page that reaches the gateway.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional cell, NOT part of the organism core: nothing in src/src names
;;;; this system. It replaces the desktop app's node-pty: the image opens the
;;;; pty itself (plain libc through SB-ALIEN, no C compiled, no Node), keeps
;;;; each terminal past the page that opened it, and streams it over one
;;;; WebSocket on the gateway's port, so the desktop window, a browser tab and
;;;; a phone draw the same shell. The page is /terminal/: xterm.js (MIT,
;;;; vendored, see NOTICE) and a hundred lines of its own. Linux and macOS;
;;;; Windows (ConPTY) is not here yet.

(defsystem "nodecode-terminal"
  :description "Your shell in the page: real terminals owned by the image; open /terminal/"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "pty")
               (:file "cell")
               (:static-file "page/index.html")
               (:static-file "page/terminal.js")
               (:static-file "page/terminal.css")
               (:static-file "page/xterm.js")
               (:static-file "page/xterm.css")
               (:static-file "page/addon-fit.js"))
  :in-order-to ((test-op (test-op "nodecode-terminal/test"))))

(defsystem "nodecode-terminal/test"
  :description "Terminal tests. Registered into the shared nodecode.test registry; RUN-TERMINAL-TESTS filters by the TERMINAL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-terminal" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "terminal-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-terminal-tests)))
