;;;; nodecode-desktop.asd --- this home's page in a window of the organism's
;;;; own: the desktop app, with no Electron.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional cell, NOT part of the organism core. It opens the web cell's
;;;; page in a window the organism draws itself through lisptron
;;;; (nodecode-ai/lisptron): the OS's own web engine, called from Lisp
;;;; in-process. lisptron is a library folder beside this one in the cells
;;;; directory; it loads its GTK and WebKit libraries at the first window, so
;;;; an organism that never opens one (a server) loads this cell for free.

(defsystem "nodecode-desktop"
  :description "Nodecode in a desktop window of its own; /desktop opens it"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "lisptron")
  :serial t
  :components ((:file "cell")))
