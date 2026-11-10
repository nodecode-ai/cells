;;;; nodecode-hackertube.asd --- focused media search, and a library of what you keep.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes plugin `hackertube' by rcmiller01: two tools the
;;;; model calls, `hackertube_search' and `hackertube_library', and one slash
;;;; command, /hackertube, that search Hacker News, Apple Podcasts, Open
;;;; Library and LibriVox (and YouTube, when a key is configured) for one
;;;; subject, rank what came back, and keep what the operator saves in a file
;;;; under the home. The peers are those five HTTP APIs; no key but
;;;; YouTube's, no subprocess, no dashboard.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting the directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-hackertube"
  :description "Focused media search across Hacker News, podcasts, books and audiobooks, with a local library"
  :license "MIT"
  :author "rcmiller01 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "sources")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hackertube/test"))))

(defsystem "nodecode-hackertube/test"
  :description "HackerTube tests. They register into the shared nodecode.test registry; RUN-HACKERTUBE-TESTS runs the HACKERTUBE-CELL- slice."
  :license "MIT"
  :author "rcmiller01 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-hackertube" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hackertube-tests)))
