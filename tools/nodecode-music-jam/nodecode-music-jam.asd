;;;; nodecode-music-jam.asd --- a four-piece synthesized band on a local page.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: the folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/, and
;;;; the gateway calls START-CELL after recovery. Nothing runs until a tool
;;;; call or /music-jam starts the band.

(defsystem "nodecode-music-jam"
  :description "A four-piece synthesized band on a local page, conducted by tools"
  :license "MIT"
  :version "0.1.0"
  :author "vdimarco (original), Nodecode (rewrite)"
  :depends-on ("nodecode" "usocket")
  :serial t
  :components ((:file "package")
               (:file "music")
               (:file "engine")
               (:file "server")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-music-jam/test"))))

(defsystem "nodecode-music-jam/test"
  :description "The band's tests: the declaration, the queue, the page's API."
  :license "MIT"
  :depends-on ("nodecode-music-jam" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-music-jam-tests)))
