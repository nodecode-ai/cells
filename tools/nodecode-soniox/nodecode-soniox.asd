;;;; nodecode-soniox.asd --- Soniox speech and transcription as Nodecode tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `soniox' section says which key, voice, model and folder every call uses.
;;;; It is a rewrite of XimilalaXiang's Soniox voice plugin for Hermes Agent --
;;;; see NOTICE.

(defsystem "nodecode-soniox"
  :description "Soniox voice: text becomes a spoken audio file, a recording becomes text"
  :license "MIT"
  :author "XimilalaXiang (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "audio")
               (:file "wire")
               (:file "speech")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-soniox/test"))))

(defsystem "nodecode-soniox/test"
  :description "Tests for the soniox cell; RUN-SONIOX-TESTS filters the shared registry by the SONIOX- prefix."
  :license "MIT"
  :author "XimilalaXiang (original), Nodecode (rewrite)"
  :depends-on ("nodecode-soniox" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-soniox-tests)))
