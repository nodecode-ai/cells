;;;; nodecode-omnivoice.asd --- OmniVoice speech, made on this machine.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `omnivoice' section says which command, model, voice and folder every
;;;; call uses. It is a rewrite of ThaungThanHan's OmniVoice TTS plugin for
;;;; Hermes Agent -- see NOTICE.

(defsystem "nodecode-omnivoice"
  :description "OmniVoice speech on this machine: a text becomes an audio file, by voice design or a reference clip"
  :license "Apache-2.0"
  :author "ThaungThanHan (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-omnivoice/test"))))

(defsystem "nodecode-omnivoice/test"
  :description "Tests for the omnivoice cell; RUN-OMNIVOICE-TESTS filters the shared registry by the OMNIVOICE- prefix."
  :license "Apache-2.0"
  :author "ThaungThanHan (original), Nodecode (rewrite)"
  :depends-on ("nodecode-omnivoice" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-omnivoice-tests)))
