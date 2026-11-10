;;;; nodecode-mask.asd --- phones, emails, ID cards, bank cards and keys masked
;;;; before the text leaves.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `mask' section, hooks the wire fold so every user message goes
;;;; out with the personal data in it replaced by <TYPE_N> placeholders, and
;;;; registers the mask_test tool, /mask and (help :mask).
;;;;
;;;; A rewrite of PerryLink/dsh-mask (see NOTICE). The detectors are the
;;;; original's ten regular expressions, in cl-ppcre, on the dependencies the
;;;; serving image already carries.

(defsystem "nodecode-mask"
  :description "Masks phones, emails, ID cards, bank cards and keys in the text you send"
  :license "Apache-2.0"
  :author "PerryLink (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "detect")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mask/test"))))

(defsystem "nodecode-mask/test"
  :description "Mask tests: the lifecycle, the ten detectors, the numbered placeholders, the restore file, the tool and /mask. RUN-MASK-TESTS filters by the MASK-CELL- prefix."
  :license "Apache-2.0"
  :author "PerryLink (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mask" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mask-tests)))
