;;;; nodecode-comfyrizz.asd --- Comfyrizz's film pipeline as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `comfyrizz' section, registers the twelve comfyrizz_* tools,
;;;; /comfyrizz and (help :comfyrizz).
;;;;
;;;; A Nodecode rewrite of mojomast/comfyrizz-hermes (see NOTICE).

(defsystem "nodecode-comfyrizz"
  :description "Comfyrizz film pipeline: twelve tools over the local engine, with the spend-approval loop in (help :comfyrizz)"
  :license "MIT"
  :version "0.1.0"
  :author "mojomast (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-comfyrizz/test"))))

(defsystem "nodecode-comfyrizz/test"
  :description "Comfyrizz cell tests. Registered into the shared nodecode.test registry; RUN-COMFYRIZZ-TESTS filters by the COMFYRIZZ-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "mojomast (original), Nodecode (rewrite)"
  :depends-on ("nodecode-comfyrizz" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-comfyrizz-tests)))
