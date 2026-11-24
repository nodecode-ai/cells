;;;; nodecode-cloudgpu.asd --- CloudGPU, cloudgpu.app's models, as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cloudgpu' section and installs the hooks and the command
;;;; cell.lisp lists.
;;;;
;;;; Rewritten from mrhy7176/hermes-cloudgpu (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-cloudgpu"
  :description "CloudGPU: DeepSeek, Kimi, GLM and MiniMax models from cloudgpu.app as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :author "mrhy7176 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "cloudgpu" :name "CloudGPU" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cloudgpu/test"))))

(defsystem "nodecode-cloudgpu/test"
  :description "CloudGPU tests. Registered into the shared nodecode.test registry; RUN-CLOUDGPU-TESTS filters by the CLOUDGPU-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cloudgpu" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cloudgpu-tests)))
