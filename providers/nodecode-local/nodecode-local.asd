;;;; nodecode-local.asd --- omp's on-device tiny models, through the workers omp runs them in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `local' section, registers the local lane and installs the
;;;; hooks cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's local provider (see NOTICE). Every dependency
;;;; rides with nodecode: the worker socket is SBCL's own sb-bsd-sockets.

(defsystem "nodecode-local"
  :description "Local models: omp's tiny-model workers (ONNX, MLX) as a Nodecode provider lane"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "worker")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-local/test"))))

(defsystem "nodecode-local/test"
  :description "Local model tests. Registered into the shared nodecode.test registry; RUN-LOCAL-TESTS filters by the LOCAL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-local" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-local-tests)))
