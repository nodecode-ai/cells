;;;; nodecode-colab.asd --- Google Colab runtimes as a Nodecode tool.
;;;;
;;;; SPDX-License-Identifier: MIT

(defsystem "nodecode-colab"
  :description "Run a Python file on a Google Colab GPU runtime and bring its outputs home"
  :author "PietrangeloManco (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-colab/test"))))

(defsystem "nodecode-colab/test"
  :description "Colab tests. Registered into the shared nodecode.test registry; RUN-COLAB-TESTS filters by the COLAB-CELL- name prefix."
  :author "PietrangeloManco (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-colab" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-colab-tests)))
