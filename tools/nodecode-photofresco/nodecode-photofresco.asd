;;;; nodecode-photofresco.asd --- the PhotoFresco editor, driven from Nodecode.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `photofresco' section and registers three tools, the
;;;; /photofresco command and the (help :photofresco) manual.
;;;;
;;;; A rewrite of photofresco/photofresco-agent: the same editor operations
;;;; over the same local browser link, spoken from Lisp instead of Node.

(defsystem "nodecode-photofresco"
  :description "Drive the PhotoFresco image editor from a browser window you approve"
  :license "MIT"
  :author "photofresco (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-photofresco/test"))))

(defsystem "nodecode-photofresco/test"
  :description "PhotoFresco tests: the link over a scripted browser, the tools' answers, the refusals."
  :license "MIT"
  :author "photofresco (original), Nodecode (rewrite)"
  :depends-on ("nodecode-photofresco" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-photofresco-tests)))
