;;;; selffix-plant.asd --- the Lisp spec's planted defect. SPDX-License-Identifier: MIT
;;;;
;;;; A cell that breaks every turn of a session whose input says SELFFIX-PLANT:
;;;; the standing sections call a generic function that has no method for a
;;;; session id, so the turn fails with NO-APPLICABLE-METHOD, the class of the
;;;; most frequent real failure. A file named `fixed' beside this one is the fix.

(defsystem "selffix-plant"
  :description "selffix Lisp spec: a planted defect"
  :license "MIT"
  :depends-on ("nodecode")
  :components ((:file "cell")))
