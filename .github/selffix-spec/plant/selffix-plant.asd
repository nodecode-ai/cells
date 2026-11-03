;;;; selffix-plant.asd --- a planted defect for the selffix spec. SPDX-License-Identifier: MIT
;;;;
;;;; A cell that breaks every turn of a session whose input says SELFFIX-PLANT:
;;;; the standing sections call a function that does not exist, the way a
;;;; layer that kept a removed name once broke real turns. Removing the folder
;;;; is the fix.

(defsystem "selffix-plant"
  :description "selffix spec: a planted defect"
  :license "MIT"
  :depends-on ("nodecode")
  :components ((:file "cell")))
