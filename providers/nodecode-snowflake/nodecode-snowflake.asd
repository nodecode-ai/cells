;;;; nodecode-snowflake.asd --- Snowflake Cortex as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `snowflake' section and installs the hooks cell.lisp
;;;; lists.
;;;;
;;;; Ported from oh-my-pi's snowflake provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-snowflake"
  :description "Snowflake Cortex: Claude and GPT models on a Snowflake account, signed in or with a PAT"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-snowflake/test"))))

(defsystem "nodecode-snowflake/test"
  :description "Snowflake Cortex tests. Registered into the shared nodecode.test registry; RUN-SNOWFLAKE-TESTS filters by the SNOWFLAKE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-snowflake" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-snowflake-tests)))
