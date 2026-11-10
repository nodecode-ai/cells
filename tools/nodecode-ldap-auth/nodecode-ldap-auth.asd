;;;; nodecode-ldap-auth.asd --- who a username and password are, per the directory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://ldap-auth' is accepted. It reads the `ldap-auth'
;;;; config section and contributes three tools (ldap_login, ldap_lookup,
;;;; ldap_verify), one command (/ldap-auth) and one manual topic
;;;; (help :ldap-auth); nothing starts a thread and nothing dials until a verb
;;;; is called. LDAP is spoken on usocket, with cl+ssl for TLS - both already
;;;; in the image - so no Python, no ldap3 and no CLI is needed.

(defsystem "nodecode-ldap-auth"
  :description "LDAP / Active Directory password login: verify a bind, mint a session, look a name up"
  :license "MIT"
  :author "angel12 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "ironclad")
  :serial t
  :components ((:file "package")
               (:file "ber")
               (:file "token")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ldap-auth/test"))))

(defsystem "nodecode-ldap-auth/test"
  :description "LDAP auth tests: registered into the shared nodecode.test registry; RUN-LDAP-AUTH-TESTS filters by the LDAP-AUTH-CELL- name prefix."
  :license "MIT"
  :author "angel12 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ldap-auth" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ldap-auth-tests)))
