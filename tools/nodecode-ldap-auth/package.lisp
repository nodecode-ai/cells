;;;; package.lisp --- the cell's package: nodecode-ldap-auth.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system and the folder, so the loader
;;;; finds START-CELL as its exported symbol and every contribution is filed
;;;; under "nodecode-ldap-auth". The slash command, the help topic and the
;;;; condition are the short name alone (ldap-auth, :ldap-auth,
;;;; LDAP-AUTH-ERROR), the way the contract asks.

(defpackage #:nodecode-ldap-auth
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:ldap-auth-error
           #:ldap-auth-error-detail
           #:run-login
           #:run-lookup
           #:run-verify
           #:run-slash
           #:escape-dn
           #:escape-filter
           #:sign-token
           #:verify-token))
