;;;; cell-test.lisp --- the cell's half: the three tools, the command, the tokens.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the declaration installs the three tools, /ldap-auth
;;;; and (help :ldap-auth) and takes them all back on stop; a login binds with
;;;; the templated DN and answers the identity and a session; an unknown user
;;;; and a wrong password answer the same line; an empty password never reaches
;;;; the directory; search-then-bind finds one entry, rejects two, and escapes
;;;; the username into the filter; a group restriction is a membership probe on
;;;; the user's own connection; a token is checked without the directory, and a
;;;; tampered or expired one is refused; and the section's rules are the
;;;; original's, each refusal naming its key.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "ldap-auth"
  (:config "server_url" +url+ "user_dn_template" +direct+ "secret" +secret+)
  (:help :ldap-auth)
  (:command "ldap-auth")
  (:running (is (equal "direct" (nodecode-ldap-auth::setting :mode)) "the bind mode reads back")
            (is (= 43200 (nodecode-ldap-auth::setting :session-ttl-seconds)) "and the access-token lifetime"))
  (:refused ("server_url" "ldap://ldap.example.com" "user_dn_template" +direct+)
            ("server_url" +url+ "user_dn_template" +direct+ "user_search_base" +base+)
            ("server_url" +url+ "user_dn_template" "uid=someone,ou=people,dc=example,dc=com")
            ("server_url" +url+ "user_search_base" +base+ "user_search_filter" "(cn=someone)"))
  )

(defmacro with-ldap ((&rest pairs) &body body) "BODY with the cell started on PAIRS (a direct bind by default)."
  `(with-cell-stop ((ldap-auth-start "server_url" +url+ "secret" +secret+ ,@pairs))
     ,@body))

;;; --- a login by direct bind -------------------------------------------------------

(deftest ldap-auth-cell-a-direct-bind-answers-the-identity-and-a-session ()
  (with-ldap ("user_dn_template" +direct+)
    (with-directory ((nodecode-ldap-auth::bind-response 1 0))
      (let ((answer (ldap-answer (ldap-login-json "jdoe" "hunter2"))))
        (is (search (format nil "verified by bind as ~a" +user-dn+) answer) "the DN the template made")
        (is (search "access token: " answer) "and the session it minted")
        (is (search +user-dn+ (sent-text)) "the bind carried the templated DN")
        (is (search "hunter2" (sent-text)) "and the password, once, to the directory only")))))

(deftest ldap-auth-cell-the-templated-dn-escapes-the-username ()
  (with-ldap ("user_dn_template" +direct+)
    (with-directory ((nodecode-ldap-auth::bind-response 1 49))
      (ldap-answer (ldap-login-json "jdoe,ou=elsewhere" "x"))
      (is (search "uid=jdoe\\,ou\\=elsewhere,ou=people" (sent-text)) "a comma and an equals in the username cannot add an RDN to the DN"))))

(deftest ldap-auth-cell-a-wrong-password-and-an-unknown-user-answer-alike ()
  (with-ldap ("user_dn_template" +direct+)
    (with-directory ((nodecode-ldap-auth::bind-response 1 49))
      (is (equal "ldap-auth: the directory rejected that username and password"
                 (ldap-answer (ldap-login-json "jdoe" "wrong"))) "a rejected bind is one line")))
  (with-ldap ("user_search_base" +base+ "bind_dn" "cn=svc,ou=services,dc=example,dc=com" "bind_password" "pw")
    (with-directory ((nodecode-ldap-auth::bind-response 1 0)
                     (nodecode-ldap-auth::search-done 2 0)
                     (nodecode-ldap-auth::bind-response 3 49))
      (is (equal "ldap-auth: the directory rejected that username and password"
                 (ldap-answer (ldap-login-json "nobody" "wrong"))) "and so is an unknown user: the two cannot be told apart"))))

(deftest ldap-auth-cell-an-empty-password-never-reaches-the-directory ()
  (with-ldap ("user_dn_template" +direct+)
    (with-directory ()
      (is (search "rejected" (ldap-answer (ldap-login-json "jdoe" "   "))) "whitespace is no password")
      (is (zerop (length *sent*)) "and no bind was attempted, which would be anonymous and always succeed"))))

;;; --- search-then-bind ------------------------------------------------------------

(deftest ldap-auth-cell-search-then-bind-finds-one-entry-then-binds-as-it ()
  (with-ldap ("user_search_base" +base+ "bind_dn" "cn=svc,ou=services,dc=example,dc=com" "bind_password" "pw")
    (with-directory ((nodecode-ldap-auth::bind-response 1 0)
                     (nodecode-ldap-auth::search-entry 2 +user-dn+ '( ("mail" "jdoe@example.com") ("cn" "Jane Doe")))
                     (nodecode-ldap-auth::search-done 2 0)
                     (nodecode-ldap-auth::bind-response 3 0))
      (let ((answer (ldap-answer (ldap-login-json "jdoe" "hunter2"))))
        (is (search "name: Jane Doe" answer) "the entry's cn is the display name")
        (is (search "mail: jdoe@example.com" answer) "and its mail travels with the session")
        (is (search "uid" (sent-text)) "the filter's attribute is on the wire")
        (is (search "jdoe" (sent-text)) "with the username in it")))))

(deftest ldap-auth-cell-a-filter-that-matches-twice-rejects-the-login ()
  (with-ldap ("user_search_base" +base+)
    (with-directory ((nodecode-ldap-auth::bind-response 1 0)
                     (nodecode-ldap-auth::search-entry 2 "uid=a,dc=example,dc=com" '())
                     (nodecode-ldap-auth::search-entry 2 "uid=b,dc=example,dc=com" '())
                     (nodecode-ldap-auth::search-done 2 0)
                     (nodecode-ldap-auth::bind-response 3 49))
      (is (search "rejected" (ldap-answer (ldap-login-json "jdoe" "hunter2"))) "an ambiguous filter must not let a bind against either entry succeed"))))

(deftest ldap-auth-cell-the-username-is-escaped-into-the-filter ()
  (with-ldap ("user_search_base" +base+)
    (with-directory ((nodecode-ldap-auth::bind-response 1 0)
                     (nodecode-ldap-auth::search-done 2 0)
                     (nodecode-ldap-auth::bind-response 3 49))
      (ldap-answer (ldap-login-json "jdoe*x" "hunter2"))
      (is (search "jdoe*x" (sent-text)) "the value reaches the directory as written, one equality"))))

(deftest ldap-auth-cell-a-parenthesis-in-a-name-cannot-add-a-filter ()
  (with-ldap ("user_search_base" +base+)
    (with-directory ((nodecode-ldap-auth::bind-response 1 0)
                     (nodecode-ldap-auth::search-done 2 0)
                     (nodecode-ldap-auth::bind-response 3 49))
      (ldap-answer (ldap-login-json "jdoe)(uid=admin" "hunter2"))
      (is (search "jdoe)(uid=admin" (sent-text)) "the parenthesis is the value's, not the filter's: escaped into the template, read back into one equality"))))

;;; --- the group restriction --------------------------------------------------------

(deftest ldap-auth-cell-a-group-restriction-is-the-users-own-probe ()
  (with-ldap ("user_dn_template" +direct+ "require_group" +group+)
    (with-directory ((nodecode-ldap-auth::bind-response 1 0)
                     (nodecode-ldap-auth::search-entry 2 +group+ '())
                     (nodecode-ldap-auth::search-done 2 0))
      (is (search (format nil "group: in ~a" +group+)
                  (ldap-answer (ldap-login-json "jdoe" "hunter2"))) "a member signs in")
      (is (search "member" (sent-text)) "the probe asks the group entry for membership")
      (is (search "memberUid" (sent-text)) "in all three group schemas at once")))
  (with-ldap ("user_dn_template" +direct+ "require_group" +group+)
    (with-directory ((nodecode-ldap-auth::bind-response 1 0)
                     (nodecode-ldap-auth::search-done 2 0))
      (is (search "rejected" (ldap-answer (ldap-login-json "jdoe" "hunter2"))) "and a non-member reads as a wrong password"))))

;;; --- tokens ---------------------------------------------------------------------

(deftest ldap-auth-cell-a-token-is-checked-without-the-directory ()
  (with-ldap ("user_dn_template" +direct+)
    (with-directory ((nodecode-ldap-auth::bind-response 1 0))
      (let ((token (token-in (ldap-answer (ldap-login-json "jdoe" "hunter2")))))
        (is (stringp token) "a login mints a token")
        (with-directory ()
          (let ((answer (ldap-tool "ldap-verify" "ldap_verify" (format nil "{\"token\": \"~a\"}" token))))
            (is (search "expires" answer) "and reading it back needs no bind")
            (is (search +user-dn+ answer) "the token still says whose it is")))
        (with-directory ()
          (is (search "signature or its expiry"
                      (ldap-tool "ldap-verify-2" "ldap_verify"
                                 (format nil "{\"token\": \"~a\"}" (subseq token 0 (1- (length token)))))) "a token with a byte missing is refused"))))))

(deftest ldap-auth-cell-an-expired-token-is-refused ()
  (let* ((key (nodecode-ldap-auth::token-key +secret+))
         (token (nodecode-ldap-auth:sign-token
                 (list (cons "sub" "jdoe") (cons "dn" +user-dn+) (cons "em" "")
                       (cons "nm" "Jane Doe") (cons "kind" "access") (cons "exp" 1000))
                 key)))
    (is (nodecode-ldap-auth:verify-token token key :now 999) "valid a second before it expires")
    (is (null (nodecode-ldap-auth:verify-token token key :now 1001)) "and nothing after")
    (is (null (nodecode-ldap-auth:verify-token token (nodecode-ldap-auth::token-key "another key, long enough") :now 999)) "a token signed by another key is nobody")))

;;; --- the lookup and the command ----------------------------------------------------

(deftest ldap-auth-cell-the-lookup-and-the-command-read-the-directory ()
  (with-ldap ("user_search_base" +base+)
    (with-directory ((nodecode-ldap-auth::bind-response 1 0)
                     (nodecode-ldap-auth::search-entry 2 +user-dn+ '(("mail" "jdoe@example.com") ("cn" "Jane Doe")))
                     (nodecode-ldap-auth::search-done 2 0))
      (let ((answer (ldap-tool "ldap-lookup" "ldap_lookup" "{\"username\": \"jdoe\"}")))
        (is (search "Jane Doe" answer) "ldap_lookup answers the entry")
        (is (search "jdoe@example.com" answer) "with its mail")))
    (with-directory ((nodecode-ldap-auth::bind-response 1 0)
                     (nodecode-ldap-auth::search-entry 2 +user-dn+ '(("mail" "jdoe@example.com") ("cn" "Jane Doe")))
                     (nodecode-ldap-auth::search-done 2 0))
      (is (search "Jane Doe" (nodecode-ldap-auth:run-slash "jdoe" nil)) "/ldap-auth <username> answers the same, for the operator")))
  (with-ldap ("user_dn_template" +direct+)
    (is (search "direct bind" (nodecode-ldap-auth:run-slash "" nil)) "alone, the command says how the cell is set up")
    (is (search "search-then-bind" (ldap-tool "ldap-lookup-2" "ldap_lookup" "{\"username\": \"jdoe\"}")) "and a lookup in direct-bind mode says which mode it would need")))

;;; --- escaping, at the bottom --------------------------------------------------------

(deftest ldap-auth-cell-a-stopped-cell-refuses-its-verbs ()
  (with-cell-stop ((ldap-auth-start "server_url" +url+ "user_dn_template" +direct+))
    (is (search "direct bind" (nodecode-ldap-auth:run-slash "" nil)) "running, it says how it is set up"))
  (is (search "not running"
              (handler-case (progn (nodecode-ldap-auth:run-slash "" nil) "no refusal")
                (nodecode-ldap-auth:ldap-auth-error (condition) (princ-to-string condition)))) "and stopped, a verb says so"))

(deftest ldap-auth-cell-escaping-keeps-a-username-one-value ()
  (is (equal "jdoe\\,ou\\=x" (nodecode-ldap-auth:escape-dn "jdoe,ou=x")) "a DN's separators are escaped")
  (is (equal "\\ jdoe" (nodecode-ldap-auth:escape-dn " jdoe")) "a leading space too")
  (is (equal "jdoe\\2ax\\28y\\29" (nodecode-ldap-auth:escape-filter "jdoe*x(y)")) "a filter's syntax is escaped")
  (is (equal "(uid=jdoe)" (nodecode-ldap-auth::substitute-username "(uid={username})" "jdoe")) "and {username} is the one placeholder"))
