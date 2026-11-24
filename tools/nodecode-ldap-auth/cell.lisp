;;;; cell.lisp --- who a username and password are, according to the directory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of angel12/hermes-ldap-auth, an LDAP / Active
;;;; Directory dashboard-auth provider. What the original gave a dashboard - a
;;;; password checked by an LDAP bind, a username escaped before it reaches a
;;;; DN or a filter, a group restriction, a stateless signed session - is here
;;;; as the three verbs anyone signing a user in needs: ldap_login checks the
;;;; password and mints a session, ldap_verify reads a session back without
;;;; touching the directory, ldap_lookup shows what the directory holds for a
;;;; name, and /ldap-auth does the last two for the operator.

(in-package #:nodecode-ldap-auth)

(nlk:define-peripheral ldap-auth :not-running t)

;;; --- the section, validated --------------------------------------------------

(defun refuse (control &rest arguments)
  "A config refusal, the way the setup panel and the cells report read it."
  (error 'nlk:config-refusal :detail (apply #'format nil control arguments)))

(defun trimmed (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or value "")))

(defun resolve-settings (values)
  "VALUES as validated: the original's own configuration rules, whose breach
is a refusal that names the key rather than a login that fails later."
  (let* ((url (trimmed (getf values :server-url)))
         (template (trimmed (getf values :user-dn-template)))
         (base (trimmed (getf values :user-search-base)))
         (filter (trimmed (getf values :user-search-filter)))
         (start-tls (getf values :start-tls))
         (insecure (getf values :allow-insecure))
         (secret (trimmed (getf values :secret))))
    (when (plusp (length url))
      (unless (or (starts-with-p "ldaps://" url) (starts-with-p "ldap://" url))
        (refuse "server_url must start with ldap:// or ldaps:// (~s is neither)" url))
      (when (and (starts-with-p "ldap://" url) (not start-tls) (not insecure))
        (refuse "plain ldap:// sends passwords in cleartext: set start_tls, use ldaps://, or set allow_insecure to accept the risk"))
      (when (and (plusp (length template)) (plusp (length base)))
        (refuse "user_dn_template (direct bind) and user_search_base (search-then-bind) are mutually exclusive: set exactly one"))
      (unless (or (plusp (length template)) (plusp (length base)))
        (refuse "no bind mode: set user_dn_template (direct bind) or user_search_base (search-then-bind)"))
      (when (and (plusp (length template)) (not (search "{username}" template)))
        (refuse "user_dn_template must contain the {username} placeholder"))
      (when (and (plusp (length base)) (not (search "{username}" (if (plusp (length filter)) filter "(uid={username})"))))
        (refuse "user_search_filter must contain the {username} placeholder"))
      (when (and (plusp (length secret)) (< (length secret) 16))
        (refuse "secret must be at least 16 bytes: it signs every session (openssl rand -base64 32)")))
    (append values
            (list :configured (plusp (length url))
                  :mode (cond ((plusp (length template)) "direct")
                              ((plusp (length base)) "search")
                              (t nil))
                  :token-key (token-key secret)
                  :secret-set (plusp (length secret))))))

;;; --- the answers -----------------------------------------------------------

(defparameter +denial+ "ldap-auth: the directory rejected that username and password"
  "One answer for an unknown user and for a wrong password alike: a login
endpoint that tells them apart probes the directory for free.")

(defun hours (seconds)
  (let ((hours (/ seconds 3600.0)))
    (if (= hours (fround hours))
        (format nil "~d h" (round hours))
        (format nil "~,1f h" hours))))

(defun iso-time (epoch)
  "EPOCH as the UTC stamp a session's expiry is read as."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (+ epoch 2208988800) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun identity-text (identity)
  "IDENTITY as the lines a caller acts on."
  (with-output-to-string (out)
    (format out "ldap-auth: verified by bind as ~a" (getf identity :dn))
    (let ((display (getf identity :display ""))
          (email (getf identity :email "")))
      (when (plusp (length display)) (format out "~%  name: ~a" display))
      (when (plusp (length email)) (format out "~%  mail: ~a" email)))
    (let ((group (getf identity :group)))
      (when group (format out "~%  group: in ~a" group)))))

(defun mint-token (settings identity kind)
  "A signed token for IDENTITY of KIND (\"access\" or \"refresh\")."
  (let ((ttl (if (equal kind "refresh")
                 (getf settings :refresh-ttl-seconds 2592000)
                 (getf settings :session-ttl-seconds 43200))))
    (sign-token (list (cons "sub" (getf identity :display (getf identity :dn "")))
                      (cons "dn" (getf identity :dn ""))
                      (cons "em" (getf identity :email ""))
                      (cons "nm" (getf identity :display ""))
                      (cons "kind" kind)
                      (cons "exp" (+ (now-seconds) ttl)))
                (getf settings :token-key))))

(defun session-text (settings identity)
  "The session IDENTITY is minted, as the lines a caller keeps."
  (let ((access (mint-token settings identity "access"))
        (refresh (mint-token settings identity "refresh")))
    (format nil "~%session: access ~a (until ~a), refresh ~a~%access token: ~a~%refresh token: ~a"
            (hours (getf settings :session-ttl-seconds 43200))
            (iso-time (+ (now-seconds) (getf settings :session-ttl-seconds 43200)))
            (hours (getf settings :refresh-ttl-seconds 2592000))
            access refresh)))

(defun reason-text (prefix condition)
  (format nil "~a: ~a" prefix (short-error condition)))

(defun concat-text (&rest parts)
  (apply #'concatenate 'string parts))

;;; --- the tools -------------------------------------------------------------

(nlk:define-contract login-arguments
  (:doc "Check a username and password against the LDAP / Active Directory
directory this cell is configured with, by binding to the directory as that user:
the password is verified there and is never stored, hashed or compared here.
Call it when someone signs in to something being built, or to confirm that a
named account works before it is used. It answers the verified identity (DN, mail,
display name, group) and a signed session token, or one line saying the directory
rejected the pair - the same line for an unknown user and a wrong password. It never
lists a directory, never changes one, and never keeps the password.")
  ("username" :text :required t :doc "the login name, as the directory knows it (uid, sAMAccountName, or a mail address, depending on the filter)")
  ("password" :text :required t :doc "the password to verify; it goes to the directory in a bind and nowhere else"))

(defun run-login (arguments)
  "ldap_login: the pair checked, and what the directory says."
  (let ((settings (running-settings)))
    (unless (getf settings :configured)
      (return-from run-login "ldap-auth: no directory is set: put server_url and one bind mode in the ldap-auth section of the config"))
    (handler-case
        (let* ((username (login-arguments-username arguments))
               (password (login-arguments-password arguments))
               (identity (verify-credentials settings username password)))
          (if (null identity)
              +denial+
              (let* ((group (getf settings :require-group ""))
                     (identity (if (plusp (length group))
                                   (append identity (list :group group))
                                   identity)))
                (concat-text (identity-text identity)
                             (session-text settings identity)))))
      (ldap-auth-error (condition) (reason-text "ldap-auth" condition)))))

(nlk:define-contract lookup-arguments
  (:doc "What the directory holds for a username, with no password: the entry's
DN, mail, display name and - when the section names a group - whether the account
is in it. Search-then-bind mode only, since a direct bind knows no username to
search for. Answers one line when no entry matches, or when a filter matches more
than one (which is worth fixing: an ambiguous filter is a login that never works).")
  ("username" :text :required t :doc "the login name to look up"))

(defun run-lookup (arguments)
  "ldap_lookup: the directory's record of a name, no password involved."
  (let ((settings (running-settings)))
    (unless (getf settings :configured)
      (return-from run-lookup "ldap-auth: no directory is set: put server_url and user_search_base in the ldap-auth section of the config"))
    (handler-case
        (let ((account (lookup-account settings (lookup-arguments-username arguments))))
          (if (null account)
              (format nil "ldap-auth: no single entry matches ~a under ~a"
                      (lookup-arguments-username arguments) (getf settings :user-search-base))
              (concat-text (identity-text account)
                           (let ((group (getf account :in-group)))
                             (if (null group) ""
                                 (format nil "~%  group: ~:[not in~;in~] ~a" group (getf settings :require-group)))))))
      (ldap-auth-error (condition) (reason-text "ldap-auth" condition)))))

(nlk:define-contract verify-arguments
  (:doc "Read a session token this cell minted back into the identity it stands
for - the signature and the expiry are checked, and the directory is not asked
(unless the section verifies the account on refresh, in which case a session whose
account has since been deleted or moved is refused). Use it instead of a second
login wherever a caller hands over a token; a token whose signature or expiry does
not hold answers one line, never another user's name.")
  ("token" :text :required t :doc "the access token, as ldap_login answered it"))

(defun run-verify (arguments)
  "ldap_verify: the identity a token stands for, or why it is no good."
  (let ((settings (running-settings)))
    (unless (getf settings :configured)
      (return-from run-verify "ldap-auth: no directory is set: put server_url and one bind mode in the ldap-auth section of the config"))
    (handler-case
        (let ((payload (verify-token (verify-arguments-token arguments) (getf settings :token-key))))
          (cond
            ((null payload) "ldap-auth: that token's signature or its expiry does not hold")
            ((and (getf settings :verify-user-on-refresh) (getf settings :user-search-base)
                  (not (still-present-p settings (cdr (assoc "dn" payload :test #'string=)))))
             "ldap-auth: the directory no longer holds that account")
            (t (format nil "ldap-auth: ~a (~a), ~a~%  expires ~a"
                       (cdr (assoc "nm" payload :test #'string=))
                       (cdr (assoc "dn" payload :test #'string=))
                       (if (plusp (length (or (cdr (assoc "em" payload :test #'string=)) "")))
                           (cdr (assoc "em" payload :test #'string=)) "no mail in the entry")
                       (iso-time (expiry-of payload))))))
      (ldap-auth-error (condition) (reason-text "ldap-auth" condition)))))

;;; --- /ldap-auth -----------------------------------------------------------

(defparameter +usage+
  "ldap-auth: /ldap-auth <username> - what the directory holds for that name (DN,
 mail, display name, group), with no password involved. Typed alone, it says how
the cell is configured. Signing in is the ldap_login tool: it verifies a password by
an LDAP bind and mints a session token, which ldap_verify reads back."
  "What /ldap-auth says when it is typed alone with nothing to show.")

(defun status-text (settings)
  (with-output-to-string (out)
    (format out "ldap-auth: ~a against ~a"
            (if (equal (getf settings :mode) "direct")
                "direct bind"
                "search-then-bind")
            (getf settings :server-url))
    (when (plusp (length (getf settings :require-group "")))
      (format out "~%  group: ~a" (getf settings :require-group)))
    (format out "~%  access ~a, refresh ~a; signing key ~:[generated for this process~;from the section~]"
            (hours (getf settings :session-ttl-seconds 43200))
            (hours (getf settings :refresh-ttl-seconds 2592000))
            (getf settings :secret-set))
    (format out "~%  /ldap-auth <username> looks an account up; ldap_login signs one in")))

(defun run-slash (args session-id)
  "The /ldap-auth command: a lookup for the name typed, the configuration alone."
  (declare (ignore session-id))
  (let ((settings (running-settings))
        (name (trimmed args)))
    (unless (getf settings :configured)
      (return-from run-slash "ldap-auth: no directory is set: put server_url and one bind mode in the ldap-auth section of the config"))
    (if (zerop (length name))
        (status-text settings)
        (handler-case
            (let ((account (lookup-account settings name)))
              (if (null account)
                  (format nil "ldap-auth: no single entry matches ~a under ~a" name (getf settings :user-search-base))
                  (concat-text (identity-text account)
                               (let ((group (getf account :in-group)))
                                 (if (null group) ""
                                     (format nil "~%  group: ~:[not in~;in~] ~a" group (getf settings :require-group)))))))
          (ldap-auth-error (condition) (reason-text "ldap-auth" condition))))))

;;; --- the manual -----------------------------------------------------------

(defparameter +manual+
  "ldap-auth: signs a user in against an LDAP / Active Directory directory, by
binding to it as that user with the password they gave - the directory is the only
thing that verifies a password here, so none is stored or hashed. Two bind modes,
exactly one configured: direct bind (user_dn_template like
\"uid={username},ou=people,dc=example,dc=com\", the username DN-escaped) and
search-then-bind (a service account bind_dn/bind_password searches user_search_base
with user_search_filter, default \"(uid={username})\", for exactly one entry and the
user is bound as its DN; use \"(sAMAccountName={username})\" for Active Directory).
Verbs: ldap_login with a username and password answers the verified identity (DN,
mail, display name) and a signed access and refresh token, or one line - the same
one for an unknown user and a wrong password; ldap_verify reads a token back into
its identity with the directory untouched, unless verify_user_on_refresh is on and
the mode is search, when it also checks the entry is still there; ldap_lookup and
/ldap-auth <username> show what the directory holds for a name, no password
involved. require_group adds a membership check on the user's own connection (the
group's member, uniqueMember or memberUid), and a non-member gets the same line as
a wrong password. Empty passwords are refused before any bind, because LDAP reads
one as an anonymous bind, which always succeeds. A filter that matches more than
one entry rejects the login. TLS is required: ldaps://, or ldap:// with start_tls,
unless allow_insecure says otherwise; ca_certs_file names a private bundle and
certificates are always verified. Referrals are never chased, so credentials never
follow a host the directory names. Sessions are stateless signed tokens: nothing to
revoke server-side, and a disabled account keeps a token until it expires - set
refresh_ttl_seconds shorter, or delete the entry, for a hard cutoff. The section's
secret signs them; without one a key is generated per process, so tokens do not
outlive a restart. timeout_seconds bounds a connect and a read."
  "What (help :ldap-auth) answers while the cell runs.")

;;; --- the cell -------------------------------------------------------------

(nle:define-cell ldap-auth
  (:section ("ldap-auth")
    (:guide "server_url is the directory (ldaps://ldap.example.com); then exactly one bind mode - user_dn_template for a direct bind, or user_search_base plus bind_dn and bind_password for search-then-bind; secret signs the sessions")
    ("server_url" :string :required t
                  :doc "the directory's URL: ldaps://host[:636], or ldap://host with start_tls")
    ("user_dn_template" :string :doc "direct bind: a DN with {username} in it, e.g. uid={username},ou=people,dc=example,dc=com")
    ("user_search_base" :string :doc "search-then-bind: the subtree a service account searches for the user")
    ("user_search_filter" :string :default "(uid={username})"
                           :doc "the filter, whose {username} is escaped; (sAMAccountName={username}) on Active Directory")
    ("bind_dn" :string :doc "the service account that searches; empty searches anonymously")
    ("bind_password" :secret :doc "the service account's password")
    ("require_group" :string :doc "a group DN the user must be in, e.g. CN=hermes-users,OU=Groups,DC=corp,DC=example,DC=com")
    ("start_tls" :boolean :default nil :doc "upgrade a plain ldap:// connection with StartTLS")
    ("allow_insecure" :boolean :default nil :doc "accept cleartext ldap:// without StartTLS (not recommended)")
    ("ca_certs_file" :path :doc "a private CA bundle; certificates are always verified")
    ("email_attribute" :string :default "mail" :doc "the attribute a token carries as mail")
    ("display_name_attribute" :string :default "cn" :doc "the attribute a token carries as the display name")
    ("display_name" :string :default "LDAP" :doc "the label a login form puts on the provider")
    ("secret" :secret :doc "the token-signing key: base64, hex or raw, at least 16 bytes (openssl rand -base64 32)")
    ("session_ttl_seconds" :integer :default 43200 :min 60 :doc "how long an access token lasts")
    ("refresh_ttl_seconds" :integer :default 2592000 :min 300 :doc "how long a refresh token lasts")
    ("timeout_seconds" :integer :default 5 :min 1 :doc "a connect and a read's ceiling")
    ("verify_user_on_refresh" :boolean :default t :doc "search mode: check the account still exists when a session is read back"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (resolve-settings values)))
  (:tool "ldap_login" (nlk:contract 'login-arguments) 'run-login)
  (:tool "ldap_lookup" (nlk:contract 'lookup-arguments) 'run-lookup)
  (:tool "ldap_verify" (nlk:contract 'verify-arguments) 'run-verify)
  (:command "ldap-auth" 'run-slash
            :description "What the directory holds for a username"
            :argument-hint "USERNAME")
  (:help :ldap-auth "ldap-auth: sign a user in against LDAP / Active Directory by an LDAP bind - ldap_login, ldap_verify, ldap_lookup, /ldap-auth"
         +manual+))
