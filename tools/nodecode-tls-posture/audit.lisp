;;;; audit.lisp --- the checks: files read, findings out, coverage never silent.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here reads files. Nothing connects to anything: this parses
;;;; certificates and TLS directives on disk and never performs a handshake.
;;;; Three properties are design constraints, not accidents:
;;;;
;;;; 1. A FIXED list of relative paths, so the tool is an audit and not a
;;;;    file reader wearing an audit's schema: an attacker who could choose
;;;;    the path could make it print a private key.
;;;; 2. No silent passes. A file that is missing, unreadable or undecodable is
;;;;    recorded in coverage.checks_skipped, never counted as clean.
;;;; 3. The certificate a server actually SERVES is joined to the file checks,
;;;;    because a fact about a file is not a fact about a service.

(in-package #:nodecode-tls-posture)
;;; --- the shapes ---------------------------------------------------------------

(defstruct evidence
  "One value read off disk, with where it came from. OBSERVED is untrusted:
a certificate's subject is text anyone who can get a certificate issued may
write."
  (source "" :type string)
  (locator "" :type string)
  (observed "" :type string))

(defstruct finding
  "One finding. ASSERTION is the claim this tool makes in its own words;
EVIDENCE carries the untrusted text the claim rests on."
  (check-id "" :type string)
  (title "" :type string)
  (severity :info)
  (assertion "" :type string)
  (rationale "" :type string)
  (remediation "" :type string)
  (evidence '())
  (tags '()))

(defstruct audit
  "One run: what it read, what it found, and what it could not check."
  (root nil)
  (expiry-days 30)
  (include-info nil)
  (findings '())
  (checks-run '())
  (skipped '())
  (evaluated '())
  (inputs '())
  (limitations '()))

;;; --- the only paths this tool will ever read ----------------------------------

(defparameter +cert-paths+
  '("etc/ssl/certs/*.pem" "etc/ssl/certs/*.crt"
    "etc/pki/tls/certs/*.pem" "etc/pki/tls/certs/*.crt"
    "etc/pki/tls/certs/localhost.crt"
    "etc/letsencrypt/live/*/cert.pem"
    ;; The file a correctly configured server actually SERVES. Auditing only
    ;; cert.pem means reading the file a server should not point at.
    "etc/letsencrypt/live/*/fullchain.pem")
  "Certificate material, relative to the audit root. Fixed, not caller-given.")

(defparameter +config-paths+
  '("etc/nginx/nginx.conf" "etc/apache2/apache2.conf" "etc/httpd/conf/httpd.conf")
  "Server configuration, by exact path.")

(defparameter +config-globs+
  '("etc/nginx/conf.d/*.conf" "etc/nginx/sites-enabled/*"
    "etc/apache2/sites-enabled/*.conf" "etc/apache2/mods-enabled/ssl.conf"
    "etc/httpd/conf.d/*.conf" "etc/httpd/conf.modules.d/*.conf")
  "Server configuration, by fixed glob.")

(defparameter +weak-protocols+
  '(("SSLV2" . "SSL 2.0") ("SSLV3" . "SSL 3.0")
    ("TLSV1" . "TLS 1.0") ("TLSV1.0" . "TLS 1.0") ("TLSV1.1" . "TLS 1.1"))
  "Deprecated versions, compared WHOLE: `TLSv1' is a substring of `TLSv1.2',
so a substring match reports a hardened host as broken.")

(defparameter +weak-cipher-tokens+ '("RC4" "3DES" "DES" "NULL" "EXPORT" "MD5" "ANON" "ADH")
  "Looked for inside an OFFERED suite, never inside an exclusion.")

(defparameter +rsa-minimum-bits+ 2048)
(defparameter +rsa-critical-bits+ 1024)
(defparameter +ec-minimum-bits+ 224)

;;; --- bookkeeping --------------------------------------------------------------

(defun audit-ran (audit &rest ids)
  "Record that a check RAN: it had real input to evaluate."
  (dolist (id ids)
    (pushnew id (audit-checks-run audit) :test #'string=)))

(defun audit-mark (audit &rest ids)
  "Note a check had input; SETTLE promotes it to `ran'."
  (dolist (id ids)
    (pushnew id (audit-evaluated audit) :test #'string=)))

(defun audit-skip (audit check-id reason)
  (let ((pair (cons check-id reason)))
    (unless (member pair (audit-skipped audit) :test #'equal)
      (push pair (audit-skipped audit)))))

(defun audit-note (audit text)
  (pushnew text (audit-limitations audit) :test #'string=))

(defun audit-add (audit finding)
  (push finding (audit-findings audit)))

(defun settle (audit)
  "Promote evaluated checks to `ran', so `no findings for these checks' means
the checks executed rather than that nothing was looked at."
  (dolist (id (sort (copy-list (audit-evaluated audit)) #'string<))
    (pushnew id (audit-checks-run audit) :test #'string=))
  (setf (audit-checks-run audit) (sort (audit-checks-run audit) #'string<)
        (audit-skipped audit) (nreverse (audit-skipped audit))
        (audit-inputs audit) (nreverse (audit-inputs audit))
        (audit-limitations audit) (nreverse (audit-limitations audit))))

;;; --- bounded reads ------------------------------------------------------------

(defun subpath-p (path directory)
  "T when PATH, resolved, is inside DIRECTORY. A symlink out of the tree is
not a file in it."
  (let ((path (namestring (truename path)))
        (directory (namestring (truename directory))))
    (and (>= (length path) (length directory))
         (string= directory path :end2 (length directory)))))

(defun audit-file (audit relative)
  "RELATIVE under the audit root; a path that escapes it is a refusal, which
is what keeps the fixed list fixed."
  (let* ((root (audit-root audit))
         (merged (merge-pathnames (string-left-trim "/" relative) root)))
    (unless (and (>= (length (namestring merged)) (length (namestring root)))
                 (string= (namestring root) (namestring merged)
                          :end2 (length (namestring root))))
      (error 'tls-posture-error
             :detail (format nil "path escapes audit root: ~s" relative)))
    (let ((resolved (ignore-errors (truename merged))))
      (when (and resolved (not (subpath-p resolved root)))
        (error 'tls-posture-error
               :detail (format nil "path escapes audit root: ~s" relative))))
    merged))

(defun audit-relative (audit path)
  "PATH as this run names it: relative to the root, so a report never
carries the operator's own directory names."
  (enough-namestring path (audit-root audit)))

(defun audit-input (audit relative)
  (pushnew relative (audit-inputs audit) :test #'string=))

(defun audit-bytes (audit relative check-id)
  "RELATIVE's octets, or NIL with a SKIP recorded: unreadable is not valid."
  (let ((path (handler-case (audit-file audit relative)
                (tls-posture-error (condition)
                  (audit-skip audit check-id (tls-posture-error-detail condition))
                  (return-from audit-bytes nil)))))
    (unless (uiop:file-exists-p path)
      (audit-skip audit check-id (format nil "~a not present" relative))
      (return-from audit-bytes nil))
    (handler-case
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
            (read-sequence octets in)
            (audit-input audit relative)
            octets))
      (error (condition)
        (audit-skip audit check-id (format nil "~a: ~a" relative condition))
        nil))))

(defun audit-text (audit relative check-id &key (limit 1048576))
  "RELATIVE as text, or NIL with a SKIP recorded."
  (let ((octets (audit-bytes audit relative check-id)))
    (when octets
      (let ((text (map 'string #'code-char octets)))
        (if (> (length text) limit) (subseq text 0 limit) text)))))

(defun merge-directory (audit pattern)
  "The files PATTERN (one fixed glob) matches under the root, as relative
names, sorted; a match that resolves outside the root is not one of them."
  (let ((root (audit-root audit))
        (out '()))
    (dolist (path (ignore-errors (directory (merge-pathnames pattern root))))
      (let ((resolved (ignore-errors (truename path))))
        (when (and resolved (subpath-p resolved root) (uiop:file-exists-p resolved))
          (pushnew (enough-namestring resolved root) out :test #'string=))))
    (sort out #'string<)))

(defun cert-files (audit)
  "Certificate material under the fixed search paths, de-duplicated."
  (let ((out '()))
    (dolist (pattern +cert-paths+ out)
      (dolist (relative (merge-directory audit pattern))
        (pushnew relative out :test #'string=)))))

(defun config-files (audit)
  "Server configuration: the exact paths that exist, then the fixed globs."
  (let ((out '()))
    (dolist (relative +config-paths+)
      (let ((path (handler-case (audit-file audit relative)
                    (tls-posture-error () nil))))
        (when (and path (uiop:file-exists-p path))
          (pushnew relative out :test #'string=))))
    (dolist (pattern +config-globs+)
      (dolist (relative (merge-directory audit pattern))
        (pushnew relative out :test #'string=)))
    (nreverse out)))

(defun load-cert (audit relative)
  "RELATIVE decoded, or NIL with the reason recorded as a skip."
  (let ((octets (audit-bytes audit relative "TLS-001")))
    (when octets
      (let* ((text (map 'string #'code-char octets))
             (der (first (pem-certificates text))))
        (or (and der (decode-certificate der))
            (progn (audit-skip audit "TLS-001"
                                (format nil "~a: not a decodable PEM certificate" relative))
                   nil))))))

;;; --- the checks ---------------------------------------------------------------

(defun evidence-of (source locator observed)
  (make-evidence :source source :locator locator :observed observed))

(defun days-between (later earlier)
  "Whole days from EARLIER to LATER, floored: with 9 days 23 hours left this
says 9, erring toward urgency."
  (floor (- later earlier) 86400))

(defun check-certificate (audit relative cert now)
  "TLS-001 .. TLS-006 for one decoded certificate."
  (let ((remaining (and (cert-not-after cert) (days-between (cert-not-after cert) now))))
    (when (cert-not-before cert)
      (audit-mark audit "TLS-003")
      (when (> (cert-not-before cert) now)
        (audit-add audit
                    (make-finding
                     :check-id "TLS-003" :title "Certificate is not valid yet"
                     :severity :medium
                     :assertion (format nil "~a has a notBefore in the future." relative)
                     :rationale "Every conforming client rejects it until the window opens, so whatever service holds it fails TLS for every visitor."
                     :remediation "Correct the issuing clock and reissue."
                     :evidence (list (evidence-of relative "notBefore" (cert-not-before-iso cert)))
                     :tags '("tls" "certificate")))))
    (when remaining
      (audit-mark audit "TLS-001" "TLS-002")
      (cond
        ((minusp remaining)
         (audit-add audit
                     (make-finding
                      :check-id "TLS-001" :title "Certificate has expired" :severity :high
                      :assertion (format nil "~a expired ~d day(s) ago." relative (- remaining))
                      :rationale "An expired certificate is refused by conforming clients, so the service fails closed."
                      :remediation "Renew the certificate, or remove the file if the service it belonged to is gone."
                      :evidence (list (evidence-of relative "notAfter" (cert-not-after-iso cert)))
                      :tags '("tls" "certificate"))))
        ((<= remaining (audit-expiry-days audit))
         (audit-add audit
                     (make-finding
                      :check-id "TLS-002"
                      :title (format nil "Certificate expires in ~d day(s)" remaining)
                      :severity (if (<= remaining 7) :high :medium)
                      :assertion (format nil "~a expires on ~a." relative (cert-not-after-iso cert))
                      :rationale "Renewal is a scheduled action; at seven days or fewer it is an outage the operator can no longer plan around."
                      :remediation "Renew before expiry and confirm the renewal is automated."
                      :evidence (list (evidence-of relative "notAfter"
                                                    (format nil "~d day(s) remaining" remaining)))
                      :tags '("tls" "certificate" "expiry"))))))
    (let ((bits (cert-key-bits cert))
          (type (cert-key-type cert)))
      (if (and bits (member type '("RSA" "DSA" "EC") :test #'string=))
          (progn
            (audit-mark audit "TLS-004")
            (let ((weak (if (string= type "EC") (< bits +ec-minimum-bits+)
                            (< bits +rsa-minimum-bits+))))
              (when weak
                (audit-add audit
                            (make-finding
                             :check-id "TLS-004"
                             :title (format nil "~a key is only ~d bits" type bits)
                             :severity (if (and (not (string= type "EC"))
                                                (< bits +rsa-critical-bits+))
                                           :critical :high)
                             :assertion (format nil "~a is signed with a ~d-bit ~a key." relative bits type)
                             :rationale "Key size sets the cost of recovering the private key; below the threshold an adversary with modest resources can forge signatures for this certificate."
                             :remediation "Reissue with an RSA key of at least 2048 bits, or an EC key on P-256 or stronger."
                             :evidence (list (evidence-of relative "public key" (format nil "~d bits" bits)))
                             :tags '("tls" "crypto" "certificate"))))))
          (audit-skip audit "TLS-004" (format nil "~a: key size not readable" relative))))
    (let ((algo (cert-sig-algo cert)))
      (if (plusp (length algo))
          (progn
            (audit-mark audit "TLS-005")
            (when (member algo '("md5" "sha1") :test #'string=)
              (audit-add audit
                          (make-finding
                           :check-id "TLS-005"
                           :title (format nil "Certificate is signed with ~:@(~a~)" algo)
                           :severity :high
                           :assertion (format nil "~a carries a ~:@(~a~) signature." relative algo)
                           :rationale "Chosen-prefix collisions in these digests let an attacker produce a second certificate with the same signature."
                           :remediation "Reissue using SHA-256 or stronger."
                           :evidence (list (evidence-of relative "signatureAlgorithm" algo))
                           :tags '("tls" "crypto" "certificate")))))
          (audit-skip audit "TLS-005" (format nil "~a: signature algorithm not readable" relative))))
    (when (and (cert-self-signed-p cert) (plusp (length (cert-subject cert))))
      (audit-mark audit "TLS-006")
      (audit-add audit
                  (make-finding
                   :check-id "TLS-006" :title "Certificate is self-signed" :severity :low
                   :assertion (format nil "~a has an issuer identical to its subject." relative)
                   :rationale "No public client trusts it, so it can only be serving internal traffic: legitimate in some deployments, an unmanaged certificate in others."
                   :remediation "Confirm it is intended for internal use, or issue one from a trusted authority."
                   :evidence (list (evidence-of relative "issuer" (cert-issuer cert)))
                   :tags '("tls" "certificate"))))))

(defun check-certificates (audit now)
  (let ((files (cert-files audit)))
    (cond
      ((null files)
       (audit-skip audit "TLS-001" "no certificate files under the fixed search paths")
       (audit-note audit
                   "No certificate material was found under the standard locations, so nothing is known about any certificate. A macOS trust store lives in the Keychain and is not a directory of PEM files, so it is not covered."))
      (t
       (audit-ran audit "TLS-001")
       (dolist (relative files)
         (let ((cert (load-cert audit relative)))
           (when cert (check-certificate audit relative cert now))))))))

;;; --- server configuration -----------------------------------------------------

(defun line-tokens (line)
  "LINE's whitespace-separated tokens, each without its trailing `;' or a
surrounding quote."
  (loop for token in (uiop:split-string line :separator '(#\Space #\Tab))
        for clean = (string-trim '(#\Space #\Tab #\; #\" #\') token)
        when (plusp (length clean)) collect clean))

(defun weak-protocol-labels (tokens)
  "The deprecated versions TOKENS ENABLE, with their labels.

nginx writes the versions it ENABLES; Apache writes `SSLProtocol all
-SSLv3 -TLSv1', the ones it REMOVES. A leading `-' or `!' means disabled, so
reporting it would be reporting the fix as the bug."
  (let ((labels '()))
    (dolist (token tokens)
      (unless (find (char token 0) "-!")
        (let* ((bare (string-upcase (string-left-trim "+" token)))
               (label (cdr (assoc bare +weak-protocols+ :test #'string=))))
          (when label (pushnew label labels :test #'string=)))))
    (nreverse labels)))

(defun offered-weak-ciphers (value)
  "The weak suites VALUE actually OFFERS: `HIGH:!aNULL:!MD5' names MD5 in
order to disable it, and a scanner that reports it reports the fix as the bug."
  (let ((offered '()))
    (dolist (token (uiop:split-string value :separator '(#\:)))
      (let ((clean (string-trim '(#\Space #\Tab #\; #\" #\') token)))
        (when (and (plusp (length clean))
                   (not (find (char clean 0) "!-+@")))
          (let ((upper (string-upcase clean)))
            (when (some (lambda (marker) (search marker upper)) +weak-cipher-tokens+)
              (push clean offered))))))
    (nreverse offered)))

(defun directive-value (tokens)
  "The value TOKENS carry: the rest of the line, its separator and quotes
removed."
  (let ((value (format nil "~{~a~^ ~}" (rest tokens))))
    (string-trim '(#\Space #\Tab #\; #\" #\') value)))

(defun check-config-lines (audit relative text include-info state)
  "TLS-010 .. TLS-012 over one configuration file, updating STATE's flags."
  (declare (ignore include-info))
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for lineno from 1
        for clean = (string-trim '(#\Space #\Tab #\Return) line)
        for tokens = (and (plusp (length clean))
                          (not (char= #\# (char clean 0)))
                          (line-tokens clean))
        when tokens
          do (check-config-line audit relative lineno clean tokens state)))

(defun check-config-line (audit relative lineno clean tokens state)
  "One configuration line: a protocol directive, a cipher directive, or a
listener that tells this audit the file concerns TLS at all."
  (let ((head (first tokens)))
    (cond
      ((member head '("ssl_protocols" "SSLProtocol") :test #'string-equal)
       (let ((value (directive-value tokens)))
         (unless (zerop (length value))
           (setf (getf state :protocol) t)
           (audit-mark audit "TLS-010")
           (report-weak-protocols audit relative lineno head tokens))))
      ((member head '("ssl_ciphers" "SSLCipherSuite") :test #'string-equal)
       (let ((value (directive-value tokens)))
         (unless (zerop (length value))
           (setf (getf state :cipher) t)
           (audit-mark audit "TLS-011")
           (report-weak-ciphers audit relative lineno head tokens))))
      ((search "listen 443" (string-downcase clean))
       (setf (getf state :tls) t)))))

(defun report-weak-protocols (audit relative lineno head tokens)
  "TLS-010: a deprecated version the directive ENABLES."
  (let ((labels (weak-protocol-labels tokens)))
    (when labels
      (audit-add audit
                  (make-finding
                   :check-id "TLS-010"
                   :title (format nil "Deprecated TLS version enabled: ~{~a~^, ~}" labels)
                   :severity (if (every (lambda (label) (string= label "TLS 1.1")) labels)
                                 :medium :high)
                   :assertion (format nil "`~a` at ~a:~d permits ~{~a~^, ~}."
                                      head relative lineno labels)
                   :rationale "Each of these versions has protocol-level breaks a client cannot defend against alone, so a server that still negotiates one can be steered down to it."
                   :remediation (format nil "Remove the deprecated versions from `~a` and require TLS 1.2 or later." head)
                   :evidence (list (evidence-of relative (format nil "line ~d" lineno)
                                                 (format nil "~{~a~^ ~}" tokens)))
                   :tags '("tls" "configuration"))))))

(defun report-weak-ciphers (audit relative lineno head tokens)
  "TLS-011: a weak suite the directive OFFERS, never one it excludes."
  (let* ((offered (offered-weak-ciphers (directive-value tokens)))
         (shown (format nil "~{~a~^, ~}" (if (> (length offered) 4) (subseq offered 0 4) offered))))
    (when offered
      (audit-add audit
                  (make-finding
                   :check-id "TLS-011"
                   :title (format nil "Weak cipher suite offered: ~a" shown)
                   :severity (if (some (lambda (suite)
                                         (some (lambda (marker) (search marker (string-upcase suite)))
                                               '("RC4" "NULL" "EXPORT" "ANON" "ADH")))
                                       offered)
                                 :high :medium)
                   :assertion (format nil "`~a` at ~a:~d offers ~d weak suite(s)."
                                      head relative lineno (length offered))
                   :rationale "A client chooses from what the server offers, so a broken suite left enabled is a downgrade target for anyone who can influence the handshake."
                   :remediation (format nil "Remove the weak suites from `~a`; keep the modern set and any `!` exclusions." head)
                   :evidence (list (evidence-of relative (format nil "line ~d" lineno)
                                                 (format nil "~{~a~^ ~}" tokens)))
                   :tags '("tls" "crypto" "configuration"))))))

(defun check-server-config (audit)
  (let ((files (config-files audit)))
    (cond
      ((null files)
       (audit-skip audit "TLS-010" "no nginx or Apache configuration under the fixed paths")
       (audit-note audit
                   "No server configuration was found, so nothing is known about which TLS versions or cipher suites anything on this tree offers."))
      (t
       (let ((state (list :protocol nil :cipher nil :tls nil)))
         (dolist (relative files)
           (let ((text (audit-text audit relative "TLS-010")))
             (when text
               (check-config-lines audit relative text (audit-include-info audit) state)
               (when (search "ssl_certificate" (string-downcase text)) (setf (getf state :tls) t))
               (when (search "sslprotocol" (string-downcase text)) (setf (getf state :tls) t)))))
         (when (and (audit-include-info audit) (not (getf state :protocol)) (getf state :tls))
           (audit-ran audit "TLS-012")
           (audit-add audit
                       (make-finding
                        :check-id "TLS-012" :title "No explicit TLS version restriction found"
                        :severity :info
                        :assertion "A TLS-enabled server configuration was found with no protocol directive."
                        :rationale "The effective set is the server's own compiled default, which this audit cannot read; an explicit directive is the only way to know."
                        :remediation "State the permitted protocols explicitly so the posture is auditable."
                        :evidence '() :tags '("tls" "configuration"))))
         (unless (getf state :cipher)
           (audit-skip audit "TLS-011" "no cipher directive in any readable server configuration")))))))

;;; --- the certificate a server actually presents -------------------------------

(defun collect-references (audit)
  "Every certificate a server configuration names, as a list of plists, plus
the locations each root-relative path is presented at and the config files
that supply Apache's chain separately.

Only ABSOLUTE paths are resolved: both servers resolve a relative
`ssl_certificate` against a prefix this audit cannot know, so guessing one
would invent a finding about a file never identified."
  (let ((references '()) (served (make-hash-table :test #'equal)) (chains '()))
    (dolist (relative (config-files audit))
      (let ((text (audit-text audit relative "TLS-010")))
        (when text
          (loop for line in (uiop:split-string text :separator '(#\Newline))
                for lineno from 1
                for clean = (string-trim '(#\Space #\Tab #\Return) line)
                for tokens = (and (plusp (length clean)) (not (char= #\# (char clean 0)))
                                  (line-tokens clean))
                when tokens
                  do (let ((head (first tokens)))
                       (cond
                         ((string-equal head "SSLCertificateChainFile")
                          (pushnew relative chains :test #'string=))
                         ((or (string-equal head "ssl_certificate")
                              (string-equal head "SSLCertificateFile"))
                          (let* ((written (second tokens))
                                 (absolute (and written (plusp (length written))
                                                (char= #\/ (char written 0)))))
                            (push (list :directive head :written (or written "") :source relative
                                        :lineno lineno
                                        :relative (if absolute (string-left-trim "/" written) ""))
                                  references)
                            (when absolute
                              (push (format nil "~a:~d" relative lineno)
                                    (gethash (string-left-trim "/" written) served)))))))))))
    (values (nreverse references) served chains)))

(defun check-cert-references (audit references served chains)
  "TLS-007 / TLS-008 / TLS-009: the certificate a server PRESENTS."
  (declare (ignore served))
  (cond
    ((null references)
     (audit-skip audit "TLS-007" "no configuration named a certificate file")
     (audit-skip audit "TLS-008" "no configuration named a certificate file"))
    (t
     (let* ((relative (remove-if (lambda (reference) (zerop (length (getf reference :relative))))
                                 references))
            (unresolvable (- (length references) (length relative))))
       (when (plusp unresolvable)
         (let ((first (find-if (lambda (reference) (zerop (length (getf reference :relative))))
                                references)))
           (audit-note audit
                       (format nil "~d certificate reference(s) were RELATIVE paths and could not be resolved (first at ~a:~d: `~a`); both servers resolve those against a prefix this audit does not know, so no statement is made about them."
                               unresolvable (getf first :source) (getf first :lineno)
                               (getf first :written)))))
       (if (null relative)
           (progn
             (audit-skip audit "TLS-007" "every certificate reference was a relative path")
             (audit-skip audit "TLS-008" "every certificate reference was a relative path"))
           (progn
             (audit-ran audit "TLS-007" "TLS-008")
             (let ((audited (cert-files audit)))
               (dolist (reference relative)
                 (check-one-reference audit reference audited chains)))))))))

(defun check-one-reference (audit reference audited chains)
  (let* ((relative (getf reference :relative))
         (written (getf reference :written))
         (source (getf reference :source))
         (lineno (getf reference :lineno))
         (directive (getf reference :directive))
         (safe (handler-case (audit-file audit relative)
                 (tls-posture-error (condition) (tls-posture-error-detail condition)))))
    (cond
      ((stringp safe)
       (audit-add audit
                   (make-finding
                    :check-id "TLS-008" :title "Configured certificate path escapes the audit root"
                    :severity :high
                    :assertion (format nil "`~a` at ~a:~d names `~a`, which ~a."
                                       directive source lineno written safe)
                    :rationale "A path outside the tree being audited cannot be read here, so nothing is known about the certificate this server presents."
                    :remediation "Point the directive at a path inside the audited tree, or widen the audit root."
                    :evidence (list (evidence-of source (format nil "line ~d" lineno) written))
                    :tags '("tls" "certificate" "configuration"))))
      ((not (uiop:file-exists-p safe))
       (audit-add audit
                   (make-finding
                    :check-id "TLS-008" :title "Configured certificate file does not exist"
                    :severity :high
                    :assertion (format nil "`~a` at ~a:~d names `~a`, which is not present under the audit root."
                                       directive source lineno written)
                    :rationale "A server presenting a file that is not there serves no TLS on that listener: it refuses to start, or fails every handshake."
                    :remediation (format nil "Restore `~a`, or point the directive at the deployed certificate." written)
                    :evidence (list (evidence-of source (format nil "line ~d" lineno) written))
                    :tags '("tls" "certificate" "configuration"))))
      (t
       (let* ((octets (handler-case (audit-bytes audit relative "TLS-007") (error () nil)))
              (held (and octets (length (pem-certificates (map 'string #'code-char octets)))))
              (sibling (merge-pathnames "fullchain.pem" (uiop:pathname-directory-pathname safe)))
              (chain (member source chains :test #'string=)))
         (cond
           ((and (eql held 1) (not (string= (pathname-name safe) "fullchain.pem"))
                 (uiop:file-exists-p sibling) (not chain))
            (audit-add audit
                        (make-finding
                         :check-id "TLS-007"
                         :title "Server presents a leaf certificate while the full chain sits beside it"
                         :severity :high
                         :assertion (format nil "`~a` at ~a:~d names `~a`, which holds one certificate, while `~a` -- the chain for the same name -- sits in the same directory."
                                            directive source lineno written
                                            (audit-relative audit sibling))
                         :rationale "The intermediate is never sent, so every client that has not cached it fails the handshake, typically intermittently rather than at once."
                         :remediation (format nil "Point `~a` at `~a`." directive (audit-relative audit sibling))
                         :evidence (list (evidence-of source (format nil "line ~d" lineno) written)
                                         (evidence-of (audit-relative audit sibling) "present" "fullchain.pem"))
                         :tags '("tls" "certificate" "configuration" "chain"))))
           ((and (eql held 1) (string-equal directive "ssl_certificate") (not chain))
            (let ((cert (and octets (decode-certificate (first (pem-certificates
                                                                (map 'string #'code-char octets)))))))
              (when (and cert (not (cert-self-signed-p cert)))
                (audit-add audit
                            (make-finding
                             :check-id "TLS-007"
                             :title "`ssl_certificate` presents a single certificate, not a chain"
                             :severity :medium
                             :assertion (format nil "`ssl_certificate` at ~a:~d names `~a`, which holds one certificate, and no chain file is configured for it."
                                                source lineno written)
                             :rationale "nginx has no separate chain directive: whatever `ssl_certificate` points at is exactly what is sent, so a single non-self-signed certificate omits its intermediate."
                             :remediation "Concatenate the intermediate into the file the directive points at."
                             :evidence (list (evidence-of source (format nil "line ~d" lineno) written))
                             :tags '("tls" "certificate" "configuration" "chain")))))))
         (unless (member relative audited :test #'string=)
           (audit-add audit
                       (make-finding
                        :check-id "TLS-009" :title "Server presents a certificate this audit did not read"
                        :severity :info
                        :assertion (format nil "`~a` at ~a:~d names `~a`, which exists but is not under the certificate search paths."
                                           directive source lineno written)
                        :rationale "A server can present a certificate every other check in this run missed, and the report would otherwise be silent about it."
                        :remediation "Move the certificate under the standard paths, or accept that its expiry, key size and signature were not evaluated."
                        :evidence (list (evidence-of source (format nil "line ~d" lineno) written))
                        :tags '("tls" "certificate" "coverage")))))))))

(defun attach-serving-context (findings served)
  "Tie every certificate finding to the server blocks that present it: a fact
about a file becomes a fact about a service."
  (dolist (finding findings findings)
    (let ((where '()))
      (dolist (evidence (finding-evidence finding))
        (dolist (location (gethash (evidence-source evidence) served))
          (pushnew location where :test #'string=)))
      (when where
        (setf (finding-assertion finding)
              (format nil "~a Served by ~{~a~^, ~}." (finding-assertion finding) (nreverse where))
              (finding-tags finding)
              (if (member "served" (finding-tags finding) :test #'string=)
                  (finding-tags finding)
                  (append (finding-tags finding) '("served"))))))))

;;; --- the run ------------------------------------------------------------------

(defun audit-tree (root &key (expiry-days 30) include-info)
  "Audit ROOT and answer the AUDIT. A tree that is not there is a refusal."
  (unless (uiop:directory-exists-p root)
    (error 'tls-posture-error
           :detail (format nil "~a is not a directory" (namestring root))))
  (let ((audit (make-audit :root (uiop:ensure-directory-pathname root)
                            :expiry-days (max 1 expiry-days)
                            :include-info (and include-info t)))
        (now (get-universal-time)))
    (multiple-value-bind (references served chains) (collect-references audit)
      (check-certificates audit now)
      (check-server-config audit)
      (check-cert-references audit references served chains)
      (settle audit)
      (setf (audit-findings audit) (nreverse (audit-findings audit)))
      ;; After settle, so a check promoted to `ran' is still correlated.
      (attach-serving-context (audit-findings audit) served))
    (audit-note audit
                 "Only the checks listed in coverage.checks_run were attempted. This audit does not connect to any service, does not perform a handshake, does not enumerate listeners and does not read private key material.")
    audit))

(defun severity-rank (severity)
  (ecase severity (:critical 0) (:high 1) (:medium 2) (:low 3) (:info 4)))

(defun severity-name (severity)
  (string-downcase (symbol-name severity)))

(defun by-severity (audit)
  "Findings by severity, worst first, only the severities that occur."
  (loop for severity in '(:critical :high :medium :low :info)
        for count = (count severity (audit-findings audit) :key #'finding-severity)
        when (plusp count) collect (cons (severity-name severity) count)))

(defun report-grade (audit)
  (let ((severities (mapcar #'finding-severity (audit-findings audit))))
    (cond ((null (audit-checks-run audit)) :inconclusive)
          ((or (member :critical severities) (member :high severities)) :action-required)
          ((member :medium severities) :review)
          ((member :low severities) :harden)
          ((audit-skipped audit) :inconclusive)
          (t :no-findings))))

(defparameter +grade-labels+
  '((:action-required . "ACTION REQUIRED") (:review . "REVIEW") (:harden . "HARDEN")
    (:no-findings . "NO FINDINGS (for these checks)") (:inconclusive . "INCONCLUSIVE"))
  "The coarse posture. Note the words that are NOT here: a percentage, or
`secure'.")

(defun report-verdict (audit)
  "One sentence. It never claims safety, only what was and was not checked."
  (format nil "~a~a -- this is not a statement that the target is secure"
          (let ((found (length (audit-findings audit)))
                (run (length (audit-checks-run audit))))
            (if (plusp found)
                (format nil "~d finding~:p across ~d check~:p" found run)
                (format nil "no findings for the ~d check~:p run" run)))
          (let ((skipped (length (audit-skipped audit))))
            (if (plusp skipped)
                (format nil "; ~d check~:p could not run" skipped)
                ""))))

(defun finding-payload (finding)
  (nlk:json-object
   "check" (finding-check-id finding)
   "title" (finding-title finding)
   "severity" (severity-name (finding-severity finding))
   "assertion" (finding-assertion finding)
   "rationale" (finding-rationale finding)
   "remediation" (finding-remediation finding)
   "evidence" (coerce (loop for evidence in (finding-evidence finding)
                             collect (nlk:json-object
                                      "source" (evidence-source evidence)
                                      "locator" (evidence-locator evidence)
                                      "observed" (evidence-observed evidence)))
                       'vector)
   "tags" (coerce (finding-tags finding) 'vector)))

(defun report-payload (audit)
  "The tool-facing payload: one JSON object, findings worst first."
  (let ((findings (stable-sort (copy-list (audit-findings audit))
                                (lambda (left right)
                                  (< (severity-rank (finding-severity left))
                                     (severity-rank (finding-severity right)))))))
    (nlk:json-object
     "tool" "tls-posture"
     "target" (namestring (audit-root audit))
     "grade" (cdr (assoc (report-grade audit) +grade-labels+))
     "verdict" (report-verdict audit)
     "by_severity" (apply #'nlk:make-json-object
                           (loop for (name . count) in (by-severity audit)
                                 append (list name count)))
     "findings" (coerce (mapcar #'finding-payload findings) 'vector)
     "coverage" (nlk:json-object
                 "checks_run" (coerce (audit-checks-run audit) 'vector)
                 "checks_skipped" (coerce (loop for (check-id . reason) in (audit-skipped audit)
                                                 collect (nlk:json-object
                                                          "check" check-id
                                                          "reason" reason))
                                           'vector)
                 "inputs_read" (coerce (audit-inputs audit) 'vector)
                 "limitations" (coerce (audit-limitations audit) 'vector)))))
