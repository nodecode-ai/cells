;;;; cell.lisp --- the tool, /tls, and the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One tool, `tls_posture`, one command, /tls, one manual topic. The tool
;;;; is read-only and speaks no TLS: it parses certificate files and server
;;;; TLS directives on disk, so a report about a certificate is a report
;;;; about a file, and the join to the server block that serves it is the
;;;; only claim it makes about a service.

(in-package #:nodecode-tls-posture)

(defparameter +manual+
  "A TLS posture audit is on through the nodecode-tls-posture cell: the
tls_posture tool reads certificate files and nginx/Apache TLS directives
under a root and answers findings that cite the file and line each came from,
plus a coverage block naming every check that could not run.

It is READ-ONLY and it speaks no TLS: no handshake, no socket, no remote
target. Pass nothing for the configured root (set root once, / for this host
or a mounted image's path); tls_posture with root=\"...\" audits another
tree, include_info=true adds INFO-severity observations, expiry_days widens
the renewal window (30 by default; seven days or fewer is graded higher).
The paths read are a FIXED list -- etc/ssl/certs, etc/pki/tls/certs, the
Let's Encrypt live directories, and the nginx/Apache configuration -- so this
is an audit, not a file reader.

Checks: TLS-001 expired, TLS-002 expiring, TLS-003 not yet valid, TLS-004
key too small, TLS-005 MD5/SHA-1 signature, TLS-006 self-signed, TLS-007 a
leaf served where a fullchain sits beside it, TLS-008 a configured
certificate that is missing or outside the root, TLS-009 a served
certificate this audit never read, TLS-010 a deprecated protocol ENABLED,
TLS-011 a weak suite OFFERED, TLS-012 no explicit protocol directive
(opt-in).

Two readings are deliberate. A cipher string is a list of OFFERS and
exclusions, so `HIGH:!aNULL:!MD5` is hardened and is not reported. nginx
writes the versions it enables and Apache the ones it removes, so a leading
`-` is disabled, and protocol tokens are compared whole (`TLSv1' is not a
match for `TLSv1.2'). A check that could not run is reported as skipped,
never as passing, and no verdict ever says the target is secure.
(help :tls) repeats this while the cell runs."
  "The primer the model reads; the tool's description also says it.")

;;; --- the section has to name a tree ------------------------------------------

(defun home-root (text)
  "TEXT with a leading `~' resolved, as a directory pathname, absolute
against this process's directory when it is relative."
  (let* ((text (string-trim '(#\Space #\Tab) text))
         (text (if (and (>= (length text) 2) (char= #\~ (char text 0)) (char= #\/ (char text 1)))
                   (namestring (merge-pathnames (subseq text 2) (user-homedir-pathname)))
                   text)))
    (uiop:ensure-directory-pathname (uiop:ensure-absolute-pathname text (uiop:getcwd)))))

(defun read-settings (values table)
  "VALUES as the settings this cell runs on, with `root' resolved once.

A section with no root refuses HERE rather than at first use, so the cells
list, /settings and the model all say the one thing to fix."
  (declare (ignore table))
  (let ((root (getf values :root)))
    (unless (and (stringp root) (plusp (length (string-trim '(#\Space #\Tab) root))))
      (error 'nlk:config-refusal
             :detail "tls-posture: set \"root\" to the tree to audit: / for this host, or the path a mounted image is under"))
    (let ((path (home-root root)))
      (unless (uiop:directory-exists-p path)
        (error 'nlk:config-refusal
               :detail (format nil "tls-posture: root ~a is not a directory" (namestring path))))
      (list* :root path values))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract tls-audit
  (:doc "Read-only audit of local TLS posture. Parses a fixed set of certificate
files and server TLS directives (nginx, Apache) and answers findings that each
cite the file and line they came from, plus a coverage block naming every check
that could not run. Covers certificate expiry, key size, signature algorithm,
and deprecated protocols or weak cipher suites named in server configuration.
Never modifies anything and never connects to anything: this reads files on
disk, it does not perform a handshake.")
  ("root" :string :default ""
   :doc "the filesystem tree to audit, when it is not the configured root: the path a mounted image is under. Only the fixed list of certificate and server-config paths is read beneath it.")
  ("include_info" :boolean
   :doc "include INFO-severity observations, such as a served certificate the audit never read (default false)")
  ("expiry_days" :integer
   :doc "report certificates expiring within this many days (default 30); seven days or fewer is graded higher"))

(defun audit-root-of (request)
  "The tree REQUEST names: its own root when it gives one, else the
section's."
  (let ((given (string-trim '(#\Space #\Tab) (or (tls-audit-root request) ""))))
    (if (plusp (length given))
        (uiop:ensure-directory-pathname (uiop:ensure-absolute-pathname given (uiop:getcwd)))
        (setting :root))))

(defun tls-posture (request)
  "The tls_posture tool: the audit as JSON. A tree that is not there, or a
run that fails, answers one ERROR line rather than an unhandled condition."
  (handler-case
      (nlk:encode-json-object
       (report-payload
        (audit-tree (audit-root-of request)
                    :expiry-days (or (tls-audit-expiry-days request) (setting :expiry-days))
                    :include-info (if (tls-audit-include-info request) t (setting :include-info)))))
    (error (condition)
      (nle:failure (format nil "ERROR: tls-posture: ~a" condition)))))

;;; --- /tls ----------------------------------------------------------------------

(defun finding-line (finding)
  (format nil "[~a ~a] ~a~@[ (~{~a~^, ~})~]"
          (finding-check-id finding)
          (string-upcase (symbol-name (finding-severity finding)))
          (finding-assertion finding)
          (finding-tags finding)))

(defun report-text (audit)
  "The audit as the few lines an operator reads first: the grade, the worst
findings, and how much of the check list could not run."
  (let* ((findings (stable-sort (copy-list (audit-findings audit))
                                 (lambda (left right)
                                   (< (severity-rank (finding-severity left))
                                      (severity-rank (finding-severity right))))))
         (skipped (audit-skipped audit)))
    (with-output-to-string (out)
      (format out "tls-posture -- ~a~%~a  |  ~a~%"
              (namestring (audit-root audit))
              (cdr (assoc (report-grade audit) +grade-labels+))
              (report-verdict audit))
      (dolist (finding (if (> (length findings) 12) (subseq findings 0 12) findings))
        (format out "  ~a~%" (finding-line finding)))
      (when (> (length findings) 12)
        (format out "  (+~d more finding(s); the tool answers all of them)~%"
                (- (length findings) 12)))
      (format out "  checks run: ~{~a~^, ~}~%" (audit-checks-run audit))
      (dolist (pair skipped)
        (format out "  skipped [~a]: ~a~%" (car pair) (cdr pair))))))

(defun run-command (args session-id)
  "One /tls invocation: no argument audits the configured root, an argument
audits that tree."
  (declare (ignore session-id))
  (let ((given (string-trim '(#\Space #\Tab) (or args ""))))
    (handler-case
        (report-text (audit-tree (if (plusp (length given)) (home-root given) (setting :root))
                                 :expiry-days (setting :expiry-days)
                                 :include-info (setting :include-info)))
      (error (condition) (format nil "tls: ~a" condition)))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell tls-posture
  (:section ("tls-posture")
    (:guide "root is the tree to audit: / for this host, or the path a mounted image is under; next step: /tls")
    ("root" :path :required t
     :doc "the filesystem tree the audit reads: / for this host, or the path a mounted image is under")
    ("expiry_days" :integer :default 30 :min 1
     :doc "report certificates expiring within this many days; seven days or fewer is graded higher")
    ("include_info" :boolean :default nil
     :doc "include INFO-severity observations, such as a served certificate the audit never read"))
  (:settings #'read-settings)
  (:help :tls "tls: a read-only audit of the certificates and TLS settings on this machine" +manual+)
  (:command "tls" 'run-command
            :description "Audit local TLS: expiry, key size, protocols, served certificates"
            :argument-hint "[ROOT]")
  (:tool "tls_posture" (nlk:contract 'tls-audit) #'tls-posture))
