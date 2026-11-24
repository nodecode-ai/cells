;;;; cell.lisp --- the one declaration, the four verbs behind it, and what the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The manual: (help :vt-check) answers the primer below while the cell
;;;; runs. The vocabulary: VT-CHECK:CHECK-FILE, CHECK-URL, CHECK-IP and STATUS
;;;; from eval, the four vt_* tools for one answer mid-turn, and /vt-check for
;;;; the operator. All of them read the same lookups the same way.
;;;;
;;;; Config, a top-level key beside `osv' and `websearch':
;;;;   "vt-check": {"enabled": true, "api_key_env": "VT_API_KEY",
;;;;                "cache_ttl_hours": 24, "timeout_seconds": 20}
;;;; The key is free and optional: without it the local findings still stand
;;;; and the verdict says unverified.

(in-package #:nodecode-vt-check)

;;; --- the manual -------------------------------------------------------------------

(defparameter +primer+
  "VirusTotal reputation for downloads and links, through the nodecode-vt-check cell: the
vt_check_file, vt_check_url, vt_check_ip and vt_status tools, and the Lisp verbs
(vt-check:check-file \"~/Downloads/setup.exe\") from eval. Each answers a block of text.
  vt_check_file path=\"~/Downloads/tool.deb\"   hash the file locally, look the SHA-256 up, and
      read the archive's own headers first. The file itself is never uploaded -- only its hash is
      sent -- so this is safe on anything private. The block gives the verdict, the three hashes,
      what the content really is by its magic bytes, VirusTotal's engine counts and report page,
      and the local findings: path traversal, symlink or encrypted entries, a decompression bomb,
      a double extension, a document whose content is an executable.
  vt_check_url url=\"https://example.com/setup\"   a READ-ONLY lookup that routes itself: a full
      URL to /urls, a bare domain to /domains, an IP to /ip_addresses. Defanged indicators are
      accepted ('hxxps://evil[.]com', '1.2.3[.]4'). submit=true queues a URL for a new scan, which
      PUBLISHES it at VirusTotal -- ask the user before using it, and it only applies to a URL that
      is unseen.
  vt_check_ip ip=\"203.0.113.9\"   one explicit IP: detections, AS owner, country and network.
  vt_status   whether the key is set and works (the EICAR test hash resolves), the free tier's
      limits, and the cache TTL.
Reading the verdict, highest first: malicious (engines flag it -- stop, do not open); suspicious (an
engine flags it, or an alert finding with a concrete consequence); caution (a warn finding: an
encrypted entry, a symlink, an archive nothing can inspect); unknown (VirusTotal has no record of
this exact build -- normal for a new build, not evidence of malware and not proof of safety; report
it as unseen and point at provenance); unverified (no key, so only the local findings apply); clean.
Report findings as facts with their consequence, never call an archive malware from local checks
alone, and never treat a code or executable file inside an archive as suspicious on its own: source
packages and installers are full of them. Free tier: 4 requests/min, 500/day; results are cached
for cache_ttl_hours, so a re-check is free, and an HTTP 429 means wait a minute, not retry."
  "What (help :vt-check) answers while the cell runs.")

;;; --- the boundaries the model crosses -------------------------------------------------

(nlk:define-contract vt-check-file
  (:doc "Check a file downloaded from the internet BEFORE opening or extracting it: hash it locally and look the SHA-256 up in VirusTotal. The file itself is never uploaded. Also inspects archives in memory -- path traversal, decompression bombs, encrypted entries, double extensions and documents that are really executables, nested up to three deep. Use it when the user asks whether a file or a zip is safe, or wants a verdict before running an installer, a .deb, an AppImage or a script.")
  ("path" :text :required t
          :doc "the path of the file to check: absolute, or ~-relative"))

(nlk:define-contract vt-check-url
  (:doc "Check a link, a domain, or an IP address with VirusTotal in a READ-ONLY lookup -- nothing is submitted, so looking something up does not publish it. It routes itself: a full URL to /urls, a bare domain to /domains, an IP to /ip_addresses. Defanged indicators are accepted ('hxxps://evil[.]com', '1.2.3[.]4'). The answer gives engine detections, category labels, reputation, and for an IP its AS owner and country. 'unknown' means VirusTotal has never seen it, which is not the same as safe. Use it for 'is this link safe?', 'is this domain legit?', phishing checks.")
  ("url" :text :required t
         :doc "a full URL (https://...), a bare domain, or an IP address")
  ("submit" :boolean :default nil
            :doc "queue a URL VirusTotal has not seen for a new scan; this publishes the URL, so use it only when the user asks"))

(nlk:define-contract vt-check-ip
  (:doc "Look up an IPv4 or IPv6 address in VirusTotal, read-only: engine detections, AS owner, country and network. Use it when the user gives an explicit IP and asks whether it is malicious, a scanner or a botnet host. For links and domains use vt_check_url.")
  ("ip" :text :required t
        :doc "an IPv4 or IPv6 address, for example 203.0.113.9"))

;;; --- what the model calls -------------------------------------------------------------

(defun check-file-tool (arguments)
  "The vt_check_file tool: one file, as the block the model reads."
  (check-file (vt-check-file-path arguments)))

(defun check-url-tool (arguments)
  "The vt_check_url tool: one indicator, read-only unless submit says otherwise."
  (check-url (vt-check-url-url arguments) :submit (vt-check-url-submit arguments)))

(defun check-ip-tool (arguments)
  "The vt_check_ip tool: one address, read-only."
  (check-ip (vt-check-ip-ip arguments)))

(defparameter +status-schema+
  (list :description
        (concatenate 'string
                     "Report whether the VirusTotal API key is configured and valid, plus the free "
                     "tier's limits (4 requests/min, 500/day) and the cache TTL. Use it when a check "
                     "failed with a key error, or when the user asks how to add or verify the key.")
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)))
  "vt_status takes no members: it asks VirusTotal one question of its own.")

(defun status-tool (arguments)
  "The vt_status tool; its arguments are empty by declaration."
  (declare (ignore arguments))
  (status))

;;; --- /vt-check -------------------------------------------------------------------------

(defparameter +usage+
  (format nil "usage: /vt-check <path|link|domain|ip>, or /vt-check status~%  ~
a file is hashed locally and looked up by its SHA-256 -- it is never uploaded; a link, a domain ~
or an IP is looked up read-only. The vt_check_* tools read the same lookups mid-turn."))

(defun run-slash (args session-id)
  "One /vt-check invocation: a path, a link, a domain, an IP, or the word status."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (cond ((zerop (length text)) +usage+)
          ((string-equal text "status") (status))
          ((let ((path (expand-path text)))
             (and path (not (uiop:directory-pathname-p path))))
           (check-file text))
          (t (multiple-value-bind (kind value) (normalise-target text)
               (if (member kind '("ip" "domain") :test #'string=)
                   (check-url value)
                   (check-file text)))))))

;;; --- the entry ---------------------------------------------------------------------------

(nle:define-cell vt-check
  (:section ("vt-check")
    (:guide "nothing is required: without a key the checks still report the local facts and say the 
verdict is unverified. A free key comes from virustotal.com (4 requests/min, 500/day); paste it in 
api_key, or leave that empty and name the variable that holds it in api_key_env. cache_ttl_hours 
bounds how long a lookup is reused (0 looks everything up); timeout_seconds bounds one call.")
    ("api_key" :secret
               :doc "the VirusTotal API key itself; a pasted one is kept in a 0600 file under the home's secrets")
    ("api_key_env" :env :default "VT_API_KEY"
                    :doc "the environment variable holding the key, read when api_key is empty")
    ("cache_ttl_hours" :integer :default 24 :min 0
                        :doc "how long a VirusTotal lookup is reused before it is asked again; 0 disables the cache")
    ("timeout_seconds" :integer :default 20 :min 1
                        :doc "how long one VirusTotal call may take"))
  (:help :vt-check
         "VirusTotal: hash a download, check a link, a domain or an IP - the vt_check_* tools, or (vt-check:check-file \"~/x.zip\")"
         +primer+)
  (:tool "vt_check_file" (nlk:contract 'vt-check-file) 'check-file-tool)
  (:tool "vt_check_url" (nlk:contract 'vt-check-url) 'check-url-tool)
  (:tool "vt_check_ip" (nlk:contract 'vt-check-ip) 'check-ip-tool)
  (:tool "vt_status" +status-schema+ 'status-tool)
  (:command "vt-check" 'run-slash
            :description "Hash a download, or check a link, a domain or an IP"
            :argument-hint "~/Downloads/setup.deb")
  (:start (lambda ()
            (setf *cache* (make-hash-table :test #'equal))
            (nle:on-stop (lambda () (setf *cache* nil))))))
