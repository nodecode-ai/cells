;;;; sources.lisp --- provenance classification, ported from the plugin's
;;;; sources.py.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two questions, both answered from the call and never from the content:
;;;; CLASSIFY-SOURCE -- who authored this result; PATHS-OF and EGRESS-OF --
;;;; which files and which hosts the call touches. The taint mark is set by
;;;; the source, which is the whole point: a novel injection phrasing still
;;;; gets gated because the turn that read a web page is gated.

(in-package #:nodecode-taint-sentry)

(defparameter +untrusted-tools+
  '("web_search" "web_extract" "web_fetch" "fetch" "fetch_url" "browse"
    "browser_exec" "computer_use" "vision_analyze" "look" "screenshot"
    "read_window_below" "drive_preview")
  "Tools whose result is authored by a third party. Exact names; the prefixes
below cover the rest.")

(defparameter +untrusted-prefixes+ '("mcp_" "web_" "browser" "chrome_" "computer_")
  "Any tool from an MCP server, or from a browser, is third-party code with
third-party content.")

(defparameter +path-gated-tools+ '("sh" "eval" "write-file" "edit" "patch"
                                   "read_file" "read-file" "search_files" "grep")
  "Tools whose trust depends on WHAT they touch: a path in the workspace is the
user's, a path in ~/.ssh is not, and a file written earlier from untrusted
content stays untrusted.")

(defparameter +write-tools+ '("write-file" "edit" "patch")
  "Tools that put their argument on disk under a path they name.")

(defparameter +sensitive-pattern+
  (concatenate 'string
               "(?i)(?:id_(?:rsa|ed25519|ecdsa|dsa)(?!\\.pub)|\\.ssh[/\\\\]|\\.aws[/\\\\]|"
               "\\.kube[/\\\\]|\\.gnupg[/\\\\]|\\.docker[/\\\\]config\\.json|"
               "\\.git-credentials|\\.netrc|\\.npmrc|\\.pypirc|\\.pgpass|"
               "(?:^|[/\\\\\\s\"'])\\.env(?!\\.(?:example|sample|template|dist|md|txt))(?:[./\\\\\\s\"']|$)|"
               "auth\\.json|credentials(?:\\.json)?|secrets?\\.(?:json|yaml|yml|toml|ini)|"
               "\\.nodecode[/\\\\]|keychain|login\\.keychain|credential manager|"
               "\\.pem\\b|\\.pfx\\b|\\.p12\\b|wallet\\.dat|keystore)")
  "What an injected instruction wants: a key, a token, a credential store.")

(defparameter +sensitive-dir-roots+
  '("~/.ssh" "~/.aws" "~/.kube" "~/.gnupg" "/.ssh" "/.aws" "/.kube" "/.gnupg"
    ".nodecode" "/.nodecode")
  "A directory on its own is a listing, not a credential; reading inside it is
not. `ls ~/.ssh' passes, `cat ~/.ssh/id_rsa' does not.")

(defparameter +instruction-pattern+
  (concatenate 'string
               "(?i)(?:\\b(?:AGENTS|CLAUDE)\\.md\\b|\\.nodecode\\.md\\b|\\bSKILL\\.md\\b|"
               "\\.nodecode[/\\\\]config\\.(?:json|jsonc|ya?ml)\\b|"
               "[/\\\\]cells[/\\\\]|\\bplugin\\.ya?ml\\b)")
  "Files the agent itself reads as instructions in a later session: a write here
is not a data change, it is a change to what the agent will do next time.")

(defparameter +net-client-pattern+
  "(?i)(?<![\\w.-])(?:curl|wget|httpie|http|nc|ncat|netcat|scp|sftp|rsync|ftp|telnet|ssh|pwsh|powershell|Invoke-WebRequest|Invoke-RestMethod|iwr|irm)(?![\\w-])"
  "Network clients that reach out. Interpreters count only when the code uses a
network library, so not every dotted identifier in a script becomes a host.")

(defparameter +network-lib-pattern+
  "(?i)(?:requests\\.|urllib|http\\.client|httpx\\.|socket\\.|urlopen|fetch\\(|axios|paramiko|ftplib|smtplib|boto3|nle:http|nlk:http)")

(defparameter +send-markers+
  '("--data" "--data-binary" "--data-raw" "-d " "--form" "--upload-file"
    "--post-file" "-X POST" "-X PUT" "--method POST" "requests.post"
    "requests.put" "urlopen(" "httpx.post" "fetch(" "--json")
  "What a command does when it means to put a body on the wire, as opposed to
just fetching a page.")

(defparameter +url-host-pattern+ "(?i)https?://([^/\\s\"'<>()\\[\\]]+)" "The host of a URL, if a URL is there.")

(defparameter +known-tlds+
  '("com" "net" "org" "edu" "gov" "int" "io" "co" "ai" "app" "dev" "sh" "xyz"
    "info" "biz" "me" "tv" "cc" "cloud" "site" "online" "store" "tech" "space"
    "live" "news" "blog" "pro" "top" "page" "link" "email" "group" "team"
    "work" "world" "today" "life" "zone" "host" "website" "press" "media"
    "agency" "digital" "studio" "design" "software" "systems" "network"
    "solutions" "services" "uk" "de" "fr" "nl" "ru" "ua" "pl" "it" "es" "se"
    "no" "fi" "dk" "cz" "gr" "tr" "cn" "jp" "in" "br" "mx" "ar" "cl" "za" "au"
    "nz" "ca" "us" "test" "example" "localhost" "internal" "lan")
  "Bare-host detection is restricted to real TLDs, or every dotted identifier in
a script reads as a hostname and feeds the egress rules.")

(defparameter +local-hosts+ '("localhost" "127.0.0.1" "::1" "0.0.0.0")
  "Never an exfiltration channel worth gating.")

;;; --- reading a call ---------------------------------------------------------

(defun downcase-name (name)
  "NAME as a lowercase string, whether it is a string or a symbol."
  (string-downcase (if (stringp name) name (princ-to-string name))))

(defun argument-strings (args)
  "Every string value in ARGS, the JSON object of one tool call."
  (let ((found '()))
    (when (hash-table-p args)
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (cond ((stringp value) (push value found))
                       ((and (consp value) (every #'stringp value))
                        (dolist (item value) (push item found)))))
               args))
    (nreverse found)))

(defun call-text (args)
  "ARGS' string values as one text: what a rule reads when it has no path or
command of its own to read."
  (format nil "~{~a~^ ~}" (argument-strings args)))

(defun argument (args key)
  "ARGS' member KEY, matched without case, or NIL."
  (when (hash-table-p args)
    (loop for k being the hash-keys of args using (hash-value v)
          when (string-equal k key) return v)))

(defun command-text (tool args)
  "The command a shell tool would run, or the form an eval would evaluate."
  (let ((value (or (argument args "command") (argument args "form")
                    (argument args "cmd") (argument args "script"))))
    (if (stringp value) value "")))

;;; --- paths ------------------------------------------------------------------

(defparameter +token-pattern+ "[^\\s\"'`(){}\\[\\],;|&<>]+" "Filename-shaped tokens.")

(defun tokenize (text)
  "TEXT's filename-shaped tokens, quotes trimmed."
  (when (and text (plusp (length text)))
    (let ((found '()))
      (cl-ppcre:do-scans (start end reg-start reg-end +token-pattern+ text)
        (declare (ignore reg-start reg-end))
        (let ((token (string-trim "'\"`" (subseq text start end))))
          (when (plusp (length token)) (push token found))))
      (nreverse found))))

(defun expand-path (path)
  "PATH with a leading ~ read against the home directory."
  (let ((text (string-trim "'\"`" (or path ""))))
    (if (and (plusp (length text)) (char= (char text 0) #\~))
        (concatenate 'string (or (uiop:getenv "HOME") "~") (subseq text 1))
        text)))

(defun sensitive-path-p (path)
  "Whether PATH names material an injected instruction would want to read."
  (let* ((text (string-trim "'\"`/ " (or path "")))
         (full (and (plusp (length text))
                    (cl-ppcre:scan +sensitive-pattern+ (expand-path path)))))
    (cond ((null full) nil)
          ((cl-ppcre:scan "(?i)\\.pub$" text) nil)
          (t (not (loop for root in +sensitive-dir-roots+
                        thereis (let ((root (string-downcase (string-trim "/" root))))
                                  (string= root (string-downcase (string-trim "/" text))))))))))

(defun instruction-path-p (path)
  "Whether writing PATH changes what the agent reads as instructions later."
  (and (stringp path) (plusp (length path))
       (not (null (cl-ppcre:scan +instruction-pattern+ path)))))

(defun temp-path-p (path)
  "Whether PATH is scratch space, which a write may leave the workspace for."
  (let ((text (string-downcase (or path "")))
        (tmp (string-downcase (or (uiop:getenv "TMPDIR") "/tmp"))))
    (or (uiop:string-prefix-p "/tmp/" text)
        (uiop:string-prefix-p "/var/tmp/" text)
        (uiop:string-prefix-p (concatenate 'string (string-right-trim "/" tmp) "/") text))))

(defun inside-p (path root)
  "Whether PATH is ROOT or under it."
  (let ((path (expand-path path)) (root (expand-path root)))
    (or (uiop:string-prefix-p (concatenate 'string root "/") (string-right-trim "/" path))
        (string= (string-right-trim "/" path) (string-right-trim "/" root)))))

(defun workspace ()
  "The folder a path is judged against: where this process was started, as a
name, so every reader of it compares strings."
  (if (ignore-errors (uiop:getcwd))
      (namestring (string-right-trim "/" (namestring (uiop:getcwd))))
      ""))

(defun paths-of (tool args &optional (root (workspace)))
  "The paths one call touches, split by what they mean.
=> a plist of :PATHS (all found), :SENSITIVE, :SENSITIVE-READS (minus the
   ones this call writes), :WRITTEN, :OUTSIDE (outside ROOT and not scratch),
   :INSTRUCTION, and :WRITES."
  (let* ((text (call-text args))
         (command (command-text tool args))
         (tool-name (downcase-name tool))
         (tokens (append (tokenize text) (tokenize command)))
         (paths (remove-if-not (lambda (token)
                                  (or (find #\/ token) (find #\. token)
                                      (sensitive-path-p token)
                                      (instruction-path-p token)))
                                tokens))
         (paths (remove-duplicates (mapcar (lambda (path) (string-trim ":" path)) paths)
                                   :test #'equal))
         (writes (or (member tool-name +write-tools+ :test #'equal)
                     (and command (not (null (cl-ppcre:scan "[>]|\\btee\\b|\\b-o\\b|--output" command))))))
         (sensitive (remove-if-not #'sensitive-path-p paths))
         (instruction (remove-if-not #'instruction-path-p paths))
         (outside (remove-if (lambda (path) (or (inside-p path root) (temp-path-p path)))
                             paths))
         (written (when writes
                    (let ((named (or (argument args "path") (argument args "file")
                                     (argument args "target"))))
                      (if (stringp named)
                          (list named)
                          (remove-if-not #'instruction-path-p paths)))))
         (sensitive-reads (if writes
                               (set-difference (copy-list sensitive) (copy-list written)
                                               :test #'equal)
                               sensitive)))
    (list :paths paths :sensitive sensitive :sensitive-reads sensitive-reads
          :written written :outside outside :instruction instruction :writes (and writes t))))

(defun written-text (args)
  "The text a write would put on disk."
  (format nil "~{~a~^ ~}"
          (loop for key in '("content" "new_string" "new_str" "text" "body" "line" "data")
                for value = (argument args key)
                when (stringp value) collect value)))

;;; --- hosts ------------------------------------------------------------------

(defun strip-port (host)
  "HOST without a :port suffix, lowercased."
  (let ((text (string-downcase (string-trim " " (or host "")))))
    (if (and (plusp (length text)) (char= (char text 0) #\[))
        (string-left-trim "[" (first (uiop:split-string text :separator "]")))
        (first (uiop:split-string text :separator ":")))))

(defun hosts-in (text)
  "Every host TEXT names: URLs first, then bare names with a real TLD."
  (when (and (stringp text) (plusp (length text)))
      (let ((found '()))
        (cl-ppcre:do-scans (start end reg-start reg-end +url-host-pattern+ text)
          (declare (ignore start end reg-start))
          (let ((host (strip-port (subseq text (aref reg-start 0) (aref reg-end 0)))))
            (when (plusp (length host)) (pushnew host found :test #'equal))))
        (dolist (token (tokenize text))
          (let ((host (strip-port token)))
            (when (and (find #\. host)
                       (member (first (last (uiop:split-string host :separator ".")))
                               +known-tlds+ :test #'equal))
              (pushnew host found :test #'equal))))
        (nreverse found))))

(defun net-capable-p (tool args)
  "Whether TOOL, with ARGS, can reach the network at all."
  (let ((command (command-text tool args))
        (text (call-text args)))
    (or (cl-ppcre:scan +net-client-pattern+ command)
        (cl-ppcre:scan +network-lib-pattern+ text)
        (some (lambda (name) (member (downcase-name tool)
                                     (list "fetch" "web_fetch" "web_search" "web_extract"
                                           "browse" "browser_exec" "http" "delegate_task")
                                     :test #'equal))
              (list tool)))))

(defun egress-of (tool args)
  "What one call would put on the wire.
=> a plist :HOSTS (every host named) :SENDS (whether it sends data)."
  (let* ((command (command-text tool args))
         (text (call-text args))
         (hosts (hosts-in (concatenate 'string command " " text)))
         (sends (and (net-capable-p tool args)
                     (or (some (lambda (marker)
                                 (not (null (cl-ppcre:scan (cl-ppcre:quote-meta-chars marker)
                                                            (concatenate 'string command " " text)))))
                               +send-markers+)
                         ;; A POST-shaped URL is a send even when the marker list misses it.
                         (not (null (cl-ppcre:scan "(?i)(?:requests\\.post|urlopen\\(|fetch\\(|\\bPOST\\b)"
                                                     (concatenate 'string command " " text))))))))
    (list :hosts hosts :sends (and sends t))))

(defun host-allowed-p (host allow)
  "Whether HOST is on ALLOW: the entry itself or any domain above it."
  (let ((host (string-downcase (or host ""))))
    (some (lambda (entry)
            (let ((entry (string-downcase (string-left-trim "." (or entry "")))))
              (and (plusp (length entry))
                   (or (string= host entry)
                       (uiop:string-prefix-p (concatenate 'string "." entry) host)
                       (uiop:string-prefix-p (concatenate 'string entry ".") host)))))
          allow)))

(defun external-hosts (egress allow)
  "EGRESS' hosts that are neither loopback nor allowlisted."
  (remove-if (lambda (host)
               (or (member host +local-hosts+ :test #'equal)
                   (host-allowed-p host allow)))
             (getf egress :hosts)))

;;; --- who authored the result ------------------------------------------------

(defun untrusted-tool-p (tool)
  "Whether TOOL's result is authored by a third party, by name or by prefix."
  (let ((name (downcase-name tool)))
    (or (member name (mapcar #'downcase-name +untrusted-tools+) :test #'equal)
        (some (lambda (prefix) (uiop:string-prefix-p (downcase-name prefix) name))
              +untrusted-prefixes+))))

(defun mcp-server-of (tool)
  "The MCP server TOOL came from, when its name says so."
  (let ((name (downcase-name tool)))
    (when (uiop:string-prefix-p "mcp_" name)
      (let ((parts (uiop:split-string (if (uiop:string-prefix-p "mcp__" name)
                                          (subseq name 5)
                                          (subseq name 4))
                                       :separator "__")))
        (or (first parts) "mcp")))))

(defun classify-source (tool args &key tainted-artifact)
  "Where the result of TOOL, called with ARGS, comes from.
Only genuine third-party authorship taints: a remote fetch, an MCP server, a
screen or an image, and a file this session itself wrote while its content was
untrusted. A plain local file read is the user's own material... flagging those
would taint every coding session and gate work for no reason.
=> (values TRUST LABEL REASON), TRUST being \"untrusted\" or \"trusted\"."
  (let ((name (downcase-name tool)))
    (cond
      ((mcp-server-of tool)
       (values "untrusted" (format nil "~a (mcp:~a)" name (mcp-server-of tool))
               "third-party MCP server"))
      ((untrusted-tool-p tool)
       (let* ((text (call-text args))
              (host (first (hosts-in text))))
         (values "untrusted"
                 (if host (format nil "~a:~a" name host) name)
                 "content authored by a third party")))
      ((and tainted-artifact (plusp (length tainted-artifact)))
       (values "untrusted"
               (format nil "file:~a" (subseq tainted-artifact 0 (min 60 (length tainted-artifact))))
               (format nil "file written from untrusted content (~a)" tainted-artifact)))
      ((and (member name (mapcar #'downcase-name +path-gated-tools+) :test #'equal)
            (getf (egress-of tool args) :hosts))
       (values "untrusted" (format nil "~a:~a" name (first (getf (egress-of tool args) :hosts)))
               "a call that reached the network returns third-party content"))
      (t (values "trusted" name "")))))
