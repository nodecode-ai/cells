;;;; cell.lisp --- what this machine can reach, before the model says it cannot.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Read-only. One scan answers what is live on this machine now: the
;;;; browsers answering on their DevTools ports and the sites signed in in
;;;; them, the services listening on their usual ports, the MCP servers this
;;;; home lists, the CLIs on PATH (and, when the section asks, whether they
;;;; are signed in) and the mounted drives. A compact map rides each turn's
;;;; prompt as a harness section; three verbs answer from the whole one: the
;;;; reachmap tool, /reachmap, and (reachmap:report).
;;;;
;;;; The original is a Hermes Agent plugin: a hook that froze a map into
;;;; every session's prompt, scanners over the machine (browsers, MCP,
;;;; services, CLIs, drives), a capability tool, and a post-hook that caught
;;;; a reply denying a reach the map held. Here the scanners read the same
;;;; machine with no Hermes runtime: DevTools over HTTP on loopback, a
;;;; listening port by one TCP connect, a CLI by one PATH lookup, and the
;;;; prompt section through the host's own harness sections.

(defpackage #:nodecode-reachmap
  (:use #:cl)
  (:nicknames #:reachmap)
  (:export #:report #:find-capability #:check-text #:capabilities #:reachmap-error))

(in-package #:nodecode-reachmap)

(nlk:define-peripheral reachmap :not-running t)

;;; --- the seams ----------------------------------------------------------------
;;; Everything that leaves this image is one function a test replaces: the
;;; exchange, the port probe, the PATH lookup, the sign-in probe, the mount
;;; list, and this home's config.

(defvar *http* #'nlk:http
  "The one HTTP exchange: (METHOD URL &KEY TIMEOUT ...) => BODY, STATUS, HEADERS.")

(defvar *port-open-p* nil
  "Something accepting a TCP connection on HOST PORT, within TIMEOUT seconds.")

(defvar *which* nil
  "The path of executable NAME on PATH, or NIL.")

(defvar *probe-runner* nil
  "Run (BINARY ARGUMENTS TIMEOUT) => (values OUTPUT EXIT-CODE).")

(defvar *mounts* nil
  "The mounted volumes, as a list of (NAME PATH).")

(defvar *config-get* nil
  "This home's config at a path, as (config-get '(\"mcp\")) reads it.")

(defvar *snapshot* nil
  "The last scan, while the cell runs: (:SCANNED-AT :MS :SCANNERS :CAPABILITIES).")

;;; --- what a scan looks for ------------------------------------------------------
;;; Data, not code, and the same order the original reported in.

(defparameter +kinds+
  '((:id "browser" :label "BROWSER SESSIONS  (signed in, answering on DevTools)" :short "browsers")
    (:id "service" :label "LOCAL SERVICES  (accepting connections on this machine)" :short "services")
    (:id "mcp" :label "MCP SERVERS  (listed in this home's config)" :short "mcp")
    (:id "cli" :label "CLI TOOLS  (on PATH)" :short "clis")
    (:id "drive" :label "DRIVES  (mounted)" :short "drives"))
  "Every kind of capability, in the order a report lists them.")

(defparameter +cdp-ports+ '(9222 9223 9224 9225 9226 9229 9230 9333)
  "The DevTools ports the original probed when nothing named them.")

(defparameter +web-services+
  '(("mail.google.com" "Gmail" ("gmail" "email" "inbox"))
    ("drive.google.com" "Google Drive" ("drive" "files" "folders"))
    ("docs.google.com" "Google Docs" ("docs" "documents"))
    ("calendar.google.com" "Google Calendar" ("calendar" "schedule"))
    ("console.cloud.google.com" "Google Cloud" ("gcp" "google cloud"))
    ("github.com" "GitHub" ("github" "repos" "pull requests" "issues"))
    ("gitlab.com" "GitLab" ("gitlab" "repos" "merge requests"))
    ("bitbucket.org" "Bitbucket" ("bitbucket" "repos"))
    ("x.com" "X" ("x" "twitter" "posts" "tweets"))
    ("twitter.com" "X" ("x" "twitter" "posts" "tweets"))
    ("linkedin.com" "LinkedIn" ("linkedin" "posts"))
    ("instagram.com" "Instagram" ("instagram" "posts"))
    ("facebook.com" "Facebook" ("facebook" "posts"))
    ("reddit.com" "Reddit" ("reddit" "posts"))
    ("studio.youtube.com" "YouTube Studio" ("youtube" "video" "uploads"))
    ("youtube.com" "YouTube" ("youtube" "video"))
    ("app.slack.com" "Slack" ("slack" "messages" "channels"))
    ("slack.com" "Slack" ("slack" "messages"))
    ("notion.so" "Notion" ("notion" "notes" "pages"))
    ("linear.app" "Linear" ("linear" "issues" "tickets"))
    ("figma.com" "Figma" ("figma" "design"))
    ("vercel.com" "Vercel" ("vercel" "deploy" "deploys"))
    ("app.netlify.com" "Netlify" ("netlify" "deploy"))
    ("netlify.com" "Netlify" ("netlify" "deploy"))
    ("dash.cloudflare.com" "Cloudflare" ("cloudflare" "dns" "workers"))
    ("supabase.com" "Supabase" ("supabase" "database"))
    ("dashboard.stripe.com" "Stripe" ("stripe" "payments" "billing"))
    ("admin.shopify.com" "Shopify admin" ("shopify" "store" "orders"))
    ("partners.shopify.com" "Shopify Partners" ("shopify" "apps"))
    ("console.aws.amazon.com" "AWS Console" ("aws" "s3" "ec2" "lambda"))
    ("huggingface.co" "Hugging Face" ("huggingface" "models" "hf"))
    ("chatgpt.com" "ChatGPT" ("chatgpt" "openai"))
    ("claude.ai" "Claude" ("claude" "anthropic"))
    ("openrouter.ai" "OpenRouter" ("openrouter" "models"))
    ("platform.openai.com" "OpenAI Platform" ("openai" "api keys"))
    ("airtable.com" "Airtable" ("airtable" "base" "bases"))
    ("trello.com" "Trello" ("trello" "boards"))
    ("app.asana.com" "Asana" ("asana" "tasks"))
    ("outlook.office.com" "Outlook" ("outlook" "mail"))
    ("teams.microsoft.com" "Teams" ("teams" "messages"))
    ("portal.azure.com" "Azure Portal" ("azure" "cloud"))
    ("app.hubspot.com" "HubSpot" ("hubspot" "crm"))
    ("mail.yandex.com" "Yandex Mail" ("yandex" "mail"))
    ("publish.buffer.com" "Buffer" ("buffer" "posts"))
    ("www.npmjs.com" "npm" ("npm" "packages"))
    ("pypi.org" "PyPI" ("pypi" "packages"))
    ("railway.com" "Railway" ("railway" "deploy")))
  "Host -> service name and the words a person says for it. A tab is
matched by the longest host that ends it.")

(defparameter +login-markers+
  '("accounts.google.com" "login.microsoftonline.com" "login.live.com"
    "github.com/login" "signin" "sign-in" "/login" "/auth/" "/session/new" "/i/flow")
  "A tab whose url carries one of these is a sign-in page, not a session.")

(defparameter +service-ports+
  '((11434 "Ollama") (1234 "LM Studio") (8188 "ComfyUI") (7860 "Gradio / Stable Diffusion")
    (5678 "n8n") (3000 "a dev server") (5173 "Vite dev server") (8000 "a dev server")
    (8080 "a web server") (6006 "Storybook") (9200 "Elasticsearch") (5432 "PostgreSQL")
    (3306 "MySQL") (6379 "Redis") (27017 "MongoDB") (9119 "the Nodecode dashboard")
    (8642 "the Nodecode gateway"))
  "PORT -> what usually answers there, on loopback.")

;;; --- reading the section ----------------------------------------------------

(defun string-list (value)
  "VALUE as a list of trimmed non-empty strings, whatever the section, a
config file or a stub handed over."
  (let ((items (cond ((null value) nil)
                     ((stringp value) (list value))
                     ((listp value) value)
                     ((vectorp value) (coerce value 'list))
                     (t (list (princ-to-string value))))))
    (loop for item in items
          for text = (nlk:trimmed (princ-to-string (or item "")))
          when (plusp (length text)) collect text)))

(defun port-number (text)
  "TEXT as a TCP port number, or NIL."
  (when (and (stringp text) (plusp (length text)) (every #'digit-char-p text))
    (let ((number (parse-integer text)))
      (and (<= 1 number 65535) number))))

(defun ports-from (key &key default)
  "The ports the section names under KEY, or DEFAULT when it names none."
  (let ((items (string-list (setting key))))
    (if items
        (loop for item in items
              for number = (port-number item)
              if number collect number
              else do (fail "~a names ~s, which is not a TCP port"
                            (string-downcase (symbol-name key)) item))
        default)))

(defun scanner-ids ()
  "The scanners this scan runs, in the order the data lists them."
  (let* ((named (string-list (setting :scanners)))
         (all (mapcar (lambda (row) (getf row :id)) +scanning+)))
    (if named (remove-if-not (lambda (id) (member id named :test #'equal)) all) all)))

(defun timeout ()
  "One probe's ceiling, in seconds."
  (or (setting :timeout-seconds) 2))

(defun flag-on-p (key)
  "The boolean member KEY, read as on: absent, NIL or a written false is off."
  (let ((value (setting key)))
    (and value (not (member value '(:false "false" "no" 0) :test #'equal)) t)))

(defun cache-seconds ()
  "How long one scan is reused."
  (or (setting :cache-seconds) 0))

;;; --- what the machine says --------------------------------------------------

(defun ends-with-p (suffix text)
  (let ((n (length suffix)))
    (and (>= (length text) n) (equal suffix (subseq text (- (length text) n))))))

(defun url-host (url)
  "The lowercased host of URL, without its port, or NIL."
  (let ((url (and (stringp url) (string-downcase url))))
    (when (and url (plusp (length url)))
      (let* ((start (let ((scheme (search "://" url))) (if scheme (+ scheme 3) 0)))
             (end (or (position-if (lambda (c) (member c '(#\/ #\? #\#))) url :start start)
                      (length url)))
             (host (subseq url start end))
             (colon (position #\: host)))
        (when (plusp (length host))
          (if colon (subseq host 0 colon) host))))))

(defun identities (text)
  "The e-mail addresses TEXT holds, in order: who is signed in."
  (let ((text (and (stringp text) text)))
    (if (and text (plusp (length text)))
        (remove-duplicates
         (cl-ppcre:all-matches-as-strings "[\\w.+-]+@[\\w-]+(?:\\.[\\w-]+)+" text)
         :test #'equal)
        nil)))

(defun json-field (object key)
  "OBJECT's KEY as a string, or NIL."
  (when (hash-table-p object)
    (let ((value (gethash key object)))
      (when value (princ-to-string value)))))

(defun http-json (url)
  "URL answered as a JSON value by the exchange, or NIL when nothing did."
  (multiple-value-bind (body status) (funcall *http* :get url :timeout (timeout))
    (when (and body (eql status 200))
      (handler-case (shasht:read-json (if (stringp body) body (princ-to-string body)))
        (error () nil)))))

(defun capability (id kind name detail &key how words)
  "One thing this machine can reach, as the verbs read it."
  (list :id id :kind kind :name name :detail detail
        :how (and how (plusp (length how)) how)
        :words (remove-duplicates
                (remove "" (mapcar (lambda (w) (string-downcase (princ-to-string w))) words))
                :test #'equal)))

;;; --- the seams' own defaults ------------------------------------------------

(defun port-open-default (host port &key (timeout 1))
  "T when something on HOST accepts a connection to PORT within TIMEOUT seconds."
  (let ((socket nil))
    (unwind-protect
         (handler-case
             (progn (setf socket (usocket:socket-connect host port
                                                          :element-type '(unsigned-byte 8)
                                                          :timeout timeout))
                    t)
           (error () nil))
      (when socket (ignore-errors (usocket:socket-close socket))))))

(defun which-default (name)
  "The first regular file named NAME on PATH, or NIL."
  (let ((path (uiop:getenv "PATH")))
    (when path
      (loop for directory in (uiop:split-string path :separator '(#\:))
            for file = (and (plusp (length directory))
                            (probe-file (merge-pathnames name
                                                          (uiop:ensure-directory-pathname directory))))
            when (and file (not (uiop:directory-pathname-p file)))
              return (namestring file)))))

(defun probe-run-default (binary arguments seconds)
  "Run BINARY with ARGUMENTS under SECONDS. => (values OUTPUT EXIT-CODE)."
  (handler-case
      (sb-ext:with-timeout seconds
        (multiple-value-bind (output error-output code)
            (uiop:run-program (cons binary (mapcar #'princ-to-string arguments))
                              :output :string :error-output :string :ignore-error-status t)
          (declare (ignore error-output))
          (values (or output "") (or code 0))))
    (sb-ext:timeout () (values "" -1))
    (error () (values "" -1))))

(defun mounts-default ()
  "The volumes mounted under the usual roots, as (NAME PATH)."
  (let ((user (uiop:getenv "USER")))
    (loop for root in (remove nil (list (and user (format nil "/run/media/~a" user))
                                        (and user (format nil "/media/~a" user))
                                        "/media" "/mnt" "/Volumes"))
          when (uiop:directory-exists-p root)
            append (loop for path in (uiop:subdirectories root)
                         for name = (car (last (pathname-directory path)))
                         when (and name (not (member (string-downcase (princ-to-string name))
                                                     '("cdrom" "floppy") :test #'equal)))
                           collect (list (princ-to-string name) (namestring path))))))

(defun config-get-default (path)
  "This home's config at PATH, or NIL when this host does not answer one."
  (let ((function (find-symbol "CONFIG-GET" "NLE")))
    (when (and function (fboundp function))
      (handler-case (funcall function path) (error () nil)))))

;;; --- browser sessions -------------------------------------------------------

(defparameter +scanning+
  '((:id "browser" :scan scan-browsers)
    (:id "mcp" :scan scan-mcp)
    (:id "service" :scan scan-services)
    (:id "cli" :scan scan-clis)
    (:id "drive" :scan scan-drives))
  "The scanners, by the id the section names: one function each, no argument.")

(defun service-row-for (url)
  "The (HOST NAME WORDS) row a tab on URL belongs to, longest host first, or NIL."
  (let* ((host (url-host url))
         (rows (and host
                    (loop for row in +web-services+
                          when (or (equal host (first row))
                                   (ends-with-p (concatenate 'string "." (first row)) host))
                            collect row))))
    (first (sort rows #'> :key (lambda (row) (length (first row)))))))

(defun login-page-p (url)
  (let ((url (string-downcase (or url ""))))
    (some (lambda (marker) (search marker url :test #'char-equal)) +login-markers+)))

(defun sites-in (pages)
  "The services PAGES holds, one plist each: name, words, tabs, signed in, who."
  (let ((sites '()))
    (dolist (page pages)
      (let* ((url (json-field page "url"))
             (title (json-field page "title"))
             (row (service-row-for url)))
        (when row
          (let* ((name (second row))
                 (known (find name sites :key (lambda (site) (getf site :name)) :test #'equal))
                 (site (or known
                           (let ((fresh (list :name name :words (third row) :tabs 0
                                              :signed-in nil :identities nil :url nil)))
                             (push fresh sites)
                             fresh))))
            (incf (getf site :tabs))
            (unless (getf site :url) (setf (getf site :url) url))
            (unless (login-page-p url)
              (setf (getf site :signed-in) t)
              (dolist (who (append (identities title) (identities url)))
                (pushnew who (getf site :identities) :test #'equal)))))))
    (nreverse sites)))

(defun render-sites (sites &key (identities t))
  "SITES as one line: what is open, whether it is signed in, and as whom."
  (format nil "~{~a~^; ~}"
          (loop for site in sites
                collect (format nil "~a (~a~@[ as ~a~]~@[, ~d tab~:p~])"
                                (getf site :name)
                                (if (getf site :signed-in) "signed in" "open, not signed in")
                                (and identities (first (getf site :identities)))
                                (getf site :tabs)))))

(defun browser-family (string)
  "The browser's name from its version string: Chrome/134.0.6998.89 -> Chrome."
  (let ((first (first (uiop:split-string (or string "") :separator '(#\Space)))))
    (if (and first (plusp (length first)))
        (let ((name (first (uiop:split-string first :separator '(#\/)))))
          (if (and name (plusp (length name))) name first))
        "a browser")))

(defun probe-browser (port)
  "The capability a DevTools endpoint on PORT answers, or NIL when none does."
  (let* ((base (format nil "http://127.0.0.1:~d" port))
         (version (http-json (format nil "~a/json/version" base))))
    (when (and version (json-field version "Browser"))
      (let* ((browser (json-field version "Browser"))
             (tabs (http-json (format nil "~a/json/list" base)))
             (pages (loop for tab across (if (vectorp tabs) tabs #())
                          when (and (hash-table-p tab) (equal "page" (json-field tab "type")))
                            collect tab))
             (sites (sites-in pages))
             (who (not (flag-on-p :hide-identities))))
        (capability (format nil "browser:~d" port) "browser"
                    (format nil "~a on CDP :~d" (browser-family browser) port)
                    (if sites
                        (render-sites sites :identities who)
                        "answering, with no site open")
                    :how (format nil "attach a browser tool to cdp_url ~a, or open ~a/json/list"
                                 base base)
                    :words (append (list "browser" "chrome" "cdp" (format nil "~d" port) browser)
                                   (mapcan (lambda (site)
                                             (append (list (getf site :name))
                                                     (getf site :words)
                                                     (and who (getf site :identities))))
                                           sites)))))))

(defun scan-browsers ()
  (loop for port in (ports-from :cdp-ports :default +cdp-ports+)
        for found = (probe-browser port)
        when found collect found))

;;; --- local services ---------------------------------------------------------

(defun scan-services ()
  "One capability per usual port that accepts a connection."
  (let ((cdp (ports-from :cdp-ports :default +cdp-ports+))
        (extra (ports-from :extra-ports :default nil)))
    (loop for (port name) in (append (loop for port in extra collect (list port "a service"))
                                      +service-ports+)
          unless (member port cdp)
            when (funcall *port-open-p* "127.0.0.1" port :timeout (timeout))
              collect (capability (format nil "service:~d" port) "service"
                                  (format nil "~a on :~d" name port)
                                  "accepting connections on loopback"
                                  :how (format nil "open http://127.0.0.1:~d, or dial it with a client" port)
                                  :words (list "localhost" "port" (format nil "~d" port) name)))))

;;; --- mcp servers -------------------------------------------------------------

(defun server-names (object)
  "The server names in OBJECT, whatever shape the config read it in."
  (cond ((null object) nil)
        ((stringp object) (list object))
        ((hash-table-p object)
         (loop for key being the hash-keys of object collect (princ-to-string key)))
        ((listp object)
         (loop for item in object
               for name = (cond ((stringp item) item)
                                ((consp item) (princ-to-string (car item)))
                                (t (princ-to-string item)))
               when (plusp (length (nlk:trimmed name))) collect name))
        (t nil)))

(defun mcp-servers ()
  "The MCP servers this home lists in its config, as names, sorted."
  (let* ((section (ignore-errors (funcall *config-get* '("mcp"))))
         (names (if (and (listp section) (getf section :servers))
                    (server-names (getf section :servers))
                    (server-names section))))
    (sort (remove-duplicates (mapcar #'nlk:trimmed names) :test #'equal) #'string-lessp)))

(defun scan-mcp ()
  (loop for name in (mcp-servers)
        collect (capability (format nil "mcp:~a" name) "mcp"
                            (format nil "MCP server ~a" name)
                            "listed in this home's mcp section"
                            :how "its tools are on the model's tool list"
                            :words (list "mcp" name))))

;;; --- clis --------------------------------------------------------------------

(defparameter +cli-tools+
  '(("gh" "GitHub CLI" ("github" "pull requests" "issues"))
    ("glab" "GitLab CLI" ("gitlab" "merge requests"))
    ("docker" "Docker" ("containers" "images"))
    ("podman" "Podman" ("containers" "images"))
    ("kubectl" "kubectl" ("kubernetes" "cluster"))
    ("helm" "Helm" ("kubernetes" "charts"))
    ("terraform" "Terraform" ("infra" "cloud"))
    ("aws" "AWS CLI" ("aws" "s3" "ec2" "lambda"))
    ("gcloud" "gcloud" ("gcp" "google cloud"))
    ("az" "Azure CLI" ("azure" "cloud"))
    ("wrangler" "Wrangler" ("cloudflare" "workers"))
    ("vercel" "Vercel CLI" ("vercel" "deploy"))
    ("netlify" "Netlify CLI" ("netlify" "deploy"))
    ("railway" "Railway CLI" ("railway" "deploy"))
    ("flyctl" "fly.io CLI" ("fly" "deploy"))
    ("supabase" "Supabase CLI" ("supabase" "database"))
    ("psql" "psql" ("postgres" "sql" "database"))
    ("sqlite3" "sqlite3" ("sqlite" "sql"))
    ("redis-cli" "redis-cli" ("redis" "cache"))
    ("ollama" "Ollama" ("ollama" "local models" "llm"))
    ("ffmpeg" "ffmpeg" ("video" "audio" "convert"))
    ("magick" "ImageMagick" ("image" "convert"))
    ("git" "git" ("git" "commits" "branches"))
    ("node" "node" ("node" "javascript"))
    ("python3" "python3" ("python"))
    ("uv" "uv" ("python" "packages"))
    ("cargo" "cargo" ("rust"))
    ("go" "go" ("golang"))
    ("just" "just" ("tasks" "recipes"))
    ("jq" "jq" ("json"))
    ("hf" "Hugging Face CLI" ("huggingface" "models" "datasets"))
    ("xurl" "xurl" ("x" "twitter" "posts")))
  "BINARY -> what it is, and the words a person says for it.")

(defparameter +auth-probes+
  '(("gh" ("auth" "status") "GitHub")
    ("aws" ("sts" "get-caller-identity") "AWS")
    ("gcloud" ("auth" "list" "--filter=status:ACTIVE" "--format=value(account)") "Google Cloud")
    ("az" ("account" "show" "--query" "user.name" "-o" "tsv") "Azure")
    ("vercel" ("whoami") "Vercel")
    ("netlify" ("status") "Netlify")
    ("wrangler" ("whoami") "Cloudflare")
    ("railway" ("whoami") "Railway")
    ("flyctl" ("auth" "whoami") "fly.io")
    ("hf" ("auth" "whoami") "Hugging Face")
    ("xurl" ("whoami") "X")
    ("supabase" ("projects" "list") "Supabase"))
  "BINARY -> the arguments that answer who it is signed in as, and its name.
A zero exit with output reads as signed in; anything else as not.")

(defun scanner-choices (&optional table)
  "The scanners the scanners member may name, as (ID . LABEL) rows."
  (declare (ignore table))
  (loop for row in +scanning+
        for kind = (find (getf row :id) +kinds+ :key (lambda (k) (getf k :id)) :test #'equal)
        collect (cons (getf row :id)
                      (or (and kind (getf kind :short)) (getf row :id)))))

(defun cli-choices (&optional table)
  "The tools the auth_probes member may name, as (ID . LABEL) rows."
  (declare (ignore table))
  (loop for (binary arguments label) in +auth-probes+
        collect (cons binary (format nil "~a (~a)" label binary))))

(defun probe-sign-in (binary)
  "How BINARY's sign-in reads: (values STATE DETAIL)."
  (let ((row (assoc binary +auth-probes+ :test #'equal))
        (path (funcall *which* binary)))
    (if (null row)
        (values "unknown" "not a tool this cell probes")
        (multiple-value-bind (output code)
            (funcall *probe-runner* path (second row) (timeout))
          (let ((output (nlk:trimmed (or output ""))))
            (cond ((null path) (values "unknown" "not installed"))
                  ((and (eql code 0) (plusp (length output)))
                   (values "signed in" (first (uiop:split-string output :separator '(#\Newline)))))
                  ((eql code 0) (values "unknown" "answered nothing"))
                  (t (values "not signed in"
                             (or (first (uiop:split-string output :separator '(#\Newline)))
                                 (format nil "exit ~a" code))))))))))

(defun scan-clis ()
  "One capability per tool on PATH, and one for each sign-in the section asks about."
  (let ((wanted (string-list (setting :auth-probes)))
        (caps '()))
    (dolist (row +cli-tools+)
      (let* ((binary (first row))
             (path (funcall *which* binary)))
        (when path
          (if (member binary wanted :test #'equal)
              (multiple-value-bind (state detail) (probe-sign-in binary)
                (push (capability (format nil "cli:~a" binary) "cli"
                                  (format nil "~a (~a)" (second row) binary)
                                  (format nil "~a -- ~a" state detail)
                                  :how (format nil "run ~a in a shell, or its API through eval" binary)
                                  :words (cons binary (third row)))
                      caps)
                (when (equal state "signed in")
                  (push (capability (format nil "auth:~a" binary) "cli"
                                    (format nil "~a signed in as ~a" (second row) detail)
                                    "its CLI answers who it is"
                                    :how (format nil "~a is signed in: its commands reach the account"
                                                 binary)
                                    :words (cons binary (third row)))
                        caps)))
              (push (capability (format nil "cli:~a" binary) "cli"
                                (format nil "~a (~a)" (second row) binary)
                                path
                                :how (format nil "run ~a in a shell" binary)
                                :words (cons binary (third row)))
                    caps)))))
    (nreverse caps)))

;;; --- drives -------------------------------------------------------------------

(defun scan-drives ()
  (loop for (name path) in (funcall *mounts*)
        collect (capability (format nil "drive:~a" name) "drive"
                            (format nil "disk ~a" name)
                            path
                            :how "read and write it as a directory"
                            :words (list "drive" "disk" "volume" name))))

;;; --- the scan ------------------------------------------------------------------

(defun scan ()
  "One scan of this machine: (:SCANNED-AT :MS :SCANNERS :CAPABILITIES)."
  (running-settings)
  (let* ((started (get-internal-real-time))
         (ids (scanner-ids))
         (found (loop for row in +scanning+
                      when (member (getf row :id) ids :test #'equal)
                        append (handler-case (funcall (getf row :scan))
                                 (error (condition)
                                   (warn "reachmap: the ~a scanner failed: ~a"
                                         (getf row :id) condition)
                                   nil))))
         (snapshot (list :scanned-at (get-universal-time)
                         :ms (round (* 1000 (- (get-internal-real-time) started))
                                    internal-time-units-per-second)
                         :scanners ids
                         :capabilities found)))
    (setf *snapshot* snapshot)
    snapshot))

(defun current-scan (&key (refresh nil))
  "The scan to answer from: the last one while it is younger than
cache_seconds, else a fresh one."
  (let ((cache (cache-seconds)))
    (if (and (not refresh) *snapshot* (plusp cache)
             (< (- (get-universal-time) (getf *snapshot* :scanned-at)) cache))
        *snapshot*
        (scan))))

(defun capabilities (&key (refresh nil))
  "Everything live, as the map holds it now."
  (getf (current-scan :refresh refresh) :capabilities))

(defun rows-of (kind caps)
  "CAPS of KIND's id, in the order they were found."
  (remove (getf kind :id) caps :key (lambda (cap) (getf cap :kind)) :test-not #'equal))

;;; --- the report ----------------------------------------------------------------

(defun report (&key (refresh nil))
  "The whole map, as text: one block per kind, then what to do with it."
  (running-settings)
  (let* ((snapshot (current-scan :refresh refresh))
         (caps (getf snapshot :capabilities)))
    (if (null caps)
        (format nil "REACHMAP: nothing answered (~{~a~^, ~} ran, in ~,2fs). ~
Start what you expected and ask again: the reachmap tool with action=rescan."
                (getf snapshot :scanners) (/ (getf snapshot :ms) 1000.0))
        (with-output-to-string (out)
          (format out "REACHMAP  ~d live, scanned in ~,2fs  |  ~{~a~^, ~}"
                  (length caps) (/ (getf snapshot :ms) 1000.0) (getf snapshot :scanners))
          (dolist (kind +kinds+)
            (let ((rows (rows-of kind caps)))
              (when rows
                (format out "~&~%~a~%" (getf kind :label))
                (dolist (cap rows)
                  (format out "  ~a -- ~a~@[~%      ~a~]~%"
                          (getf cap :name) (getf cap :detail) (getf cap :how))))))
          (format out "~&~%What is here is live now, not a promise about later. ~
Before you say something is out of reach, the reachmap tool with action=find and its name.")))))

;;; --- find ----------------------------------------------------------------------

(defun score-of (cap words)
  "How well CAP answers WORDS: a hit in its name counts more than one in its detail."
  (let ((name (string-downcase (getf cap :name)))
        (text (string-downcase (format nil "~a ~{~a~^ ~}"
                                       (getf cap :name) (getf cap :words)))))
    (loop for word in words
          when (and (>= (length word) 2) (search word text))
            sum (if (search word name) 3 1))))

(defun find-capability (query &key (refresh nil))
  "The live capabilities QUERY matches, best first, as (SCORE . CAPABILITY)."
  (running-settings)
  (let ((words (remove "" (uiop:split-string (string-downcase (nlk:trimmed (or query "")))
                                              :separator '(#\Space #\Tab #\, #\; #\:))
                       :test #'equal)))
    (when (null words)
      (fail "find needs something to look for: \"gmail\", \"docker\", \"ollama\", a drive's name."))
    (let ((scored (loop for cap in (capabilities :refresh refresh)
                        for score = (score-of cap words)
                        when (plusp score) collect (cons score cap))))
      (sort scored #'> :key #'car))))

(defun find-text (query &key (refresh nil))
  "The find verb's answer: what QUERY reaches here, or that nothing does."
  (let ((hits (find-capability query :refresh refresh)))
    (if (null hits)
        (format nil "reachmap: nothing live matches ~s on this machine (~d live now). ~
If the operator expected it, start it and call the tool with action=rescan."
                query (length (capabilities)))
        (with-output-to-string (out)
          (format out "reachmap: ~d live hit~:p for ~s" (length hits) query)
          (dolist (hit hits)
            (let ((cap (cdr hit)))
              (format out "~%  ~a -- ~a~@[~%      ~a~]"
                      (getf cap :name) (getf cap :detail) (getf cap :how))))))))

;;; --- the denial check ------------------------------------------------------
;;; The original's post-hook read a finished reply and caught the model
;;; denying a reach the map held. These are the phrases it looked for, kept
;;; so a person who knew them there knows them here.

(defparameter +denial-patterns+
  '(("no access to" . "no access to")
    ("don'?t have (access|credentials|a (tool|connector)|an? [a-z]+ (connector|integration))"
     . "don't have")
    ("(not|never) (connected|signed in|logged in|authenticated)" . "not signed in")
    ("can'?t (access|reach|see|check|open|read|post|query|send)" . "can't")
    ("unable to (access|reach|see|check|open|read|post|query|send)" . "unable to")
    ("outside (my|its) reach" . "outside my reach")
    ("no (connector|integration|tool) for" . "no connector for")
    ("(haven'?t|have not) (checked|scanned|been given)" . "haven't checked")
    ("not available to me" . "not available"))
  "The denials the original caught, as (PATTERN . THE PHRASE IT READS LIKE).")

(defun denial-in (text)
  "The phrase TEXT denies in, or NIL."
  (let ((text (string-downcase (or text ""))))
    (loop for (pattern . said) in +denial-patterns+
          when (cl-ppcre:scan pattern text) return said)))

(defun mentions-p (cap text)
  "Does TEXT name CAP? Its name, or one of its words."
  (let ((text (string-downcase (or text ""))))
    (or (search (string-downcase (getf cap :name)) text)
        (some (lambda (word) (and (>= (length word) 3) (search word text)))
              (getf cap :words)))))

(defun check-text (text &key (refresh nil))
  "Does TEXT deny a reach this machine has? => the answer, as text."
  (running-settings)
  (let ((text (nlk:trimmed (or text ""))))
    (when (zerop (length text)) (fail "check needs the reply to read."))
    (let ((said (denial-in text)))
      (unless said
        (return-from check-text
          (format nil "reachmap: that text claims nothing out of reach, so there is nothing to correct (~d live now)."
                  (length (capabilities :refresh refresh)))))
      (let* ((caps (capabilities :refresh refresh))
             (hits (remove-if-not (lambda (cap) (mentions-p cap text)) caps)))
        (if (null hits)
            (format nil "reachmap: ~s reads as a denial (~a), but nothing live in ~d capabilities matches it. ~
If it is expected here, action=rescan after starting it; /reachmap shows the map."
                    text said (length caps))
            (format nil "reachmap: ~s reads as a denial (~a), yet this machine answers: ~{~a~^; ~}. ~
Say what it reaches, not what it lacks."
                    text said
                    (loop for cap in (subseq hits 0 (min 4 (length hits)))
                          collect (format nil "~a (~a)" (getf cap :name) (getf cap :detail)))))))))

;;; --- one dispatcher for the tool and the command ---------------------------------

(defun dispatch (action query text &key (refresh nil))
  "The one answer behind the tool and /reachmap."
  (let ((action (string-downcase (nlk:trimmed (or action "show")))))
    (cond ((member action '("show" "map" "scan" "") :test #'equal) (report :refresh refresh))
          ((member action '("rescan" "refresh" "again") :test #'equal) (report :refresh t))
          ((member action '("find" "search" "look" "reach") :test #'equal)
           (find-text query :refresh refresh))
          ((member action '("check" "denial" "audit") :test #'equal)
           (check-text (if (plusp (length (nlk:trimmed (or text "")))) text query)
                       :refresh refresh))
          (t (format nil "reachmap: ~s is not one of show, find, check, rescan. ~
find takes what to look for; check takes the reply to read." action)))))

;;; --- the tool, the command --------------------------------------------------------

(nlk:define-contract reachmap-call
  (:doc "What this machine can reach right now: the browsers answering on their DevTools ports and the sites signed in in them, the services listening on their usual ports, the MCP servers this home lists, the CLIs on PATH (and whether they are signed in, when the section asks) and the mounted drives. Read-only, and everything it reads is on this machine. Call it before saying something is out of reach.")
  ("action" :text :required t :options '("show" "find" "check" "rescan")
            :doc "show or rescan the whole map; find what a name reaches; check whether a reply denies a reach")
  ("query" :text :default ""
           :doc "for find: what the operator says it is -- gmail, docker, ollama, a drive's name")
  ("text" :text :default ""
          :doc "for check: the reply or claim to read for a denial"))

(defun guarded (body)
  "Run BODY, answering its text; a refusal becomes the tool answer ERROR: ..."
  (handler-case (funcall body)
    (reachmap-error (condition) (nle:failure (format nil "ERROR: REACHMAP-ERROR: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: REACHMAP-ERROR: ~a" condition)))))

(defun call-tool (call)
  "The reachmap tool: the contract's call, answered as text."
  (guarded (lambda () (dispatch (reachmap-call-action call)
                                 (reachmap-call-query call)
                                 (reachmap-call-text call)))))

(defun run-command (args session-id)
  "The /reachmap command: no argument shows the map, a verb does the rest."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (nlk:trimmed (or args ""))
                                               :separator '(#\Space #\Tab))
                        :test #'equal))
         (verb (first words))
         (rest (format nil "~{~a~^ ~}" (rest words))))
    (guarded (lambda () (dispatch (or verb "show") rest rest)))))

;;; --- the map in the prompt ----------------------------------------------------

(defun compact-map (&key (refresh nil))
  "The map as one harness block, or NIL when nothing answered."
  (let* ((snapshot (handler-case (current-scan :refresh refresh) (error () nil)))
         (caps (and snapshot (getf snapshot :capabilities))))
    (when caps
      (format nil "Reach on this machine, live now (~d thing~:p, scanned ~ds ago): ~{~a~^ | ~}. ~
It is a fact about this scan, not a promise about later: call the reachmap tool (action=find, or action=check on a reply that denies a reach) before saying something is out of reach."
              (length caps)
              (max 0 (- (get-universal-time) (getf snapshot :scanned-at)))
              (loop for kind in +kinds+
                    for rows = (rows-of kind caps)
                    when rows
                      collect (format nil "~a: ~{~a~^, ~}"
                                      (getf kind :short)
                                      (loop for cap in (subseq rows 0 (min 6 (length rows)))
                                            collect (getf cap :name))))))))

(defun observe-frame (op)
  "A :FRAME observer: when the operator's words are committed for a turn,
leave the map in that session's harness block, where the turn reads it."
  (when *reachmap*
    (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
      (declare (ignore payload metadata))
      (when (and turn (equal type "turn.input_committed"))
        (let ((session (getf op :session-id)))
          (when (and session (nlk:store-open-p))
            (let ((block (handler-case (compact-map) (error () nil))))
              (if block
                  (nlk:set-harness-section session "reachmap" block)
                  (nlk:clear-harness-section session "reachmap")))))))))

;;; --- the section ----------------------------------------------------------------

(defun section-declares-p (table name)
  "Did the section itself name NAME? The table is what the config holds."
  (cond ((hash-table-p table) (and (nth-value 1 (gethash name table)) t))
        ((listp table) (and (member name table :test #'equal) t))
        (t nil)))

(defun read-section (values table)
  "The section as the cell runs on it: every list trimmed, every port a
number, every scanner and probed tool one this cell has, refusing on
anything else and naming it."
  (let ((values (copy-list values)))
    (dolist (key '(:scanners :auth-probes :cdp-ports :extra-ports))
      (setf (getf values key) (string-list (getf values key))))
    ;; A section that names none of these runs on the declaration's defaults:
    ;; an absent boolean is not a false one.
    (dolist (row '(("cache_seconds" :cache-seconds 90)
                   ("timeout_seconds" :timeout-seconds 2)))
      (unless (section-declares-p table (first row))
        (setf (getf values (second row)) (third row))))
    (let ((known (mapcar (lambda (row) (getf row :id)) +scanning+)))
      (dolist (name (getf values :scanners))
        (unless (member name known :test #'equal)
          (nlk:config-error "reachmap: ~s is not a scanner this cell has: ~{~a~^, ~}. Leave scanners empty to run all of them."
                            name known))))
    (let ((probed (mapcar #'first +auth-probes+)))
      (dolist (name (getf values :auth-probes))
        (unless (member name probed :test #'equal)
          (nlk:config-error "reachmap: auth_probes names ~s, which this cell cannot probe: ~{~a~^, ~}"
                            name probed))))
    (dolist (key '(:cdp-ports :extra-ports))
      (dolist (text (getf values key))
        (unless (port-number text)
          (nlk:config-error "reachmap: ~a names ~s, which is not a TCP port"
                            (string-downcase (symbol-name key)) text))))
    values))

(defun start ()
  (setf *snapshot* nil)
  (nle:on-stop (lambda () (setf *snapshot* nil))))

(setf *port-open-p* #'port-open-default
      *which* #'which-default
      *probe-runner* #'probe-run-default
      *mounts* #'mounts-default
      *config-get* #'config-get-default)

;;; --- what the model is told it has ------------------------------------------------

(defparameter +primer+
  "This machine's reach is on the manual: what is live right now on it.
  reachmap   the tool: {\"action\": \"show\"|\"find\"|\"check\"|\"rescan\",
      \"query\": what to look for, \"text\": the reply to read}. show prints
      the whole map (browsers on their DevTools ports and the sites signed in
      in them, local services, MCP servers, CLIs on PATH, drives); find
      answers what one name reaches; check reads a reply and answers what it
      denies that is live; rescan forgets the cache and scans again.
  /reachmap [show|find <thing>|check <text>|rescan]   the same, by hand.
  (reachmap:report)   the whole map as text; (reachmap:find-capability
      \"gmail\")   the live rows matching a name; (reachmap:check-text ...)
      one reply.
A map line means the thing answered a probe on this scan: a port accepted a
connection, a DevTools endpoint answered, a binary is on PATH. The cell is
read only, never runs what it finds, and writes nothing but the session's own
prompt section. Before you tell the operator something is out of reach, call
the tool with action=find.")

(nle:define-cell reachmap
  (:section ("reachmap")
    (:guide "nothing is required: the defaults map the usual DevTools ports (9222-9226, 9229, 9230, 9333), the usual local-service ports and the CLIs on PATH; name cdp_ports or extra_ports to look elsewhere, list tools in auth_probes to check their sign-in; then /reachmap shows the map")
    ("cdp_ports" :list
                 :doc "DevTools ports to probe on 127.0.0.1; empty is the usual eight")
    ("extra_ports" :list
                   :doc "more local ports to check, listed as a service with no name")
    ("scanners" :list :choices #'scanner-choices
               :doc "any of browser, mcp, service, cli, drive; empty runs all five")
    ("auth_probes" :list :choices #'cli-choices
                   :doc "tools whose sign-in to verify by running them; empty probes none")
    ("hide_identities" :boolean :default nil
                        :doc "leave the addresses out of the map; by default a browser session says who it is signed in as")
    ("cache_seconds" :integer :default 90 :min 0
                     :doc "how long one scan is reused before the next turn scans again")
    ("timeout_seconds" :integer :default 2 :min 1
                       :doc "the ceiling on one probe: a port, a DevTools endpoint, a sign-in check"))
  (:settings #'read-section)
  (:tool "reachmap" (nlk:contract 'reachmap-call) #'call-tool)
  (:command "reachmap" #'run-command
            :description "What this machine can reach right now"
            :argument-hint "[show|find <thing>|check <text>|rescan]")
  (:help :reachmap
         "reachmap: what this machine can reach right now - the reachmap tool, /reachmap, (reachmap:report)"
         +primer+)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-reachmap"))
  (:start #'start))
