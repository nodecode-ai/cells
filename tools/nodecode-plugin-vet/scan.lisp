;;;; scan.lisp --- the vet engine: a target in, a five-dimension risk card out.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Read-only by construction: local targets are READ, remote ones are two or
;;;; three GETs (api.github.com, raw.githubusercontent.com, registry.npmjs.org),
;;;; and nothing is executed. Nine checks score five dimensions -- license,
;;;; source, dependencies, build scripts, maintenance -- and every finding names
;;;; the audit skill section that continues the review by hand.
;;;;
;;;; A published plugin speaks through three shapes, and so does this: a GitHub
;;;; "owner/repo[@ref]", an "npm:name@version", or a directory on this machine.

(in-package #:nodecode-plugin-vet)

;;; --- text and JSON helpers -------------------------------------------------------

(defparameter +user-agent+ "nodecode-plugin-vet/0.1.0 (+https://github.com/nodecode-ai/cells)")

(defun nonblank (value)
  "VALUE when it is a string with something in it, else NIL."
  (let ((value (and (stringp value) (nlk:trimmed value))))
    (and value (plusp (length value)) value)))

(defun clip (text limit)
  "TEXT cut at LIMIT characters, marked when it was cut."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a..." (subseq text 0 limit)))))

(defun commas (strings)
  (format nil "~{~a~^, ~}" strings))

(defun jget (object type &rest path)
  (apply #'nlk:json-value object type path))

(defun hash-items (object)
  "OBJECT's (KEY . VALUE) pairs, or NIL when it is not an object."
  (when (hash-table-p object)
    (loop for key being the hash-keys of object using (hash-value value)
          collect (cons key value))))

(defun decode (text)
  (handler-case (nlk:decode-json text)
    (error () nil)))

(defun regex (pattern text)
  (ppcre:scan (ppcre:create-scanner pattern :case-insensitive-mode t) text))

(defun matches (pattern text)
  (ppcre:all-matches-as-strings (ppcre:create-scanner pattern :case-insensitive-mode t) text))

(defun split-ci (pattern text)
  (ppcre:split (ppcre:create-scanner pattern :case-insensitive-mode t) text))

;;; --- what a target is ------------------------------------------------------------

(defun line-of (text position)
  "1-based line number of POSITION in TEXT."
  (1+ (loop for i from 0 below (min position (length text))
            count (char= (char text i) #\Newline))))

;;; --- the wire -------------------------------------------------------------------

(defun github-token ()
  (let ((name (nonblank (setting :token-env))))
    (and name (nonblank (uiop:getenv name)))))

(defun request-headers ()
  (let ((token (github-token)))
    (append (list (cons "accept" "application/vnd.github+json")
                  (cons "user-agent" +user-agent+))
            (when token (list (cons "authorization" (format nil "Bearer ~a" token)))))))

(defun http-text (url)
  "URL's body. Signals PLUGIN-VET-ERROR when the peer does not answer 2xx."
  (multiple-value-bind (body status)
      (nlk:http :get url :headers (request-headers)
                :timeout (setting :timeout-seconds) :max-bytes (* 4 1024 1024))
    (if (and (integerp status) (<= 200 status 299))
        (or body "")
        (fail "~a answered HTTP ~a" url status))))

(defun http-json (url)
  (let ((value (decode (http-text url))))
    (or (and (hash-table-p value) value)
        (fail "~a answered no JSON object" url))))

;;; --- reading a local directory ---------------------------------------------------

(defparameter +text-extensions+
  '("json" "jsonc" "md" "txt" "yml" "yaml" "toml" "js" "mjs" "cjs" "ts" "mts" "cts"
    "jsx" "tsx" "sh" "bash" "zsh" "ps1" "psm1" "py" "rb" "pl" "lua" "html" "css" "patch" "diff")
  "The file kinds every check reads.")

(defparameter +skip-directories+
  '(".git" "node_modules" ".venv" "venv" "dist" "build" "target" "vendor" ".next" ".cache" "__pycache__")
  "Directories that hold other people's code, not this package's own.")

(defun read-text-file (path limit)
  (handler-case
      (with-open-file (in path :external-format :utf-8 :element-type 'character)
        (let* ((size (file-length in))
               (want (min size limit))
               (buffer (make-string want)))
          (subseq buffer 0 (read-sequence buffer in))))
    (error () nil)))

(defun interesting-file-p (name)
  (or (member (pathname-type (pathname name)) +text-extensions+ :test #'string-equal)
      (let ((lower (string-downcase name)))
        (some (lambda (prefix) (eql 0 (search prefix lower)))
              '("license" "copying" "notice" "readme")))))

(defun collect-files (root &key (max-files 400) (max-bytes 262144))
  "The readable text files under ROOT, as (:path \"relative\" :text \"...\"), capped."
  (let ((files '()) (count 0))
    (labels ((walk (directory)
               (dolist (file (sort (uiop:directory-files directory) #'string< :key #'namestring))
                 (when (< count max-files)
                   (when (interesting-file-p (file-namestring file))
                     (let ((text (read-text-file file max-bytes)))
                       (when text
                         (push (list :path (enough-namestring file root) :text text) files)
                         (incf count))))))
               (dolist (sub (sort (uiop:subdirectories directory) #'string< :key #'namestring))
                 (unless (member (string-downcase (car (last (pathname-directory sub))))
                                 +skip-directories+ :test #'equal)
                   (walk sub)))))
      (walk root))
    (nreverse files)))

(defun file-text (files path)
  (let ((entry (find path files :test #'equal :key (lambda (file) (getf file :path)))))
    (and entry (getf entry :text))))

(defun manifest-of (files)
  "The root package.json, decoded, or NIL."
  (let ((text (file-text files "package.json")))
    (and text (let ((value (decode text))) (and (hash-table-p value) value)))))

;;; --- resolving a target ----------------------------------------------------------

(defun split-github (raw)
  "OWNER, REPO and REF out of \"owner/repo[@ref]\", or NIL."
  (let* ((at (position #\@ raw))
         (id (if at (subseq raw 0 at) raw))
         (ref (if at (subseq raw (1+ at)) ""))
         (slash (position #\/ id)))
    (when (and slash (plusp slash))
      (values (subseq id 0 slash) (subseq id (1+ slash)) (or (nonblank ref) "")))))

(defun split-npm (raw)
  "NAME and VERSION out of \"npm:name@version\" (a scope keeps its @)."
  (let* ((id (subseq raw 4))
         (at (position #\@ id :start (if (and (plusp (length id)) (char= (char id 0) #\@)) 1 0))))
    (if at
        (values (subseq id 0 at) (subseq id (1+ at)))
        (values id "latest"))))

(defun directory-target-p (raw)
  (and (stringp raw) (plusp (length raw))
       (let ((found (ignore-errors (probe-file (pathname raw)))))
         (and found (uiop:directory-pathname-p found)))))

(defun resolve-target (raw)
  "TARGET as a plist: its kind, what it resolves to, its files and metadata."
  (cond
    ((null (nonblank raw)) (fail "name a target: a GitHub owner/repo, an npm:name@version, or a directory"))
    ((and (>= (length raw) 4) (string-equal "npm:" (subseq raw 0 4)))
     (multiple-value-bind (name version) (split-npm raw)
       (let* ((meta (http-json (format nil "https://registry.npmjs.org/~a/~a" name version)))
              (manifest (jget meta :object "versions" (jget meta :string "dist-tags" "latest") "package")))
         (declare (ignore manifest))
         (list :kind :npm :raw raw :resolved (format nil "~a@~a" name version)
               :ref "" :files '() :manifest nil :npm meta))))
    ((directory-target-p raw)
     (let* ((root (uiop:ensure-directory-pathname (truename (pathname raw))))
            (files (collect-files root :max-files (setting :max-files)
                                       :max-bytes (setting :max-file-bytes))))
       (list :kind :local :raw raw :resolved (namestring root) :ref ""
             :files files :manifest (manifest-of files) :npm nil)))
    (t
     (multiple-value-bind (owner repo ref) (split-github raw)
       (unless (and owner repo)
         (fail "~a is not owner/repo, npm:name@version or a directory" raw))
       (let* ((meta (http-json (format nil "https://api.github.com/repos/~a/~a" owner repo)))
              (branch (or (nonblank ref) (nonblank (jget meta :string "default_branch")) "HEAD"))
              (manifest-text (ignore-errors
                               (http-text (format nil "https://raw.githubusercontent.com/~a/~a/~a/package.json"
                                                  owner repo branch))))
              (manifest (and manifest-text (let ((value (decode manifest-text)))
                                              (and (hash-table-p value) value))))
              (license-text (loop for name in '("LICENSE" "LICENSE.md" "LICENSE.txt" "COPYING")
                                   for text = (ignore-errors
                                               (http-text (format nil "https://raw.githubusercontent.com/~a/~a/~a/~a"
                                                                  owner repo branch name)))
                                   when (nonblank text)
                                     return (list :path name :text text))))
         (list :kind :github :raw raw :resolved (format nil "~a/~a" owner repo) :ref ref
               :files (if license-text (list license-text) '())
               :manifest manifest :npm nil :github meta))))))

;;; --- one check, its findings, its score ------------------------------------------

(defun finding (level message &key location skill evidence)
  (list :level level :message message :location location :skill skill :evidence evidence))

(defun make-check (id name score findings skill &key skip)
  (list :id id :name name
        :score (max 0 (min 100 score))
        :verdict (cond (skip :skip)
                       ((find :fail findings :key (lambda (item) (getf item :level))) :fail)
                       ((find :warn findings :key (lambda (item) (getf item :level))) :warn)
                       (t :pass))
        :findings findings :skill skill :skip (or skip "")))

(defun skip-check (id name reason skill)
  (make-check id name 60 '() skill :skip reason))

;;; --- license ---------------------------------------------------------------------

(defparameter +known-spdx+
  '("MIT" "APACHE-2.0" "BSD-2-CLAUSE" "BSD-3-CLAUSE" "ISC" "MPL-2.0" "LGPL-2.1" "LGPL-3.0"
    "GPL-2.0" "GPL-3.0" "AGPL-3.0" "UNLICENSE" "CC0-1.0" "0BSD" "BSL-1.0" "ARTISTIC-2.0")
  "The SPDX ids common enough to check without a dependency.")

(defun manifest-license (manifest)
  (let ((value (jget manifest :any "license")))
    (cond ((stringp value) (nonblank value))
          ((hash-table-p value) (nonblank (jget value :string "type")))
          (t nil))))

(defun license-check (target)
  (let* ((manifest (getf target :manifest))
         (npm (getf target :npm))
         (declared (or (manifest-license manifest) (nonblank (jget npm :string "license"))))
         (license-file (find-if (lambda (file) (regex "^((un)?license|copying)" (getf file :path)))
                                (getf target :files)))
         (base (and declared (first (split-ci "\\s+(OR|AND|WITH)\\s+" declared))))
         (findings '())
         (score 100))
    (cond
      ((null declared)
       (if license-file
           (setf score 70)
           (setf score 40))
       (push (finding (if license-file :warn :fail)
                      (if license-file
                          "a licence file is present but the package declares no licence"
                          "no licence declared and no licence file found")
                      :location "package.json")
             findings))
      ((find (string-upcase base)
             '("UNLICENSED" "NOASSERTION" "UNKNOWN" "NONE" "SEE LICENSE IN LICENSE") :test #'string=)
       (setf score 30)
       (push (finding :fail (format nil "licence ~s is not a usable declaration" (clip declared 40))
                      :location "package.json")
             findings))
      ((find (string-upcase base) +known-spdx+ :test #'string=)
       (when (regex "^(GPL|AGPL|SSPL)" base)
         (push (finding :info (format nil "~a is copyleft: confirm the terms fit this use" base)
                        :location "package.json")
               findings)))
      (t
       (setf score 70)
       (push (finding :warn (format nil "licence ~s is not a common SPDX id" (clip declared 40))
                      :location "package.json")
             findings)))
    (make-check "license" "License scan" score (nreverse findings) "dependency-audit §3")))

;;; --- dependencies ----------------------------------------------------------------

(defun dependency-specs (manifest &optional (key "dependencies"))
  (hash-items (jget manifest :object key)))

(defun unpinned-spec-p (spec)
  "SPEC floats: a range, a tag, a local path, or nothing at all."
  (or (null (nonblank spec))
      (regex "^[\\^~><*xX]|^(latest|workspace:|file:|link:|npm:|git\\+|https?://)" spec)))

(defun sbom-check (target)
  (let ((manifest (getf target :manifest)))
    (if (null manifest)
        (skip-check "sbom" "SBOM dependency tree" "no package.json to read dependencies from" "dependency-audit §7")
        (let* ((direct (dependency-specs manifest "dependencies"))
               (dev (dependency-specs manifest "devDependencies"))
               (findings '())
               (score 100)
               (unpinned (remove-if-not (lambda (pair) (unpinned-spec-p (cdr pair))) direct)))
          (when unpinned
            (setf score 70)
            (push (finding :warn
                           (format nil "~d direct dependencies float (~a): the installed version is not fixed"
                                   (length unpinned)
                                   (commas (mapcar #'car (subseq unpinned 0 (min 4 (length unpinned))))))
                           :location "package.json")
                  findings))
          (unless (or (file-text (getf target :files) "package-lock.json")
                      (file-text (getf target :files) "pnpm-lock.yaml")
                      (file-text (getf target :files) "yarn.lock"))
            (when (eq (getf target :kind) :local)
              (setf score (- score 15))
              (push (finding :warn "no lockfile: transitive versions are whatever install resolves"
                             :location "package.json")
                    findings)))
          (when (> (+ (length direct) (length dev)) 40)
            (push (finding :info (format nil "~d dependencies in total: ~d direct, ~d dev"
                                          (+ (length direct) (length dev)) (length direct) (length dev))
                           :location "package.json")
                  findings))
          (make-check "sbom" "SBOM dependency tree" score (nreverse findings) "dependency-audit §7")))))

;;; --- commit pinning --------------------------------------------------------------

(defparameter +pin-regex+ "#([0-9A-Za-z._-]+)")

(defun commit-lock-check (target)
  (let ((manifest (getf target :manifest))
        (findings '())
        (score 100))
    (dolist (key '("dependencies" "devDependencies" "optionalDependencies"))
      (dolist (pair (dependency-specs manifest key))
        (let ((spec (cdr pair)))
          (when (and (stringp spec) (regex "^(git\\+|github:|https?://)" spec))
            (let* ((pin (first (matches +pin-regex+ spec))))
              (unless (and pin (regex "^#[0-9a-f]{40}$" pin))
                (setf score (- score 25))
                (push (finding :warn
                               (format nil "dependency ~a follows ~a, not a 40-character commit"
                                       (car pair) (or pin "a branch"))
                               :location "package.json")
                      findings)))))))
    (let ((ref (getf target :ref)))
      (when (and (nonblank ref) (not (regex "^[0-9a-f]{40}$" ref)))
        (setf score (- score 20))
        (push (finding :warn (format nil "the target ref ~a is not a 40-character commit" ref))
              findings)))
    (dolist (file (getf target :files))
      (when (regex "^\\.github/workflows/.+\\.ya?ml$" (getf file :path))
        (dolist (use (matches "uses:[ \t]*[^ \t@]+@[^ \t#\"']+" (getf file :text)))
          (unless (regex "@[0-9a-f]{40}$" use)
            (setf score (- score 10))
            (push (finding :warn (format nil "workflow action ~a is a tag, not a commit"
                                         (clip (subseq use (min (length use) 6)) 40))
                           :location (getf file :path))
                  findings)))))
    (make-check "commit-lock" "Commit lock verification" score (nreverse findings) "supply-chain-review §3")))

;;; --- install scripts -------------------------------------------------------------

(defparameter +lifecycle-scripts+ '("preinstall" "install" "postinstall" "prepare"))

(defparameter +download+ "(curl|wget|iwr\\b|Invoke-WebRequest|Invoke-RestMethod|bitsadmin|certutil)")
(defparameter +exec+ "(eval\\b|\\bexec\\b|execSync|\\bsh\\b|\\bbash\\b|\\bnode\\b|powershell|\\bpython3?\\b|\\bperl\\b|\\bruby\\b|\\bcmd\\b|chmod|Invoke-Expression|\\biex\\b|spawn\\s*\\(|spawnSync|execFile|child_process)")
(defparameter +encoded+ "(base64\\s+(-d|--decode)\\b|FromBase64String|atob\\(|Buffer\\.from\\([^)]*base64)")
(defparameter +cred-touch+ "(\\.ssh\\b|id_rsa|id_ed25519|\\.npmrc|\\.gitconfig|\\.aws\\b|credentials|known_hosts)")
(defparameter +global-write+ "(/etc/profile|\\.zshrc|\\.bashrc|\\.bash_profile|\\.profile\\b|%APPDATA%|HKCU\\\\Software|HKEY_CURRENT_USER)")
(defparameter +build-tools+ '("esbuild" "sharp" "core-js" "node-gyp" "puppeteer" "cypress" "playwright" "electron" "swc" "@swc/core"))

(defun scripts-of (manifest)
  (jget manifest :object "scripts"))

(defun install-scripts-check (target)
  (let* ((manifest (getf target :manifest))
         (scripts (or (scripts-of manifest) (jget (getf target :npm) :object "scripts")))
         (findings '())
         (score 100)
         (checked 0)
         (name (or (nonblank (jget manifest :string "name")) (getf target :resolved))))
    (dolist (hook +lifecycle-scripts+)
      (let ((body (nonblank (jget scripts :string hook))))
        (when body
          (incf checked)
          (let* ((download (regex +download+ body))
                 (exec (regex +exec+ body))
                 (encoded (regex +encoded+ body))
                 (cred (regex +cred-touch+ body))
                 (global (regex +global-write+ body))
                 (plain-http (regex "http://(?!localhost|127\\.0\\.0\\.1)" body))
                 (level nil) (reason nil))
            (cond
              ((and encoded exec)
               (setf level :fail reason "an obfuscated payload, decoded and then run"))
              ((or cred global)
               (setf level :fail reason "touches credentials or a global shell profile"))
              ((and download exec)
               (setf level (if (member (string-downcase name) +build-tools+ :test #'string=) :warn :fail)
                     reason "downloads executable content and runs it"))
              ((and download (regex "(-o\\s+|--output\\s+|-O\\s+|OutFile\\s+)" body))
               (setf level :warn reason "downloads a file"))
              (plain-http
               (setf level :warn reason "fetches over plain http"))
              (exec
               (setf level :info reason "runs a command")))
            (when level
              (when (member level '(:fail :warn))
                (setf score (- score (if (eq level :fail) 40 15))))
              (push (finding level (format nil "scripts.~a: ~a" hook reason)
                             :location "package.json" :evidence (clip body 140))
                    findings))))))
    (when (zerop checked)
      (push (finding :info "no preinstall/install/postinstall/prepare lifecycle scripts"
                     :location "package.json")
            findings))
    (make-check "install-scripts" "Install script checks" score (nreverse findings) "supply-chain-review §1")))

;;; --- network exfiltration --------------------------------------------------------

(defparameter +exfil-domains+
  '("webhook.site" "requestbin.net" "requestcatcher.com" "beeceptor.com" "ngrok.io" "ngrok-free.app"
    "localhost.run" "oast.fun" "oastify.com" "oast.pro" "interact.sh" "burpcollaborator.net"
    "pipedream.net" "pastebin.com" "transfer.sh" "file.io" "0x0.st" "rentry.co" "canarytokens.com"
    "discord.com/api/webhooks" "api.telegram.org/bot")
  "Hosts that receive data more often than they serve a package.")

(defun exfil-host-p (host)
  (some (lambda (domain) (or (string= host domain)
                             (let ((suffix (concatenate 'string "." domain)))
                               (and (> (length host) (length suffix))
                                    (string= suffix (subseq host (- (length host) (length suffix))))))))
        +exfil-domains+))

(defun url-host (url)
  (let* ((start (search "//" url))
         (rest (if start (subseq url (+ start 2)) ""))
         (end (or (position-if (lambda (char) (find char "/:?#")) rest) (length rest))))
    (string-downcase (subseq rest 0 end))))

(defun url-hits (files limit)
  "URLs in script and manifest files, as (HOST . LOCATION)."
  (let ((hits '()))
    (dolist (file files)
      (when (and (< (length hits) limit)
                 (regex "\\.(js|mjs|cjs|ts|mts|cts|jsx|tsx|sh|bash|ps1|psm1|py|rb|pl|lua|json|ya?ml|toml)$"
                        (getf file :path)))
        (let ((text (getf file :text)))
          (dolist (url (matches "https?://[^ \t\"'`<>)\]]+" (or text "")))
            (when (< (length hits) limit)
              (let ((host (url-host url)))
                (unless (zerop (length host))
                  (push (list host (format nil "~a:~d" (getf file :path) (line-of text (search url text)))
                              (regex "\\.(js|mjs|cjs|ts|mts|cts|sh|bash|ps1|py|rb)$" (getf file :path)))
                        hits))))))))
    (nreverse hits)))

(defun network-exfil-check (target)
  (let* ((files (getf target :files))
         (hits (url-hits files 240))
         (findings '())
         (score 100)
         (seen '()))
    (unless files
      (return-from network-exfil-check
        (skip-check "network-exfil" "Network exfil detection" "no files to read on this target" "dependency-audit §4.4")))
    (dolist (hit hits)
      (destructuring-bind (host location script) hit
        (unless (member host seen :test #'string=)
          (let ((level nil) (reason nil))
            (cond
              ((exfil-host-p host)
               (setf level (if script :fail :warn) reason "an exfil/receiver host"))
              ((regex "^[0-9]{1,3}(\\.[0-9]{1,3}){3}$" host)
               (setf level (if script :warn :info) reason "a raw IP address"))
              ((regex "\\.(tk|ml|ga|cf|gq|top|xyz|icu|rest|quest)$" host)
               (setf level (if script :warn :info) reason "a free TLD")))
            (when level
              (push host seen)
              (when (member level '(:fail :warn))
                (setf score (- score (if (eq level :fail) 40 15))))
              (push (finding level (format nil "~a: ~a" reason host) :location location)
                    findings))))))
    (make-check "network-exfil" "Network exfil detection" score (nreverse findings) "dependency-audit §4.4")))

;;; --- obfuscation -----------------------------------------------------------------

(defun obfuscation-check (target)
  (let* ((files (getf target :files))
         (findings '())
         (score 100))
    (unless files
      (return-from obfuscation-check
        (skip-check "obfuscation" "Obfuscation scan" "no files to read on this target" "supply-chain-review §1")))
    (dolist (file files)
      (when (regex "\\.(js|mjs|cjs|ts|mts|cts|sh|bash|ps1|psm1|py|rb)$" (getf file :path))
        (let* ((text (getf file :text))
               (blob (find-if (lambda (run) (>= (length run) 256))
                              (matches "[A-Za-z0-9+/]{256,}={0,2}" text))))
          (when (and blob (regex "(atob\\(|FromBase64String|base64\\s+-d|Buffer\\.from\\([^)]*base64)" text))
            (setf score (- score 40))
            (push (finding :fail "a long base64 blob beside a decoder: an encoded payload"
                           :location (getf file :path) :evidence (clip blob 80))
                  findings))
          (when (and (null blob) (> (length text) 20000) (regex "eval\\(" text))
            (setf score (- score 20))
            (push (finding :warn "a large script that evaluates generated code"
                           :location (getf file :path))
                  findings)))))
    (make-check "obfuscation" "Obfuscation scan" score (nreverse findings) "supply-chain-review §1")))

;;; --- where it comes from ---------------------------------------------------------

(defun source-check (target)
  (let ((github (getf target :github))
        (npm (getf target :npm)))
    (cond
      (github
       (let ((findings '())
             (score 100)
             (stars (or (jget github :integer "stargazers_count") 0))
             (created (or (nonblank (jget github :string "created_at")) "")))
         (when (jget github :boolean "archived")
           (setf score 20)
           (push (finding :fail "the repository is archived: it no longer gets fixes")
                 findings))
         (when (jget github :boolean "fork")
           (setf score (- score 25))
           (push (finding :warn "the repository is a fork") findings))
         (when (zerop stars)
           (push (finding :info "no stars yet: it has not been read by strangers") findings))
         (when (nonblank created)
           (push (finding :info (format nil "created ~a" (subseq created 0 (min 10 (length created)))))
                 findings))
         (make-check "source" "Source trust signals" score (nreverse findings) "dependency-audit §4.3")))
      (npm
       (let* ((findings '())
              (maintainers (jget npm :array "maintainers"))
              (count (if (vectorp maintainers) (length maintainers) 0)))
         (when (jget npm :text "deprecated")
           (push (finding :warn (format nil "deprecated: ~a" (clip (jget npm :text "deprecated") 100)))
                 findings))
         (push (finding :info (format nil "~d maintainers, registry version ~a"
                                      count (or (jget npm :string "version") "?")))
               findings)
         (make-check "source" "Source trust signals" (if findings 100 100) (nreverse findings) "dependency-audit §4.3")))
      (t (skip-check "source" "Source trust signals" "a local directory carries no upstream metadata" "dependency-audit §4.3")))))

;;; --- is anybody home -------------------------------------------------------------

(defun days-from-civil (year month day)
  "Days since 1970-01-01 for a civil date (Howard Hinnant's algorithm)."
  (let* ((year (if (<= month 2) (1- year) year))
         (era (floor year 400))
         (yoe (- year (* era 400)))
         (doy (+ (floor (* 153 (+ month (if (> month 2) -3 9))) 5) (1- day)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (- (+ (* era 146097) doe) 719468)))

(defun date-days (stamp)
  "STAMP's date as a day number, or NIL when it is not an ISO date."
  (when (and (stringp stamp) (>= (length stamp) 10))
    (let ((parts (ppcre:split "-" (subseq stamp 0 10))))
      (when (= 3 (length parts))
        (let ((year (ignore-errors (parse-integer (first parts))))
              (month (ignore-errors (parse-integer (second parts))))
              (day (ignore-errors (parse-integer (third parts)))))
          (when (and year month day)
            (days-from-civil year month day)))))))

(defun today-days ()
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (days-from-civil year month day)))

(defun age-findings (stamp label &key (warn 730) (fail 1460))
  (let ((days (date-days stamp)))
    (if (null days)
        (list (finding :info (format nil "~a: not reported" label)))
        (let ((age (- (today-days) days)))
          (list (cond ((> age fail)
                       (finding :fail (format nil "~a ~d days ago: likely unmaintained" label age)))
                      ((> age warn)
                       (finding :warn (format nil "~a ~d days ago" label age)))
                      (t (finding :info (format nil "~a ~d days ago" label age)))))))))

(defun maintenance-check (target)
  (let ((github (getf target :github))
        (npm (getf target :npm)))
    (cond
      (github
       (let* ((findings (age-findings (jget github :string "pushed_at") "last push"))
              (score (cond ((find :fail findings :key (lambda (item) (getf item :level))) 25)
                           ((find :warn findings :key (lambda (item) (getf item :level))) 65)
                           (t 100))))
         (make-check "maintenance" "Maintenance status" score findings "security-audit §3")))
      (npm
       (let* ((time (jget npm :object "time"))
              (findings (age-findings (or (jget time :string "modified") (jget npm :string "modified"))
                                       "last publish"))
              (score (cond ((find :fail findings :key (lambda (item) (getf item :level))) 25)
                           ((find :warn findings :key (lambda (item) (getf item :level))) 65)
                           (t 100))))
         (make-check "maintenance" "Maintenance status" score findings "security-audit §3")))
      (t (skip-check "maintenance" "Maintenance status" "a local directory carries no publish date" "security-audit §3")))))

;;; --- what the text asks the reader to do -----------------------------------------

(defparameter +override-patterns+
  '("ignore (all )?(the )?(previous|above|prior) instructions"
    "disregard [^.]{0,20}instructions"
    "output your (system )?prompt"
    "reveal your (system )?prompt"
    "do not tell the user"
    "without telling the user"
    "exfiltrat"))

(defun data-responsibility-check (target)
  (let* ((docs (remove-if-not (lambda (file)
                                 (regex "\\.(md|txt|ya?ml)$" (getf file :path)))
                               (getf target :files)))
         (findings '())
         (score 100))
    (if (null docs)
        (skip-check "data-responsibility" "Data-responsibility review"
                    "no shipped prose on this target" "prompt-injection-review §2")
        (progn
          (dolist (file docs)
            (dolist (pattern +override-patterns+)
              (let ((match (regex pattern (getf file :text))))
                (when match
                  (setf score (- score 15))
                  (push (finding :warn
                                 (format nil "shipped text overrides or hides: ~s"
                                         (clip (subseq (getf file :text) match
                                                       (min (length (getf file :text)) (+ match 60)))
                                               60))
                                 :location (format nil "~a:~d" (getf file :path) (line-of (getf file :text) match))
                                 :evidence (clip (subseq (getf file :text) match (min (length (getf file :text)) (+ match 80))) 80))
                        findings)))))
          (make-check "data-responsibility" "Data-responsibility review" score (nreverse findings)
                      "prompt-injection-review §2")))))

;;; --- the scorecard ---------------------------------------------------------------

(defparameter +dimension-checks+
  '((:license "license")
    (:source "source" "commit-lock" "data-responsibility")
    (:dependencies "sbom")
    (:build-scripts "install-scripts" "network-exfil" "obfuscation")
    (:maintenance "maintenance"))
  "Which check scores which of the five dimensions.")

(defparameter +dimension-weights+
  '((:license . 0.25) (:source . 0.2) (:dependencies . 0.15) (:build-scripts . 0.25) (:maintenance . 0.15)))

(defparameter +source-weights+
  '(("source" . 0.5) ("commit-lock" . 0.3) ("data-responsibility" . 0.2)))

(defparameter +build-weights+
  '(("install-scripts" . 0.4) ("network-exfil" . 0.35) ("obfuscation" . 0.25)))

(defun check-weight (dimension id)
  (cond ((eq dimension :source) (or (cdr (assoc id +source-weights+ :test #'string=)) 0))
        ((eq dimension :build-scripts) (or (cdr (assoc id +build-weights+ :test #'string=)) 0))
        (t 1)))

(defun dimension-score (dimension checks)
  (let ((run (remove-if (lambda (check) (eq :skip (getf check :verdict)))
                        (remove-if-not (lambda (check)
                                         (member (getf check :id) (rest (assoc dimension +dimension-checks+))
                                                 :test #'string=))
                                       checks))))
    (if (null run)
        60
        (let* ((weighted (loop for check in run
                                sum (* (getf check :score) (check-weight dimension (getf check :id)))))
               (divisor (if (member dimension '(:source :build-scripts)) 1 (length run))))
          (round weighted divisor)))))

(defun scorecard (checks)
  (let* ((dims (loop for (dimension . ids) in +dimension-checks+
                     collect (cons dimension (dimension-score dimension checks))))
         (overall (round (loop for (dimension . nil) in +dimension-checks+
                               sum (* (cdr (assoc dimension dims))
                                      (or (cdr (assoc dimension +dimension-weights+)) 0))))))
    (cons (cons :overall overall) dims)))

(defun report-verdict (checks)
  (cond ((every (lambda (check) (eq :skip (getf check :verdict))) checks) :skip)
        ((find :fail checks :key (lambda (check) (getf check :verdict))) :fail)
        ((find :warn checks :key (lambda (check) (getf check :verdict))) :warn)
        (t :pass)))

;;; --- one scan --------------------------------------------------------------------

(defparameter +check-order+ '("license" "sbom" "commit-lock" "install-scripts" "network-exfil"
                              "obfuscation" "source" "maintenance" "data-responsibility"))

(defun run-checks (target &optional requested)
  "Every check over TARGET, narrowed to REQUESTED ids when that is non-empty."
  (let ((checks
          (list (license-check target)
                (sbom-check target)
                (commit-lock-check target)
                (install-scripts-check target)
                (network-exfil-check target)
                (obfuscation-check target)
                (source-check target)
                (maintenance-check target)
                (data-responsibility-check target))))
    (if requested
        (remove-if-not (lambda (check) (member (getf check :id) requested :test #'string=)) checks)
        checks)))

(defun all-skipped (reason)
  (mapcar (lambda (id) (skip-check id id reason "")) +check-order+))

(defun scan (raw &key requested)
  "RAW scanned: (values TARGET CHECKS). A target that cannot be read leaves
every check skipped, the way an offline run reports itself."
  (handler-case
      (let ((target (resolve-target raw)))
        (values target (run-checks target requested)))
    (plugin-vet-error (condition)
      (values (list :raw raw :resolved raw :kind nil :files '() :manifest nil :ref "")
              (all-skipped (plugin-vet-error-detail condition))))))

;;; --- the card the model reads ----------------------------------------------------

(defun verdict-mark (verdict)
  (cond ((eq verdict :fail) "🔴") ((eq verdict :warn) "🟡") ((eq verdict :pass) "🟢") (t "⚪")))

(defun verdict-name (verdict)
  (string-upcase (or (and verdict (string verdict)) "skip")))

(defun check-line (check)
  (format nil "- ~a [~a] ~a - deep-dive skill: `~a`~@[~%~a~]"
          (verdict-mark (getf check :verdict))
          (verdict-name (getf check :verdict))
          (getf check :name)
          (getf check :skill)
          (format nil "~{~a~^~%~}"
                  (if (eq :skip (getf check :verdict))
                      (list (format nil "  - the check was skipped: ~a" (getf check :skip)))
                      (loop for finding in (subseq (getf check :findings)
                                                   0 (min 6 (length (getf check :findings))))
                            collect (format nil "  - ~a ~a~@[ `~a`~]~@[ - evidence: `~a`~]"
                                            (verdict-mark (getf finding :level))
                                            (getf finding :message)
                                            (getf finding :location)
                                            (getf finding :evidence)))))))

(defun scores-line (scores)
  (format nil "license ~d/100 · source ~d/100 · dependencies ~d/100 · build scripts ~d/100 · maintenance ~d/100 · overall ~d/100"
          (cdr (assoc :license scores)) (cdr (assoc :source scores)) (cdr (assoc :dependencies scores))
          (cdr (assoc :build-scripts scores)) (cdr (assoc :maintenance scores))
          (cdr (assoc :overall scores))))

(defun render-report (target checks &key policy)
  (let* ((scores (scorecard checks))
         (verdict (report-verdict checks))
         (lines
           (list (format nil "## plugin_vet ~a" (getf target :raw))
                 ""
                 (format nil "~a **Verdict: ~a** - ~a" (verdict-mark verdict) (verdict-name verdict) (scores-line scores))
                 ""
                 (format nil "Target: ~a~@[ @~a~] (~(~a~))" (getf target :resolved)
                         (nonblank (getf target :ref)) (getf target :kind)))))
    (dolist (check checks)
      (push (check-line check) lines))
    (push "" lines)
    (push (format nil "**Next**: `supply-chain-review` for install scripts and pins, `dependency-audit` for licences and dependencies, `prompt-injection-review` for shipped prose.")
          lines)
    (push "" lines)
    (push (if (eq (string-downcase (or policy "inherit")) "deny")
              (format nil "Gate: deny - a FAIL blocks the install; a WARN asks a person to look.")
              (format nil "Gate: warn - report only; a person decides."))
          lines)
    (format nil "~{~a~^~%~}" (nreverse lines))))

;;; --- the verb -------------------------------------------------------------------

(define-verb vet (raw &key requested policy)
  "Scan RAW -- owner/repo[@ref], npm:name@version, or a directory -- and answer
the five-dimension risk card as text. Read-only: nothing in the target is
written or run."
  (multiple-value-bind (target checks) (scan raw :requested requested)
    (render-report target checks :policy policy)))
