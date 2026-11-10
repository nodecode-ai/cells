;;;; cell.lisp --- the inventory: "do I already have X?" answered, not guessed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An agent that has to re-derive what it already has wastes a turn doing it,
;;;; and one that guesses gets it wrong. This cell answers the question from a
;;;; digest of the host that somebody else already wrote: the JSON file the
;;;; inventory-reminder scanners leave behind (~/.hermes/inventory.json by
;;;; default), holding the host's repositories and which of them are cloned
;;;; here, the skills installed, the MCP servers, the CLIs on PATH, the model
;;;; files on disk and the Python packages in the harness's venv.
;;;;
;;;; Read-only, and no rescan per turn: the file is read when a tool is called
;;;; and nothing here writes it. What it answers is the same text the original
;;;; plugin answered -- one line per hit, grouped by section, with the detail
;;;; that makes the hit usable (where the clone is, whether the server is
;;;; enabled, the model's size and path) -- and the same one-line digest for a
;;;; session bootstrap. A missing or unreadable digest is a fact it reports,
;;;; never an empty answer guessed at.

(defpackage #:nodecode-inventory-reminder
  (:use #:cl)
  (:nicknames #:inventory-reminder)
  (:export #:lookup #:digest #:digest-path #:*sections* #:inventory-reminder-error))

(in-package #:nodecode-inventory-reminder)

(nlk:define-peripheral inventory-reminder :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "The inventory is a cached, read-only digest of this host, read through the
nodecode-inventory-reminder cell: Lisp functions in the inventory-reminder:
package, called through eval, each answering a string; a refusal is ERROR:
INVENTORY-REMINDER-ERROR.
  (inventory-reminder:lookup \"aider\")   every section searched for a term
  (inventory-reminder:digest)            the one-line digest for a bootstrap
  (inventory-reminder:digest-path)       which file would be read
The sections are skills, repos_owned, repos_foreign, mcp_servers, cli_tools,
models and key_packages; a term is a case-insensitive substring of an item's
name, and every hit carries the detail line that makes it usable.
No network and no rescan: the file is read when asked. In the config, the
`inventory-reminder' section's inventory_json names the digest to read;
unset, $INVENTORY_JSON is read, then ~/.hermes/inventory.json. The operator
gets the same answers from /inventory-reminder, and the model from the
inventory_where and inventory_summary tools."
  "What (help :inventory-reminder) answers while the cell runs.")

;;; --- the digest file ------------------------------------------------------------

(defparameter +sections+ '("skills" "repos_owned" "repos_foreign" "mcp_servers"
                            "cli_tools" "models" "key_packages")
  "Every section this cell searches, in reading order: what a report groups by.")

(defparameter +section-labels+
  '(("skills" . "SKILLS") ("repos_owned" . "REPOS (owned)")
    ("repos_foreign" . "REPOS (foreign)") ("mcp_servers" . "MCP SERVERS")
    ("cli_tools" . "CLI TOOLS") ("models" . "MODELS")
    ("key_packages" . "KEY PACKAGES"))
  "How each section is titled in a report, as the original plugin titled it.")

(defun setting-value (key default)
  "KEY's value in the running cell's settings, else DEFAULT: a reader that is
asked outside a started cell (/cells offline, a snippet before the boot
finished) answers the default rather than refusing."
  (or (ignore-errors (setting key)) default))

(defun digest-path ()
  "The digest file to read: the section's inventory_json, else $INVENTORY_JSON,
else the plugin's own default under the home directory."
  (let ((configured (setting-value :inventory-json nil))
        (environment (uiop:getenv "INVENTORY_JSON")))
    (cond ((and configured (plusp (length (princ-to-string configured))))
           (uiop:ensure-pathname (princ-to-string configured)))
          ((and environment (plusp (length environment)))
           (uiop:ensure-pathname environment))
          (t (merge-pathnames (make-pathname :directory '(:relative ".hermes")
                                              :name "inventory" :type "json")
                              (user-homedir-pathname))))))

(defun read-json-object (path)
  "PATH as a decoded JSON object, or NIL when it is absent or not an object."
  (when (probe-file path)
    (handler-case
        (let ((value (nlk:decode-json (uiop:read-file-string path) :whole t)))
          (and (hash-table-p value) value))
      (error () nil))))

(defun read-digest ()
  "The digest as a decoded object, and the words for why there is none.
=> (VALUES OBJECT NIL) or (VALUES NIL REASON)."
  (let ((path (digest-path)))
    (if (probe-file path)
        (let ((object (read-json-object path)))
          (if object
              (values object nil)
              (values nil (format nil "~a is not a JSON object" (namestring path)))))
        (values nil (format nil "no inventory digest at ~a" (namestring path))))))

;;; --- reading the digest ---------------------------------------------------------

(defun field (object key)
  "KEY's member of the decoded OBJECT, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun items (value)
  "VALUE as a list: a decoded JSON array is a vector and anything else is none."
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun section-available-p (inventory name)
  "Whether the digest's NAME section was scanned: an unavailable section is a
gap to report, never an empty one."
  (let ((section (field inventory name)))
    (and (hash-table-p section) (field section "available") t)))

(defun contains-p (term text)
  "Whether TEXT holds TERM, whatever the case of either."
  (and (stringp text)
       (not (zerop (length term)))
       (search (string-downcase term) (string-downcase text))))

(defun hit (section item detail)
  (list :section section :item (princ-to-string item) :detail (princ-to-string detail)))

(defun repo-name (repo)
  "REPO's last path segment: "owner/name" answers "name"."
  (let ((slash (position #\/ repo :from-end t)))
    (if slash (subseq repo (1+ slash)) repo)))

(defun skill-hits (inventory term)
  (when (section-available-p inventory "skills")
    (let* ((section (field inventory "skills"))
           (sources (field section "by_source")))
      (loop for name in (items (field section "names"))
            when (contains-p term name)
              collect (hit "skills" name (or (field sources name) "local"))))))

(defun owned-repo-hits (inventory term)
  (when (section-available-p inventory "repos")
    (loop for repo in (items (field (field inventory "repos") "local_repos"))
          when (contains-p term repo)
            collect (hit "repos_owned" repo
                         (format nil "LOCAL — ~a" (repo-name repo))))))

(defun foreign-repo-hits (term)
  "Clones of other people's repositories, from the repos.json the scanner keeps
beside the digest: the one section the digest itself does not carry."
  (let ((data (read-json-object (merge-pathnames "repos.json" (digest-path)))))
    (loop for clone in (items (field data "foreign_clones"))
          for repo = (field clone "repo")
          when (contains-p term repo)
            collect (hit "repos_foreign" repo
                         (format nil "LOCAL — ~a"
                                 (or (first (items (field clone "local_paths"))) "?"))))))

(defun mcp-hits (inventory term)
  (when (section-available-p inventory "tools")
    (loop for server in (items (field (field inventory "tools") "mcp_servers"))
          for name = (field server "name")
          when (contains-p term name)
            collect (let ((command (field server "command")))
                      (hit "mcp_servers" name
                           (format nil "~a → ~a"
                                   (if (field server "enabled") "enabled" "disabled")
                                   (if (and (stringp command) (plusp (length command)))
                                       command "—")))))))

(defun cli-hits (inventory term)
  (when (section-available-p inventory "tools")
    (let ((tools (field (field inventory "tools") "cli_tools")))
      (loop for name being the hash-keys of (if (hash-table-p tools) tools (make-hash-table))
              using (hash-value path)
            when (contains-p term name)
              collect (hit "cli_tools" name
                           (let ((path (princ-to-string path)))
                             (if (plusp (length path)) path "**absent**")))))))

(defun model-hits (inventory term)
  (when (section-available-p inventory "models")
    (loop for model in (items (field (field inventory "models") "models"))
          for name = (field model "name")
          when (contains-p term name)
            collect (hit "models" name
                         (format nil "~a — ~a" (or (field model "size") "?")
                                 (or (field model "path") "?"))))))

(defun package-hits (inventory term)
  (when (section-available-p inventory "apps")
    (let ((packages (field (field inventory "apps") "key_packages")))
      (loop for name being the hash-keys of (if (hash-table-p packages) packages (make-hash-table))
              using (hash-value version)
            when (contains-p term name)
              collect (hit "key_packages" name (or version "installed"))))))

(defun search-inventory (term)
  "Every hit for TERM across the sections in reading order, each a plist of
:section, :item and :detail. A digest that cannot be read refuses in words
naming the file, so a miss is never confused with a missing digest."
  (let ((term (string-trim '(#\Space #\Tab #\Newline) (princ-to-string (or term "")))))
    (when (zerop (length term))
      (fail "a search needs a term: inventory_where wants the thing to look for"))
    (multiple-value-bind (inventory reason) (read-digest)
      (unless inventory (fail "~a" reason))
      (append (skill-hits inventory term)
              (owned-repo-hits inventory term)
              (foreign-repo-hits term)
              (mcp-hits inventory term)
              (cli-hits inventory term)
              (model-hits inventory term)
              (package-hits inventory term)))))

;;; --- the report ----------------------------------------------------------------

(defun format-hits (term hits &key (cap 40))
  "HITS for TERM as the report /inventory-reminder prints: the sections that
hold a hit, in reading order, one line each, and the count when CAP cut it."
  (when (null hits)
    (return-from format-hits
      (format nil "MISS: ~a — not found in any section of ~a" term (namestring (digest-path)))))
  (let ((shown (if (<= (length hits) cap) hits (subseq hits 0 cap))))
    (with-output-to-string (out)
      (format out "~d hit~:p for ~s in ~a~@[ (the first ~d shown)~]"
              (length hits) term (namestring (digest-path))
              (and (> (length hits) cap) cap))
      (dolist (section +sections+)
        (let ((group (remove-if-not (lambda (one) (string= section (getf one :section)))
                                     shown)))
          (when group
            (format out "~%~%  ~a" (cdr (assoc section +section-labels+ :test #'string=)))
            (dolist (one group)
              (format out "~%    ~a    (~a)" (getf one :item) (getf one :detail))))))
      (unless (<= (length hits) cap)
        (format out "~%  ... ~d more; raise max_hits or ask for a narrower term"
                (- (length hits) cap))))))

(defun hits-json (term hits)
  "HITS as the JSON the tool answers with --json: the same shape the original
plugin's --json emitted, so a script that read one reads this."
  (nlk:encode-json-object
   (nlk:make-json-object
    "query" term
    "hits" (coerce (loop for one in hits
                          collect (nlk:make-json-object
                                   "section" (getf one :section)
                                   "item" (getf one :item)
                                   "detail" (getf one :detail)))
                    'vector))))

(defun summary-line ()
  "One line for a session bootstrap: what this host has, section by section,
with UNAVAILABLE spelled out rather than read as zero."
  (multiple-value-bind (inventory reason) (read-digest)
    (unless inventory
      (fail "~a — write one with the inventory-reminder scanners, or point inventory_json at it" reason))
    (let* ((repos (field inventory "repos"))
           (skills (field inventory "skills"))
           (tools (field inventory "tools"))
           (apps (field inventory "apps"))
           (models (field inventory "models"))
           (services (field inventory "services"))
           (stamp (let ((generated (field inventory "generated_at")))
                    (if (and (stringp generated) (>= (length generated) 16))
                        (substitute #\Space #\T (subseq generated 0 16))
                        "?"))))
      (format nil "INVENTORY ~a | repos ~a | skills ~a | mcp ~a | pkgs ~a | models ~a | services ~a"
              stamp
              (if (field repos "available")
                  (format nil "~a (~aL/~aO)" (field repos "owned_total")
                          (field repos "local") (field repos "online_only"))
                  "N/A")
              (if (field skills "available")
                  (format nil "~a~@[ (~a)~]" (field skills "total")
                          (let ((sources (field skills "by_source")))
                            (and (hash-table-p sources)
                                 (format nil "~{~a~^/~}"
                                         (loop for name being the hash-keys of sources
                                                 using (hash-value count)
                                               collect (format nil "~a~a" count
                                                               (subseq name 0 1)))))))
                  "N/A")
              (if (field tools "available")
                  (format nil "~a/~a" (length (items (field tools "mcp_enabled")))
                          (or (field tools "mcp_total") "?"))
                  "N/A")
              (if (field apps "available") (field apps "count") "N/A")
              (if (field models "available")
                  (format nil "~a (~a)" (field models "count") (field models "total_size"))
                  "N/A")
              (if (field services "available")
                  (format nil "~a/~a" (field services "count") (field services "running_total"))
                  "N/A")))))

;;; --- the verbs -------------------------------------------------------------------

(define-verb lookup (term)
  "Search every section of this host's inventory digest for TERM, a
case-insensitive substring of a skill's name, a repository's, an MCP server's,
a CLI's, a model's or a package's: what is there, and the detail that makes
the hit usable (where the clone is, whether the server is enabled, the model's
size and path). Answers MISS in words when nothing matches. => the report."
  (format-hits (string-trim '(#\Space #\Tab #\Newline) (princ-to-string (or term "")))
               (search-inventory (or term ""))
               :cap (setting-value :max-hits 40)))

(define-verb digest ()
  "The one-line digest of this host -- repos, skills, MCP servers, packages,
models and services, and when the digest was generated -- for a session
bootstrap, exactly the line the original plugin's inventory_summary answered.
=> the line."
  (summary-line))

;;; --- the tools --------------------------------------------------------------------

(nlk:define-contract inventory-where-request
  (:doc "Search the cached inventory of this host for something the user asks about before searching elsewhere or building it again: repositories (and which are cloned here), skills, MCP servers, CLIs, local model files and key packages. Read-only, one file, no network. A miss says so; a missing digest says which file it looked for.")
  ("term" :text :required t
          :doc "what to look for, as a case-insensitive substring: a repository, skill, CLI, MCP server, model or package name, or a piece of one")
  ("json" :boolean :default nil
          :doc "answer the machine-readable JSON the original plugin's --json emitted instead of the grouped text"))

(defparameter +summary-schema+
  (list :description
        "One line saying what this host already has -- repositories and how many are cloned, skills, MCP servers, Python packages, local models and services -- with the time the digest was generated. For a session bootstrap, so the agent starts knowing its own cogs instead of re-deriving them."
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:json-object)))
  "The tool that takes no arguments, so its schema is written by hand.")

(defun run-where (request)
  "The model's inventory_where: REQUEST's term searched, as the report."
  (let ((term (inventory-where-request-term request)))
    (if (inventory-where-request-json request)
        (hits-json term (search-inventory term))
        (format-hits term (search-inventory term) :cap (setting-value :max-hits 40)))))

(defun run-summary (arguments)
  "The model's inventory_summary: the digest's one line."
  (declare (ignore arguments))
  (summary-line))

;;; --- /inventory-reminder ------------------------------------------------------------

(defun run-slash (args session-id)
  "/inventory-reminder TERM: every section searched for TERM;
/inventory-reminder alone: the one-line digest of this host. => the text."
  (declare (ignore session-id))
  (let ((term (nlk:trimmed (or args ""))))
    (if (plusp (length term))
        (format-hits term (search-inventory term) :cap (setting-value :max-hits 40))
        (summary-line))))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell inventory-reminder
  (:section ("inventory-reminder")
    (:guide "inventory_json names the digest file the scanners write; nothing else is needed; unset, $INVENTORY_JSON is read, then ~/.hermes/inventory.json")
    ("inventory_json" :path
                      :doc "the digest file to read: a JSON object of repos, skills, tools, apps, models and services; unset, $INVENTORY_JSON is read, then ~/.hermes/inventory.json")
    ("max_hits" :integer :default 40 :min 1
                :doc "how many hits one report carries before it says how many were left out"))
  (:tool "inventory_where" (nlk:contract 'inventory-where-request) 'run-where)
  (:tool "inventory_summary" +summary-schema+ 'run-summary)
  (:help :inventory-reminder
         "inventory-reminder:lookup and inventory_where: do I already have X? from a cached host digest"
         +primer+)
  (:command "inventory-reminder" 'run-slash
            :catches 'inventory-reminder-error
            :argument-hint "TERM"
            :description "Do I already have X? Search this host's cached inventory"))
