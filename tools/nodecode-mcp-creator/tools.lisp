;;;; tools.lisp --- the nine tools, the original's names and arguments.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Each answers the JSON string the model reads; a refusal signals
;;;; MCP-CREATOR-ERROR, which the engine renders as "ERROR: <detail>". The
;;;; names are the original plugin's, so a prompt, a skill or a habit that
;;;; says mcp_creator_scaffold keeps working.

(in-package #:nodecode-mcp-creator)

;;; --- declarations: one schema the provider is shown, one record the body reads ---

(nlk:define-contract servers-arguments
  (:doc "List the MCP servers this profile configures, what launches each one, and the builds already made from them. Start here.")
  ("server" :string :doc "one server's entry in full; leave it out for every one"))

(nlk:define-contract introspect-arguments
  (:doc "Connect once to an MCP server, read its self-description -- server info, instructions, tools with their schemas, prompts and resources -- and save it as the baseline drift compares against. Read-only: lists only, never calls a tool.")
  ("server" :text :required t :doc "the server's name as the mcp section lists it")
  ("config" :object :doc "an inline entry instead, in the mcp.servers shape: {command,args,env,cwd} or {url,headers}")
  ("timeout" :number :doc "seconds for each exchange (default: the section's timeout_seconds)")
  ("max_desc" :integer :doc "characters kept per tool description (default: the section's max_desc)"))

(nlk:define-contract mine-arguments
  (:doc "Mine what the model has actually done with this server: per-tool call and error counts, error samples, and which tool a call tended to follow. The main source of pitfalls for the skill.")
  ("server" :text :doc "keep only tools whose name carries this server's name")
  ("since_days" :number :doc "only calls from the last N days")
  ("redact_paths" :boolean :doc "also strip absolute paths (for a publish build)"))

(nlk:define-contract convert-arguments
  (:doc "Turn one mcp.servers entry into a portable mcp.json entry: a literal secret becomes ${ITS_NAME}, and what a publish build must fix (an unpinned launcher, a profile-only key, an absolute path in an argument) is reported.")
  ("server" :text :required t :doc "the server's name")
  ("config" :object :doc "an inline entry instead of a configured one"))

(nlk:define-contract scaffold-arguments
  (:doc "Write the knowledge cell for a server: nodecode-<name>.asd, skill.lisp, SKILL.md, mcp.json and manifest.json, every one derived from its last introspection snapshot. Refuses to touch a folder that is not this cell's own build.")
  ("plugin_name" :text :required t :doc "the package name: lowercase letters, digits, dash, e.g. 'blender-studio'")
  ("server" :text :required t :doc "the server to build from; it must have been introspected first")
  ("description" :string :doc "one paragraph: what it does, what it needs")
  ("skill_md" :string :doc "replaces the generated prose when you have written a better one")
  ("build" :string :options '("local" "publish") :doc "publish refuses a package carrying private detail"))

(nlk:define-contract validate-arguments
  (:doc "Check a built package before installing it: the manifest, the system name, the skill, the fingerprint the manifest claims, and whether a publish build still carries private detail.")
  ("root" :text :required t :doc "the package folder"))

(nlk:define-contract install-arguments
  (:doc "Copy a built package into this home's cells directory, so the next Nodecode boot loads it. Refuses a folder that is not this cell's own build unless replace is set.")
  ("root" :text :required t :doc "the package folder")
  ("replace" :boolean :slot overwrite :doc "take a folder of the same name that this cell did not make"))

(nlk:define-contract drift-arguments
  (:doc "Compare a server's current catalog with the snapshot the build was written against: the tools added, removed or changed, and a verdict -- unchanged, additive (update the skill's tool table) or recompile (rewrite the sections that name the changed tools).")
  ("server" :text :required t :doc "the server's name")
  ("config" :object :doc "an inline entry instead of a configured one"))

(nlk:define-contract review-arguments
  (:doc "Re-examine a build: what changed on the server since it was written (drift) and what the call log says about its tools since then, with refresh_suggested and the reasons. With no plugin, lists what has been built.")
  ("plugin" :string :doc "the package name; leave it out to list the builds")
  ("probe" :boolean :doc "connect to the server for drift (default true)"))

;;; --- servers ------------------------------------------------------------------

(defun build-folders ()
  "Every build this cell made: its manifest, read as a plist."
  (let ((root (builds-root)))
    (when (uiop:directory-exists-p root)
      (loop for folder in (uiop:subdirectories root)
            for manifest = (merge-pathnames "manifest.json" folder)
            when (file-exists-p manifest)
              collect (list :name (car (last (pathname-directory folder)))
                            :manifest (handler-case (decode (read-text manifest)) (error () nil))
                            :root (namestring folder))))))

(defun builds-json ()
  (coerce (loop for build in (build-folders)
                for manifest = (getf build :manifest)
                collect (object "name" (getf build :name)
                                "server" (and manifest (member-text manifest "server"))
                                "built" (and manifest (member-text manifest "built"))
                                "fingerprint" (and manifest (member-text manifest "fingerprint"))
                                "build" (and manifest (member-text manifest "build"))
                                "root" (getf build :root)))
          'vector))

(defun run-servers (arguments)
  (declare (ignore arguments))
  (json-string
   (object "servers" (coerce (loop for name in (server-names)
                                    collect (server-json name (server-entry name)))
                              'vector)
           "builds" (builds-json)
           "data_dir" (namestring (uiop:ensure-directory-pathname *data-dir*))
           "install_dir" (namestring (uiop:ensure-directory-pathname (install-root))))))

;;; --- introspect ---------------------------------------------------------------

(defun inline-config (config)
  (and (hash-table-p config) config))

(defun run-introspect (arguments)
  (let* ((name (princ-to-string (introspect-arguments-server arguments)))
         (timeout (introspect-arguments-timeout arguments))
         (server (server-plist name (inline-config (introspect-arguments-config arguments))))
         (snapshot nil))
    (when timeout (setf (getf server :timeout) timeout))
    (setf snapshot (snapshot-server
                    server
                    :max-desc (or (introspect-arguments-max-desc arguments)
                                  (ignore-errors (setting :max-desc)) 220)))
    (let ((path (save-snapshot snapshot)))
      (json-string
       (object "server" name
               "transport" (getf snapshot :transport)
               "protocol" (getf snapshot :protocol)
               "server_info" (getf snapshot :server-info)
               "instructions" (getf snapshot :instructions)
               "fingerprint" (getf snapshot :fingerprint)
               "tools" (coerce (map 'list #'tool-json (getf snapshot :tools)) 'vector)
               "tool_count" (length (getf snapshot :tools))
               "prompts" (length (getf snapshot :prompts))
               "resources" (length (getf snapshot :resources))
               "snapshot_path" (namestring path))))))

;;; --- mine ---------------------------------------------------------------------

(defun run-mine (arguments)
  (let* ((days (mine-arguments-since-days arguments))
         (since (and days (numberp days) (plusp days)
                     (- (now) (round (* days 86400)))))
         (server (mine-arguments-server arguments)))
    (json-string (usage-report :server (and server (plusp (length server)) server)
                               :since since
                               :paths (and (mine-arguments-redact-paths arguments) t)))))

;;; --- convert ------------------------------------------------------------------

(defun run-convert (arguments)
  (json-string (convert-server (princ-to-string (convert-arguments-server arguments))
                               (inline-config (convert-arguments-config arguments)))))

;;; --- scaffold -----------------------------------------------------------------

(defun run-scaffold (arguments)
  (json-string
   (scaffold (princ-to-string (scaffold-arguments-plugin-name arguments))
             (princ-to-string (scaffold-arguments-server arguments))
             :description (or (scaffold-arguments-description arguments) "")
             :build (or (scaffold-arguments-build arguments) "local")
             :skill-md (scaffold-arguments-skill-md arguments))))

;;; --- validate -----------------------------------------------------------------

(defun run-validate (arguments)
  (multiple-value-bind (problems warnings)
      (package-problems (princ-to-string (validate-arguments-root arguments)))
    (json-string (object "root" (princ-to-string (validate-arguments-root arguments))
                         "ok" (null problems)
                         "problems" (coerce problems 'vector)
                         "warnings" (coerce warnings 'vector)))))

;;; --- install ------------------------------------------------------------------

(defun run-install (arguments)
  (json-string (install-package (princ-to-string (install-arguments-root arguments))
                                 :replace (and (install-arguments-overwrite arguments) t))))

;;; --- drift and review -----------------------------------------------------------

(defun run-drift (arguments)
  (json-string (drift-report (princ-to-string (drift-arguments-server arguments))
                              :config (inline-config (drift-arguments-config arguments)))))

(defun run-review (arguments)
  (let ((plugin (review-arguments-plugin arguments)))
    (if (or (null plugin) (zerop (length plugin)))
        (json-string (object "builds" (builds-json)))
        (json-string (review-build plugin :probe (if (review-arguments-probe arguments) t t))))))
