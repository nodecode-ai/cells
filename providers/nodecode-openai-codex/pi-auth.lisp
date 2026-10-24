;;;; pi-auth.lisp --- native discovery and exact Pi accounts on the Codex wire.
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-openai-codex)

(defvar *pi-discovery* nil "(PATH DEADLINE IDS): short-lived, IDs-only inventory.")
(defvar *pi-known-accounts* nil "Discovered slots retained for fail-closed routing after logout.")
(defvar *pi-model-listing* nil "Never resolve a Pi grant inside delegated listing HTTP.")

(defun pi-explicit-source (provider)
  "An explicit Pi selector, including malformed selectors that must fail closed."
  (let ((entry (nle::configured-provider-entry provider)))
    (when (and entry (not (and (gethash "pi_discovered" entry) (pi-native-owner-p provider))))
      (multiple-value-bind (value present) (gethash "pi_auth" entry)
        (when present
          (or (and (stringp value) (nle::trimmed-config-string entry "pi_auth")) :invalid))))))

(defun pi-store-entry ()
  "The optional cell-level store override; never a credential."
  (let ((path (setting :pi-auth-file)))
    (nlk:json-object :when (and path (plusp (length path))) "pi_auth_file" path)))

(defun pi-helper-entry (provider)
  "Per-provider paths override the cell's optional default path."
  (let ((entry (nle::configured-provider-entry provider)))
    (if (and entry (nth-value 1 (gethash "pi_auth_file" entry))) entry (pi-store-entry))))

(defun pi-accounts ()
  "Discover exact OAuth keys, not grants. Missing Pi never breaks native sign-ins."
  (when (setting :pi-accounts)
    (let* ((entry (pi-store-entry)) (path (gethash "pi_auth_file" entry))
           (identity (list path (uiop:getenv "PI_CODING_AGENT_DIR") (user-homedir-pathname) (uiop:getcwd)))
           (now (get-internal-real-time)) (memo *pi-discovery*))
      (unless (and memo (equal identity (first memo)) (< now (second memo)))
        (let* ((answer (handler-case (call-pi-helper :discover entry t)
                         (nle:credential-error () nil)))
               (ids (remove-duplicates
                     (remove-if-not #'pi-codex-source-p
                                    (coerce (or (nlk:json-value answer :array "accounts") #()) 'list))
                     :test #'equal)))
          (setf *pi-known-accounts* (union ids *pi-known-accounts* :test #'equal)
                *pi-discovery* (list identity (+ now (* 5 internal-time-units-per-second)) ids))))
      (pi-import-accounts (third *pi-discovery*))
      (third *pi-discovery*))))

(defun pi-native-owner-p (provider)
  "An existing native Codex sign-in keeps its provider; Pi never replaces it."
  (and (equal provider +provider+)
       (handler-case (stored-entry (nle::read-auth-file nle::*auth-file-path*))
         (nle:credential-error () nil))))

(defun pi-import-account (id)
  "Conditional metadata insertion under the native config writer's lock."
  (when nle::*shared-config-path*
    (bt2:with-lock-held (nle::*shared-config-write-lock*)
      ;; Recheck the current document inside the same critical section as the
      ;; write. An earlier inventory check cannot authorize overwriting a
      ;; provider another native config operation has since created.
      (setf nle::*shared-config-memo* nil)
      (unless (or (nle::configured-provider-entry id) (pi-native-owner-p id))
        (let* ((file nle::*shared-config-path*)
               (text (and (probe-file file) (uiop:read-file-string file)))
               (updated (nlk:set-jsonc-path
                         (or text "") (list "providers" id)
                         (nlk:json-object "sdk" "openai-responses" "pi_auth" id "pi_discovered" t))))
          ;; The native writer's pure splice and mode-preserving atomic I/O;
          ;; this insert-only operation copies no grants or account metadata.
          (nlk:write-file-atomically file updated :mode :keep)
          (setf nle::*shared-config-memo* nil))))))

(defun pi-import-accounts (ids)
  "Import native provider metadata only. The source binding survives logout/restart."
  (dolist (id ids)
    (unless (or (nle::configured-provider-entry id) (pi-native-owner-p id))
      (handler-case (pi-import-account id) (error () nil)))))

(defun pi-resolve (next provider &rest keys &key (auth-path nle::*auth-file-path*) &allow-other-keys)
  "Ownership uses the caller's actual store, never an unrelated global store."
  (let ((nle::*auth-file-path* auth-path)) (apply next provider keys)))

(defun pi-source (provider)
  "An exact native provider source, including its durable Pi import metadata."
  (or (pi-explicit-source provider)
      (when (and (pi-codex-source-p provider)
                 (null (nle::configured-provider-entry provider))
                 (setting :pi-accounts))
        (pi-accounts)
        (or (pi-explicit-source provider)
            (and (member provider *pi-known-accounts* :test #'equal)
                 (null (nle::configured-provider-entry provider))
                 (not (pi-native-owner-p provider)) provider)))))

(defun pi-provider-ids ()
  "Available automatic providers plus optional explicit Pi provider entries."
  (let ((available (pi-accounts)))
    (loop for (id . entry) in (nle::provider-config-entries)
          when (and (pi-codex-source-p (pi-explicit-source id))
                    (or (not (gethash "pi_discovered" entry)) (member id available :test #'equal)))
            collect id)))

(defun pi-responses-p (provider)
  "Automatic accounts use the cell's lane; explicit entries must name Responses."
  (let ((entry (nle::configured-provider-entry provider)))
    (or (null entry) (equal "openai-responses" (nle::trimmed-config-string entry "sdk")))))

(defun pi-codex-source-p (source)
  (and (stringp source) (cl-ppcre:scan "\\Aopenai-codex(?:-[1-9][0-9]*)?\\z" source) t))

(defun pi-lane (next provider)
  "Pi aliases use this cell's specialization of the Responses transport."
  ;; Even malformed selections stay out of OpenAI-family native OAuth hooks.
  ;; The credential point refuses them before any foreign store is opened.
  (if (or (pi-source provider)
          (nlk:json-value (nle::configured-provider-entry provider) :boolean "pi_discovered"))
      +lane+
      (funcall next provider)))

(defun pi-base (next provider)
  "Pi credentials never follow an arbitrary configured base_url."
  (if (pi-source provider)
      "https://chatgpt.com/backend-api/codex"
      (funcall next provider)))

(defun call-pi-helper (source entry probe)
  "Private subprocess transport. Never include its output in diagnostics."
  (let* ((path (nle::trimmed-config-string entry "pi_auth_file"))
         (request (progn
                    (when (and entry (nth-value 1 (gethash "pi_auth_file" entry)) (null path))
                      (nle::credential-fail "pi_auth_file must be a non-empty path string"))
                    (if (eq source :discover)
                        (nlk:json-object "discover" t :opt "path" path)
                        (nlk:json-object "provider" source "probe" (if probe t :false) :opt "path" path)))))
    (multiple-value-bind (output errors status)
        (handler-case
            (nlk:run-bounded
             (list "node" (namestring (asdf:system-relative-pathname +key+ "pi-auth.mjs")))
             ;; Read-lock wait + refresh-lock wait + refresh/persistence fit here.
             :input (nlk:encode-json-object request) :seconds (if (eq source :discover) 10 120) :error-output nil)
          (error () (nle::credential-fail "Pi auth helper could not run; check Node.js and Pi")))
      (declare (ignore errors))
      (unless (eql status 0)
        (nle::credential-fail "Pi authentication unavailable; check the selected Pi login"))
      (handler-case (nlk:decode-json output)
        (error () (nle::credential-fail "Pi auth helper returned an invalid response"))))))

(defun pi-credential (op source)
  "Read/refresh SOURCE exactly; never borrow another account or the environment."
  (let ((probe (null (getf op :endpoint))))
    (handler-case
        (progn
          (when *pi-model-listing*
            (nle::credential-fail "Pi credentials cannot be used for model-listing HTTP"))
          (unless (pi-explicit-source (getf op :provider))
            (nle::credential-fail "Pi account import could not be saved; make Nodecode config writable"))
          (unless (and (pi-codex-source-p source) (eq (getf op :family) :openai-codex)
                       (pi-responses-p (getf op :provider)))
            (nle::credential-fail "Pi Codex accounts require sdk openai-responses and the Codex cell"))
          (let* ((result (call-pi-helper source (pi-helper-entry (getf op :provider)) probe))
                 (key (if probe "present" (nlk:json-value result :text "key")))
                 (account (unless probe (nlk:json-value result :text "accountId"))))
            (unless (and (equal "oauth" (nlk:json-value result :text "type")) key
                         (or probe (and account
                                        (notany (lambda (ch) (or (< (char-code ch) 33)
                                                                (= (char-code ch) 127))) account))))
              (nle::credential-fail "Pi Codex helper returned an invalid credential"))
            (let ((claimed (unless probe (token-profile key))))
              (when (and claimed (not (equal claimed account)))
                (nle::credential-fail "Pi Codex account identity is inconsistent")))
            (let* ((credential (entry-credential
                                (nlk:json-object "access_token" key :opt "account_id" account) :oauth))
                   (attributes (getf credential :attributes))
                   ;; Stable across refresh; changes when a slot is logged into another account.
                   (scope (if probe source (format nil "~a/~a" source account))))
              (setf (getf attributes :endpoint) "https://chatgpt.com/backend-api/codex/responses"
                    (getf attributes :cache-key) scope
                    (getf attributes :pi-account) scope
                    (getf credential :attributes) attributes)
              credential)))
      (nle:credential-error (condition)
        (if probe (nle:make-credential "public" :public) (error condition))))))

(defun pi-freeze (next &rest args)
  "A model-level SDK override must not move a Pi credential to another wire."
  (let* ((provider (or (first args) nle::*provider*))
         (source (pi-source provider)))
    (when (and source
               (not (and (pi-codex-source-p source)
                         (pi-responses-p provider))))
      (nle::credential-fail "Pi Codex selection must name an exact account on openai-responses")))
  (let ((config (apply next args)))
    (when (and (nle::credential-attribute config :pi-account)
               (not (member (nle::effective-provider-config-lane config)
                            (list +lane+ "openai-responses") :test #'equal)))
      (nle::credential-fail "Pi Codex accounts require the Responses wire"))
    config))

(defun pi-inventory (next &rest args)
  "Expose Pi subscriptions through the native inventory; never copy credentials."
  (multiple-value-bind (ids models) (apply next args)
    (dolist (id (pi-provider-ids)) (pushnew id ids :test #'string-equal))
    (dolist (pair (nle::provider-config-entries))
      (destructuring-bind (id . entry) pair
        (when (pi-codex-source-p (pi-explicit-source id))
          (loop for row across (or (nlk:json-value entry :array "models") #())
                for model = (nle::trimmed-config-string row "id")
                when model
                  do (push (list :provider-id id
                                 :provider-display (or (nle::trimmed-config-string entry "display_name") id)
                                 :model-id model :context-window (nle::config-context-window row)) models)))))
    (values (sort ids #'string<) models)))

(defun pi-list-models (next provider &rest keys)
  "Serve the cell's normal Codex model metadata, without credential-bearing HTTP."
  (if (pi-source provider)
      (values (loop for row across +models+
                    for id = (nlk:json-value row :text "id")
                    unless (member id +image-models+ :test #'equal)
                      collect (list :id id :display (nlk:json-value row :text "name")
                                    :context-window (nlk:json-value row :integer "context") :kind "chat"))
              nil)
      (let ((*pi-model-listing* t)) (apply next provider keys))))
