;;;; cell.lisp --- the cell: one lane, the request path under it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The claude-code provider rides a lane of its own name whose fold is the
;;;; Anthropic lane's (CALL-ANTHROPIC-STREAMING): Nodecode builds the round's
;;;; Messages body as for any Anthropic model. Five hooks make that body the
;;;; CLI's request and keep the rest of the organism honest about it:
;;;;
;;;;   WALK-PROVIDER-STREAM            a claude-code round hands its body to the
;;;;                                   CLI (cli.lisp) and sends back what it
;;;;                                   wrote, folding the stream with the tool
;;;;                                   names turned back (wire.lisp)
;;;;   :CREDENTIAL                     the provider's credential is the CLI's
;;;;                                   login, which this image never holds
;;;;   CONFIGURED-PROVIDER-INVENTORY   /models lists claude-code/<model>
;;;;   LISTABLE-PROVIDER-IDS           and asks for its listing, which
;;;;   LIST-PROVIDER-MODELS            is Anthropic's own /v1/models, asked
;;;;                                   with the headers the CLI's login
;;;;                                   sends: a model the day it ships
;;;;   RESOLVE-MODEL-CAPABILITY        a model's window, output ceiling and
;;;;                                   effort ladder are Anthropic's own row,
;;;;                                   a model no catalog has yet its
;;;;                                   listing's window
;;;;   MODEL-PRICE                     Anthropic's API list price: a
;;;;                                   subscription round is not billed per
;;;;                                   token, so its cost is what the round
;;;;                                   would have cost on the API
;;;;
;;;; Config, a sibling top-level key next to `websearch' and `cron':
;;;;   "claude-code": {"command": "claude",
;;;;                   "models": ["claude-haiku-4-5"],
;;;;                   "timeout_seconds": 120,
;;;;                   "reuse_seconds": 600}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-claude-code)

(defun claude-code-config-p (config)
  "Whether the frozen provider CONFIG rides the claude-code lane."
  (and config (equal (nle::effective-provider-config-lane config) +provider+)))

(defun round-directory ()
  "Where the CLI runs for this round: the session's own directory, so the
environment the CLI describes to the model is the one Nodecode's prompt
names; the home directory outside a turn."
  (let* ((turn (nle:turn))
         (cwd (and turn (nlk:find-session-cwd (getf turn :session-id)))))
    (or (and cwd (ignore-errors (probe-file (uiop:ensure-directory-pathname cwd))))
        (user-homedir-pathname))))

(defun walk (next fold &rest keys &key config request-json &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a claude-code round sends the request the
CLI wrote for its body, and folds the answer with Nodecode's tool names.
A request sent again (AUTHOR-REQUEST) that fails is forgotten, and one the
API refuses for its login is written afresh by the CLI and sent once more."
  (if (claude-code-config-p config)
      (let* ((body (nlk:decode-json request-json))
             (turn (nle:turn))
             (author (list :model (nle::effective-provider-config-model config)
                           :window (nle::effective-provider-config-context-tokens config)
                           :directory (round-directory)
                           :session (and turn (getf turn :session-id))))
             (rest (alexandria:remove-from-plist keys :request-json :headers :endpoint)))
        (flet ((send (headers endpoint octets)
                 (apply next (unprefixed-fold fold)
                        :request-json octets :headers headers :endpoint endpoint rest)))
          (multiple-value-bind (headers endpoint octets reused) (apply #'author-request body author)
            (if (null reused)
                (send headers endpoint octets)
                (block round
                  (handler-bind
                      ((nle::provider-error
                         (lambda (condition)
                           (forget-authored reused)
                           ;; refused before a byte of answer, so nothing was folded
                           (when (member (nle::provider-error-status condition) '(401 403))
                             (return-from round
                               (multiple-value-bind (headers endpoint octets)
                                   (apply #'author-request body :fresh t author)
                                 (send headers endpoint octets)))))))
                    (send headers endpoint octets)))))))
      (apply next fold keys)))

(defun credential (op next)
  "The :CREDENTIAL answer for claude-code: a placeholder key, since the login
is the CLI's and every header the round sends is the CLI's own."
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential +provider+ :oauth)
      (funcall next op)))

(defun model-rows ()
  "The /models rows the claude-code provider offers: the section's models,
each with Anthropic's window for it."
  (loop for id in (coerce (setting :models) 'list)
        collect (list :provider-id +provider+
                      :provider-display "Claude Code"
                      :model-id id
                      :context-window (ignore-errors
                                       (nle::model-capability-context-tokens
                                        (nle::resolve-model-capability id +provider+))))))

(defparameter +listing-round+
  "{\"max_tokens\":1,\"messages\":[{\"role\":\"user\",\"content\":\"models\"}]}"
  "The one-word round the CLI writes so a listing can ride its login: the
relay keeps the request, so it is never sent and spends nothing.")

(defun login-headers ()
  "The headers the CLI's login sends with, read off the request it writes
for +LISTING-ROUND+: the relay keeps it, so nothing leaves through the CLI."
  ;; `sonnet' is the CLI's own alias: the request is never sent, so any model
  ;; the CLI takes will do, and an alias never retires.
  (values (%author-request (nlk:decode-json +listing-round+)
                           :model "sonnet" :directory (uiop:temporary-directory))))

(defvar *listed* (make-hash-table :test #'equal :synchronized t)
  "Model id -> (:EFFORT LEVELS :THINKING TYPES), what the login's last
listing in this image declared of it (capabilities.effort, weakest rung
first, and capabilities.thinking.types), each NIL when it declared none.")

(defun declared (entry &rest path)
  "The names under ENTRY's object at PATH whose own `supported' is true, or
NIL when ENTRY declares no such object."
  (let ((object (apply #'nlk:json-value entry :object path)))
    (and (hash-table-p object)
         (loop for name being the hash-keys of object using (hash-value value)
               when (and (hash-table-p value) (nlk:json-value value :boolean "supported"))
                 collect name))))

(defun models-url ()
  "The Models API beside the lane's endpoint, as the core keys a model's
thinking types by it (ANTHROPIC-THINKING-TYPES)."
  (nle::anthropic-models-url (concatenate 'string +upstream+ "/v1/messages")))

(defun note-declared (entry &aux (id (nlk:json-value entry :string "id")))
  "Keep what the listing ENTRY declares of its model's effort and thinking."
  ;; The thinking types go to the core's own table as well, so a round that
  ;; takes thinking off spells it the way the model takes: Sonnet 5.5 refuses
  ;; "disabled", and its first such round was a 400 and a retry.
  (when id
    (let ((effort (sort (declared entry "capabilities" "effort") #'<
                        :key (lambda (level) (or (nle::effort-rank level) most-positive-fixnum))))
          (thinking (declared entry "capabilities" "thinking" "types")))
      (setf (gethash id *listed*) (list :effort effort :thinking thinking))
      (when thinking
        (setf (gethash (cons (models-url) id) nle::*anthropic-thinking-types*) thinking)))))

(defun listing-rows (text rows)
  "ROWS, the kernel's reading of the listing TEXT, each with the window
Anthropic's max_input_tokens gives it, and the section's models the listing
leaves out (an alias, an id it does not name) after them. What each entry
declares of effort and thinking is kept (NOTE-DECLARED)."
  (let ((windows (make-hash-table :test #'equal)))
    (loop for entry across (or (nlk:json-value (nlk:decode-json text) :array "data") #())
          for id = (nlk:json-value entry :string "id")
          for window = (nlk:json-value entry :integer "max_input_tokens")
          do (note-declared entry)
          when (and id window (plusp window)) do (setf (gethash id windows) window))
    (append (mapcar (lambda (row &aux (row (copy-list row)))
                      (unless (getf row :context-window)
                        (setf (getf row :context-window) (gethash (getf row :id) windows)))
                      row)
                    rows)
            (loop for id in (coerce (setting :models) 'list)
                  unless (find id rows :key (lambda (row) (getf row :id)) :test #'string=)
                    collect (list :id id)))))

(defun login-listing ()
  "(values ROWS ERROR), LIST-PROVIDER-MODELS for claude-code: the models the
CLI's login may call, Anthropic's /v1/models asked with the headers the CLI
sends (LOGIN-HEADERS), or NIL and why it could not be asked."
  ;; The listing is the login's own word, so a model Anthropic ships today is
  ;; offered today, and one the plan does not carry is not.
  (unless (cli-program (setting :command))
    (return-from login-listing
      (values nil (format nil "no ~a on PATH" (setting :command)))))
  (multiple-value-bind (body status)
      (handler-case (nle::http-fetch (concatenate 'string +upstream+ "/v1/models?limit=1000")
                                     :headers (login-headers)
                                     :timeout nle::*provider-models-fetch-timeout-seconds*)
        (nle::provider-error (condition) (values nil (nle::provider-error-detail condition)))
        (error (condition) (values nil (princ-to-string condition))))
    (if (stringp body)
        (multiple-value-bind (rows error) (nle::parse-provider-listing body)
          (if error (values nil error) (values (listing-rows body rows) nil)))
        (values nil (if (stringp status) status "empty listing")))))

(defun listing (next provider &rest arguments)
  "LIST-PROVIDER-MODELS advice: claude-code's listing is its login's."
  (if (equal provider +provider+)
      (login-listing)
      (apply next provider arguments)))

(defun thinking-types (next url config)
  "FETCH-ANTHROPIC-THINKING-TYPES advice: a claude-code model's thinking
types are what the login's own listing declares of it."
  ;; The core asks GET /v1/models/<id> with the lane's key, which for
  ;; claude-code is a placeholder Anthropic refuses: it then sent "disabled"
  ;; and learned the model's spelling from the 400. A listing kept from this
  ;; image answers at once; else one is asked. A model the listing does not
  ;; name (an alias) declares nothing, as before; a listing that fails is no
  ;; answer, and the next round asks again.
  (if (claude-code-config-p config)
      (let ((model (nle::effective-provider-config-model config)))
        (multiple-value-bind (kept present) (gethash model *listed*)
          (cond (present (values (getf kept :thinking) t))
                ((nth-value 1 (login-listing)) (values nil nil))
                (t (values (getf (gethash model *listed*) :thinking) t)))))
      (funcall next url config)))

(defun listable (next &rest arguments)
  "LISTABLE-PROVIDER-IDS advice: claude-code among the providers whose
listing /models fetches; one that cannot be asked says why in its row."
  (sort (adjoin +provider+ (apply next arguments) :test #'string-equal) #'string<))

(defun inventory (next &rest arguments)
  "CONFIGURED-PROVIDER-INVENTORY advice: claude-code among the credentialed
providers, its models among the configured ones."
  (multiple-value-bind (ids models) (apply next arguments)
    (values (sort (adjoin +provider+ ids :test #'string-equal) #'string<)
            (append models (model-rows)))))

(defun capability (next model-id &optional (provider nle::*provider*))
  "RESOLVE-MODEL-CAPABILITY advice: a claude-code model is the Anthropic
row of the same id; one no source knows as Anthropic's yet is what the
core makes of it under claude-code, its window the login's listing's."
  ;; The day a model ships, models.dev has no row for it, and the core takes
  ;; a window from the provider's own cached listing (LISTED-CONTEXT-WINDOW)
  ;; -- claude-code's, not anthropic's, which only an API key fetches.
  ;; Its effort ladder, too, is the listing's when no catalog names one.
  (if (equal provider +provider+)
      (let ((row (funcall next model-id "anthropic")))
        (if (nle::model-capability-known-p row)
            row
            (with-listed-effort (funcall next model-id provider) model-id)))
      (funcall next model-id provider)))

(defun with-listed-effort (row model-id &aux (levels (getf (gethash model-id *listed*) :effort)))
  "ROW, or when it names no effort ladder and the login's listing declared
one for MODEL-ID, ROW with that ladder."
  (if (and levels (null (nle::model-capability-effort-levels row)))
      (nle::make-model-capability
       :context-tokens (nle::model-capability-context-tokens row)
       :context-tokens-operator-p (nle::model-capability-context-tokens-operator-p row)
       :output-limit (nle::model-capability-output-limit row)
       :reasoning-p t
       :effort-levels levels
       :known-p (nle::model-capability-known-p row))
      row))

(defun price (next model-id provider)
  "MODEL-PRICE advice: a claude-code model is priced as the Anthropic row of
the same id, its API list price -- what the round would have cost on the
API, as openai-codex prices its subscription models."
  (funcall next model-id (if (equal provider +provider+) "anthropic" provider)))

(defun register-lane ()
  "The claude-code lane: the Anthropic fold under this provider's name."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'nle::call-anthropic-streaming
                            :family :anthropic
                            :reasoning-carry :text
                            :default-endpoint (concatenate 'string +upstream+ "/v1/messages")
                            :path "/messages")))

(defun unregister-lane ()
  "Take the claude-code lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))

(defun start ()
  "Register the lane, and say so when there is no CLI to drive."
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (unless (cli-program (setting :command))
    (nle:notice (format nil "claude-code: no ~a on PATH, so claude-code/ models cannot run; install Claude Code, run `claude' once and /login, then (restart-cells \"nodecode-claude-code\")"
                        (setting :command))
                :level :warning :key "nodecode-claude-code")
    (nle:on-stop (lambda () (nle:notice nil :key "nodecode-claude-code")))))

(nle:define-cell claude-code
  (:section ("claude-code")
    (:guide "command is the claude CLI, logged in with `claude' then /login; /models lists what that login may call as claude-code/<model>")
    ("command" :string :default "claude"
     :doc "the Claude Code CLI: a name on PATH or a path")
    ("models" :list :default (vector)
     :doc "Anthropic model ids /models offers under claude-code/ beside the ones the login lists: an alias, or an id its listing leaves out")
    ("timeout_seconds" :integer :default 120 :min 1
     :doc "how long the CLI may take to write one round's request, its history replay included")
    ("reuse_seconds" :integer :default 600 :min 0
     :doc "how long a request the CLI wrote is sent again around a later round's own messages when nothing else of the round changed; 0 starts the CLI every round"))
  (:start #'start)
  (:hook :credential #'credential)
  (:hook 'nle::walk-provider-stream #'walk)
  (:hook 'nle::configured-provider-inventory #'inventory)
  (:hook 'nle::listable-provider-ids #'listable)
  (:hook 'nle::list-provider-models #'listing)
  (:hook 'nle::fetch-anthropic-thinking-types #'thinking-types)
  (:hook 'nle::resolve-model-capability #'capability)
  (:hook 'nle::model-price #'price))
