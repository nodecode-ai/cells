;;;; cell-test.lisp --- fusion's lifecycle, its detection and its validator.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every harness config the cell reads is one the test writes into a scratch
;;;; folder of its own, and the environment is stubbed, so nothing here touches
;;;; this machine's harnesses and nothing dials out.

(in-package #:nodecode.test)

(define-test-slice "fusion" "FUSION-" :start nodecode-fusion:start-cell)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "fusion"
  (:command "fusion")
  (:help :fusion)
  (:refused ("templates_file" "/nonexistent/fusion-templates.json")))

;;; --- the scratch home and the tool calls --------------------------------------

(defun fusion-dir (name) "A scratch directory of this test's own, made."
  (let ((dir (merge-pathnames (format nil "fusion-tests/~a/" name)
                               (user-homedir-pathname))))
    (ensure-directories-exist (merge-pathnames "x" dir))
    dir))

(defmacro with-fusion-dir ((var name) &body body)
  `(let ((,var (fusion-dir ,name)))
     (nlk:with-cleanup ((uiop:delete-directory-tree (pathname ,var) :validate t
                                                                  :if-does-not-exist :ignore))
       ,@body)))

(defun fusion-write (dir name text)
  (let ((path (merge-pathnames name dir)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string text out))
    path))

(defun fusion-tool (name &rest pairs) "NAME's tool as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun fusion-teams-call (&rest pairs)
  (apply #'fusion-tool "fusion_teams" pairs))

(defun fusion-validate-call (text)
  (fusion-tool "fusion_validate" "packet" text))

(defparameter *codex-config* "model = \"gpt-5.6\"
model_provider = \"openai\"

[model_providers.moonshot]
model = \"kimi-k2-fast\" " "A Codex config: a top-level model, and one provider block.")

(defparameter *hermes-config* "model:
  provider: anthropic
  default: claude-opus-4-8
  deepseek:
    model: deepseek-chat
  fallback_providers:
    - provider: xai
      model: grok-4.6
tools:
  enabled: true " "A Hermes config: a default pairing, a provider block, and the fallback list.")

(defparameter *claude-settings* "{\"model\": \"claude-haiku-4-5\"}" "A Claude settings file naming one model.")

;;; --- detection ---------------------------------------------------------------

(deftest fusion-reads-every-harness-and-builds-the-rosters ()
  (with-fusion-dir (dir "every-harness")
    (fusion-write dir "config.toml" *codex-config*)
    (fusion-write dir "config.yaml" *hermes-config*)
    (fusion-write dir "settings.json" *claude-settings*)
    (with-cell-stop ((fusion-start "hermes_home" (namestring dir) "codex_home" (namestring dir) "claude_home" (namestring dir)))
      (let ((teams (fusion-teams-call)))
        (let ((seen (format nil "it read:~%~a" teams)))
          (is (search "[hermes] anthropic:claude-opus-4-8 (frontier)" teams) seen)
          (is (search "[hermes] deepseek:deepseek-chat (cheap)" teams) seen)
          (is (search "[hermes] xai:grok-4.6 (frontier)" teams) seen)
          (is (search "[codex] openai:gpt-5.6 (frontier)" teams) seen)
          (is (search "[codex] moonshot:kimi-k2-fast (cheap)" teams) seen)
          (is (search "[claude] anthropic:claude-haiku-4-5 (cheap)" teams) seen)
          (is (search "MM:       claude-haiku-4-5 via anthropic" teams) seen)
          (is (search "worker-1=gpt-5.6@openai" teams) seen)
          (is (search "worker-2=claude-opus-4-8@anthropic" teams) seen)
          (is (search "consults: consult" teams) seen))
      (let ((json (fusion-teams-call "template" "technical")))
        (is (search "roster technical:" json) "one roster can be asked for as JSON")
        (is (search "\"task_type\"" json) "with its task type")
        (is (search "claude-haiku-4-5" json)
            (format nil "and its middle manager; it answered ~a" json)))
      (is (search "unknown template nope" (fusion-teams-call "template" "nope")) "a roster this machine does not have is named, not guessed")))))

(deftest fusion-parses-a-hermes-config ()
  (with-fusion-dir (dir "hermes-only")
    (fusion-write dir "config.yaml" *hermes-config*)
    (let ((entries (nodecode-fusion::detect-hermes-yaml *hermes-config*)))
      (is (= 3 (length entries))
          (format nil "the text reads as three entries; it read ~s" entries)))
    (with-cell-stop ((fusion-start "hermes_home" (namestring dir) "codex_home" (namestring dir) "claude_home" (namestring dir) "count_env_keys" nil))
      (let* ((home (nodecode-fusion::folder (nodecode-fusion::setting-text :hermes-home) "~/.hermes"))
             (entries (nodecode-fusion::detect-hermes)))
        (is (search (namestring dir) (namestring home))
            (format nil "the folder read is the test's: ~s" (namestring home)))
        (is entries (format nil "and the file reads as entries; it read ~s" entries))))))

(deftest fusion-counts-a-provider-key-set-in-the-environment ()
  (with-fusion-dir (dir "env-keys")
    (let ((nodecode-fusion::*getenv*
            (lambda (name) (if (string= name "ANTHROPIC_API_KEY") "set" nil))))
      (with-cell-stop ((fusion-start "hermes_home" (namestring dir) "codex_home" (namestring dir) "claude_home" (namestring dir)))
        (let ((teams (fusion-teams-call)))
          (is (search "[claude] anthropic:claude-opus-4-8 (frontier)" teams) "a key and no config reads as the standard frontier pairing")
          (is (search "[claude] anthropic:claude-haiku-4-5 (cheap)" teams) "and the cheap half of it")
          (is (not (search "[grok]" teams)) "a key it does not hold is not counted"))))))

(deftest fusion-with-no-config-says-so ()
  (with-fusion-dir (dir "empty")
    (with-cell-stop ((fusion-start "hermes_home" (namestring dir) "codex_home" (namestring dir) "claude_home" (namestring dir) "count_env_keys" nil))
      (let ((teams (fusion-teams-call)))
        (is (search "none: no config under the folders" teams) "an empty machine is said plainly, not guessed")
        (is (search "MM:       ? via ?" teams) "and a roster with no models says so here too")))))

(deftest fusion-reads-a-templates-file ()
  (with-fusion-dir (dir "templates")
    (fusion-write dir "templates.json" "{\"team_templates\": {\"tuned\": {\"task_type\": \"technical\", \"middle_manager\": {\"model\": \"my-planner\", \"provider\": \"mine\"}, \"members\": [], \"consults\": []}}}")
    (with-cell-stop ((fusion-start "hermes_home" (namestring dir) "codex_home" (namestring dir) "claude_home" (namestring dir) "templates_file" (namestring (merge-pathnames "templates.json" dir))))
      (let ((json (fusion-teams-call "template" "tuned"))
            (teams (fusion-teams-call)))
        (is (search "my-planner" json)
            (format nil "a roster the templates file pins is answered as is; it answered ~a" json))
        (is (search "tuned  [technical]" teams)
            (format nil "and listed beside the detected ones; it listed ~a" teams))
        (is (search "your templates_file adds" teams) "the listing says where it came from")))))

;;; --- the packet validator ----------------------------------------------------

(defun fusion-packet (&key (workers 1)) "A valid v4 packet, with WORKERS workers."
  (nlk:make-json-object "fusion_version" "4.0" "task_type" "technical" "lead_model" "gpt-5.6" "lead_provider" "openai" "judgment_to_retain" (vector "keep this") "model_selection_rationale" "the frontier model implements" "phases" (vector (nlk:make-json-object "phase" 1 "model" "gpt-5.6" "provider" "openai" "model_reason" "hard" "max_turns" 4 "output" "the patch")) "inputs" #() "deliverables" (vector "a patch") "scope_in" (vector "one file") "scope_out" #() "constraints" (vector "no new deps") "evidence_requirements" (vector "tests run") "verification" (vector "the suite passes") "stop_conditions" (vector "the suite passes twice") "max_retries" 2 "team" (nlk:make-json-object "template" "technical" "middle_manager" (nlk:make-json-object "model" "deepseek-chat" "provider" "deepseek") "members" (let ((members (make-array workers)))
                        (dotimes (index workers members)
                          (setf (aref members index)
                                (nlk:make-json-object "role" (format nil "worker-~d" (1+ index)) "model" "gpt-5.6" "provider" "openai" "delegation" "delegate_task" "thread_policy" "continuous")))))))

(deftest fusion-validates-a-plan-packet ()
  (with-cell-stop ((fusion-start))
    (let ((answer (fusion-validate-call (shasht:write-json (fusion-packet) nil))))
      (is (search "VALID plan: version 4.0" answer)
          (format nil "a good packet is valid; it answered ~a" answer))
      (is (search "middle manager deepseek-chat via deepseek" answer) "and the answer names the pinned middle manager"))
    (let ((broken (fusion-packet)))
      (remhash "lead_model" broken)
      (let ((answer (fusion-validate-call (shasht:write-json broken nil))))
        (is (search "INVALID" answer) "a broken packet is refused")
        (is (search "- missing field: lead_model" answer)
            (format nil "naming the field it lacks; it answered ~a" answer))))
    (let ((answer (fusion-validate-call
                   (shasht:write-json (fusion-packet :workers 5) nil))))
      (is (search "team.members has 5 workers" answer)
          (format nil "a roster wider than four is refused; it answered ~a" answer)))
    (let ((answer (fusion-validate-call
                   (format nil "Here is the plan you asked for:~%~a~%Ship it.~%"
                           (shasht:write-json (fusion-packet) nil)))))
      (is (search "VALID plan" answer) "a packet inside model output is found and read"))
    (is (search "no JSON object found" (fusion-validate-call "no packet here")) "and text with no packet in it says so")
    (let ((packet (fusion-packet)))
      (setf (gethash "max_retries" packet) 9)
      (setf (gethash "task_type" packet) "vibes")
      (let ((answer (fusion-validate-call (shasht:write-json packet nil))))
        (is (search "max_retries must be between 1 and 4" answer) "the retry bound holds")
        (is (search "task_type must be one of" answer) "and the task-type list")))))

(deftest fusion-command-answers-the-listing ()
  (with-cell-stop ((fusion-start))
    (let ((answer (nodecode-fusion:run-fusion "" "s1")))
      (is (search "Fusion rosters" answer) "/fusion answers the listing")
      (is (search "MM:" answer) "with each roster's middle manager"))
    (is (search "roster default:" (nodecode-fusion:run-fusion "default" "s1")) "/fusion TEMPLATE answers one roster's JSON")))

