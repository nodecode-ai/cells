;;;; cell.lisp --- the verbs, /import, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The verbs are plain functions in the NIK package, every one answering
;;;; a string, every refusal an IMPORT-ERROR the eval snippet renders as text:
;;;; (nik:worlds) lists what is on the box, (nik:scan) is the dry run's
;;;; report, (nik:import ...) applies. The slash is `/import [WORLD]
;;;; [--source PATH] [--only KINDS] [--overwrite] [--keep-running] [--yes]':
;;;; a preview unless --yes, the report posted as a notice, the summary as
;;;; the composer hint. Keys and tokens come with everything else; a
;;;; foreign gateway that polls bots is stopped and disabled so they answer
;;;; from here, unless --keep-running. Naming no world reads every world
;;;; the box carries and merges them.
;;;;
;;;; The setup wizard's import stage speaks to the same entrypoint by two
;;;; methods, `plan' and `apply', and reads the plan back as JSON; `scan'
;;;; is the third, the redacted machine-readable report that is worth
;;;; pasting into a bug about a home this build read wrong; `verify' asks
;;;; whether the target an import selected actually answers.
;;;;
;;;; Config, a sibling top-level key next to `cron' and `memory':
;;;;   "import": {"enabled": true, "session_budget": 200}

(in-package #:nodecode-import-kit)

;;; --- the verbs ----------------------------------------------------------------------------

(defun run-import (&key source overwrite (takeover t) only without apply)
  "The plan for SOURCE, applied when APPLY — and then the watch started
for any bot it held for a running foreign gateway. => the plan."
  (let ((plan (make-plan :source source :overwrite overwrite :takeover takeover
                         :only only :without without)))
    (cond (apply
           (apply-plan plan)
           (funcall *start-watch*))
          (t (write-report plan)))
    plan))

(defun worlds (&aux (found (nlk:detect-agent-worlds)))
  "Every coding-agent home on this box, most recently used first, as one
line each: the world, where it sits, and when it was last used."
  ;; A stat probe — nothing is opened.
  (if (null found)
      "no coding-agent home found on this box"
      (with-output-to-string (out)
        (dolist (row found)
          (format out "~a~30t~a~@[~60t~a~]~%"
                  (nlk:agent-world-name row.world)
                  (namestring row.root)
                  (iso-from-universal row.last-used))))))

(defun scan (&key source only)
  "What an import of SOURCE would do, as the report; nothing is written."
  ;; SOURCE names a world or a path; naming none reads every world on the box.
  (report-text (run-import :source source :only only)))

(defun import (&key source overwrite (takeover t) only)
  "Import SOURCE, keys and tokens with it: OVERWRITE replaces what is
already here, ONLY names the kinds (providers, instructions, memory,
skills, mcp, channels, cron, sessions, profiles) comma-separated."
  ;; A foreign gateway polling bots is stopped and disabled so they answer
  ;; from here; TAKEOVER nil leaves it running, and its bots land off and come
  ;; on by themselves once it stops. Answers the report.
  (report-text (run-import :source source :overwrite overwrite :takeover takeover
                           :only only :apply t)))

;;; --- does the imported target answer ---------------------------------------------------
;;; A key an import carried over is a key that worked once, somewhere else.
;;; It may be revoked, out of credit or bound to an address that has since
;;; moved, and an operator told "imported" whose first message then fails
;;; has been told something that was not true. So before the walk says the
;;; provider is there, one side call asks it: the smallest completion the
;;; target will run, on the default target alone (no auxiliary override,
;;; no fallback), under a deadline. A refusal is reported in the provider's
;;; own words, with the key the check sent taken out of them — a provider
;;; sometimes quotes the key it refused.

(defvar *verify-target* 'verify-target
  "What checks that this organism's default target answers; a test stubs it.")

(defun target-secrets ()
  "The key this organism's default target sends, as a list, or NIL."
  ;; Frozen as the check's request freezes it, so a !command runs and its
  ;; output is the key: probed, the chain answered the command's text, and a
  ;; refusal quoting the key it was sent went out whole (2026-09-30). A target
  ;; that does not freeze sends nothing; a keyless one's `public' is no secret.
  (let ((key (ignore-errors (nle::effective-provider-config-api-key
                             (nle::snapshot-effective-provider-config)))))
    (and (stringp key) (>= (length key) 8) (list key))))

(defun redact-secrets (text secrets &key (limit 160))
  "TEXT cut to LIMIT with each of SECRETS taken out of it, whole and in part:
every run of eight or more key characters that is a piece of one replaced."
  ;; The values the check sent, never a shape: twelve characters holding a
  ;; letter and a digit took the model id out too, `anthropic/[redacted]'
  ;; (2026-09-30). A piece is out as well: a provider may quote a key's head.
  (flet ((piece-p (run) (some (lambda (secret) (cl:search run secret)) secrets)))
    (one-line-of (ppcre:regex-replace-all
                  "[\\w.~+/=-]{8,}"
                  (reduce (lambda (text secret)
                            (ppcre:regex-replace-all (ppcre:quote-meta-chars secret) text "[redacted]"))
                          secrets :initial-value text)
                  (lambda (run) (if (piece-p run) "[redacted]" run))
                  :simple-calls t)
                 limit)))

(defun verify-target (&aux (secrets (target-secrets)))
  "One tiny completion on this organism's default target."
  ;; => (values OK
  ;; REASON CODE STATUS): T, or NIL, the provider's own sentence (or the
  ;; transport fact, below HTTP) with the key taken out, the provider
  ;; failure's reason label (`connection_refused' when nothing listens at
  ;; the endpoint, which is no verdict on the key), or NIL, and the HTTP
  ;; status it answered, or NIL. The sentence, never the failure's report:
  ;; its `[provider model]' and its remedy were said twice over in the walk's
  ;; closing line (2026-09-30).
  (handler-case
      ;; The deadline covers the whole check, a cold endpoint's first token included.
      (sb-sys:with-deadline (:seconds 30)
        (let ((nle::*auxiliary-model* nil))
          (nle:complete "Answer with the single word ok." "ok" :max-tokens 16 :fallback-p nil))
        (values t nil nil nil))
    (nle::provider-error (condition)
      (values nil
              (redact-secrets (if (nle::provider-error-status condition)
                                  (nle::provider-said (nle::provider-error-detail condition))
                                  (nle::provider-error-detail condition))
                              secrets)
              (nle::provider-failure-reason condition)
              (nle::provider-error-status condition)))
    ((or error sb-sys:deadline-timeout) (condition)
      (values nil (redact-secrets (princ-to-string condition) secrets) nil nil))))

;;; --- /import -------------------------------------------------------------------------------

(defun run-slash (args &aux (words (nlk:split-words args))
                            (head (first words))
                            (source nil) (overwrite nil) (takeover t) (only nil) (yes nil))
  "The answer to one /import: the summary line and where the report file is,
said as a notice too."
  (when (and head (not (uiop:string-prefix-p "--" head)))
    (pop words)
    (unless (nlk:find-agent-world head)
      (fail "~a is not a world this build knows (~{~a~^, ~}); name a path with --source"
            head (nlk:agent-world-names)))
    (setf source head))
  (loop for word = (pop words) while word
        do (alexandria:switch (word :test #'string=)
             ("--source" (setf source (or (pop words) (fail "--source needs a PATH"))))
             ("--only" (setf only (or (pop words) (fail "--only needs KINDS, comma-separated"))))
             ("--overwrite" (setf overwrite t))
             ("--keep-running" (setf takeover nil))
             ("--yes" (setf yes t))
             ("--dry-run" (setf yes nil))
             (t (fail "unknown word ~a; usage /import [WORLD] [--source PATH] [--only KINDS] ~
                       [--overwrite] [--keep-running] [--yes]"
                      word))))
  (let* ((plan (run-import :source source :overwrite overwrite :takeover takeover
                           :only only :apply yes))
         (report (and plan.report-path
                      (format nil "the report is in ~a"
                              (nlk:home-abbreviated (namestring plan.report-path))))))
    ;; The report is a file, and the notice and the hint say where: posted
    ;; whole, it was a toast cut at four rows that left after five seconds,
    ;; and a toast alone takes the path with it (ip-01).
    (nle:notice (if report
                    (format nil "import ~a~:[ (dry run)~;~]: ~a — ~a"
                            (plan-source-line plan) plan.applied (summary-line plan) report)
                    (report-text plan))
                :level :info)
    (if yes
        (format nil "import ~a: ~a~@[ — ~a~]" (plan-source-line plan) (summary-line plan) report)
        (format nil "import ~a (dry run): ~a — /import~@[ ~a~] --yes applies~@[; ~a~]"
                (plan-source-line plan) (summary-line plan) source report))))

(defun param-flag (params key)
  (eq (nlk:json-value params :any key) t))

(defun run-method (method params)
  "The wizard's methods over the params object {source, only, without,
overwrite, keep_running}, source a world name, a path or a list of world
names: `plan' previews, `apply' writes, `scan' answers the same plan with
nothing written — the pasteable report."
  ;; Nothing is posted: the wizard writes the answer's report into the
  ;; transcript of the shell that walks it. Posted, it was a toast cut at four
  ;; rows that left after five seconds (ip-01).
  (plan-json (run-import :source (or (nlk:json-value params :text "source")
                                     (coerce (nlk:json-value params :array "source") 'list))
                         :only (nlk:json-value params :text "only")
                         :without (nlk:json-value params :text "without")
                         :overwrite (param-flag params "overwrite")
                         :takeover (not (param-flag params "keep_running"))
                         :apply (equal method "apply"))))

(defun import-route (env &aux (request (nle::gateway-request-json env))
                               (method (nlk:json-value request :string "method")))
  "/api/import: POST {method, source, only, without, overwrite, keep_running}
runs the setup walk's METHOD in the image that serves the store -- `plan',
`apply' and `scan' (RUN-METHOD), `verify' (one small request to the default
target) -- and answers its value."
  (cond ((equal method "verify")
         (handler-case (multiple-value-bind (ok reason code status) (funcall *verify-target*)
                         (nlk:json-object "ok" (if ok t :false)
                                          "provider" (or nle::*provider* :null)
                                          "model" (or nle::*model* :null)
                                          "reason" (or reason :null)
                                          "code" (or code :null)
                                          "status" (or status :null)))
           (error (condition)
             (nlk:json-object "ok" :false "reason" (redact-secrets (princ-to-string condition)
                                                                   (target-secrets))))))
        ((member method '("plan" "apply" "scan") :test #'equal)
         (handler-case (run-method method request)
           (error (condition)
             (nlk:json-object "status_text" (format nil "import: ~a" condition)
                              "error" (princ-to-string condition)))))
        (t (fail "no method ~s; plan, apply, scan or verify" method))))

;;; --- the entry ----------------------------------------------------------------------------

(defun stop-watch ()
  "The watch stopped, and waited for, bounded, to leave."
  (bt2:with-lock-held (*watch-lock*)
    (setf *watch* (nlk:worker-stop *watch*))))

(defun install ()
  "The watch over any bot an earlier import held for a running foreign
gateway."
  (funcall *start-watch*)
  (nle:on-stop #'stop-watch))

;; The settings, *IMPORT*: :HOME is this organism's own folder (~/.nodecode/),
;; where the instructions, skills and reports land; :SESSION-BUDGET how many
;; conversations one plan will carry over, newest first, before the rest
;; wait for `/import <world> --only sessions'.
(nle:define-cell import
  (:section ("import")
    (:guide "session_budget bounds how many conversations one plan carries, newest first")
    ("session_budget" :integer :default 200 :min 1
                      :doc "conversations one plan carries before the rest wait"))
  ;; The organism's own folder is where every piece lands; it is not a member
  ;; an operator answers.
  (:settings (lambda (values table)
               (declare (ignore table))
               (list* :home (nlk:home) values)))
  (:command "import" (lambda (args session-id)
                       (declare (ignore session-id))
                       (run-slash args))
            :description "Bring another coding agent's home into this one: preview first, --yes applies"
            :argument-hint "[WORLD] [--source PATH] [--only KINDS] [--overwrite] [--keep-running] [--yes]")
  (:route "/api/import" #'import-route)
  (:start #'install))
