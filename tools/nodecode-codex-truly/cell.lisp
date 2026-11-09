;;;; cell.lisp --- Codex Truly: the real window, measured and written.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Codex catalog a subscription's own account is served understates what
;;;; the backend accepts: it advertises 272K or 500K per model and publishes an
;;;; 872K ceiling, while the backend completes a request carrying ~920K input
;;;; tokens. Nodecode reads providers.<id>.models[].context_window out of the
;;;; shared config before anything it learned, so one scoped row per model is
;;;; the supported way to hand a Codex subscription the window it pays for - no
;;;; core patch, and revert takes back exactly what apply wrote.
;;;;
;;;; Rewritten from kyssta-exe's Hermes Agent plugin (see NOTICE): the catalog
;;;; read, the measurement, and the bisecting probe are the original's, moved
;;;; from Hermes' model_overrides onto Nodecode's providers.<id>.models rows.

(in-package #:nodecode-codex-truly)

;;; --- the manual ------------------------------------------------------------------

(defparameter *manual*
  "Codex Truly hands a ChatGPT/Codex subscription the context window the backend really
accepts, and says where each number comes from. One tool, which a person can also call
from eval as (codex-truly:status), plus /codex-truly.
  codex_truly  {action, model, target_context_window, dry_run}
      action='status'   per model: what the account's catalog advertises (context_window),
                        the ceiling it publishes (max_context_window), the window
                        providers.<id>.models sets now, and the largest input a live probe
                        accepted. Read-only and free.
      action='apply'    writes context_window for every model still below the target, through
                        the shared config's own atomic writer. dry_run=true previews it.
      action='revert'   takes back exactly the values apply wrote.
      action='probe'    bisects the live backend for one model's true input ceiling. A refused
                        request is free, an accepted one bills its input to the subscription,
                        so run it only when the person asks.
The window is a real trade: a 900K window means every turn may submit far more input, which
is what a long document or a large repository wants and a short chat does not. 900,000 is the
safe target, measured; anything past 918,000 is clamped to it, and there is no full 1M of
input on this route (about 1.05M total context minus a 128K output reserve). A turn that dies
with context_length_exceeded mid-stream means a window was set above what the backend takes:
lower target_context_window and apply again."
  "What (help :codex-truly) answers while the cell runs.")

;;; --- the backend, and what was measured against it ----------------------------------

(defparameter +base-url+ "https://chatgpt.com/backend-api/codex"
  "The Codex backend core itself calls: the one host this cell leaves for.")

(defparameter +models-url+ (concatenate 'string +base-url+ "/models?client_version=99.0.0")
  "The account-scoped model catalog. It answers an empty list without the sign-in's headers.")

(defparameter +responses-url+ (concatenate 'string +base-url+ "/responses")
  "The Responses endpoint a probe measures against.")

(defparameter +originator+ "codex_cli_rs"
  "The client the Codex backend insists a subscription's request names.")

(defparameter +measured-on+ "2026-10-14"
  "The day the numbers below were taken off the wire.")

(defparameter +measured+ '( ("gpt-6.1-sol" . (919956 . 930151)) )
  "Direct measurements against the live backend: slug -> (largest accepted . smallest rejected).")

(defparameter +core-verified+ 900000
  "Nodecode's own live-verified figure for the rest of the Codex family.")

(defparameter +min-window+ 272000
  "The smallest window worth writing: the legacy advertisement Hermes and Nodecode both knew.")

(defparameter +max-window+ 918000
  "The highest window backed by measurement: past it the backend refuses the request outright.")

(defparameter +target-default+ 900000
  "The default target: the measured ceiling with ~25K of margin.")

(defparameter +pad-unit+ "word "
  "What a probe fills its request with: about one token a unit on these models.")

(defparameter +calibration-units+ 4000
  "One small accepted request, to learn the real tokens-per-unit ratio.")

(defparameter +min-output-tokens+ 16
  "The backend refuses an output cap lower than this.")

;;; --- small parts ------------------------------------------------------------------

(defun nonblank (value)
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun started-p () (and *codex-truly* t))

(defmacro answering (&body body)
  "BODY's answer, a failure's words as the answer instead of an unhandled error."
  `(handler-case (progn ,@body)
     (codex-truly-error (condition)
       (nle:failure (format nil "ERROR: ~a" (codex-truly-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Codex Truly: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun require-started ()
  (unless (started-p)
    (fail "the nodecode-codex-truly cell is not running: turn it on in /cells, or (restart-cells \"nodecode-codex-truly\")")))

;;; --- the section, and the sign-in it runs on ---------------------------------------

(defun read-settings (values table)
  "The declared VALUES, clamped and tidied, plus the auth store a sign-in lives in."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :provider) (or (nonblank (getf values :provider)) "openai-codex")
          (getf plist :token-env) (or (nonblank (getf values :token-env)) "OPENAI_CODEX_OAUTH_TOKEN")
          (getf plist :auth-file) (or (nonblank (getf values :auth-file)) "")
          (getf plist :models) (remove-if-not #'nonblank (mapcar #'nlk:trimmed (or (getf values :models) '())))
          (getf plist :timeout) (or (getf values :timeout) 60)
          (getf plist :target-context-window)
          (let ((target (or (getf values :target-context-window) +target-default+)))
            ;; Out of range is clamped, never rejected: the person asked for more or
            ;; less, and a number the backend refuses must not break the cell.
            (max +min-window+ (min +max-window+ target))))
    plist))

(defun auth-file-path ()
  (let ((named (setting :auth-file)))
    (if (plusp (length named)) named (namestring (nlk:home "auth.json")))))

(defun provider-matches-p (provider name)
  (let ((name (or name "")))
    (or (string-equal provider name)
        (uiop:string-prefix-p (concatenate 'string provider ":") name))))

(defun stored-entry ()
  "The auth.json oauth_tokens entry this section's provider signed in with, or NIL."
  (let ((file (probe-file (auth-file-path))))
    (when file
      (let* ((auth (ignore-errors (nlk:decode-json (uiop:read-file-string file))))
             (tokens (and (hash-table-p auth) (nlk:json-value auth :object "oauth_tokens"))))
        (when (hash-table-p tokens)
          (let ((found '()))
            (maphash (lambda (key entry)
                       (when (and (hash-table-p entry)
                                  (nlk:json-value entry :text "access_token")
                                  (provider-matches-p (setting :provider)
                                                      (or (nlk:json-value entry :string "provider") key)))
                         (push entry found)))
                     tokens)
            (when (rest found)
              (fail "more than one saved sign-in matches provider ~a: name one in the section's provider"
                    (setting :provider)))
            (first found)))))))

(defun stored-token ()
  (nlk:json-value (stored-entry) :text "access_token"))

(defun token ()
  (or (nonblank (nle::credential-env (setting :token-env)))
      (stored-token)
      (fail "no ChatGPT/Codex sign-in for provider ~a: sign in with /openai-codex, export the token in ~a, or name the store in auth_file"
            (setting :provider) (setting :token-env))))

(defun jwt-auth-claim (jwt)
  "The https://api.openai.com/auth claim of JWT, or NIL. A malformed token is skipped."
  (let* ((parts (and (stringp jwt) (uiop:split-string jwt :separator ".")))
         (text (and (= (length parts) 3) (second parts))))
    (when text
      (let ((payload (ignore-errors
                      (cl-base64:base64-string-to-string
                       (concatenate 'string text (make-string (mod (- (length text)) 4)
                                                              :initial-element #\.))
                       :uri t))))
        (and payload (nlk:json-value (ignore-errors (nlk:decode-json payload))
                                      :text "https://api.openai.com/auth" "chatgpt_account_id"))))))

(defun account-id ()
  (or (nlk:json-value (stored-entry) :string "account_id")
      (ignore-errors (jwt-auth-claim (token)))))

(defun request-headers (&optional (extra '()))
  (append (list (cons "authorization" (format nil "Bearer ~a" (token)))
                (cons "originator" +originator+)
                (cons "accept" "application/json"))
          (let ((account (account-id)))
            (when account (list (cons "chatgpt-account-id" account))))
          extra))

(defun send (method url &key content)
  "One request to the Codex backend => (values BODY STATUS)."
  (let ((options (list :headers (request-headers (when content '(("content-type" . "application/json"))))
                       :timeout (setting :timeout)
                       :connect-timeout (setting :timeout))))
    (when content (setf options (append options (list :content content))))
    (handler-case (apply #'nlk:http method url options)
      (error (condition)
        (fail "the Codex backend did not answer: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- the catalog, and what this account is offered ---------------------------------

(defun catalog ()
  "The account's own model catalog => a list of (:slug :advertised :ceiling)."
  (multiple-value-bind (text status) (send :get +models-url+)
    (unless (and (integerp status) (<= 200 status 299))
      (fail "the Codex catalog answered HTTP ~a~@[: ~a~]" status
            (and (stringp text) (nlk:clip text 200))))
    (let* ((object (ignore-errors (nlk:decode-json text)))
           (rows (and (hash-table-p object) (nlk:json-array object "models"))))
      (unless (and rows (plusp (length rows)))
        (fail "the Codex catalog returned no models: the sign-in may belong to an account with no Codex access"))
      (loop for row across rows
            for slug = (nonblank (nlk:json-value row :string "slug"))
            when slug
              collect (list :slug slug
                            :advertised (nlk:json-value row :integer "context_window")
                            :ceiling (nlk:json-value row :integer "max_context_window"))))))

(defun offered-p (slug)
  (let ((only (setting :models)))
    (or (null only)
        (member (string-downcase slug) only :test #'string=))))

(defun promised (row)
  "The largest window ROW's catalog entry itself promises."
  (max (or (getf row :advertised) 0) (or (getf row :ceiling) 0)))

;;; --- the shared config's own rows --------------------------------------------------

(defun configured-rows ()
  "The providers.<provider>.models array as the config holds it, as a vector."
  (let ((value (ignore-errors (nle:config-get (list "providers" (setting :provider) "models")))))
    (cond ((vectorp value) value)
          ((listp value) (coerce value 'vector))
          (t #()))))

(defun row-field (row key)
  (cond ((hash-table-p row) (gethash (string-downcase (symbol-name key)) row))
        ((listp row)
         (loop for (name value) on row by #'cddr
               when (and name (string-equal (symbol-name name) (symbol-name key)))
                 return value))
        (t nil)))

(defun copy-row (row)
  (cond ((hash-table-p row)
         (let ((copy (make-hash-table :test #'equal)))
           (maphash (lambda (key value) (setf (gethash key copy) value)) row)
           copy))
        ((listp row) (copy-list row))
        (t row)))

(defun set-row-field (row key value)
  (cond ((hash-table-p row) (setf (gethash (string-downcase (symbol-name key)) row) value))
        ((listp row)
         (let ((pair (loop for tail on row by #'cddr
                           when (and (consp tail) (car tail)
                                     (string-equal (symbol-name (car tail)) (symbol-name key)))
                             return tail)))
           (if pair (setf (second pair) value) (setf (getf row key) value))))
        (t nil))
  row)

(defun configured-window (rows slug)
  (let ((row (find slug rows :test #'equal :key (lambda (item) (row-field item :id)))))
    (row-field row :context_window)))

;;; --- the picture /codex-truly and codex_truly report ---------------------------------

(defun status-rows (&optional target)
  (let ((target (or target (setting :target-context-window)))
        (configured (configured-rows)))
    (loop for row in (remove-if-not (lambda (item) (offered-p (getf item :slug))) (catalog))
          for slug = (getf row :slug)
          for promised = (promised row)
          collect (list :model slug
                        :advertised (getf row :advertised)
                        :ceiling (getf row :ceiling)
                        :configured (configured-window configured slug)
                        :target target
                        :widenable (or (zerop promised) (< promised target))
                        :measured (cdr (assoc slug +measured+ :test #'equal))))))

(defun fmt (value)
  (if (integerp value) (format nil "~:d" value) "-"))

(defun row-json (row)
  (let ((measured (getf row :measured)))
    (nlk:json-object "model" (getf row :model)
                      "advertised" (getf row :advertised)
                      "catalog_ceiling" (getf row :ceiling)
                      "configured" (getf row :configured)
                      "target" (getf row :target)
                      "widenable" (and (getf row :widenable) t)
                      "measured_accepted" (car measured)
                      "measured_rejected" (cdr measured))))

(defun summary-line (rows target)
  (let ((gaps (remove-if-not (lambda (row) (getf row :widenable)) rows)))
    (if rows
        (format nil "Codex Truly: ~d model~:p from this account's catalog, ~d below the ~a target~@[ (catalog caps them at ~a)~].~@[ Apply with /codex-truly apply.~]"
                (length rows) (length gaps) (fmt target)
                (and gaps (fmt (reduce #'max gaps :key (lambda (row) (or (getf row :ceiling) 0)))))
                (and gaps t))
        "Codex Truly: the catalog named no Codex model for this sign-in.")))

(defun report-text (&optional target)
  "The table /codex-truly and /codex-truly full answer with."
  (answering
    (require-started)
    (let* ((rows (status-rows target))
           (target (or target (setting :target-context-window))))
      (with-output-to-string (out)
        (format out "Codex Truly - the window a ChatGPT/Codex subscription really gets~%")
        (format out "  target window : ~a (safety ceiling ~a)~%" (fmt target) (fmt +max-window+))
        (format out "  provider      : ~a~%" (setting :provider))
        (format out "  measured on   : ~a~%~%" +measured-on+)
        (format out "  ~22a~10a~12a~12a~12a~%" "model" "advertised" "catalog max" "configured" "measured ok")
        (dolist (row rows)
          (format out "  ~22a~10a~12a~12a~12a~%"
                  (getf row :model) (fmt (getf row :advertised)) (fmt (getf row :ceiling))
                  (fmt (getf row :configured)) (fmt (car (getf row :measured)))))
        (format out "~%  ~a~%" (summary-line rows target))
        (format out "  Full 1M of input is not available on this route: about 1.05M total context~%")
        (format out "  shares a 128K output reserve, leaving about 920K of input.~%")))))

(defun status ()
  "What /codex-truly answers with, and (codex-truly:status) from eval."
  (report-text))

;;; --- apply and revert: providers.<provider>.models[].context_window ---------------

(defun apply-windows (&key dry-run target)
  "Give every model still below TARGET the target window. Answers the JSON the tool returns."
  (answering
    (require-started)
    (let* ((target (or target (setting :target-context-window)))
           (rows (remove-if-not (lambda (row) (getf row :widenable)) (status-rows target))))
      (cond ((null rows)
             (nlk:encode-json-object
              (nlk:json-object "ok" t "changed" (vector) "dry_run" (and dry-run t)
                               "note" "Nothing to change: every Codex model this account is offered already reaches the target.")))
            (dry-run
             (nlk:encode-json-object
              (nlk:json-object "ok" t "dry_run" t "target" target
                               "would_write" (coerce (loop for row in rows
                                                           collect (nlk:json-object "model" (getf row :model)
                                                                                   "context_window" target))
                                                     'vector))))
            (t
             (let* ((models (configured-rows))
                    (written '()))
               (dolist (row rows)
                 (let* ((slug (getf row :model))
                        (existing (find slug models :test #'equal
                                        :key (lambda (item) (row-field item :id))))
                        (copy (set-row-field (if existing (copy-row existing) (list :id slug))
                                             :context_window target)))
                   (setf models (if existing
                                    (substitute copy existing models :test #'eq)
                                    (concatenate 'vector models (vector copy))))
                   (push slug written)))
               (nle:config-set (list "providers" (setting :provider) "models") models)
               (nlk:encode-json-object
                (nlk:json-object "ok" t "target" target
                                 "written" (coerce (nreverse written) 'vector)
                                 "note" "A new session reads the wider window; a turn already running keeps the one it froze."))))))))

(defun revert-windows (&key target)
  "Take back the context_window values apply wrote, and no one else's."
  (answering
    (require-started)
    (let* ((target (or target (setting :target-context-window)))
           (models (configured-rows))
           (removed '())
           (kept '()))
      (when (plusp (length models))
        (loop for row across models
              for window = (row-field row :context_window)
              do (if (equal window target)
                     (progn (push (row-field row :id) removed)
                            ;; A row holding nothing but its id came from apply: drop it.
                            (let ((copy (remove-row-field (copy-row row) :context_window)))
                              (when (row-field-without-window copy) (push copy kept))))
                     (push row kept)))
        (let ((kept (coerce (nreverse kept) 'vector)))
          (nle:config-set (list "providers" (setting :provider) "models")
                           (if (plusp (length kept)) kept :remove))))
      (nlk:encode-json-object
       (nlk:json-object "ok" t "target" target
                        "removed" (coerce (remove nil (nreverse removed)) 'vector)
                        "note" (if removed
                                    "Those windows are gone; a new session reads the advertised one again."
                                    "No window equal to the target was set, so nothing was removed."))))))

(defun remove-row-field (row key)
  "ROW without its KEY member, of either representation."
  (cond ((hash-table-p row) (remhash (string-downcase (symbol-name key)) row) row)
        ((listp row)
         (loop for (name value) on row by #'cddr
               unless (and name (string-equal (symbol-name name) (symbol-name key)))
                 append (list name value)))
        (t row)))

(defun row-field-without-window (row)
  "True when ROW carries a member other than id and context_window."
  (cond ((hash-table-p row)
         (let ((other nil))
           (maphash (lambda (key value)
                      (declare (ignore value))
                      (unless (member key '("id" "context_window" "name") :test #'string-equal)
                        (setf other t)))
                    row)
           other))
        ((listp row)
         (loop for (key value) on row by #'cddr
               thereis (and key (not (member (string-downcase (symbol-name key))
                                             '("id" "context_window" "name") :test #'string-equal)))))
        (t t)))

;;; --- probe: the true ceiling, off the wire ----------------------------------------

(defun pad (units)
  (with-output-to-string (out)
    (loop repeat units do (write-string +pad-unit+ out))))

(defun probe-body (model units)
  (nlk:encode-json-object
   (nlk:json-object
    "model" model
    "instructions" "Answer with the single word OK."
    "input" (vector (nlk:json-object
                      "type" "message" "role" "user"
                      "content" (vector (nlk:json-object "type" "input_text" "text" (pad units)))))
    "stream" t "store" nil "tools" (vector)
    "max_output_tokens" +min-output-tokens+
    "reasoning" (nlk:json-object "effort" "low"))))

(defun sse-messages (text)
  "Every data: payload of an SSE body, decoded, malformed frames skipped."
  (loop for line in (uiop:split-string (or text "") :separator '(#\Newline))
        when (uiop:string-prefix-p "data: " line)
          append (let ((payload (ignore-errors (nlk:decode-json (subseq line 6)))))
                   (when (hash-table-p payload) (list payload)))))

(defun error-message (text)
  (or (loop for payload in (sse-messages text)
            when (equal (nlk:json-value payload :string "type") "error")
              return (let ((error (nlk:json-value payload :object "error")))
                       (nlk:json-value error :string "message")))
      (let ((head (or text ""))) (nlk:clip head 200))))

(defun rejection-p (text)
  (or (search "context_length_exceeded" (or text ""))
      (search "exceeds the context window" (or text ""))
      (search "response.failed" (or text ""))))

(defun streamed-input-tokens (text)
  (loop for payload in (sse-messages text)
        for response = (nlk:json-value payload :object "response")
        for usage = (nlk:json-value response :object "usage")
        for tokens = (nlk:json-value usage :integer "input_tokens")
        when (and tokens (plusp tokens)) return tokens))

(defun classify-probe (status text)
  "(accepted input-tokens detail) for one reply; ACCEPTED is :unknown when size was not the point."
  (cond ((not (and (integerp status) (<= 200 status 299)))
         (values (if (rejection-p text) :rejected :unknown) nil
                 (format nil "HTTP ~a~@[: ~a~]" status (error-message text))))
        ((rejection-p text)
         (values :rejected nil (format nil "rejected: ~a" (error-message text))))
        (t (values :accepted (streamed-input-tokens text) "completed"))))

(defun send-probe (model units)
  (multiple-value-bind (text status) (send :post +responses-url+ :content (probe-body model units))
    (classify-probe status text)))

(defun calibrate (model)
  "Measured tokens per padding unit, from one small accepted request."
  (multiple-value-bind (accepted tokens) (send-probe model +calibration-units+)
    (if (and (eq accepted :accepted) tokens)
        (/ (float tokens) (float +calibration-units+))
        1.0)))

(defun probe (model &key (low 872000) (high 1200000) (tolerance 20000) (max-rounds 6))
  "Bisect [LOW, HIGH] for the largest input the backend accepts, and answer the JSON."
  (answering
    (require-started)
    (let* ((model (or (nonblank model)
                      (getf (first (remove-if-not (lambda (row) (offered-p (getf row :slug))) (catalog)))
                            :slug)
                      (fail "the catalog named no model to probe: name one in the section's models")))
           (ratio (calibrate model))
           (low (max 1 low))
           (attempts '())
           (cursor-low low)
           (cursor-high high)
           (failure nil))
      (loop repeat max-rounds
            do (unless (> (- cursor-high cursor-low) tolerance)
                 (return))
        (let* ((midpoint (floor (+ cursor-low cursor-high) 2))
               (units (max 1 (round midpoint ratio))))
          (multiple-value-bind (accepted tokens detail) (send-probe model units)
            (push (nlk:json-object "probe_tokens" midpoint "units" units
                                   "reported_input_tokens" tokens
                                   "verdict" (string-downcase (symbol-name accepted))
                                   "detail" detail)
                  attempts)
            (case accepted
              (:unknown (setf failure detail) (return))
              (:accepted (setf cursor-low midpoint))
              (t (setf cursor-high midpoint))))))
      (if failure
          (nlk:encode-json-object
           (nlk:json-object "ok" nil "model" model "error" failure
                            "attempts" (coerce (nreverse attempts) 'vector)))
          (nlk:encode-json-object
           (nlk:json-object "ok" t "model" model
                            "tokens_per_unit" ratio
                            "largest_accepted" cursor-low
                            "smallest_rejected" cursor-high
                            "attempts" (coerce (nreverse attempts) 'vector)
                            "note" "The largest accepted is the highest input known to fit; the true limit sits between it and the smallest rejection. Accepted probes billed their input to the subscription."))))))

;;; --- the tool --------------------------------------------------------------------

(defun codex-truly (call)
  "The codex_truly tool: dispatch one action, always a JSON string."
  (let ((action (string-downcase (nlk:trimmed (or (request-action call) "status")))))
    (cond ((string= action "status") (report-text))
          ((string= action "apply")
           (apply-windows :dry-run (request-dry-run call)
                          :target (request-target-context-window call)))
          ((string= action "revert")
           (revert-windows :target (request-target-context-window call)))
          ((string= action "probe") (probe (request-model call)))
          (t (answering
               (require-started)
               (nlk:encode-json-object
                (nlk:json-object "ok" nil "error" (format nil "unknown action ~s" action))))))))

;;; --- what the operator reads -------------------------------------------------------

(defun run-command (args session-id)
  "The /codex-truly command: status by default, full, apply, revert, or probe."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab)) :test #'equal))
         (verb (string-downcase (or (first words) "")))
         (rest (rest words)))
    (cond ((or (string= verb "") (string= verb "status")) (values (report-text) (and (started-p) (status-dialog))))
          ((string= verb "full") (report-text))
          ((string= verb "apply")
           (let ((payload (apply-windows :dry-run (and (member "dry-run" rest :test #'string=) t))))
             (if (answered-ok-p payload) (apply-note payload) payload)))
          ((string= verb "revert")
           (let ((payload (revert-windows)))
             (if (answered-ok-p payload) (apply-note payload) payload)))
          ((string= verb "probe") (probe (first rest)))
          (t (values "Usage: /codex-truly [status|full|apply|revert|probe [MODEL]]. status is the one-line picture, full the table, apply writes the wider window (add dry-run to preview), revert takes it back, probe re-measures one model live."
                     (and (started-p) (status-dialog)))))))

(defun answered-ok-p (payload)
  "True when PAYLOAD, the JSON a verb answered, says ok."
  (let ((object (ignore-errors (nlk:decode-json payload))))
    (and (hash-table-p object) (nlk:json-value object :boolean "ok") t)))

(defun apply-note (payload)
  "A JSON answer of apply or revert, as the sentence the command shows."
  (let ((object (ignore-errors (nlk:decode-json payload))))
    (or (and (hash-table-p object) (nlk:json-value object :string "note")) payload)))

(defun status-dialog ()
  (nle:list-dialog
   :title "Codex Truly"
   :summary "The context window a Codex subscription really gets."
   :rows (list (nle:ask-row "Show the table" "Every window, per model" "/codex-truly full" :verb "show")
               (nle:ask-row "Apply the wider window" "Preview first" "/codex-truly apply dry-run" :verb "preview"))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell codex-truly
  (:section ("codex-truly")
    (:guide "sign in to the subscription once with /openai-codex (its token lands in Nodecode's shared auth.json); next step: /codex-truly - it reports what the catalog advertises against what the backend accepts, and apply writes the wider window into providers.<provider>.models")
    ("provider" :string :default "openai-codex"
     :doc "the provider id whose sign-in and model rows are read: openai-codex, or a custom provider that speaks the Codex wire")
    ("token_env" :env :default "OPENAI_CODEX_OAUTH_TOKEN"
     :doc "the environment variable holding a Codex access token, used before the saved sign-in; empty when unset")
    ("auth_file" :string :default ""
     :doc "the shared auth.json holding the sign-in; empty means the organism's own")
    ("target_context_window" :integer :default 900000 :min 272000
     :doc "the window apply grants each Codex model; clamped to 918000, the highest value measured")
    ("models" :list
     :doc "model ids to manage; empty manages every model the account's catalog exposes")
    ("timeout" :integer :default 60 :min 5
     :doc "how long one request to the Codex backend may take, in seconds"))
  (:settings #'read-settings)
  (:help :codex-truly
         "codex-truly: codex_truly reports and writes the real context window of a Codex subscription"
         *manual*)
  (:tool "codex_truly" (nlk:contract 'request) #'codex-truly)
  (:command "codex-truly" 'run-command
            :description "Codex Truly: the real context window of a Codex subscription"
            :argument-hint "status | full | apply [dry-run] | revert | probe [MODEL]"))
