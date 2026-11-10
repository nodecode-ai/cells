;;;; provider.lisp --- what LowRouter is: its address, its key, its catalog, its balance.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of the LowRouter provider plugin for Hermes (see NOTICE). The
;;;; plugin's one idea, kept here: LowRouter is a router, so /v1/models lists
;;;; every route it can serve -- each pinned host route beside the `auto/'
;;;; id, embeddings, realtime models, models that take no tool calls, models
;;;; too small for an agent -- while a Nodecode turn can run on only some of
;;;; them. The picker therefore offers an `auto/<creator>/<model>' id or the
;;;; account's own `alias/<name>', a chat modality, tool calling, and at
;;;; least 64,000 tokens of context; an `auto/' id is the one that fails
;;;; over between hosts, and a pinned `<host>/<creator>/<model>' route is
;;;; left out because the `auto/' id already covers it.
;;;;
;;;; The window each route really has comes from the same answer, not from
;;;; one number assumed for the whole endpoint, so compression runs when the
;;;; model's own window fills. And since LowRouter bills in euros while
;;;; Nodecode counts US dollars, the plugin's answer to `what have I got
;;;; left' was a `/usage' line in EUR; here it is `/lowrouter'.

(in-package #:nodecode-lowrouter)

(defparameter +base+ "https://api.lowrouter.ai/v1"
  "Where LowRouter is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("LOWROUTER_API_KEY")
  "The environment variable a key is read from, in order; /connect's saved key answers first.")

(defparameter +key-page+ "https://lowrouter.ai/app/keys"
  "Where a LowRouter key is made.")

(defparameter +docs+ "https://lowrouter.ai/models"
  "LowRouter's own model and price page: the hosts each route may be served by.")

(defparameter +timeout+ 8
  "Seconds one of this cell's own reads may take, connect included.")

(defparameter +min-context+ 64000
  "The smallest window a picked route may have: below it a Nodecode turn
does not fit its own context.")

(defparameter +listed-prefixes+ '("auto/" "alias/")
  "The only id shapes the picker offers: an `auto/' route, which fails over
between hosts, and the account's own `alias/' model.")

(defparameter +fallback-rows+
  '(("auto/mistralai/mistral-large-2512" 256000)
    ("auto/mistralai/mistral-small-2603" nil)
    ("auto/qwen/qwen3-coder-30b-a3b-instruct" 128000)
    ("auto/anthropic/claude-haiku-4.5" 200000))
  "The picker when the live catalog is unreachable: the plugin's own four
fallback models, with the context windows LowRouter publishes for them and
NIL where it publishes none.")

(defparameter +currency-symbols+ '(("EUR" . "€") ("USD" . "$") ("GBP" . "£"))
  "The currencies LowRouter may bill in, and the symbol each is shown with.")

;;; --- the address and the key ---------------------------------------------------

(defun base ()
  "The base this cell reaches LowRouter at: the section's."
  (let ((set (setting :base-url)))
    (string-right-trim "/" (if (and (stringp set) (plusp (length set))) set +base+))))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun saved-key ()
  "The key one of this cell's own reads carries: the one /connect saved, else
the environment variable, and NIL when there is none -- never the stand-in a
public credential answers with."
  (let ((key (or (ignore-errors
                   (nle:credential-key (nle::resolve-provider-credential +provider+ :probe t)))
                 (env-key))))
    (and (stringp key) (plusp (length key)) (not (equal key "public")) key)))

(defun credential (op next)
  "The :CREDENTIAL answer for lowrouter: the key LOWROUTER_API_KEY holds,
else a public one -- /v1/models answers without it, as the plugin's does. A
key /connect saved in auth.json answers before this point."
  ;; Never NEXT for lowrouter: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to LowRouter.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun request-headers (key)
  "The headers one of this cell's own reads carries: KEY as the bearer when
there is one, and a request for JSON."
  (append (when (and (stringp key) (plusp (length key)) (not (equal key "public")))
            (list (cons "authorization" (format nil "Bearer ~a" key))))
          (list (cons "accept" "application/json"))))

(defun get-json (url key)
  "(values PAYLOAD WHY): URL read as a JSON object with this cell's headers."
  (let ((answer nil) (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :get url :headers (request-headers key) :timeout +timeout+
                    :connect-timeout (min 5 +timeout+)))
      ((or error sb-sys:deadline-timeout) () (return-from get-json (values nil "no answer"))))
    (unless (and (integerp status) (< status 300))
      (return-from get-json (values nil (format nil "HTTP ~a" status))))
    (let ((payload (handler-case (nlk:decode-json answer) (error () nil))))
      (if (hash-table-p payload)
          (values payload nil)
          (values nil "an answer that is not a JSON object")))))

;;; --- the catalog ---------------------------------------------------------------

(defun listed-p (id)
  "Whether ID has one of the two shapes the picker offers."
  (and (stringp id) (some (lambda (prefix) (uiop:string-prefix-p prefix id)) +listed-prefixes+)))

(defun tool-calling-p (entry)
  "Whether the listing says ENTRY takes tool calls: the `tools' marker among
its supported parameters."
  (let ((params (nlk:json-value entry :array "supported_parameters")))
    (and (vectorp params) (find "tools" params :test #'equal) t)))

(defun chat-p (entry)
  "Whether the listing says ENTRY is a chat model."
  (equal "chat" (nlk:json-value entry :string "modality")))

(defun wide-enough-p (entry)
  "Whether ENTRY's window, when the listing publishes one, is wide enough; a
route with no published window stays, and a turn looks it up itself."
  (let ((context (nlk:json-value entry :integer "context_length")))
    (or (null context) (>= context +min-context+))))

(defun runs-in-nodecode-p (entry)
  "Whether ENTRY is a route a Nodecode turn can run on: an `auto/' or
`alias/' chat model that takes tool calls and fits a turn -- the plugin's
`runs_in_hermes', with the same 64,000-token floor."
  (and (listed-p (nlk:json-value entry :string "id"))
       (chat-p entry)
       (tool-calling-p entry)
       (wide-enough-p entry)))

(defun images-p (entry)
  "Whether the listing says ENTRY takes images in."
  (let* ((architecture (nlk:json-value entry :object "architecture"))
         (modalities (and architecture (nlk:json-value architecture :array "input_modalities"))))
    (and (vectorp modalities) (find "image" modalities :test #'equal) t)))

(defun row (entry)
  "ENTRY as this cell keeps a row: the id, the name, the window, and whether
it takes images in."
  (let ((id (nlk:json-value entry :string "id")))
    (list :id id
          :name (or (nlk:json-value entry :string "name") id)
          :context (nlk:json-value entry :integer "context_length")
          :images (images-p entry))))

(defun fetch-models ()
  "(values ROWS TOTAL WHY): the routes /v1/models lists that a turn can run
on, how many models the answer held, and why there is no answer."
  (multiple-value-bind (payload why) (get-json (concatenate 'string (base) "/models")
                                               (saved-key))
    (if (null payload)
        (values nil 0 why)
        (let ((entries (nlk:json-value payload :array "data")))
          (unless (vectorp entries)
            (return-from fetch-models (values nil 0 "a listing with no data[]")))
          (let ((rows '()))
            (loop for entry across entries
                  when (and (hash-table-p entry) (runs-in-nodecode-p entry))
                    do (push (row entry) rows))
            (values (nreverse rows) (length entries) nil))))))

(defvar *live-rows* nil
  "The rows the live catalog answered, or NIL while it is unfetched or
unreachable; NIL is what makes the plugin's four fallbacks answer instead.")

(defun fallback-rows ()
  "The plugin's four fallback routes, as rows."
  (loop for (id context) in +fallback-rows+
        collect (list :id id :name id :context context :images nil)))

(defun refresh (base)
  "One step of the catalog worker: fetch the live catalog when BASE is
LowRouter's own, keep its rows, and stop the worker once they have landed.
Answers NIL to be woken again after the worker's wait, so an unreachable
catalog is retried rather than given up on."
  (when (equal base (base))
    (multiple-value-bind (rows total why) (fetch-models)
      (declare (ignore why))
      (when rows
        (setf *live-rows* rows)
        (forget-catalog)
        (nle:notice (format nil "lowrouter: ~d of ~d route~:p in the picker"
                            (length rows) total))
        :stop))))

;;; --- the provider row --------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
LowRouter's row in it.")

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base or other rows builds a new one."
  (setf *catalog* (cons nil nil)))

(defun catalog-model (row)
  "ROW, a plist from the catalog or the fallback list, as a CATALOG-MODEL."
  (destructuring-bind (&key id name context images) row
    ;; every row here takes tool calls by construction, and answers in text
    (nle::make-catalog-model (or name id) context nil
                              (if images #("text" "image") #("text"))
                              #("text") nil nil nil t nil)))

(defun catalog-row (&optional prior)
  "LowRouter as a models.dev provider: the chat lane's package, this
section's base, the key variable, and the routes this cell read over PRIOR's
(the row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test #'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (dolist (row (or *live-rows* (fallback-rows)))
      (setf (gethash (getf row :id) models) (catalog-model row)))
    (nlk:json-object "name" "LowRouter"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base)
                     "doc" +docs+
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with LowRouter's row, made once
per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

;;; --- the balance -------------------------------------------------------------

(defun money (amount currency)
  "AMOUNT in CURRENCY, with its symbol when it has one."
  (let ((symbol (cdr (assoc currency +currency-symbols+ :test #'equal))))
    (if symbol
        (format nil "~a~,2f" symbol amount)
        (format nil "~,2f ~a" amount currency))))

(defun balance-lines ()
  "(values LINES WHY): the credit balance /v1/credits answers with, as the
plugin's `credit_lines' shows it in /usage, and why it did not."
  (let ((key (saved-key)))
    (if (null key)
        (values nil (format nil "no key yet (make one at ~a)" +key-page+))
        (multiple-value-bind (payload why)
            (get-json (concatenate 'string (base) "/credits") key)
          (let ((data (and payload (nlk:json-value payload :object "data"))))
            (if (null data)
                (values nil (or why "no balance in the answer"))
                (let ((total (nlk:json-value data :number "total_credits"))
                      (used (nlk:json-value data :number "total_usage"))
                      (currency (string-upcase (or (nlk:json-value data :string "currency") ""))))
                  (if (and total used (plusp (length currency)))
                      (values (list (format nil "Credits balance: ~a"
                                            (money (max 0.0 (- total used)) currency))
                                    (format nil "Purchased ~a • used ~a"
                                            (money total currency) (money used currency)))
                              nil)
                      (values nil "no balance in the answer")))))))))

;;; --- the command ------------------------------------------------------------

(defun balance-report ()
  "The /lowrouter answer with no argument: the balance, and what the picker holds."
  (multiple-value-bind (lines why) (balance-lines)
    (if lines
        (format nil "~{~a~%~}~d route~:p in the picker."
                lines (length (or *live-rows* (fallback-rows))))
        (format nil "LowRouter: ~a." why))))

(defun models-report ()
  "The /lowrouter models answer: read the live catalog and list the routes
it offers, taking them into the picker now."
  (multiple-value-bind (rows total why) (fetch-models)
    (if (null rows)
        (format nil "LowRouter: the catalog at ~a/models did not answer (~a)." (base) why)
        (progn
          (setf *live-rows* rows)
          (forget-catalog)
          (with-output-to-string (out)
            (format out "LowRouter: ~d of ~d catalog model~:p take tool calls on a chat route; /models offers them now:~%"
                    (length rows) total)
            (dolist (row (subseq rows 0 (min 25 (length rows))))
              (format out "  ~a~@[ (~d tokens)~]~%" (getf row :id) (getf row :context))))))))

(defun run-command (args session-id)
  "The /lowrouter command: your balance, or the routes the picker offers."
  (declare (ignore session-id))
  (let ((asked (string-downcase (string-trim '(#\Space #\Tab) (or args "")))))
    (cond ((member asked '("" "balance") :test #'equal) (balance-report))
          ((equal asked "models") (models-report))
          (t (format nil "LowRouter: /lowrouter shows your balance; /lowrouter models lists the routes the picker offers.")))))
