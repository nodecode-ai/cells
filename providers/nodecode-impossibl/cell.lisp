;;;; cell.lisp --- the cell: Impossibl among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but impossibl:
;;;;
;;;;   MODELS-CATALOG-TABLE    the catalog carries Impossibl's row: the chat
;;;;                            lane's package, this section's base, the key
;;;;                            variable and the fallback roster, so /connect
;;;;                            offers it and a turn resolves its lane and
;;;;                            address the way any catalog provider's does
;;;;   :CREDENTIAL              a key from IMPOSSIBL_API_KEY and no other
;;;;                            variable; a key /connect saved in auth.json
;;;;                            answers before this point does
;;;;   LIST-PROVIDER-MODELS    the lane's listing is the catalogue entries
;;;;                            that serve chat completions and take tools, the
;;;;                            core's own reading when that read fails or
;;;;                            names none
;;;;
;;;; Config, a sibling top-level key:
;;;;   "impossibl": {"base_url": "https://api.impossibl.com/v1", "chat_only": true}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-impossibl)

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Impossibl's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Impossibl's row, made once
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

;;; --- the listing -----------------------------------------------------------

(defvar *chat-rows* nil
  "The rows the last catalogue read named: a catalogue that answered once is
not asked again until a restart, which is the plugin's own fallback cache.")

(defun served-p (provider)
  "Whether PROVIDER is the lane this cell serves."
  (and (stringp provider) (string-equal provider +provider+)))

(defun tool-declined-p (entry)
  "Whether ENTRY's catalogue row says, in the JSON itself, that it takes no
tool calls: the plugin's `supports_tools is False', and only that."
  (multiple-value-bind (value present) (nlk:json-value entry :any "supports_tools")
    (and present (null value))))

(defun chat-route-p (entry)
  "Whether ENTRY, one model of the catalogue, is one an agent turn can drive:
it serves /v1/chat/completions, or names no `endpoints' at all (a plain
OpenAI list behind a custom base), and does not decline tool calls."
  (and (not (tool-declined-p entry))
       (let ((endpoints (nlk:json-value entry :array "endpoints")))
         (or (null endpoints)
             (loop for endpoint across endpoints
                   thereis (and (stringp endpoint)
                                (equal endpoint +chat-endpoint+)))))))

(defun catalog-rows (body)
  "The rows BODY, an Impossibl /v1/models answer, carries for the models an
agent turn can drive: (:id :display :context-window) each, in the catalogue's
order and each id once; NIL for a body that is not the catalogue or names
none such."
  ;; A row's display is the catalogue's name when it says more than the id:
  ;; the picker's own ids are long, and a name that repeats one is noise.
  (let ((catalog (ignore-errors (nlk:decode-json body)))
        (seen (make-hash-table :test 'equal))
        (rows '()))
    (loop for entry across (or (nlk:json-value catalog :array "data") #())
          for id = (nlk:json-value entry :text "id")
          for name = (nlk:json-value entry :text "name")
          for window = (or (nlk:json-value entry :integer "context_length")
                           (nlk:json-value entry :integer "max_model_len"))
          when (and id (chat-route-p entry) (not (gethash id seen)))
            do (setf (gethash id seen) t)
               (push (list :id id
                           :display (and name (string/= name id) name)
                           :context-window (and window (plusp window) window))
                     rows))
    (nreverse rows)))

(defun models-url ()
  "The catalogue this cell reads: the section's base with /models on it."
  (uiop:strcat (string-right-trim "/" (setting :base-url)) "/models"))

(defun fetch-rows (&key api-key)
  "The rows Impossibl's catalogue names for the models an agent turn can drive,
or NIL when the read fails or names none."
  ;; HTTP-FETCH is the one bounded GET whose failures, a deadline's included,
  ;; are values: the listing worker never unwinds through here.
  (let ((headers (append '(("accept" . "application/json"))
                          (and api-key
                               (list (cons "authorization" (format nil "Bearer ~a" api-key)))))))
    (multiple-value-bind (body)
        (nle:http-fetch (models-url) :headers headers :timeout (setting :timeout-seconds))
      (and (stringp body) (catalog-rows body)))))

(defun listing-key (key)
  "The key the catalogue read carries: KEY as the caller passed it, else
IMPOSSIBL_API_KEY, else a key /connect saved -- never the placeholder a
keyless lane resolves."
  (or key
      (env-key)
      (let ((saved (ignore-errors
                     (nle:credential-key (nle::resolve-provider-credential +provider+)))))
        (and saved (string/= saved "public") saved))))

(defun chat-rows (key)
  "The rows the catalogue names, kept once a read answers."
  (or *chat-rows*
      (setf *chat-rows* (fetch-rows :api-key key))))

(defun filtering-p (base)
  "Whether this listing is one this cell may interpret: the base the section
serves, and chat_only on. A base the operator pointed somewhere else is a
relay's catalogue, not Impossibl's, so the core's own reading stands."
  (and (setting :chat-only)
       (let ((base (string-right-trim "/" (or base ""))))
         (or (zerop (length base))
             (equal base (string-right-trim "/" (setting :base-url)))))))

(defun list-models (next provider &rest keys &key base &allow-other-keys)
  "(values ROWS ERROR): the served lane's listing is the models the catalogue
routes through chat completions with tool calls; any other lane's, and a
catalogue that answered none, is NEXT's own."
  (if (and (served-p provider) (filtering-p base))
      (alexandria:if-let (rows (chat-rows (listing-key (getf keys :key))))
        (values rows nil)
        (apply next provider keys))
      (apply next provider keys)))

(defun forget-catalog ()
  "Drop the merged catalog and the rows read, so a restart with another base
or another key asks again."
  (setf *catalog* (cons nil nil) *chat-rows* nil))

(nle:define-cell impossibl
  (:section ("impossibl")
    (:guide "make a key at https://impossibl.com; save it with /connect or set IMPOSSIBL_API_KEY; base_url is where Impossibl is served; chat_only lists only the models its catalogue routes through chat completions with tool calls")
    ("base_url" :string :default +base+
     :doc "the Impossibl API base the chat lane appends /chat/completions to")
    ("chat_only" :boolean :default t
     :doc "list only the models the catalogue routes through /v1/chat/completions that take tool calls")
    ("timeout_seconds" :integer :default 8 :min 1
     :doc "one catalogue read's ceiling, in seconds"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::list-provider-models #'list-models))
