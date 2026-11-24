;;;; provider.lisp --- what Aixy is: its address, its key, its catalog, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from aixy-tech/aixy-hermes-plugin (see NOTICE): aixy_setup.py's
;;;; validate_url (HTTPS, HTTP only on loopback, no credentials, query or
;;;; fragment), fetch_models (one authenticated GET of {base}/models, redirects
;;;; refused) and parse_catalog (the OpenAI listing's data[], each id once, the
;;;; context length only when the gateway advertises one), and the provider
;;;; entry the plugin writes into the profile: the openai-chat transport, the
;;;; key reference, and the X-Aixy-Session-ID header that groups a
;;;; conversation in Aixy Activity.
;;;;
;;;; Aixy needs little of its own here. The catalog row below names the chat
;;;; lane's package and this section's address, the key is the derived
;;;; AIXY_API_KEY, and the core's own listing already asks {base}/models with
;;;; it. What this folder adds is the row, the address a key may be sent to,
;;;; and the session header.

(in-package #:nodecode-aixy)

(defparameter +catalog-timeout-seconds+ 20
  "One catalog read's ceiling: the plugin's own 20 seconds.")

(defparameter +listed+ 60
  "How many model ids /aixy prints before it says how many it left out.")

;;; --- the address ------------------------------------------------------------

(defun loopback-host-p (host)
  "Whether HOST is one of this machine's own loopback names or addresses."
  (let ((host (string-downcase (or host ""))))
    (or (string= host "localhost")
        (string= host "::1")
        (string= host "[::1]")
        (string= host "0:0:0:0:0:0:0:1")
        (uiop:string-prefix-p "127." host))))

(defun refuse (control &rest arguments)
  "Signal a config refusal saying CONTROL, formatted with ARGUMENTS."
  (error 'nlk:config-refusal :detail (apply #'format nil control arguments)))

(defun split-address (text)
  "TEXT as (SCHEME AUTHORITY PATH) when it is scheme://authority[/path], else NIL."
  (let* ((mark (search "://" text))
         (scheme (and mark (plusp mark) (string-downcase (subseq text 0 mark))))
         (rest (and scheme (subseq text (+ mark 3))))
         (slash (and rest (position #\/ rest)))
         (authority (and rest (subseq rest 0 (or slash (length rest)))))
         (path (and rest (if slash (subseq rest slash) ""))))
    (and scheme (plusp (length authority)) (list scheme authority path))))

(defun validate-base-url (value)
  "VALUE as the base this cell sends a project key to, right-trimmed of
slashes; signals a refusal naming what is wrong with it."
  (let ((text (string-trim " " (or value ""))))
    (when (zerop (length text))
      (refuse "aixy: base_url is empty; it is ~a unless your gateway is elsewhere" +base+))
    (when (find-if (lambda (character)
                     (or (<= (char-code character) 32) (= (char-code character) 127)))
                   text)
      (refuse "aixy: base_url must hold no spaces, control characters or line breaks"))
    (when (or (find #\\ text) (find #\? text) (find #\# text))
      (refuse "aixy: base_url may carry no query, fragment or backslash; the address alone, like ~a"
              +base+))
    (let* ((parts (split-address text))
           (scheme (first parts))
           (authority (second parts)))
      (unless scheme
        (refuse "aixy: base_url must be an address like ~a" +base+))
      (unless (member scheme '("https" "http") :test #'string=)
        (refuse "aixy: base_url must use https; http is allowed only on this machine's loopback"))
      (when (or (find #\@ authority) (find #\% authority))
        (refuse "aixy: base_url names no usable host; no credentials or escapes belong in it"))
      (let* ((colon (position #\: authority))
             (host (if colon (subseq authority 0 colon) authority))
             (port (and colon (subseq authority (1+ colon)))))
        (when (zerop (length host))
          (refuse "aixy: base_url names no host; an address like ~a" +base+))
        (when (and port
                   (or (zerop (length port))
                       (not (every #'digit-char-p port))
                       (not (<= 1 (parse-integer port) 65535))))
          (refuse "aixy: base_url's port must be a number from 1 to 65535"))
        (when (and (string= scheme "http") (not (loopback-host-p host)))
          (refuse "aixy: base_url over http must be on this machine's loopback; ~a is remote, so it needs https"
                  host)))
      (string-right-trim "/" text))))

(defparameter *base* nil
  "This start's validated base: what the row and the listing use.")

(defun gateway ()
  "This section's base, right-trimmed: the address a turn and a listing dial."
  (or *base* (string-right-trim "/" (or (setting :base-url) +base+))))

;;; --- the catalog row ---------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Aixy's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Aixy's row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun catalog-row (&optional prior)
  "Aixy as a models.dev provider: the chat lane's package, this section's base,
the key variable, and whatever PRIOR (models.dev's own row, when it publishes
one) lists."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (nlk:json-object "name" "Aixy"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (gateway)
                     "env" (vector +env+)
                     "models" models)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

;;; --- the key, and the gateway's own catalog ---------------------------------

(defun credential ()
  "The project key this image would send, or NIL: the one /connect saved, then
AIXY_API_KEY, and never another lane's."
  (let ((credential (ignore-errors (nle::resolve-provider-credential +provider+ :probe t))))
    (and credential
         (not (eq :public (nle:credential-source credential)))
         (nle:credential-key credential)
         credential)))

(defun key-state ()
  "Where an Aixy project key comes from, in words: never the key itself."
  (let ((credential (credential)))
    (cond ((null credential)
           (format nil "no key yet: save one with /connect, Aixy, or set ~a" +env+))
          ((eq :env (nle:credential-source credential)) (format nil "a key read from ~a" +env+))
          (t "a key saved in auth.json"))))

(defun catalog-models (text &aux (payload (ignore-errors (nlk:decode-json text)))
                            (seen (make-hash-table :test #'equal))
                            (rows '()))
  "TEXT, a gateway's /models answer, as (:id :context :name) rows: the ids
its data[] carries, each once, a context length only when the gateway
advertises one."
  (loop for entry across (or (nlk:json-value payload :array "data") #())
        for id = (nlk:json-value entry :string "id")
        when (and (stringp id) (plusp (length id)) (not (gethash id seen)))
          do (setf (gethash id seen) t)
             (push (list :id id
                         :context (nlk:json-value entry :integer "context_length")
                         :name (nlk:json-value entry :string "name"))
                   rows))
  (nreverse rows))

(defun read-catalog (&aux (credential (credential)))
  "(values MODELS STATUS): what the gateway's /models answers, as rows, or NIL
and why nothing came of it."
  (multiple-value-bind (body status)
      (nle:http-fetch (concatenate 'string (gateway) "/models")
                       :headers (if credential
                                    (list (cons "accept" "application/json")
                                          (cons "authorization"
                                                (concatenate 'string "Bearer "
                                                             (nle:credential-key credential))))
                                    '(("accept" . "application/json")))
                       :timeout +catalog-timeout-seconds+)
    (if (null body)
        (values nil (or status "no answer"))
        (let ((models (catalog-models body)))
          (if models
              (values models nil)
              (values nil "an answer that is not a model catalog"))))))

;;; --- the session header ------------------------------------------------------

(defun session-scope ()
  "The conversation this request belongs to, or NIL: the value of the session
header, bounded and printable."
  (let ((id (getf (nle:turn) :session-id)))
    (and (stringp id)
         (plusp (length id))
         (every (lambda (character)
                  (and (> (char-code character) 32) (< (char-code character) 127)))
                id)
         (subseq id 0 (min 128 (length id))))))

(defun session-headers ()
  "The header naming this turn's conversation to Aixy, or NIL: the section's
session_header, unless it says none, and only when the turn has a session."
  (let* ((name (string-trim " " (or (setting :session-header) "")))
         (id (session-scope)))
    (and (plusp (length name))
         (not (member (string-downcase name) '("none" "off" "-") :test #'string=))
         id
         (list (cons name id)))))

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an aixy turn."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: an aixy round carries the session header."
  (if (ours-p config)
      (apply next fold :headers (append headers (session-headers))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

;;; --- the command's answer ----------------------------------------------------

(defun model-line (model)
  "MODEL as /aixy shows it: its id, and its context window when the gateway
advertises one."
  (let ((context (getf model :context)))
    (if (and (integerp context) (plusp context))
        (format nil "  ~a (~:d context tokens)" (getf model :id) context)
        (format nil "  ~a" (getf model :id)))))

(defun models-report ()
  "The /aixy answer: the gateway, where its key comes from, and what it serves."
  (multiple-value-bind (models status) (read-catalog)
    (if (null models)
        (format nil "Aixy at ~a listed no models: ~a. ~a" (gateway) status (key-state))
        (let ((lines (mapcar #'model-line models)))
          (format nil "Aixy at ~a serves ~d model~:p. ~a.~%~{~a~^~%~}~a"
                  (gateway) (length models) (key-state)
                  (subseq lines 0 (min +listed+ (length lines)))
                  (if (> (length models) +listed+)
                      (format nil "~%  ... and ~d more; /aixy prints ~d at a time."
                              (- (length models) +listed+) +listed+)
                      ""))))))

(defun key-report ()
  "The /aixy key answer: the gateway, the key's source, and where to make one."
  (format nil "Aixy at ~a: ~a. Make a project key at ~a, save it with /connect, Aixy, or set ~a."
          (gateway) (key-state) +key-page+ +env+))

(defun run-command (args session-id)
  "The /aixy command: what the gateway serves, and where its key comes from."
  (declare (ignore session-id))
  (let ((args (string-downcase (string-trim " " (or args "")))))
    (cond ((or (string= args "") (string= args "models")) (models-report))
          ((string= args "key") (key-report))
          (t (concatenate 'string
                          "usage: /aixy -- the gateway, its key, and what it serves"
                          (format nil "~%/aixy key -- where the project key comes from"))))))

;;; --- what the model reads ----------------------------------------------------

(defparameter +manual+
  "Aixy is an inference gateway: the models it serves are its own ids, and a turn on provider aixy goes to the section's base over the OpenAI chat wire. Save a project key with /connect, Aixy, or set AIXY_API_KEY. /aixy lists what the gateway serves, an id and its context window, and where its key comes from; /models is the picker the turn's model is set from. A gateway that advertises no context length leaves the window at Nodecode's default, and a model whose window is larger can be pinned in providers.aixy.models[].context. Each request carries the turn's conversation id in the X-Aixy-Session-ID header, which Aixy Activity groups by; the section's session_header names the header, and the word none turns it off."
  "What (help :aixy) answers, and the line every request carries.")

(defun check-config ()
  "Refuse a section whose base this cell would not send a project key to."
  (setf *base* (validate-base-url (setting :base-url))))
