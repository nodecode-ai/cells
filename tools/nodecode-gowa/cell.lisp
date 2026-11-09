;;;; cell.lisp --- the primer, the section, three tools, /gowa.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of anpicasso/hermes-go-whatsapp (see NOTICE). The original was
;;;; three Hermes tools over a GOWA (go-whatsapp-web-multidevice) server --
;;;; the device slots it holds, the pairing of one, the removal of one -- plus
;;;; a setup command that downloaded and installed GOWA itself. A cell has no
;;;; installer, so the server is the operator's: this cell talks to the one
;;;; base_url names, and /gowa is the operator's list where the CLI's setup
;;;; used to be.

(in-package #:nodecode-gowa)

;;; --- what the model is told -------------------------------------------------------

(defparameter +primer+
  "GOWA is the go-whatsapp-web-multidevice server: several WhatsApp device slots
under one local (or remote) REST origin, each slot an account linked the way
WhatsApp Web is. This cell manages the slots that server holds, through three
tools and /gowa. It does not send messages and does not read them: GOWA's own
MCP endpoint serves sending, messages, chats and groups.
  gowa_devices {action: list|get, device_id?}
      list every slot with its state, or one slot by its exact id. The answer
      carries phone and account metadata; do not copy it into memory unless
      the operator asked.
  gowa_device_login {action: qr|code, device_id, phone?, create_if_missing?}
      begin a pairing the operator asked for. qr writes a PNG under the home
      and answers its path -- the operator scans it from WhatsApp's Linked
      devices; (look PATH) shows it. code asks GOWA for a pairing code for an
      international phone number instead. A slot that is already linked or
      pairing is refused, and so is a number already linked to another slot.
      create_if_missing makes the slot when the server does not hold it yet.
  gowa_device_remove {device_id, confirm: true}
      purge one exact slot and its stored session, and ask WhatsApp to unlink
      it. Irreversible: ask the operator first, then call with confirm true.
/gowa with no argument lists the slots; /gowa DEVICE shows one. A QR image
and a pair code are temporary credentials for one account: show them to the
operator, never save them, never share them. The gowa section of config.jsonc
names base_url (the origin), auth_env or auth_file (the whole Basic or Bearer
Authorization value the server prints), plain_http, state_dir and
timeout_seconds. ERROR: gowa: ... means the server did not answer, or refused."
  "What (help :gowa) answers while the cell runs.")

;;; --- the tools' schemas ------------------------------------------------------------

(defun tool-schema (description properties &optional required)
  "DESCRIPTION and PROPERTIES -- (NAME OBJECT DOC) ... -- as a tool schema, as
the original wrote its three: OBJECT is the member's own JSON object."
  (list :description description
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name object doc) in properties
                                   nconc (list name (apply #'nlk:make-json-object
                                                           "description" doc object))))
         "additionalProperties" nil
         "required" (coerce required 'vector))))

(defparameter *devices-schema*
  (tool-schema
   "List the GOWA device slots, or retrieve one exact slot. The answer carries phone and account metadata, so do not store it in memory unless the operator asked."
   '(("action" ("type" "string" "enum" #("list" "get")) "list every slot, or get one by device_id")
     ("device_id" ("type" "string") "required for get: the exact slot id, no guessing"))
   '("action"))
  "gowa_devices: the original's schema, in the same shape.")

(defparameter *login-schema*
  (tool-schema
   "Begin a pairing the operator asked for. Refuses a slot that is already linked or pairing, and a number already linked to another slot. A QR file and a pair code are temporary credentials: show them to the operator only, and never save them to memory. Set create_if_missing only when provisioning a new slot."
   '(("action" ("type" "string" "enum" #("qr" "code")) "qr writes a QR image to scan; code pairs a phone number")
     ("device_id" ("type" "string") "the exact slot id to pair")
     ("phone" ("type" "string") "required for code: an international number, 7-15 digits, optional leading +")
     ("create_if_missing" ("type" "boolean") "make the slot when the server does not hold it yet; false unless said"))
   '("action" "device_id"))
  "gowa_device_login: the original's schema, in the same shape.")

(defparameter *remove-schema*
  (tool-schema
   "Permanently purge one exact GOWA device slot and its stored session, and ask WhatsApp to unlink it. Call only after the operator confirmed in this conversation, with confirm true."
   '(("device_id" ("type" "string") "the exact slot id; nothing is resolved for you")
     ("confirm" ("type" "boolean") "true, after the operator's explicit confirmation"))
   '("device_id" "confirm"))
  "gowa_device_remove: the original's schema, in the same shape.")

;;; --- the verbs ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: gowa: ~a" condition)))))

(define-verb devices-text ()
  "Every slot the server holds, one a line: what /gowa prints with no argument."
  (multiple-value-bind (payload text status) (request :get "/devices")
    (unless (ok-status-p status) (http-failure :get "/devices" status text))
    (let ((devices (json-list payload "results")))
      (with-output-to-string (out)
        (format out "GOWA ~a: ~d device slot~:p~%" (base-url) (length devices))
        (if (null devices)
            (format out "  none: pair one with gowa_device_login~%")
            (dolist (device devices) (format out "  ~a~%" (device-line device))))))))

(define-verb device-text (id)
  "Slot ID as the server holds it."
  (let ((path (format nil "/devices/~a" (uri-quote id))))
    (multiple-value-bind (payload text status) (request :get path)
      (unless (ok-status-p status) (http-failure :get path status text))
      (format nil "GOWA slot ~a: ~a~%" id (device-line (records payload))))))

(defun pair-with-qr (id created)
  "Slot ID's QR downloaded, and the words the model reads: where the image is, and
what it is for."
  (let ((path (format nil "/devices/~a/login" (uri-quote id))))
    (multiple-value-bind (payload text status) (request :get path :login t)
      (unless (ok-status-p status) (http-failure :get path status text))
      (let* ((results (records payload "login"))
             (link (field results "qr_link"))
             (seconds (nlk:json-value results :integer "qr_duration")))
        (unless (and link (plusp (length link)))
          (fail "GOWA's login answer carried no qr_link"))
        (let ((file (save-qr link id)))
          (with-output-to-string (out)
            (format out "GOWA slot ~a~:[ is pairing~; was made, and is pairing~]: open ~a in a WhatsApp ~
                         scanner (Linked devices, Link a device)~@[ within ~d seconds~].~%"
                    id created file seconds)
            (format out "The operator scans it from the intended account; (look ~s) shows the image. ~
                         It is a temporary credential: never save or share it.~%" file)))))))

(defun pair-with-code (id phone)
  "Slot ID paired with a code for PHONE, in the words the model reads."
  (let ((path (format nil "/devices/~a/login/code?phone=~a" (uri-quote id) (uri-quote phone))))
    (multiple-value-bind (payload text status) (request :post path :login t)
      (unless (ok-status-p status) (http-failure :post path status text))
      (let ((code (field (records payload "login") "pair_code")))
        (unless (and code (plusp (length code)))
          (fail "GOWA's login answer carried no pair_code"))
        (format nil "GOWA slot ~a pairs with +~a: enter ~a in WhatsApp (Linked devices, Link with ~
                     phone number). It is a temporary credential: never save or share it.~%"
                id phone code)))))

(define-verb pair-device (id &key phone create)
  "Pair slot ID, from a QR image (no PHONE) or a pairing code for PHONE, making the
slot first when CREATE says so."
  (let ((path (format nil "/devices/~a" (uri-quote id)))
        (missing nil))
    (multiple-value-bind (payload text status) (request :get path)
      (cond ((ok-status-p status) (ensure-unpaired (records payload) id))
            ((missing-device-p status text id)
             (unless create
               (fail "the server holds no device slot ~a: pass create_if_missing true to make one ~
                      first" id))
             (setf missing t))
            (t (http-failure :get path status text))))
    (when phone
      (let ((duplicate (duplicate-phone phone id)))
        (when duplicate
          (fail "the number is already linked to device ~a: pair that slot, or remove it first"
                duplicate))))
    (when missing (request :post "/devices" :body (nlk:make-json-object "device_id" id)))
    (if phone (pair-with-code id phone) (pair-with-qr id missing))))

(define-verb remove-device (id)
  "Slot ID purged from the server, and WhatsApp asked to unlink it."
  (let ((path (format nil "/devices/~a" (uri-quote id))))
    (multiple-value-bind (payload text status) (request :delete path)
      (declare (ignore payload))
      (unless (ok-status-p status) (http-failure :delete path status text))
      (format nil "GOWA: removed slot ~a; its stored session is gone and WhatsApp was asked to ~
                   unlink it. It is gone from this server: pairing it again makes a new slot.~%"
              id))))

;;; --- the tools ------------------------------------------------------------------

(defun action-of (args default)
  "ARGS' action as a lowercase word, or DEFAULT when the call left it out."
  (let ((value (gethash "action" args)))
    (if (stringp value) (string-downcase (nlk:trimmed value)) default)))

(defun tool-devices (args)
  (answering
    (let ((action (action-of args "list")))
      (cond ((string= action "list") (devices-text))
            ((string= action "get") (device-text (device-id (gethash "device_id" args))))
            (t (fail "action must be list or get, and ~s is neither" action))))))

(defun tool-device-login (args)
  (answering
    (let* ((action (action-of args "qr"))
           (id (device-id (gethash "device_id" args)))
           (create (eq t (gethash "create_if_missing" args))))
      (cond ((string= action "qr") (pair-device id :create create))
            ((string= action "code")
             (pair-device id :phone (phone-number (gethash "phone" args)) :create create))
            (t (fail "action must be qr or code, and ~s is neither" action))))))

(defun tool-device-remove (args)
  (answering
    (unless (eq t (gethash "confirm" args))
      (fail "confirm must be true after the operator's explicit approval: removing a slot is ~
             irreversible"))
    (remove-device (device-id (gethash "device_id" args)))))

;;; --- the operator's command -------------------------------------------------------

(defun run-command (args session-id)
  "One /gowa invocation: no argument lists every slot, one names a slot to show."
  (declare (ignore session-id))
  (handler-case
      (let ((argument (nlk:trimmed (or args ""))))
        (if (plusp (length argument))
            (device-text (device-id argument))
            (devices-text)))
    ((or error sb-sys:deadline-timeout) (condition)
      (nle:failure (format nil "ERROR: gowa: ~a" condition)))))

;;; --- the cell --------------------------------------------------------------------

(defun start ()
  "Refuse a base_url the cell cannot use, before a tool has to; nothing to take back."
  (handler-case (base-url)
    (gowa-error (condition)
      (error 'nlk:config-refusal :detail (princ-to-string condition)))))

(nle:define-cell gowa
  (:section ("gowa")
    (:guide "set base_url to the origin the GOWA server answers on (http://127.0.0.1:3000 for one installed on this machine); auth_env names the environment variable holding the whole Basic or Bearer Authorization value the server prints, or auth_file a file of KEY=VALUE lines holding it; then gowa_devices lists the slots, gowa_device_login pairs one from a QR or a phone code, and gowa_device_remove purges one")
    ("base_url" :string :required t
     :doc "the GOWA server's origin, such as http://127.0.0.1:3000")
    ("auth_env" :env :default "GOWA_AUTH_HEADER"
     :doc "the environment variable holding the whole Authorization value, Basic or Bearer")
    ("auth_file" :path
     :doc "a file of KEY=VALUE lines holding that value instead, as the GOWA setup writes it")
    ("plain_http" :boolean :default nil
     :doc "whether an http:// origin outside loopback is allowed: the Authorization value then travels in clear")
    ("state_dir" :path
     :doc "where a pairing QR is written: <home>/gowa/ unless named here")
    ("timeout_seconds" :integer :default 15 :min 1
     :doc "one request's ceiling; a pairing request gets 135 seconds whatever this says"))
  (:start #'start)
  (:help :gowa
         "gowa: list, pair and remove the device slots of a GOWA WhatsApp server - (help :gowa)"
         +primer+)
  (:command "gowa" 'run-command
            :description "GOWA: the WhatsApp device slots, or one of them"
            :argument-hint "[DEVICE]")
  (:tool "gowa_devices" *devices-schema* 'tool-devices)
  (:tool "gowa_device_login" *login-schema* 'tool-device-login)
  (:tool "gowa_device_remove" *remove-schema* 'tool-device-remove))
