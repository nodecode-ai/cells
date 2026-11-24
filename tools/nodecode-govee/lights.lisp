;;;; lights.lisp --- the lights, what each one advertises, and one command.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original's own rules, kept whole: a light is found by exact device ID
;;;; or by a name that matches exactly one light; a command is checked against
;;;; what that model advertises -- the capability must exist, the value must
;;;; sit inside the device's own range, a scene name must match one scene --
;;;; and only then sent. Afterwards the state is read back twice, and the
;;;; answer says "accepted" and "verified" apart, because Govee taking a
;;;; command is not the same as the light having changed.

(in-package #:nodecode-govee)

(defparameter +actions+
  '(("power"       "devices.capabilities.on_off"          "powerSwitch")
    ("brightness"  "devices.capabilities.range"           "brightness")
    ("color"       "devices.capabilities.color_setting"   "colorRgb")
    ("temperature" "devices.capabilities.color_setting"   "colorTemperatureK")
    ("scene"       "devices.capabilities.dynamic_scene"   "lightScene"))
  "Each action as (ACTION CAPABILITY-TYPE INSTANCE): what a light has to
advertise before the action is sent, exactly as the original mapped them.")

(defparameter +light-type+ "devices.types.light"
  "The device type this cell speaks for. Heaters, plugs and the rest of
Govee's appliances are deliberately left out, as in the original.")

(defparameter +max-target-chars+ 256
  "How long a name or device ID the model may send may be.")

;;; --- what a light is ------------------------------------------------------------

(defun light-device (light)
  "The light's own device ID, the one a call names it by."
  (nlk:json-value light :string "device"))

(defun light-name (light)
  "The name the person gave it in the Govee Home app, or NIL."
  (nlk:json-value light :string "deviceName"))

(defun light-sku (light)
  "The model number, which Govee wants beside the device ID in every call."
  (nlk:json-value light :string "sku"))

(defun light-label (light)
  "What a refusal calls this light: its name, else its id."
  (or (light-name light) (light-device light) "that light"))

(defun light-capabilities (light)
  "The capabilities the account's own answer lists for LIGHT, as a list."
  (coerce (nlk:json-array light "capabilities") 'list))

(defun capability-p (capability type instance)
  "Whether CAPABILITY is the one for (TYPE, INSTANCE)."
  (and (hash-table-p capability)
       (equal type (nlk:json-value capability :string "type"))
       (equal instance (nlk:json-value capability :string "instance"))))

;;; --- discovery ------------------------------------------------------------------

(defun light-list ()
  "Every light on the account, as the answer's own device objects: names,
ids, model numbers and the capabilities each one advertises."
  (let* ((answer (request "/user/devices"))
         (data (nlk:json-value answer :array "data")))
    (unless data
      (fail "govee: the answer carried no device list; is this key an account's? (/govee shows what is set)"))
    (remove-if-not (lambda (device)
                     (and (hash-table-p device)
                          (equal +light-type+ (nlk:json-value device :string "type"))))
                   (coerce data 'list))))

(defun resolve-light (target)
  "TARGET -- an exact device ID, or a name that matches one light -- as that
light's device object. Nothing guesses from a substring, as in the original:
a partial or shared name is refused rather than applied to the wrong light."
  (let ((target (nlk:trimmed (or target ""))))
    (when (or (zerop (length target)) (> (length target) +max-target-chars+))
      (fail "govee: name the light by its exact device ID, or by the name the list action answers, in at most ~d characters"
            +max-target-chars+))
    (let* ((lights (light-list))
           (by-id (remove-if-not (lambda (light) (equal target (light-device light))) lights))
           (matches (or by-id
                        (remove-if-not (lambda (light)
                                         (let ((name (light-name light)))
                                           (and name (string-equal target name))))
                                       lights))))
      (cond ((null matches)
             (fail "govee: no light is called ~s; run the list action to see the names and exact ids" target))
            ((rest matches)
             (fail "govee: ~s names more than one light; use the exact device ID from list" target))
            (t (first matches))))))

(defun advertised-capability (light type instance)
  "The one capability LIGHT advertises for (TYPE, INSTANCE)."
  (let ((matches (remove-if-not (lambda (capability) (capability-p capability type instance))
                                 (light-capabilities light))))
    (cond ((null matches)
           (fail "govee: ~a does not support ~a; its capabilities are in the list answer"
                 (light-label light) instance))
          ((rest matches)
           (fail "govee: ~a advertises ~a more than once; refusing to guess"
                 (light-label light) instance))
          (t (first matches)))))

(defun device-payload (light endpoint)
  "One of the light's own endpoints (/device/state, /device/scenes) => its
decoded payload object."
  (let* ((answer (request endpoint
                           (nlk:json-object
                            "requestId" (request-id)
                            "payload" (nlk:json-object "sku" (light-sku light)
                                                        "device" (light-device light)))))
         (payload (nlk:json-value answer :object "payload")))
    (or payload
        (fail "govee: ~a answered with no payload for ~a" endpoint (light-label light)))))

(defun light-state (light)
  "The light's current state: power, brightness, colour, white temperature and
whether it is online."
  (device-payload light "/device/state"))

(defun scene-options (light)
  "Every scene the light offers, static and dynamic together, each kept once
and with the value Govee returned -- an integer, or an object of id and
paramId, which the control call sends back exactly as it came."
  (let ((static (remove-if-not (lambda (capability)
                                  (capability-p capability "devices.capabilities.dynamic_scene"
                                                 "lightScene"))
                                (light-capabilities light)))
        (dynamic (coerce (nlk:json-array (device-payload light "/device/scenes") "capabilities") 'list))
        (options '()))
    (dolist (capability (append static dynamic))
      (when (capability-p capability "devices.capabilities.dynamic_scene" "lightScene")
        (dolist (option (coerce (nlk:json-array capability "parameters" "options") 'list))
          (unless (member option options :test #'same-value-p)
            (setf options (append options (list option)))))))
    options))

;;; --- one value, as the wire carries it -------------------------------------------

(defun scalar-json-p (value)
  "Whether VALUE is a JSON scalar, which EQUAL already compares."
  (or (null value) (eq value t) (stringp value) (numberp value)))

(defun same-value-p (one two)
  "Whether ONE and TWO are the same JSON value: scalars by EQUAL, objects and
arrays by the encoding, which is the only reading the wire has of them."
  (if (and (scalar-json-p one) (scalar-json-p two))
      (equal one two)
      (equal (nlk:encode-json-object (nlk:json-object "value" one))
             (nlk:encode-json-object (nlk:json-object "value" two)))))

(defun value-words (value)
  "VALUE as a refusal names it, saying so when the call left it out."
  (cond ((eq value :missing) "nothing (value is missing)")
        ((null value) "false")
        ((eq value t) "true")
        (t (format nil "~s" value))))

(defun colour-p (text)
  "Whether TEXT is #RRGGBB, the one colour spelling the original took."
  (and (stringp text)
       (= (length text) 7)
       (char= (char text 0) #\#)
       (every (lambda (character) (digit-char-p character 16)) (subseq text 1))))

(defun wire-value (light action value)
  "VALUE for ACTION as Govee wants it on the wire, or a refusal naming what
the call should have sent. Nothing is coerced: a scene keeps the exact value
the scene list answered with."
  (cond ((string= action "power")
         (cond ((eq value t) 1)
               ((null value) 0)
               ((eq value :missing)
                (fail "govee: power needs a value: true to turn the light on, false to turn it off"))
               (t (fail "govee: power takes true or false, not ~a" (value-words value)))))
        ((string= action "color")
         (unless (colour-p value)
           (fail "govee: color takes #RRGGBB, like #FF8800, not ~a" (value-words value)))
         (parse-integer (subseq value 1) :radix 16))
        ((string= action "scene")
         (unless (and (stringp value) (plusp (length value)) (<= (length value) +max-target-chars+))
           (fail "govee: scene takes the exact name the scenes action answers, not ~a" (value-words value)))
         (let ((matches (remove-if-not (lambda (option)
                                          (let ((name (nlk:json-value option :string "name")))
                                            (and name (string-equal value name))))
                                        (scene-options light))))
           (cond ((null matches)
                  (fail "govee: ~a has no scene called ~s; run the scenes action first"
                        (light-label light) value))
                 ((rest matches)
                  (fail "govee: more than one scene on ~a is called ~s; refusing to guess"
                        (light-label light) value))
                 (t (nlk:json-value (first matches) :any "value")))))
        (t (unless (integerp value)
             (fail "govee: ~a takes a whole number inside the light's own range, not ~a"
                   action (value-words value)))
           value)))

(defun advertised-range (capability)
  "The (MIN . MAX) the capability advertises, or NIL."
  (let* ((range (nlk:json-value capability :object "parameters" "range"))
         (low (and range (nlk:json-value range :integer "min")))
         (high (and range (nlk:json-value range :integer "max"))))
    (and low high (cons low high))))

(defun check-range (capability action value)
  "Refuse VALUE unless the light itself advertised it inside its range."
  (let ((range (advertised-range capability)))
    (unless range
      (fail "govee: this light advertised no ~a range; refusing to send ~a" action value))
    (unless (<= (car range) value (cdr range))
      (fail "govee: ~a must be between ~d and ~d on this light, not ~d"
            action (car range) (cdr range) value))))

(defun check-power-option (capability value)
  "Refuse a power value the light did not advertise."
  (let ((options (coerce (nlk:json-array capability "parameters" "options") 'list)))
    (unless (some (lambda (option) (eql value (nlk:json-value option :integer "value"))) options)
      (fail "govee: this light did not advertise power value ~a; it may not switch" value))))

;;; --- one command, and its read-back ----------------------------------------------

(defun send-control (light capability value)
  "One POST to /device/control: the capability's type and instance with VALUE."
  (request "/device/control"
           (nlk:json-object
            "requestId" (request-id)
            "payload" (nlk:json-object
                       "sku" (light-sku light)
                       "device" (light-device light)
                       "capability" (nlk:json-object
                                     "type" (nlk:json-value capability :string "type")
                                     "instance" (nlk:json-value capability :string "instance")
                                     "value" value)))))

(defun offline-p (capability)
  "Whether the light's own state says this device is offline."
  (let ((state (nlk:json-value capability :object "state")))
    (and (equal "online" (nlk:json-value capability :string "instance"))
         state
         (multiple-value-bind (value present) (gethash "value" state)
           (and present (null value) t)))))

(defun state-verifies-p (state type instance wire)
  "Whether STATE shows the light online and (TYPE, INSTANCE) at WIRE."
  (let ((capabilities (coerce (nlk:json-array state "capabilities") 'list)))
    (and (notany #'offline-p capabilities)
         (some (lambda (capability)
                 (and (capability-p capability type instance)
                      (same-value-p wire (nlk:json-value capability :any "state" "value"))))
               capabilities)
         t)))

(defun read-back (light type instance wire)
  "The answer a control call reports: the command accepted, and whether two
state reads about two seconds apart saw it land. A read that fails is
reported as one, never as a verification."
  (let ((result (nlk:json-object
                 "ok" t
                 "accepted" t
                 "verified" nil
                 "device" (light-device light)
                 :opt "deviceName" (light-name light)
                 "requested" (nlk:json-object "type" type "instance" instance "value" wire))))
    (loop repeat 2
          do (funcall *sleeper* +read-back-delay+)
             (let ((state nil))
               (handler-case (setf state (light-state light))
                 (govee-error (condition)
                   (setf (gethash "verification_error" result) (govee-error-detail condition))
                   (return)))
               (setf (gethash "state" result) state)
               (setf (gethash "verified" result)
                     (and (state-verifies-p state type instance wire) t))
               (when (gethash "verified" result) (return))))
    (unless (gethash "verified" result)
      (setf (gethash "note" result)
            (concatenate 'string
                         "Govee accepted the command, but the state read-back did not confirm it: "
                         "do not say the light changed, and do not resend it on your own.")))
    result))

(defun control (target action value)
  "One light command: find the light, check what it advertises, send, read back."
  (let* ((entry (assoc action +actions+ :test #'string=))
         (type (second entry))
         (instance (third entry))
         (light (resolve-light target))
         (capability (advertised-capability light type instance))
         (wire (wire-value light action value)))
    (when (member action '("brightness" "color" "temperature") :test #'string=)
      (check-range capability action wire))
    (when (string= action "power")
      (check-power-option capability wire))
    (send-control light capability wire)
    (read-back light type instance wire)))

;;; --- what the model and the operator read ----------------------------------------

(defun answer-light (light)
  "LIGHT as the fields an answer names it by."
  (nlk:json-object "device" (light-device light) :opt "deviceName" (light-name light)))

(defun answer-list ()
  "The list action's answer: every light, whole, with the capabilities each
one advertises -- what the model needs to send a value that light accepts."
  (nlk:json-object "ok" t "lights" (coerce (light-list) 'vector)))

(defun answer-state (target)
  "The state action's answer: one light's current state."
  (let ((light (resolve-light target)))
    (nlk:json-object
     "ok" t
     "device" (light-device light)
     :opt "deviceName" (light-name light)
     "state" (light-state light))))

(defun answer-scenes (target)
  "The scenes action's answer: the names and the values to send back."
  (let ((light (resolve-light target)))
    (nlk:json-object
     "ok" t
     "device" (light-device light)
     :opt "deviceName" (light-name light)
     "scenes" (coerce (scene-options light) 'vector))))

(defun answer-call (call)
  "The govee_lights tool: one action, answered as JSON, or the refusal in the
one line the model repairs from."
  (answering
    (let ((action (light-call-action call)))
      (encode
       (cond ((string= action "list") (answer-list))
             ((string= action "state") (answer-state (light-call-device call)))
             ((string= action "scenes") (answer-scenes (light-call-device call)))
             (t (control (light-call-device call) action (light-call-value call))))))))

(defun lights-words ()
  "The lights, one line each, for the operator's command."
  (let ((lights (light-list)))
    (if (null lights)
        "govee: this account has no lights the key can see."
        (format nil "Govee lights (~d):~%~{  ~a~%~}~%Name one to read its state: /govee <name>"
                (length lights)
                (mapcar (lambda (light)
                          (format nil "~a  ~a  ~a"
                                  (or (light-name light) "(no name)")
                                  (or (light-sku light) "?")
                                  (light-device light)))
                        lights)))))

(defun state-language (capability)
  "One capability's state, in the words the operator reads."
  (let ((instance (nlk:json-value capability :string "instance"))
        (value (nlk:json-value capability :any "state" "value")))
    (format nil "  ~a  ~a" instance
            (cond ((and (equal instance "colorRgb") (integerp value))
                   (format nil "#~6,'0x" value))
                  ((null value) "false")
                  (t value)))))

(defun state-words (light)
  "One light's state, in the lines the operator reads."
  (format nil "~a  ~a~%~{~a~%~}"
          (light-label light) (or (light-sku light) "")
          (mapcar #'state-language
                  (coerce (nlk:json-array (light-state light) "capabilities") 'list))))

(defun run-command (args session-id)
  "The /govee command: with no argument the lights and their ids, with one
light named, that light's state."
  (declare (ignore session-id))
  (let ((target (nlk:trimmed (or args ""))))
    (handler-case
        (if (plusp (length target))
            (state-words (resolve-light target))
            (lights-words))
      (govee-error (condition)
        (format nil "govee: ~a" (govee-error-detail condition)))
      (error (condition)
        (format nil "govee: ~a" (nlk:one-line (princ-to-string condition)))))))
