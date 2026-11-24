;;;; cell-test.lisp --- lifecycle, govee_lights over a stubbed wire, and /govee.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every request is a stubbed NLK:HTTP and every wait is a stub too, so
;;;; nothing reaches Govee, needs a key or sleeps. What is proved is the cell's
;;;; own wiring: the section (a healthy start, a wrong value refused, a missing
;;;; key refused), the one tool through the seam the model calls it by -- its
;;;; method, url, header and body, and the answer it hands back -- and the
;;;; operator's command.

(in-package :nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *routes* '()
  "The stubbed answers, newest first: (URL-FRAGMENT ANSWER STATUS).")

(defvar *sleeps* '()
  "The waits a read-back asked for, newest first: no test sleeps for real.")

(defun govee-answer-for (url)
  "The stubbed answer for URL: the newest route whose fragment it carries, so
a later Govee-ANSWERS for one endpoint replaces what an earlier one said."
  (loop for (fragment answer status) in *routes*
        when (search fragment url) do (return (values answer status))
        finally (error "no stubbed answer for ~a" url)))

(defun govee-answers (fragment answer &optional (status 200))
  "Answer every later request whose url carries FRAGMENT with ANSWER at STATUS."
  (push (list fragment answer status) *routes*)
  answer)

(defmacro with-govee-wire (&body body)
  "BODY with NLK:HTTP stubbed and the read-back's wait stubbed, so no test
dials Govee or spends the two seconds a command waits."
  `(let ((*requests* '())
         (*routes* '())
         (*sleeps* '())
         (nodecode-govee::*sleeper* (lambda (seconds) (push seconds *sleeps*))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (govee-answer-for url))
       ,@body)))

(defun govee-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun govee-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun govee-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun govee-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "govee-call" name arguments)))

;;; --- the fixtures' JSON --------------------------------------------------------

(defparameter +desk+ "AA:BB:CC:DD:EE:01"
  "The device id of the light every test talks to.")

(defparameter +strip+ "AA:BB:CC:DD:EE:02"
  "The device id of the second light, so a name match has something to pick.")

(defun capability (type instance &key range options)
  "One capability as a device answers it."
  (nlk:json-object
   "type" type
   "instance" instance
   "parameters" (nlk:json-object
                 :opt "range" (and range (nlk:json-object "min" (car range) "max" (cdr range)))
                 :opt "options" (and options (coerce options 'vector)))))

(defun light (device name &rest capabilities)
  "One light as the device list answers it."
  (nlk:json-object "sku" "H6001" "device" device "deviceName" name
                    "type" "devices.types.light"
                    "capabilities" (coerce capabilities 'vector)))

(defun plug (device name)
  "A device the account has that is not a light."
  (nlk:json-object "sku" "H5080" "device" device "deviceName" name
                    "type" "devices.types.socket" "capabilities" #()))

(defun aurora ()
  "One static scene, whose value is the object Govee returns for it."
  (nlk:json-object "name" "Aurora" "value" (nlk:json-object "id" 7 "paramId" 3)))

(defun power-option (value)
  (nlk:json-object "name" (if (zerop value) "Off" "On") "value" value))

(defun desk-capabilities (&key (power-options '(0 1)) (brightness '(1 . 100)) (scenes t))
  "What the desk lamp advertises: the capabilities every action is checked against."
  (remove nil
          (list (capability "devices.capabilities.on_off" "powerSwitch"
                             :options (mapcar #'power-option power-options))
                (capability "devices.capabilities.range" "brightness" :range brightness)
                (capability "devices.capabilities.color_setting" "colorRgb" :range '(0 . 16777215))
                (capability "devices.capabilities.color_setting" "colorTemperatureK" :range '(2000 . 9000))
                (and scenes (capability "devices.capabilities.dynamic_scene" "lightScene"
                                        :options (list (aurora)))))))

(defun desk (&rest capabilities)
  "The desk lamp, with CAPABILITIES or the ones every test uses."
  (apply #'light +desk+ "Desk Lamp" (or capabilities (desk-capabilities))))

(defun strip ()
  "A second light, so a name and an id are two different answers."
  (light +strip+ "Shelf Strip" (capability "devices.capabilities.range" "brightness"
                                            :range '(1 . 100))))

(defun device-answer (&rest devices)
  "The account's device list, as Govee answers it."
  (nlk:encode-json-object (nlk:json-object "code" 200 "message" "success"
                                            "data" (coerce devices 'vector))))

(defun state-capability (type instance value)
  "One capability's reported state, under the capability type it belongs to."
  (nlk:json-object "type" type "instance" instance
                    "state" (nlk:json-object "value" value)))

(defun state-answer (&key power brightness color kelvin scene (online t))
  "One light's state, as Govee answers it, carrying only what the call named."
  (nlk:encode-json-object
   (nlk:json-object
    "code" 200 "msg" "success"
    "payload" (nlk:json-object
               "sku" "H6001" "device" +desk+
               "capabilities"
               (coerce (remove nil
                               (list (state-capability "devices.capabilities.on_off" "online" online)
                                     (and power (state-capability "devices.capabilities.on_off"
                                                                  "powerSwitch" power))
                                     (and brightness (state-capability "devices.capabilities.range"
                                                                       "brightness" brightness))
                                     (and color (state-capability "devices.capabilities.color_setting"
                                                                  "colorRgb" color))
                                     (and kelvin (state-capability "devices.capabilities.color_setting"
                                                                   "colorTemperatureK" kelvin))
                                     (and scene (state-capability "devices.capabilities.dynamic_scene"
                                                                  "lightScene" scene))))
                       'vector)))))

(defun scenes-answer (&rest options)
  "The light's dynamic scenes, as Govee answers them."
  (nlk:encode-json-object
   (nlk:json-object
    "code" 200 "msg" "success"
    "payload" (nlk:json-object
               "sku" "H6001" "device" +desk+
               "capabilities" (vector (nlk:json-object
                                        "type" "devices.capabilities.dynamic_scene"
                                        "instance" "lightScene"
                                        "parameters" (nlk:json-object
                                                      "options" (coerce options 'vector))))))))

(defparameter +control-ok+ "{\"code\":200,\"msg\":\"success\",\"payload\":{}}"
  "What Govee answers a command it took.")

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "govee" "GOVEE-CELL-" :start nodecode-govee:start-cell)

(define-cell-lifecycle-tests "govee"
  (:fixture with-govee-wire)
  (:config "api_key" "gv-test")
  (:help :govee)
  (:command "govee")
  (:running
   (is (nle::find-tool "govee_lights") "the original's one tool is registered")
   (is (= 1 (count-if (lambda (entry) (eql 0 (search "govee_" (first entry)))) nle::*tools*)) "one tool, and no others"))
  (:stopped
   (is (null (nle::find-tool "govee_lights")) "and stop takes it back"))
  (:refused ("timeout_seconds" 0)))

(deftest govee-cell-a-start-with-no-key-refuses-and-installs-nothing (with-govee-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignorable name)) nil)
    (let ((text (refusal-text nlk:config-refusal (govee-start))))
      (is-carrying text "needs a Govee developer API key" "GOVEE_API_KEY")
      (is (null (nle::find-tool "govee_lights")) "nothing was installed"))))

(deftest govee-cell-the-key-may-come-from-the-environment (with-govee-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (if (equal name "GOVEE_API_KEY") "gv-env" nil))
    (with-cell-stop ((govee-start))
      (govee-answers "/user/devices" (device-answer (desk)))
      (govee-call "govee_lights" "{\"action\": \"list\"}")
      (is (equal "gv-env" (govee-request-header (first (govee-asked)) "govee-api-key")) "the exported key is the one sent"))))

;;; --- list, state, scenes --------------------------------------------------------

(deftest govee-cell-list-answers-the-lights-and-what-they-advertise (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk) (strip) (plug "PLUG-1" "Heater Plug")))
    (let ((answer (govee-call "govee_lights" "{\"action\": \"list\"}")))
      (let ((request (first (govee-asked))))
        (is (eq :get (first request)) "the device list is one GET")
        (is (equal "https://openapi.api.govee.com/router/api/v1/user/devices" (second request)) "at the account's own device endpoint")
        (is (equal "gv-test" (govee-request-header request "govee-api-key")) "with the key in Govee's header"))
      (is (search "Desk Lamp" answer) "the light's name reaches the model")
      (is (search +desk+ answer) "with the exact device id a call names it by")
      (is (search "colorTemperatureK" answer) "and the capability the value has to fit")
      (is (null (search "Heater Plug" answer)) "a plug is not a light, so it is left out"))))

(deftest govee-cell-state-finds-a-light-by-name-and-reads-it (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk) (strip)))
    (govee-answers "/device/state" (state-answer :power 1 :brightness 40))
    (let ((answer (govee-call "govee_lights" "{\"action\": \"state\", \"device\": \"desk lamp\"}")))
      (let ((state (second (govee-asked))))
        (is (eq :post (first state)) "the state read is one POST")
        (is (equal +desk+ (nlk:json-value (govee-request-body state) :string "payload" "device")) "naming the device the name resolved to, case-insensitively"))
      (is (search "brightness" answer) "the state comes back whole")
      (is (search "40" answer) "with the values the light reported"))))

(deftest govee-cell-a-shared-name-is-refused-not-guessed (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk) (light +strip+ "Desk Lamp")))
    (let ((answer (govee-call "govee_lights" "{\"action\": \"state\", \"device\": \"Desk Lamp\"}")))
      (is (search "ERROR" answer) "a name two lights answer to is a refusal")
      (is (search "more than one light" answer) "saying exactly that")
      (is (= 1 (length (govee-asked))) "and nothing was sent to either light"))))

(deftest govee-cell-scenes-answer-static-and-dynamic-together (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk)))
    (govee-answers "/device/scenes" (scenes-answer (nlk:json-object "name" "Sunrise" "value" 12)))
    (let ((answer (govee-call "govee_lights" "{\"action\": \"scenes\", \"device\": \"Desk Lamp\"}")))
      (is (search "Aurora" answer) "the static scene the capability advertised")
      (is (search "Sunrise" answer) "and the dynamic one the scenes endpoint added")
      (is (search "paramId" answer) "with the value to send back, exactly as it came"))))

;;; --- one command, and its read-back ---------------------------------------------

(deftest govee-cell-brightness-is-checked-then-read-back (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk)))
    (govee-answers "/device/control" +control-ok+)
    (govee-answers "/device/state" (state-answer :brightness 40))
    (let ((answer (govee-call "govee_lights"
                               "{\"action\": \"brightness\", \"device\": \"Desk Lamp\", \"value\": 40}")))
      (let* ((control (second (govee-asked)))
             (body (govee-request-body control)))
        (is (eq :post (first control)) "one POST to the control endpoint")
        (is (equal "devices.capabilities.range"
                   (nlk:json-value body :string "payload" "capability" "type")) "carrying the capability the light advertised")
        (is (= 40 (nlk:json-value body :integer "payload" "capability" "value")) "and the value asked for")
        (is (stringp (nlk:json-value body :string "requestId")) "under a fresh request id"))
      (is (search "accepted" answer) "the answer says Govee took it")
      (is (search "true" answer) "and that the read-back matched")
      (is (equal '(2.1) *sleeps*) "after one wait of 2.1 seconds, inside Govee's rate limit"))))

(deftest govee-cell-a-value-outside-the-range-never-leaves (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk)))
    (let ((answer (govee-call "govee_lights"
                               "{\"action\": \"brightness\", \"device\": \"Desk Lamp\", \"value\": 250}")))
      (is (search "ERROR" answer) "a value the light never advertised is refused")
      (is (search "between 1 and 100" answer) "naming the light's own range")
      (is (= 1 (length (govee-asked))) "and nothing was sent")
      (is (null *sleeps*) "and nothing was waited for"))))

(deftest govee-cell-a-colour-is-one-hex-number-on-the-wire (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk)))
    (govee-answers "/device/control" +control-ok+)
    (govee-answers "/device/state" (state-answer :color 16746496))
    (let ((answer (govee-call "govee_lights"
                               "{\"action\": \"color\", \"device\": \"Desk Lamp\", \"value\": \"#FF8800\"}")))
      (is (= 16746496 (nlk:json-value (govee-request-body (second (govee-asked)))
                                       :integer "payload" "capability" "value")) "#RRGGBB is sent as the integer Govee reads")
      (is (search "true" answer) "and the read-back confirms the colour"))
    (govee-answers "/user/devices" (device-answer (desk)))
    (let ((answer (govee-call "govee_lights"
                               "{\"action\": \"color\", \"device\": \"Desk Lamp\", \"value\": \"orange\"}")))
      (is (search "#RRGGBB" answer) "a colour that is not #RRGGBB is refused in the words to repair from"))))

(deftest govee-cell-power-false-reaches-the-wire-as-zero (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk)))
    (govee-answers "/device/control" +control-ok+)
    (govee-answers "/device/state" (state-answer :power 0))
    (let ((answer (govee-call "govee_lights"
                               "{\"action\": \"power\", \"device\": \"Desk Lamp\", \"value\": false}")))
      (let ((body (govee-request-body (second (govee-asked)))))
        (is (equal "powerSwitch" (nlk:json-value body :string "payload" "capability" "instance")) "the power capability")
        (is (eql 0 (nlk:json-value body :integer "payload" "capability" "value")) "false is Govee's zero, not a missing value"))
      (is (search "true" answer) "and the dark light is what the read-back shows"))))

(deftest govee-cell-a-scene-sends-back-the-value-it-was-given (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk)))
    (govee-answers "/device/scenes" (scenes-answer))
    (govee-answers "/device/control" +control-ok+)
    (govee-answers "/device/state" (state-answer :scene (nlk:json-object "id" 7 "paramId" 3)))
    (let ((answer (govee-call "govee_lights"
                               "{\"action\": \"scene\", \"device\": \"Desk Lamp\", \"value\": \"aurora\"}")))
      (let ((body (govee-request-body (third (govee-asked)))))
        (is (= 7 (nlk:json-value body :integer "payload" "capability" "value" "id")) "the scene's own object goes back exactly as it came, not its name"))
      (is (search "true" answer) "and a scene read-back is compared whole"))))

(deftest govee-cell-an-unconfirmed-read-back-is-not-a-change (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk)))
    (govee-answers "/device/control" +control-ok+)
    (govee-answers "/device/state" (state-answer :brightness 30))
    (let ((answer (govee-call "govee_lights"
                               "{\"action\": \"brightness\", \"device\": \"Desk Lamp\", \"value\": 40}")))
      (is (search "accepted" answer) "Govee took the command")
      (is (search "did not confirm" answer) "but the read-back did not confirm it, and the answer says so")
      (is (= 2 (length *sleeps*)) "after two reads, and no more"))))

(deftest govee-cell-a-refused-key-and-a-refused-code-are-the-model-s-repair (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" "{\"message\": \"unauthorized\"}" 401)
    (let ((answer (govee-call "govee_lights" "{\"action\": \"list\"}")))
      (is (search "ERROR" answer) "a refused key is the tool result, not a condition up the stack")
      (is (search "HTTP 401" answer) "naming the status")
      (is (search "api_key" answer) "and where a usable key goes"))
    (govee-answers "/user/devices" "{\"code\": 400, \"msg\": \"device offline\"}")
    (let ((answer (govee-call "govee_lights" "{\"action\": \"list\"}")))
      (is (search "code 400" answer) "a 200 whose own code is not 200 is a refusal too")
      (is (search "device offline" answer) "in Govee's own words"))))

;;; --- the operator's command -----------------------------------------------------

(deftest govee-cell-the-command-lists-the-lights-and-reads-one (with-govee-wire)
  (with-cell-stop ((govee-start "api_key" "gv-test"))
    (govee-answers "/user/devices" (device-answer (desk)))
    (let ((listing (nodecode-govee::run-command "" nil)))
      (is (search "Desk Lamp" listing) "no argument lists the lights")
      (is (search +desk+ listing) "with the id a name has to be told by")
      (is (search "/govee" listing) "and says how to read one"))
    (govee-answers "/device/state" (state-answer :power 1 :brightness 40 :kelvin 2700))
    (let ((state (nodecode-govee::run-command "Desk Lamp" nil)))
      (is (search "brightness" state) "a named light reports its state")
      (is (search "2700" state) "including the white temperature"))))
