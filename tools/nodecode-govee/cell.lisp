;;;; cell.lisp --- the one tool, its contract, the manual and /govee.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original shipped one tool, govee_lights, and one skill that told the
;;;; model how to use it without changing the wrong light. The tool is here;
;;;; the skill is +MANUAL+, which (help :govee) answers and whose one line
;;;; rides every request. Nothing hooks the provider path: no model is served.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "govee": {"api_key": "...", "api_key_env": "GOVEE_API_KEY",
;;;;             "base_url": "https://openapi.api.govee.com/router/api/v1",
;;;;             "timeout_seconds": 15}
;;;; A vetoed section ("enabled": false) installs nothing, and a section with
;;;; no key refuses at start rather than failing on the first command.

(in-package #:nodecode-govee)

;;; --- the manual ---------------------------------------------------------------

(defparameter +manual+
  "Govee lights are reachable through the nodecode-govee cell: one tool, govee_lights, on
the Govee account the API key belongs to. Govee's cloud API is the only thing it talks to.
  govee_lights  {action, device, value}
      list         every light: its name, its exact device ID, its model, and the
                   capabilities that model advertises with their ranges. Run it first.
      state        one light's power, brightness, colour, white temperature and whether the
                   device is online. Needs device.
      scenes       the light's static and dynamic scenes, with the value to send back. Needs
                   device.
      power        boolean value: true turns the light on, false turns it off.
      brightness   integer percent, inside the range that light advertises.
      color        #RRGGBB, like #FF8800.
      temperature  integer Kelvin, inside the range that light advertises.
      scene        the exact scene name the scenes action answered.
      device is an exact device ID from list, or a name that matches exactly one light
      (case-insensitively). A partial or shared name is refused, so the wrong light is never
      the one that changes.
How to work: list to discover, then name the light the person named. Only change the lights
they asked for; do not guess a target from part of a name, and never apply one change to every
light on the account. A clear request authorises its own bounded change -- a light did not
need a second confirmation to be turned off. No unsolicited schedules, effects or test flashes.
Capabilities and ranges differ by model: read them from list, never assume them. Colour
temperature is Kelvin and lower is warmer. A scene value may be an integer or an object of
{id, paramId}; the cell sends back exactly what the scene list gave it.
Every command is read back automatically: accepted means Govee took it, verified means the
state came back matching. Say a light changed only when verified is true, and when it is not,
say so plainly and do not repeat the command blindly. Govee allows 30 state and scene reads
a minute per device, so this is not a tool for animation or rapid polling; on a rate limit,
stop and wait. Results are JSON: a refusal arrives as ERROR: with what to fix.
The key lives in the govee section or in GOVEE_API_KEY; never ask for it in chat and never
put it in an argument. The operator can list the lights with /govee and read one with
/govee <name>."
  "What (help :govee) answers while the cell runs: the original's skill, kept whole.")

;;; --- the tool's arguments -------------------------------------------------------

(nlk:define-contract light-call
  (:doc "Discover and control Govee lights. Run list first: it answers each light's exact device ID, its name, and the capabilities that model advertises, which is where the ranges come from. Then name one light and one change: power, brightness, color, temperature or scene. A command is checked against that light's own capabilities before it is sent, and its state is read back afterwards, so accepted (Govee took it) is reported apart from verified (the state shows it). Change only the lights the person asked for.")
  ("action" :text :required t
   :options '("list" "state" "scenes" "power" "brightness" "color" "temperature" "scene")
   :doc "what to do: list, state, scenes, power, brightness, color, temperature or scene")
  ("device" :string :default ""
   :doc "the light to act on: an exact device ID from list, or a name that matches exactly one light; not needed for list")
  ("value" :any
   :default :missing
   :doc "the change: true or false for power, an integer percent for brightness, #RRGGBB for color, an integer Kelvin for temperature, a scene name for scene; not needed for list, state or scenes"))

;;; --- the section ---------------------------------------------------------------

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's values, with the key
resolved out of its variable, and the endpoint's own default."
  (declare (ignore table))
  (list :api-key (or (nonempty (getf values :api-key))
                     (let ((variable (nonempty (getf values :api-key-env))))
                       (and variable (nonempty (uiop:getenv variable))))
                     (nonempty (uiop:getenv "GOVEE_API_KEY")))
        :api-key-env (getf values :api-key-env)
        :base-url (or (nonempty (getf values :base-url)) +base-url+)
        :timeout-seconds (getf values :timeout-seconds)))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell govee
  (:section ("govee")
    (:guide "make a Govee developer API key in the Govee Home app under Profile > Settings > Apply for API Key, then export it as GOVEE_API_KEY or paste it in api_key here; next step: /govee lists the lights that key can see")
    ("api_key" :secret
     :doc "the Govee developer API key (Govee emails it to the account)")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (GOVEE_API_KEY is read either way)")
    ("base_url" :string
     :doc "the API root: https://openapi.api.govee.com/router/api/v1 unless you go through a proxy of your own")
    ("timeout_seconds" :integer :default 15 :min 1
     :doc "one Govee request's ceiling, in seconds; a command adds up to two state read-backs"))
  (:settings #'read-settings)
  (:help :govee
         "govee: govee_lights finds and controls Govee lights, checking every change by a state read-back"
         +manual+)
  (:tool "govee_lights" (nlk:contract 'light-call) #'answer-call)
  (:command "govee" 'run-command
            :description "Govee: the lights, or one light's state"
            :argument-hint "LIGHT")
  (:start (lambda ()
            (check-config))))
