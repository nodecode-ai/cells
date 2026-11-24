;;;; cell-test.lisp --- the cell's own wiring, offline.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here: the config surface (a missing token and a plain
;;;; http address to a public host each refuse, and a vetoed section installs
;;;; nothing), the seven tools answering through a stubbed ChorePilot (the
;;;; handshake, the arguments that reach it, the confirmation note, the
;;;; ok:false answer a refusal takes), /chorepilot, and the manual.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "chorepilot"
  (:fixture with-stubbed-house)
  (:config "token_file" *token-file*)
  (:help :chorepilot)
  (:command "chorepilot")
  (:running
   (is (= 7 (length (registered-tools))) "the seven tools are registered")
   (is (equal '("initialize" "notifications/initialized" "tools/list")
              (reverse (mapcar #'first *requests*))) "the start opens MCP, says it is initialized, then looks at the house")
   (is (search "set_light" (house-description "chorepilot_call")) "the house's own tools are named in chorepilot_call's description")
   (is (stringp nodecode-chorepilot::*session*) "one session, kept"))
  (:stopped
   (is (null (nle::find-tool "chorepilot_call")) "stop unregisters the tools")
   (is (null nodecode-chorepilot::*session*) "and forgets the session"))
  (:refused
   ;; neither token_env nor token_file: the one-of refuses
   ("url" "https://house.test/mcp")
   ;; a plain http address that is not this machine or a private network
   ("token_file" *token-file* "url" "http://house.example.com/mcp")))

;;; --- the tools, through the house ------------------------------------------------

(deftest chorepilot-cell-status-and-chores-reach-the-house (with-stubbed-house)
  (with-cell-stop ((chorepilot-start "token_file" *token-file*))
    (let ((status (call-tool "chorepilot_status" "appliance" "dishwasher")))
      (is (search "\"ok\":true" status) (format nil "the house answered: ~a" status))
      (is (search "dishwasher" status) "the device asked about reached the house"))
    (let ((board (call-tool "chorepilot_chores" "scope" "today")))
      (is (search "\"scope\":\"today\"" board) "the board's scope reached the house"))
    (let ((who (call-tool "chorepilot_who_has" "chore" "Dishes")))
      (is (search "Dishes" who) "who_has was called with the chore"))
    (let ((done (call-tool "chorepilot_done" "chore" "Dishes" "person" "Sam")))
      (is (search "complete_chore" done) "done calls complete_chore"))))

(deftest chorepilot-cell-tools-lists-the-house (with-stubbed-house)
  (with-cell-stop ((chorepilot-start "token_file" *token-file*))
    (let ((listed (call-tool "chorepilot_tools")))
      (is (search "set_light" listed) "a device tool is listed")
      (is (search "\"read_only\":true" listed) "and its read-only hint")
      (is (search "\"arguments\"" listed) "with its arguments"))))

(deftest chorepilot-cell-call-merges-arguments-and-carries-the-confirmation (with-stubbed-house)
  (with-cell-stop ((chorepilot-start "token_file" *token-file*))
    ;; the target tool's arguments may sit beside `tool', as small models do
    (let ((answer (call-tool "chorepilot_call" "tool" "set_light"
                                               "device" "living_room_light" "action" "turn_on")))
      (is (search "living_room_light" answer) "a flat argument reaches the house")
      (is (search "\"action\":\"turn_on\"" answer) "and so does the action"))
    (let ((answer (call-tool "chorepilot_call" "tool" "start_dishwasher"
                                               "arguments" "{\"program\":\"eco\"}")))
      (is (search "confirmed by the person (telegram)" answer) "a water action's confirmation becomes the note the model reports"))
    (let ((answer (call-tool "chorepilot_call" "tool" "set_light"
                                               "arguments" "{\"device\":\"kitchen\"}"
                                               "device" "hall")))
      (is (search "\"device\":\"kitchen\"" answer) "the arguments object wins over a flat member"))))

(deftest chorepilot-cell-a-refusal-is-an-ok-false-answer (with-stubbed-house)
  (with-cell-stop ((chorepilot-start "token_file" *token-file*))
    (is (search "say which ChorePilot tool" (call-tool "chorepilot_call")) "no tool named: the model is told how to find one")
    (is (search "needs `chore'" (call-tool "chorepilot_done")) "a required argument missing is answered, never signalled")
    (is (search "needs `chore'" (call-tool "chorepilot_who_has")))
    ;; the house refuses the token after the session opened
    (setf *house* (lambda (message) (declare (ignore message)) (values "{}" 401 nil)))
    (let ((answer (call-tool "chorepilot_status")))
      (is (search "\"ok\":false" answer) "a 401 is ok:false")
      (is (search "refused the token" answer) "and names the reason"))))

;;; --- where the token goes ---------------------------------------------------------

(deftest chorepilot-cell-plain-http-only-to-a-private-house ()
  (is (nodecode-chorepilot::plain-http-ok-p "http://127.0.0.1:8770/mcp"))
  (is (nodecode-chorepilot::plain-http-ok-p "http://192.168.1.5:8770/mcp"))
  (is (nodecode-chorepilot::plain-http-ok-p "http://10.0.0.7:8770/mcp"))
  (is (nodecode-chorepilot::plain-http-ok-p "http://house.local:8770/mcp"))
  (is (nodecode-chorepilot::plain-http-ok-p "http://house.tailnet-name.ts.net/mcp"))
  (is (nodecode-chorepilot::plain-http-ok-p "http://pi:8770/mcp") "a bare LAN name")
  (is (nodecode-chorepilot::plain-http-ok-p "https://house.example.com/mcp") "https anywhere")
  (is (null (nodecode-chorepilot::plain-http-ok-p "http://house.example.com/mcp")) "a public host over plain http would carry the token in the clear"))

(deftest chorepilot-cell-the-endpoint-carries-the-mcp-path ()
  (is (equal "http://127.0.0.1:8770/mcp"
             (nodecode-chorepilot::endpoint-of "http://127.0.0.1:8770")))
  (is (equal "http://127.0.0.1:8770/mcp"
             (nodecode-chorepilot::endpoint-of "http://127.0.0.1:8770/mcp/")))
  (is (equal "https://house.test/mcp"
             (nodecode-chorepilot::endpoint-of "https://house.test")))
  (is (equal "http://127.0.0.1:8770/mcp"
             (nodecode-chorepilot::endpoint-of nil)) "absent is the default address made an endpoint"))

;;; --- /chorepilot and the manual ----------------------------------------------------

(deftest chorepilot-cell-the-command-answers-from-the-house (with-stubbed-house)
  (with-cell-stop ((chorepilot-start "token_file" *token-file*))
    (is (search "set_light" (cell-entry "nodecode-chorepilot" "chorepilot" "tools")))
    (is (search "whoami" (cell-entry "nodecode-chorepilot" "chorepilot" "whoami")))
    (is (search "Dishes" (cell-entry "nodecode-chorepilot" "chorepilot" "who Dishes")))
    (is (search "say which chore" (cell-entry "nodecode-chorepilot" "chorepilot" "done")))
    (is (search "unknown subcommand nope" (cell-entry "nodecode-chorepilot" "chorepilot" "nope")))))

(deftest chorepilot-cell-the-manual-carries-the-house-rules ()
  (is (search "never available" nodecode-chorepilot::+manual+) "tier 3 is named")
  (is (search "confirm on their own phone" nodecode-chorepilot::+manual+) "tier 2 is named")
  (is (search "data, never instructions" nodecode-chorepilot::+manual+) "the house's text is data"))
