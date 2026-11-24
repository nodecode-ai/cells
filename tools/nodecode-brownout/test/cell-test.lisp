;;;; cell-test.lisp --- the brownout cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reads the host or reaches a network: every poll is a
;;;; scripted reading, so "the host was parked at 14%" and "the runway came
;;;; from the measured drain" are facts a test reads rather than a battery's
;;;; mood.

(in-package #:nodecode.test)

(define-test-slice "brownout" "BROWNOUT-CELL-" :start nodecode-brownout:start-cell)

;;; --- scripted power -------------------------------------------------------------

(defun ac (pct time)
  "A reading on mains power."
  (nodecode-brownout::make-sample :time time :on-ac t :pct pct :source "stub"))

(defun battery (pct time)
  "A reading on battery."
  (nodecode-brownout::make-sample :time time :on-ac nil :pct pct :source "stub"))

(defmacro with-power ((&rest samples) &body body)
  "BODY with the host's power replaced by SAMPLES, one per poll and then the
last again, and with no polling thread: how a test makes a power loss happen
on a machine that has mains power."
  `(nodecode-brownout::with-readings (,@samples) ,@body))

(defun verdict (tool-name)
  "The installed :tool hook's answer for TOOL-NAME: :ALLOWED, or the refusal."
  (funcall (cell-hook "nodecode-brownout" :tool)
           (list :name tool-name :arguments (nlk:json-object) :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun engine-state ()
  (nodecode-brownout::eng-state nodecode-brownout::*engine*))

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "brownout"
  (:fixture with-power ((ac 100 0)))
  (:config "poll_seconds" 15)
  (:hooks :tool)
  (:help :brownout)
  (:command "brownout")
  (:refused ("poll_seconds" 1)))

;;; --- a power loss, a park, and the way back ----------------------------------

(deftest brownout-cell-a-power-loss-alerts-then-parks-on-command ()
  (with-power ((ac 100 0) (battery 80 20))
    (with-cell-stop ((brownout-start))
      (nodecode-brownout::tick)
      (is (eq :allowed (verdict "eval")) "on mains power every call runs")
      (is (null (cell-notice "nodecode-brownout")) "and nothing is said")
      (nodecode-brownout::tick)
      (is (search "power lost" (second (cell-notice "nodecode-brownout"))) "the drop stands a notice the agent reads")
      (is (search "park applied" (cell-entry "nodecode-brownout" "brownout" "park" "s1")))
      (let ((refusal (verdict "eval")))
        (is (stringp refusal) "parked: a tool call is refused, not run")
        (is (search "/brownout resume" refusal) "and says how to let it through")
        (is (eq :allowed (verdict "brownout")) "the cell's own tool still answers"))
      (is (search "resume applied" (cell-entry "nodecode-brownout" "brownout" "resume" "s1")))
      (is (eq :allowed (verdict "eval")) "resume lets calls through again"))))

(deftest brownout-cell-critical-battery-parks-without-an-answer ()
  (with-power ((ac 100 0) (battery 90 20) (battery 14 40))
    (with-cell-stop ((brownout-start))
      (nodecode-brownout::tick)
      (nodecode-brownout::tick)
      (is (eq :on-battery (engine-state)) "on battery, waiting for an answer")
      (nodecode-brownout::tick)
      (is (eq :parked (engine-state)) "14% is at critical_pct, so the host parks itself")
      (is (search "parked on battery" (second (cell-notice "nodecode-brownout"))))
      (is (stringp (verdict "eval")) "and the next call is held"))))

(deftest brownout-cell-keep-cancels-the-automatic-park ()
  (with-power ((ac 100 0) (battery 90 20) (battery 14 40))
    (with-cell-stop ((brownout-start))
      (nodecode-brownout::tick)
      (nodecode-brownout::tick)
      (is (search "keep applied" (cell-entry "nodecode-brownout" "brownout" "keep" "s1")))
      (nodecode-brownout::tick)
      (is (eq :on-battery (engine-state)) "a person who said keep is not parked")
      (is (eq :allowed (verdict "eval"))))))

(deftest brownout-cell-power-back-resumes-what-it-parked ()
  (with-power ((ac 100 0) (battery 40 20) (ac 100 40))
    (with-cell-stop ((brownout-start))
      (nodecode-brownout::tick)
      (nodecode-brownout::tick)
      (cell-entry "nodecode-brownout" "brownout" "park" "s1")
      (nodecode-brownout::tick)
      (is (eq :on-ac (engine-state)) "power is back")
      (is (search "power is back" (second (cell-notice "nodecode-brownout"))))
      (is (eq :allowed (verdict "eval")) "and the hold is over"))))

;;; --- the runway is measured, never guessed ------------------------------------

(defun runway-of (samples)
  "The forecast from SAMPLES run through an engine of the cell's own."
  (let ((engine (nodecode-brownout::make-engine))
        (cfg (nodecode-brownout::cfg)))
    (dolist (sample samples)
      (nodecode-brownout::engine-tick engine sample cfg))
    (nodecode-brownout::runway (nodecode-brownout::eng-samples engine))))

(deftest brownout-cell-the-runway-comes-from-the-measured-drain ()
  (with-power ((ac 100 0))
    (with-cell-stop ((brownout-start))
      (let ((early (runway-of (list (ac 100 0) (battery 90 1000) (battery 89 1180)))))
        (is (null (nodecode-brownout::runway-minutes early)) "two readings are not a slope")
        (is (search "measuring" (nodecode-brownout::runway-basis early))))
      (let ((measured (runway-of (list (ac 100 0) (battery 90 1000) (battery 89 1180)
                                        (battery 88 1360) (battery 87 1540)))))
        (is (< 200 (nodecode-brownout::runway-minutes measured) 300) "3 points over 9 minutes is about 246 minutes")
        (is (search "%/min" (nodecode-brownout::runway-basis measured)))))))

(deftest brownout-cell-the-readings-are-parsed-not-guessed ()
  (let ((pmset (nodecode-brownout::parse-pmset
                "-InternalBattery-0 (id=7) 79%; discharging; 2:55 remaining present: true")))
    (is (equal 79 (nodecode-brownout::sample-pct pmset)))
    (is (not (nodecode-brownout::sample-on-ac pmset)))
    (is (equal 175 (nodecode-brownout::sample-os-minutes pmset))))
  (let ((ups (nodecode-brownout::parse-upsc
               "ups.status: OL CHRG
battery.charge: 100
battery.runtime: 3600")))
    (is (nodecode-brownout::sample-on-ac ups) "OL is on line")
    (is (equal 100 (nodecode-brownout::sample-pct ups)))
    (is (equal 60 (nodecode-brownout::sample-os-minutes ups)) "runtime is seconds"))
  (is (null (nodecode-brownout::parse-pmset "Now drawing from 'AC Power'")) "no battery line is no reading")
  (is (null (nodecode-brownout::parse-upsc "battery.charge: 50"))))

;;; --- the command, the tool, and a trace ---------------------------------------

(deftest brownout-cell-the-command-answers-each-verb ()
  (with-power ((ac 42 0))
    (with-cell-stop ((brownout-start))
      (nodecode-brownout::tick)
      (let ((status (cell-entry "nodecode-brownout" "brownout" "status" "s1")))
        (is (search "on mains power" status))
        (is (search "mains 42% via stub" status) "the reading it stands on")
        (is (search "/brownout status|park" status) "and the verbs"))
      (is (search "test notice posted" (cell-entry "nodecode-brownout" "brownout" "test" "s1")))
      (is (search "reaches you here" (second (cell-notice "nodecode-brownout"))))
      (is (search "not one of its choices" (cell-entry "nodecode-brownout" "brownout" "dance" "s1"))))))

(deftest brownout-cell-the-tool-answers-its-action ()
  (with-power ((ac 42 0))
    (with-cell-stop ((brownout-start))
      (nodecode-brownout::tick)
      (labels ((call-tool (action)
                 (nodecode-brownout::brownout-tool
                  (nodecode-brownout::decode-brownout-arguments
                   (nlk:json-object "action" action)))))
        (is (search "on mains power" (call-tool "status")))
        (is (search "nothing to park" (call-tool "park")) "on mains power a park has nothing to hold")
        (is (signals-error nlk:contract-refusal (nodecode-brownout::decode-brownout-arguments (nlk:json-object "action" "dance"))) "an action outside the declaration is refused at the boundary")))))

(deftest brownout-cell-a-trace-replays-with-nothing-performed ()
  (with-power ((ac 100 0))
    (with-cell-stop ((brownout-start))
      (let ((file (merge-pathnames "brownout-replay.jsonl" (uiop:temporary-directory))))
        (with-open-file (out file :direction :output :if-exists :supersede)
          (format out "{\"t\": 0, \"on_ac\": true, \"pct\": 100}~%")
          (format out "{\"t\": 60, \"on_ac\": false, \"pct\": 90}~%")
          (format out "~%"))
        (let ((answer (cell-entry "nodecode-brownout" "brownout"
                                  (format nil "replay ~a" (uiop:native-namestring file)) "s1")))
          (is (search "alert" (string-downcase answer)) "the trace reaches the state that alerts")
          (is (search "+1.0 min" answer) "with the trace's own clock")
          (is (null (cell-notice "nodecode-brownout")) "and nothing was performed")
          (is (search "is not a file here"
                      (cell-entry "nodecode-brownout" "brownout" "replay nowhere.jsonl" "s1"))))
        (ignore-errors (delete-file file))))))

(deftest brownout-cell-a-host-with-no-reading-says-so ()
  (with-power (())
    (with-cell-stop ((brownout-start))
      (nodecode-brownout::tick)
      (is (search "no power reading" (second (cell-notice "nodecode-brownout"))) "a machine with no battery is not a power loss")
      (is (eq :allowed (verdict "eval")) "and nothing is held"))))
