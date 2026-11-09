;;;; cell-test.lisp --- the COtrip cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; FETCH-FEATURES is the cell's one seam: every test stubs it with a
;;;; COtrip answer written as the wire writes it, so the filtering, the
;;;; schedule, the direction and the Waze label are facts a test reads rather
;;;; than a road's afternoon.

(in-package #:nodecode.test)

(define-test-slice "colorado-traffic" "COLORADO-TRAFFIC-CELL-" :start nodecode-colorado-traffic:start-cell)

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "colorado-traffic"
  (:help :colorado-traffic)
  (:command "traffic")
  (:running (is (equal nodecode-colorado-traffic::+primer+ (nle:help :colorado-traffic)) "(help :colorado-traffic) is the primer")
            (is (string= "i70-golden-denver" (nodecode-colorado-traffic::setting :corridor)) "the plugin's own default corridor")
            (is (= 12 (nodecode-colorado-traffic::setting :max-items)) "twelve events a layer"))
  (:refused ("corridor" "i70-vail") ("timeout_seconds" 0))
  (:idle cotrip:colorado-traffic-error (cotrip:events)))

;;; --- COtrip's answers, as the wire writes them -------------------------------

(defparameter *incidents*
  "{\"features\":[
     {\"properties\":{\"id\":\"4471\",\"route\":\"I-70\",\"title\":\"Crash on I-70 eastbound\",
       \"eventType\":\"Incident\",\"updated\":4003140000000,
       \"eventReport\":{\"location\":{\"linkDirection\":\"POSITIVE_DIRECTION\",
                                     \"routePositiveBearing\":\"E\",
                                     \"primaryPoint\":{\"linearReference\":260.1},
                                     \"secondaryPoint\":{\"linearReference\":262.0}},
                        \"eventDescription\":{\"descriptionHeader\":\"Crash\",
                                              \"descriptionFull\":\"Right lane blocked<br/>Expect delays &amp; slow traffic\"}}}},
     {\"properties\":{\"id\":\"9001\",\"route\":\"I-70\",\"title\":\"Far west of the box\",
       \"eventReport\":{\"location\":{\"linkDirection\":\"POSITIVE_DIRECTION\",
                                     \"routePositiveBearing\":\"E\",
                                     \"primaryPoint\":{\"linearReference\":100.0}}}}},
     {\"properties\":{\"id\":\"9002\",\"route\":\"I-25\",\"title\":\"Another route\",
       \"eventReport\":{\"location\":{\"linkDirection\":\"POSITIVE_DIRECTION\",
                                     \"routePositiveBearing\":\"N\",
                                     \"primaryPoint\":{\"linearReference\":210.0}}}}}]}"
  "Two of these are outside the corridor: one by route, one by mile point.")

(defparameter *i25*
  "{\"features\":[
     {\"properties\":{\"id\":\"5150\",\"route\":\"I-25\",\"title\":\"Stalled car on I-25\",
       \"eventReport\":{\"location\":{\"linkDirection\":\"POSITIVE_DIRECTION\",
                                     \"routePositiveBearing\":\"N\",
                                     \"primaryPoint\":{\"linearReference\":210.4},
                                     \"secondaryPoint\":{\"linearReference\":211.0}}}}},
     {\"properties\":{\"id\":\"5151\",\"route\":\"I-25\",\"title\":\"Southbound crash\",
       \"eventReport\":{\"location\":{\"linkDirection\":\"NEGATIVE_DIRECTION\",
                                     \"routePositiveBearing\":\"N\",
                                     \"primaryPoint\":{\"linearReference\":212.0}}}}},
     {\"properties\":{\"id\":\"5152\",\"route\":\"I-25\",\"title\":\"Both ways\",
       \"eventReport\":{\"location\":{\"linkDirection\":\"BOTH_DIRECTIONS\",
                                     \"primaryPoint\":{\"linearReference\":215.0}}}}}]}"
  "Positive is northbound on I-25, and BOTH_DIRECTIONS is neither.")

(defparameter *work*
  "{\"features\":[
     {\"properties\":{\"id\":\"7788\",\"route\":\"I-70\",\"title\":\"Overnight closure\",
       \"active\":true,\"eventType\":\"RoadWork\",
       \"eventReport\":{\"location\":{\"linkDirection\":\"BOTH_DIRECTIONS\",
                                     \"routePositiveBearing\":\"E\",
                                     \"primaryPoint\":{\"linearReference\":265.0}},
                        \"scheduleOccurrences\":[{\"startTime\":{\"time\":4003459200000},
                                                  \"endTime\":{\"time\":4003545600000}}]}}}]}"
  "active:true, and the schedule is three days out: the tag, not the flag.")

(defparameter *waze*
  "{\"features\":[
     {\"properties\":{\"id\":\"Waze-55\",\"route\":\"I-70\",\"title\":\"Waze: heavy traffic\",
       \"eventReport\":{\"location\":{\"linkDirection\":\"POSITIVE_DIRECTION\",
                                     \"routePositiveBearing\":\"E\",
                                     \"primaryPoint\":{\"linearReference\":266.0}}}}}]}"
  "CDOT republishes Waze as unverified crowd reports.")

(defparameter *layers*
  (list (cons "roadReports" *incidents*)
        (cons "roadClosures" "")
        (cons "roadWork" *work*)
        (cons "wazeReports" *waze*))
  "The four layers a bare query asks for, as the wire answers them.")

(defun features-of (text)
  "TEXT decoded as a COtrip answer => its FEATURES vector."
  (nlk:json-value (nlk:decode-json (if (plusp (length text)) text "{\"features\":[]}"))
                   :array "features"))

(defmacro with-cotrip (layers &body body)
  "BODY with FETCH-FEATURES answering one classification from LAYERS, a
((CLASSIFICATION . JSON-TEXT) ...) alist; a classification it does not name
answers an empty feature collection."
  `(let ((routes (or ,layers *layers*)))
     (with-stubbed-fdefinition (nodecode-colorado-traffic::fetch-features (classification)
                                   (features-of (or (cdr (assoc classification routes :test #'string=))
                                                    "")))
       ,@body)))

;;; --- what the corridor does with the answer -----------------------------------

(deftest colorado-traffic-cell-only-the-corridor-is-answered ()
  (with-cell-stop ((colorado-traffic-start))
    (with-cotrip ()
      (let ((answer (cotrip:events)))
        (is (search "I-70 Golden to east Denver (MP 259-279" answer) "the shipped corridor is the default")
        (is (search "Crash on I-70 eastbound" answer) "the event inside the box is answered")
        (is (search "[now]" answer) "and tagged as happening now")
        (is (search "MP 260.1 to 262.0" answer) "with the stretch it covers")
        (is (search "eastbound" answer) "and its direction")
        (is (search "Expect delays & slow traffic" answer) "its prose, HTML and entities read")
        (is (null (search "Far west of the box" answer)) "an event west of MP 259 is not answered")
        (is (null (search "Another route" answer)) "nor one on another route")
        (is (search "https://www.cotrip.org/events/4471" answer) "the COtrip link is carried")))))

(deftest colorado-traffic-cell-a-future-schedule-outranks-the-active-flag ()
  (with-cell-stop ((colorado-traffic-start))
    (with-cotrip ()
      (let ((answer (cotrip:events)))
        (is (search "Scheduled work (1)" answer) "the work layer is its own section")
        (is (search "[upcoming]" answer) "active:true on future night work reads upcoming, as the plugin's own rule says")
        (is (search "Overnight closure" answer) "the work itself is listed")
        (is (search "2026-11-12 01:00 MST to 2026-11-13 01:00 MST" answer) "and the window is given in US Mountain time")
        (is (null (search "[now] Overnight closure" answer)) "never as happening now")))))

(deftest colorado-traffic-cell-waze-is-labelled-for-what-it-is ()
  (with-cell-stop ((colorado-traffic-start))
    (with-cotrip ()
      (let ((answer (cotrip:events)))
        (is (search "Waze via COtrip (1)" answer) "a Waze layer is headed as CDOT's republication")
        (is (search "Waze: heavy traffic" answer) "with the report itself")
        (is (search "unverified crowd reports" answer) "and the caveat that it is unconfirmed")))))

(deftest colorado-traffic-cell-direction-follows-the-positive-bearing ()
  (with-cell-stop ((colorado-traffic-start))
    (with-cotrip (list (cons "roadReports" *i25*))
      (let ((answer (cotrip:events :corridor "i70-golden-i25" :route "I-25"
                                    :min-mp 209 :max-mp 217 :layers "incidents")))
        (is (search "I-25 MP 209.0-217.0" answer) "an overridden box says so in its heading")
        (is (search "northbound" answer) "positive on I-25 is northbound, not eastbound")
        (is (search "southbound" answer) "negative is the other way")
        (is (search "both directions" answer) "BOTH_DIRECTIONS is neither")))))

(deftest colorado-traffic-cell-a-layer-and-an-ended-event-are-optional ()
  (with-cell-stop ((colorado-traffic-start))
    (with-cotrip ()
      (let ((answer (cotrip:events :layers "closures" :route "I-70")))
        (is (search "Closures: none" answer) "a layer with nothing answers so, not silently")
        (is (null (search "Incidents" answer)) "and the layers asked for are the only ones fetched")))
    (is (signals-error cotrip:colorado-traffic-error (cotrip:events :layers "cameras")) "a layer COtrip does not know refuses, naming itself")))

(deftest colorado-traffic-cell-the-tool-answers-what-the-contract-carries ()
  (with-cell-stop ((colorado-traffic-start))
    (with-cotrip (list (cons "roadReports" *i25*))
      (let* ((args (nodecode-colorado-traffic::decode-cotrip-events
                    (nlk:json-object "route" "I-25" "min_mp" 209 "max_mp" 217
                                     "layers" "incidents")))
             (answer (nodecode-colorado-traffic::tool-events args)))
        (is (= 209.0d0 (nodecode-colorado-traffic::cotrip-events-min-mp args)) "the contract carries the mile point")
        (is (search "Stalled car on I-25" answer) "the tool answers the corridor it was handed")
        (is (search "northbound" answer) "through the same direction rule")))))

(deftest colorado-traffic-cell-the-command-answers-and-refuses ()
  (with-cell-stop ((colorado-traffic-start))
    (with-cotrip ()
      (let ((answer (cell-entry "nodecode-colorado-traffic" "traffic" "" "cotrip-c1")))
        (is (search "Crash on I-70 eastbound" answer) "/traffic alone asks the shipped corridor"))
      (let ((named (cell-entry "nodecode-colorado-traffic" "traffic" "i70-golden-i25" "cotrip-c1")))
        (is (search "I-70 Golden to I-25 (MP 259-274.5)" named) "a corridor name picks that corridor"))
      (let ((bogus (cell-entry "nodecode-colorado-traffic" "traffic" "i-270" "cotrip-c1")))
        (is (search "is no corridor" bogus) "an unknown corridor refuses in words")
        (is (search "i70-golden-denver" bogus) "and names the ones there are")))))

(deftest colorado-traffic-cell-a-mountain-time-stamp-follows-daylight-saving ()
  (is (equal "2026-11-08 10:00 MST" (nodecode-colorado-traffic::stamp 4003146000000)) "November is standard time")
  (is (equal "2026-07-04 12:00 MDT" (nodecode-colorado-traffic::stamp 3992176800000)) "July is daylight time")
  (is (null (nodecode-colorado-traffic::stamp nil)) "and an absent time stamps nothing"))
