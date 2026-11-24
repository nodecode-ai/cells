;;;; cell-test.lisp --- the cell's half: the tools, the command, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the declaration installs the eight tools, /hotwire and
;;;; (help :hotwire) and takes all of them back on stop; hotwire_check measures
;;;; specs through the wire a provider's call comes through; a wiring error is a
;;;; verdict, not a crash; the DC and AC analyses answer numbers a builder can
;;;; act on; /hotwire answers the operator; and the section's bounds are the ones
;;;; the reads keep to.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "hotwire"
  (:config)
  (:help :hotwire)
  (:command "hotwire")
  (:running
   (is (= 60 (nodecode-hotwire::setting :ac-points)) "the sweep samples 60 points per decade")
   (is (= 400 (nodecode-hotwire::setting :max-elements)) "and one netlist may carry 400 elements"))
  (:refused ("ac_points" 0)))

(deftest hotwire-cell-check-measures-every-spec ()
  (with-cell-stop ((hotwire-start))
    (let ((answer (hotwire-tool "hotwire-call-1" "hotwire_check" "netlist" +divider+)))
      (is (search "PASS" answer) "the divider's one spec passes")
      (is (search "v(out) in 4.9..5.1 V" answer) "the spec is quoted with its target")
      (is (search "measured 5 V" answer) "and with what was measured"))
    (let ((answer (hotwire-tool "hotwire-call-2" "hotwire_check" "netlist" +common-emitter+)))
      (is (search "Q1 active" answer) "the transistor is reported in its operating region")
      (is (search "PASS" answer) "and a biased stage passes its specs"))
    (let ((answer (hotwire-tool "hotwire-call-3" "hotwire_check" "library" "rc_lowpass")))
      (is (search "RC low-pass filter" answer) "a reference circuit is simulated from the library")
      (is (search "fc in 960..1160 Hz" answer) "with the corner it is proven against")
      (is (search "PASS" answer) "and passes it"))))

(deftest hotwire-cell-a-wiring-error-is-a-verdict ()
  (with-cell-stop ((hotwire-start))
    (let ((answer (hotwire-tool "hotwire-call-4" "hotwire_check" "netlist" +floating+)))
      (is (search "FAIL" answer) "a node with no DC path fails before any simulation")
      (is (search "no DC path to ground" answer) "and the rule that broke is named"))
    (let ((answer (hotwire-tool "hotwire-call-5" "hotwire_check" "netlist" "")))
      (is (search "hotwire: the netlist is empty" answer) "an empty netlist answers with a line, not a failed call"))
    (let ((answer (hotwire-tool "hotwire-call-6" "hotwire_simulate" "netlist" +no-output+ "analysis" "ac")))
      (is (search "AC sweep needs" answer) "an AC sweep with no '*@ out' says which annotation is missing"))))

(deftest hotwire-cell-simulate-answers-one-analysis ()
  (with-cell-stop ((hotwire-start))
    (let ((answer (hotwire-tool "hotwire-call-7" "hotwire_simulate" "netlist" +divider+ "analysis" "op")))
      (is (search "operating point" answer) "the op analysis answers the idle state")
      (is (search "out 5 V" answer) "with the node voltage it solved"))
    (let ((answer (hotwire-tool "hotwire-call-8" "hotwire_simulate" "library" "rc_lowpass" "analysis" "ac")))
      (is (search "AC response" answer) "the ac analysis answers a frequency response")
      (is (search "-3 dB corner" answer) "with the corner of the filter")
      (is (search "1000." answer) "and a gain at 1 kHz"))))

(deftest hotwire-cell-the-command-answers-the-operator ()
  (with-cell-stop ((hotwire-start))
    (is (search "analog circuit bench" (hotwire-command "")) "bare, /hotwire says what it does")
    (is (search "rc_lowpass" (hotwire-command "library")) "and lists the reference circuits")
    (is (search "PASS" (hotwire-command "check rc_lowpass")) "check runs a reference circuit")
    (is (search "hotwire: " (hotwire-command "check no-such-circuit")) "an id nothing answers to comes back with the reason")))

(deftest hotwire-cell-the-section-bounds-the-reads ()
  (with-cell-stop ((hotwire-start "max_elements" 10))
    (let ((answer (hotwire-tool "hotwire-call-9" "hotwire_check" "netlist" +twelve-parts+)))
      (is (search "at most 10" answer) "a netlist past the element bound is refused, not cut"))))

(deftest hotwire-cell-explain-measures-each-part ()
  (with-cell-stop ((hotwire-start))
    (let ((answer (hotwire-tool "hotwire-call-10" "hotwire_explain" "netlist" +detuned-lowpass+)))
      (is (search "What each part does" answer) "the explanation says what it did")
      (is (search "R1" answer) "the resistor is reported")
      (is (search "fc " answer) "with the spec it moves and by how much"))
    (let ((answer (hotwire-tool-raw "hotwire-call-11" "hotwire_explain" "library" "\"rc_lowpass\""
                                     "step" "0.2")))
      (is (search "20%" answer) "the nudge the caller asked for is the one reported"))))

(deftest hotwire-cell-bom-snaps-and-groups ()
  (with-cell-stop ((hotwire-start))
    (let ((answer (hotwire-tool "hotwire-call-12" "hotwire_bom" "netlist" +detuned-lowpass+)))
      (is (search "Parts list" answer) "the list is named")
      (is (search "resistor" answer) "with the kinds of part to buy")
      (is (search "4.7k" answer) "a 4.5k resistor is snapped to the standard value above it")
      (is (search "is not a E24 value" answer) "and the netlist's own value is called out"))
    (let ((answer (hotwire-tool "hotwire-call-13" "hotwire_bom" "library" "sallen_key_lowpass")))
      (is (search "op-amp" answer) "a semiconductor is named by its description"))))

(deftest hotwire-cell-tune-finds-a-standard-value ()
  (with-cell-stop ((hotwire-start))
    (let ((answer (hotwire-tool-raw "hotwire-call-14" "hotwire_tune" "netlist"
                                     (format nil "~a" (hotwire-json-string +detuned-lowpass+))
                                     "vary" "[\"R1\"]" "rounds" "4")))
      (is (search "Tune:" answer) "the answer says what it tuned")
      (is (search "Before:" answer) "with the measurements before the search")
      (is (search "After:" answer) "and after it")
      (is (search "5.1k" answer) "the standard value that meets the corner is found"))
    (let ((answer (hotwire-tool-raw "hotwire-call-15" "hotwire_tune" "library" "\"rc_lowpass\"")))
      (is (search "name the parts to tune" answer) "without parts to vary the refusal says what to pass"))))

(deftest hotwire-cell-yield-counts-real-builds ()
  (with-cell-stop ((hotwire-start))
    (let ((answer (hotwire-tool-raw "hotwire-call-16" "hotwire_yield" "netlist"
                                     (format nil "~a" (hotwire-json-string +divider+))
                                     "builds" "20" "tolerance" "0.01")))
      (is (search "Yield:" answer) "the yield is reported")
      (is (search "/20 builds" answer) "over the number of builds asked for")
      (is (search "Part spread:" answer) "and the part spread it used"))))

(deftest hotwire-cell-export-writes-a-deck ()
  (with-cell-stop ((hotwire-start))
    (let ((answer (hotwire-tool "hotwire-call-17" "hotwire_export" "netlist" +divider+)))
      (is (search "1.000000E+4" answer) "a resistor is written as the decimal another SPICE reads")
      (is (search ".op" answer) "the deck asks for an operating point")
      (is (search "*@ spec v(out) in 4.9..5.1 V" answer) "and keeps the spec as a comment"))
    (let ((answer (hotwire-tool "hotwire-call-18" "hotwire_export" "library" "sallen_key_lowpass"
                                "format" "json")))
      (is (char= (char answer 0) #\{) "the json export is one object")
      (is (search "\"verdict\": \"pass\"" answer) "carrying the verdict it measured")
      (is (search "\"elements\"" answer) "and every element"))
    (let ((answer (hotwire-tool "hotwire-call-19" "hotwire_export" "netlist" +divider+ "format" "kicad")))
      (is (search "one of" answer) "a format the cell does not write is refused with the choices"))))
