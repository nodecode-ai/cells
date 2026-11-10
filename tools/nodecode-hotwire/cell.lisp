;;;; cell.lisp --- the circuit bench: eight tools, /hotwire, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of RohanGlitched/hotwire, a Hermes Agent plugin that
;;;; wrote a SPICE netlist from plain English, simulated it with its own numpy
;;;; solver, measured '*@ spec' lines and judged the result. What is kept here
;;;; is that loop for the analyses this cell carries: the DC operating point,
;;;; the electrical rule check, the small-signal AC sweep, the parts stress and
;;;; the pass/fail verdict per spec.

(in-package #:nodecode-hotwire)

(nlk:define-peripheral hotwire :not-running t)

;;; --- reading the circuit a call names ----------------------------------------

(defun design-text (netlist library path)
  "The netlist a call names: its own text, a library id, or a file's contents."
  (progn
    (cond
      (netlist netlist)
      (library (or (third (library-entry library))
                   (netlist-error 0 "no reference circuit called '~a' (library: ~{~a~^, ~})"
                         library (library-ids))))
      (path (if (probe-file path)
                (let ((text (uiop:read-file-string path)))
                  (if (> (length text) 400000)
                      (netlist-error 0 "the file ~a is larger than this cell reads (400,000 characters)" path)
                      text))
                (or (third (library-entry path))
                    (netlist-error 0 "no file at ~a, and no reference circuit of that name" path))))
      (t (netlist-error 0 "give the circuit: a netlist, a library id, or a path to a .cir file")))))

(defun strings-of (value)
  "VALUE, a JSON string array, as the list the analyses take."
  (cond ((null value) '())
        ((listp value) value)
        (t (coerce value 'list))))

(defun parsed (text)
  "TEXT as a DESIGN, bounded by the section's own limits."
  (let ((design (parse-netlist text)))
    (when (> (length (design-elements design)) (setting :max-elements))
      (netlist-error 0 "the netlist has ~d elements; this cell simulates at most ~d (raise hotwire.max_elements)"
            (length (design-elements design)) (setting :max-elements)))
    design))

;;; --- the reports ---------------------------------------------------------------

(defun issue-line (issue)
  (format nil "  ~(~a~): ~a" (getf issue :level) (getf issue :message)))

(defun check-text (design extra-specs)
  "The whole check as the text a model reads: verdict, every spec, the ERC."
  (multiple-value-bind (verdict summary bench results issues) (check-design design extra-specs)
    (with-output-to-string (out)
      (format out "~a: ~a~%~a~%" (design-title design) (string-upcase verdict) summary)
      (when bench
        (format out "Nodes: ~a~%" (describe-op bench))
        (unless results
          (format out "Specs: none ('*@ spec gain_db(1k) > 18' adds one)~%"))
        (dolist (result results)
          (format out "~a~%" (spec-line result))))
      (when issues
        (format out "Rules:~%~{~a~%~}" (mapcar #'issue-line issues)))
      (when (eq verdict "fail")
        (format out "Verdict: FAIL. Do not tell the user this circuit works until a spec passes.~%")))))

(defun simulate-text (design analysis node-names)
  "One raw analysis as text: the operating point, or the AC response."
  (let ((bench (bench-of design)))
    (unless (bench-converged bench)
      (return-from simulate-text
        (format nil "~a: the DC operating point did not converge, so no analysis was run."
                (design-title design))))
    (if (string-equal analysis "ac")
        (with-output-to-string (out)
          (let ((output (design-output design)))
            (unless (and (design-input design) output)
              (netlist-error 0 "an AC sweep needs '*@ in <source>' and '*@ out <node>' in the netlist"))
            (format out "~a: AC response from ~a to ~a~%" (design-title design)
                    (design-input design) output)
            (format out "~10@a ~12@a ~10@a~%" "freq" "gain" "phase")
            (dolist (frequency '(10d0 100d0 1000d0 10000d0 100000d0))
              (let ((response (ac-point design (bench-op bench) frequency output)))
                (if response
                    (format out "~10,4g ~9,4fdB ~9,2f deg~%" frequency (decibels response)
                            (/ (* 180d0 (atan (imagpart response) (realpart response))) pi))
                    (format out "~10,4g   (singular at this frequency)~%" frequency))))
            (loop for node in node-names
                  do (let ((response (ac-point design (bench-op bench) 1000d0 node)))
                       (when response
                         (format out "  ~a at 1 kHz: ~a~%" node (format-value response "V")))))
            (let ((corner (corner-frequency bench nil)))
              (format out "-3 dB corner: ~a~%" (format-value corner "Hz")))))
        (with-output-to-string (out)
          (format out "~a: operating point (~(~a~), ~d iterations)~%" (design-title design)
                  (bench-method bench) (bench-steps bench))
          (format out "Nodes: ~a~%" (describe-op bench))
          (dolist (element (design-elements design))
            (when (member (element-kind element) '(:v :l :q) :test #'eq)
              (format out "  ~a current: ~a~%" (element-name element)
                      (format-value (element-current design (bench-op bench) (element-name element)) "A"))))
          (when (plusp (length (design-supplies design)))
            (format out "  supply current: ~a~%" (format-value (supply-current design bench) "A")))))))

(defun library-text-of (query)
  "The library listing, or the one circuit QUERY names with its netlist."
  (let ((entry (and query (plusp (length (string-trim " " query))) (library-entry query))))
    (if entry
        (format nil "~a~%~%~a" (second entry) (third entry))
        (format nil "Reference circuits (pass one as `library`, or paste its netlist):~%~a" (library-text)))))

;;; --- the tools ------------------------------------------------------------------

(nlk:define-contract hotwire-design
  (:doc "Simulate an analog circuit and prove it works before anyone builds it.
Give the circuit as a SPICE netlist: R C L V I D Q elements, a *@ in source, a
*@ out node, and '*@ spec' lines for everything that must hold. The netlist is
parsed, checked for wiring mistakes, solved for its DC operating point, and every
spec is measured against its target. Call it after every change; never tell the
user a spec passes without its result.")
  ("netlist" :string
             :doc "the circuit as SPICE text; '*@ in VIN' names the signal source, '*@ out <node>' the output, '*@ spec gain_db(1k) > 18' a requirement")
  ("library" :string :doc "a bundled reference circuit's id, instead of a netlist (hotwire_library lists them)")
  ("path" :string :doc "a path to a .cir file, or a reference circuit's id, instead of a netlist")
  ("specs" :array :of :string :doc "extra requirements, same syntax as '*@ spec': 'gain_db(1k) in 18..24', 'fc < 2k', 'v(out) in 4.3..4.7'")
  ("node" :string :doc "the node circuit_simulate reports; defaults to '*@ out'"))

(nlk:define-contract hotwire-analysis
  (:doc "Run one raw analysis on an analog circuit: 'op' for the DC node
voltages, currents and transistor bias, or 'ac' for the frequency response from
the '*@ in source to the '*@ out node. Use hotwire_check for verdicts; use this
to investigate.")
  ("netlist" :string :doc "the circuit as SPICE text")
  ("library" :string :doc "a bundled reference circuit's id, instead of a netlist")
  ("path" :string :doc "a path to a .cir file, or a reference circuit's id")
  ("analysis" :string :options (quote ("op" "ac")) :doc "op (default) or ac")
  ("nodes" :array :of :string :doc "nodes to report at 1 kHz in an AC sweep"))

(defun run-check (arguments)
  "The hotwire_check tool: the circuit's verdict, spec by spec."
  (handler-case
      (let ((design (parsed (design-text (hotwire-design-netlist arguments)
                                         (hotwire-design-library arguments)
                                         (hotwire-design-path arguments)))))
        (check-text design (strings-of (hotwire-design-specs arguments))))
    (hotwire-error (failure) (format nil "hotwire: ~a" (hotwire-error-detail failure)))))

(defun run-simulate (arguments)
  "The hotwire_simulate tool: one analysis, as numbers."
  (handler-case
      (let ((design (parsed (design-text (hotwire-analysis-netlist arguments)
                                         (hotwire-analysis-library arguments)
                                         (hotwire-analysis-path arguments)))))
        (simulate-text design (or (hotwire-analysis-analysis arguments) "op")
                       (strings-of (hotwire-analysis-nodes arguments))))
    (hotwire-error (failure) (format nil "hotwire: ~a" (hotwire-error-detail failure)))))

(nlk:define-contract hotwire-library-query
  (:doc "List the reference circuits this cell bundles, or read one: its netlist,
its knobs and the specs it is proven against. Start a design from one instead of
writing a netlist from nothing.")
  ("query" :string :doc "a reference circuit's id, or a word such as 'fuzz', 'low-pass', 'tone stack'"))

(defun run-library (arguments)
  "The hotwire_library tool: what there is to start from."
  (library-text-of (hotwire-library-query-query arguments)))

;;; --- the bench around a check ---------------------------------------------------

(nlk:define-contract hotwire-explain-query
  (:doc "Measure what each part of a circuit does: nudge every resistor, capacitor,
inductor and pot by 10% and report which spec each one moves. Use it to answer
'what does this part do' with numbers, and to find the two parts worth tuning.")
  ("netlist" :string :doc "the circuit as SPICE text")
  ("library" :string :doc "a bundled reference circuit's id, instead of a netlist")
  ("path" :string :doc "a path to a .cir file, or a reference circuit's id")
  ("specs" :array :of :string :doc "extra requirements, same syntax as '*@ spec'")
  ("step" :number :default 0.1d0 :slot nudge
            :doc "how far each part is nudged, as a fraction (0.1 is 10%)"))

(nlk:define-contract hotwire-bom-query
  (:doc "The parts list: equal parts grouped, every resistor and capacitor snapped
to a preferred value, the package to buy, and the nearest standard value when the
netlist names one that is not in the series. A pedal's hardware is added at the end.")
  ("netlist" :string :doc "the circuit as SPICE text")
  ("library" :string :doc "a bundled reference circuit's id, instead of a netlist")
  ("path" :string :doc "a path to a .cir file, or a reference circuit's id")
  ("series" :string :default "E24" :options (list "E6" "E12" "E24")
            :doc "the preferred-value series resistors are snapped to (E24 unless asked)"))

(nlk:define-contract hotwire-tune-query
  (:doc "Pick preferred values for the parts you name so the specs pass with the
most margin: a coordinate search over the neighbouring E-series values, scored
first by total violation and then by the smallest margin. It answers the before
and after measurements, never a promise.")
  ("netlist" :string :doc "the circuit as SPICE text")
  ("library" :string :doc "a bundled reference circuit's id, instead of a netlist")
  ("path" :string :doc "a path to a .cir file, or a reference circuit's id")
  ("vary" :array :of :string :doc "the parts to tune, e.g. [\"R3\", \"C2\"]; hotwire_explain shows which ones matter")
  ("specs" :array :of :string :doc "extra requirements, same syntax as '*@ spec'")
  ("series" :string :options (list "E6" "E12" "E24")
            :doc "the series to search; resistors use E24 and capacitors E12 when this is left out")
  ("rounds" :integer :default 3 :doc "how many sweeps over the named parts (1 to 8)")
  ("steps" :integer :default 2 :doc "how many standard values either side of the current one (1 to 6)"))

(nlk:define-contract hotwire-yield-query
  (:doc "Monte Carlo: build the circuit N times with every resistor, capacitor and
inductor drawn uniformly inside a part tolerance, and count the builds that still
meet every spec. Use it before anyone solders: it says how many real builds work
and which spec breaks first.")
  ("netlist" :string :doc "the circuit as SPICE text")
  ("library" :string :doc "a bundled reference circuit's id, instead of a netlist")
  ("path" :string :doc "a path to a .cir file, or a reference circuit's id")
  ("specs" :array :of :string :doc "extra requirements, same syntax as '*@ spec'")
  ("builds" :integer :default 60 :doc "how many circuits to build (10 to 300)")
  ("tolerance" :number :default 0.05d0 :doc "part spread as a fraction of the value (0.05 is 5%)"))

(nlk:define-contract hotwire-export-query
  (:doc "Write the circuit out: 'spice' gives a self-contained deck for ngspice or
LTspice, its models included and its op-amps an ideal 100k-gain source, so the
numbers this cell measured can be reproduced elsewhere; 'json' gives the netlist and
its verdict as one object for another program.")
  ("netlist" :string :doc "the circuit as SPICE text")
  ("library" :string :doc "a bundled reference circuit's id, instead of a netlist")
  ("path" :string :doc "a path to a .cir file, or a reference circuit's id")
  ("format" :string :default "spice" :slot fmt :options (list "spice" "json")
            :doc "spice (default) or json")
  ("specs" :array :of :string :doc "extra requirements to judge the JSON verdict by"))

(defmacro answering (&body body)
  "BODY's value, or the HOTWIRE-ERROR it signals as one line of text."
  `(handler-case (progn ,@body)
     (hotwire-error (failure) (format nil "hotwire: ~a" (hotwire-error-detail failure)))))

(defun design-of (netlist library path)
  "The circuit a call names: its netlist, a library id, or a file's text, parsed."
  (parsed (design-text netlist library path)))

(defun run-explain (arguments)
  "The hotwire_explain tool: every part's measured effect on the specs."
  (answering
   (explain-text (design-of (hotwire-explain-query-netlist arguments)
                             (hotwire-explain-query-library arguments)
                             (hotwire-explain-query-path arguments))
                 (strings-of (hotwire-explain-query-specs arguments))
                 (hotwire-explain-query-nudge arguments))))

(defun run-bom (arguments)
  "The hotwire_bom tool: the parts list, grouped and snapped."
  (answering
   (bom-text (design-of (hotwire-bom-query-netlist arguments)
                         (hotwire-bom-query-library arguments)
                         (hotwire-bom-query-path arguments))
             (hotwire-bom-query-series arguments))))

(defun run-tune (arguments)
  "The hotwire_tune tool: preferred values that pass with the most margin."
  (answering
   (tune-text (design-of (hotwire-tune-query-netlist arguments)
                          (hotwire-tune-query-library arguments)
                          (hotwire-tune-query-path arguments))
              (strings-of (hotwire-tune-query-vary arguments))
              (strings-of (hotwire-tune-query-specs arguments))
              (hotwire-tune-query-series arguments)
              (hotwire-tune-query-rounds arguments)
              (hotwire-tune-query-steps arguments))))

(defun run-yield (arguments)
  "The hotwire_yield tool: how many real builds still pass."
  (answering
   (yield-text (design-of (hotwire-yield-query-netlist arguments)
                           (hotwire-yield-query-library arguments)
                           (hotwire-yield-query-path arguments))
                (strings-of (hotwire-yield-query-specs arguments))
                (hotwire-yield-query-builds arguments)
                (hotwire-yield-query-tolerance arguments))))

(defun run-export (arguments)
  "The hotwire_export tool: a SPICE deck, or JSON."
  (answering
   (export-text (design-of (hotwire-export-query-netlist arguments)
                           (hotwire-export-query-library arguments)
                           (hotwire-export-query-path arguments))
                (strings-of (hotwire-export-query-specs arguments))
                (hotwire-export-query-fmt arguments))))

;;; --- /hotwire --------------------------------------------------------------------


(defparameter +usage+
  "hotwire: an analog circuit bench
  /hotwire                  this help and the reference circuits
  /hotwire library          every reference circuit
  /hotwire check <id|file>  check a reference circuit, or a .cir file
  /hotwire explain <id>     what each part does, measured
  /hotwire bom <id>         the parts list to buy
Or just ask: \"design a 9 V fuzz that cleans up with the guitar volume\"")

(defun command-target (args prefix)
  "ARGS without its leading PREFIX and its space, or NIL when it has none."
  (when (and (< (length prefix) (length args))
             (string-equal prefix (subseq args 0 (length prefix))))
    (string-trim " " (subseq args (length prefix)))))

(defun run-slash (args session-id)
  "The /hotwire command: help, the library, or one circuit checked or priced."
  (declare (ignore session-id))
  (running-settings)
  (let* ((args (nlk:trimmed (or args "")))
         (bom (command-target args "bom "))
         (explain (command-target args "explain "))
         (check (command-target args "check ")))
    (cond
      ((or (zerop (length args)) (string-equal args "help"))
       (format nil "~a~%~%Reference circuits:~%~a" +usage+ (library-text)))
      ((string-equal args "library") (library-text-of nil))
      (bom (answering (bom-text (design-of nil nil bom) "E24")))
      (explain (answering (explain-text (design-of nil nil explain) '() 0.1d0)))
      (check (answering (check-text (design-of nil nil check) '())))
      (t (answering (check-text (design-of nil nil args) '()))))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +manual+
  "hotwire: an analog circuit bench. A circuit is a SPICE netlist with a few
'*@' comment annotations: '*@ in VIN' (the signal source), '*@ out out' (the node
the user hears), '*@ supply VCC', '*@ pot RV1 a w b 100k taper=log pos=0.5' and
'*@ spec gain_db(1k) > 18' (a requirement, repeatable). Elements: R C L V I D Q,
an ideal op-amp 'X1 in+ in- vcc vee out TL072', a VCVS 'E1 a b c d 10'. Built-in
parts need no .model line: 1N4148 1N914 1N34A 1N5817 1N4001 LED_RED LED_GREEN
2N3904 2N2222 BC549C 2N5088 MPSA18 2N3906 BC559C TL071 TL072 NE5532 LM358.
Values may be written 4.7k 100n 2meg 1e-9 4k7; M is milli. Ground is node 0.
what is simulated: the electrical rule check (floating nodes, no DC path,
shorted parts, source loops), the DC operating point by Newton's method, the
small-signal AC sweep from 1 Hz to 200 kHz, part stress (resistor power,
capacitor voltage), and every spec. Metrics: gain_db(f) phase(f) fc f3db_low
f3db_high peak_db peak_f v(node) i(element) p(element) supply_current
zin(f) region(Q1), compared with < <= > >= == or 'in lo..hi'. Nothing here is a
transient run: thd, headroom and the audio audition of the original are not
measured, and an op-amp is ideal and linear. hotwire_check takes a netlist, a
library id or a .cir path and returns a verdict, one line per spec with its
measured value and margin, and the rules it broke. hotwire_simulate runs one
analysis (op or ac). hotwire_library lists the bundled reference circuits.
hotwire_explain nudges each part 10% and reports which spec it moves.
hotwire_bom groups equal parts, snaps them to E24/E12 and names the package.
hotwire_tune searches neighbouring preferred values for the parts you name and
returns the values that pass with the most margin. hotwire_yield builds the
circuit up to 300 times inside a part tolerance and counts the builds that still
pass every spec. hotwire_export writes a self-contained ngspice/LTspice deck, or
JSON with the verdict. What this rewrite does not do: play the circuit (no
transient solver, no WAV), compare two WAV takes for knob settings, lay out a
stripboard, or write a KiCad netlist."
  "What (help :hotwire) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell hotwire
  (:section ("hotwire")
    (:guide "nothing here is required; ac_points sets how finely the AC sweep is sampled, max_elements bounds what one netlist may carry")
    ("ac_points" :integer :default 60 :min 5 :doc "AC sweep samples per decade, from 1 Hz to 200 kHz")
    ("max_elements" :integer :default 400 :min 10 :doc "the most elements one netlist may carry"))
  (:settings (lambda (values table) (declare (ignore table)) values))
  (:tool "hotwire_check" (nlk:contract 'hotwire-design) 'run-check)
  (:tool "hotwire_simulate" (nlk:contract 'hotwire-analysis) 'run-simulate)
  (:tool "hotwire_library" (nlk:contract 'hotwire-library-query) 'run-library)
  (:tool "hotwire_explain" (nlk:contract 'hotwire-explain-query) 'run-explain)
  (:tool "hotwire_bom" (nlk:contract 'hotwire-bom-query) 'run-bom)
  (:tool "hotwire_tune" (nlk:contract 'hotwire-tune-query) 'run-tune)
  (:tool "hotwire_yield" (nlk:contract 'hotwire-yield-query) 'run-yield)
  (:tool "hotwire_export" (nlk:contract 'hotwire-export-query) 'run-export)
  (:command "hotwire" 'run-slash
            :description "Analog circuit bench: check, explain, price or export a circuit"
            :argument-hint "[library|check <id|file.cir>|bom <id|file.cir>|explain <id|file.cir>]")
  (:help :hotwire "hotwire: check, simulate and explain an analog circuit - /hotwire check <id>" +manual+))
