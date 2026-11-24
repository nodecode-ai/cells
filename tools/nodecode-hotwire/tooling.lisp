;;;; tooling.lisp --- what the original did around a check: explain, tune, yield,
;;;; export.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; hotwire_explain nudges every part 10% and reports which spec each one
;;;; moves; hotwire_tune picks preferred values that pass with the most margin;
;;;; hotwire_yield builds the circuit N times inside real part tolerance and
;;;; counts the builds that still pass; hotwire_export writes a deck another
;;;; SPICE can run, or the circuit as JSON. All of it runs on the same solver
;;;; the check uses: a perturbed design is a new design, solved from scratch.

(in-package #:nodecode-hotwire)

;;; --- a design with other part values ----------------------------------------

(defun design-with (design &key values pots)
  "DESIGN with element VALUES (name . value) and pot POSITIONS: a new design.
The nodes and their numbering do not move, so the solve is the only work."
  (let ((copy (copy-design design)))
    (setf (design-elements copy)
          (loop for element in (design-elements design)
                for name = (element-name element)
                for replacement = (assoc name values :test #'string=)
                for position = (and (element-pot element)
                                    (assoc (element-pot element) pots :test #'string=))
                collect (let ((new (copy-element element)))
                          (cond
                            (replacement (setf (element-value new) (cdr replacement)))
                            (position
                             (let* ((pot (gethash (element-pot element) (design-pots design)))
                                    (fraction (pot-fraction (cdr position) (pot-taper pot)))
                                    (share (if (string= name (concatenate 'string (pot-name pot) "_A"))
                                               fraction
                                               (- 1d0 fraction))))
                               (setf (element-value new) (max 1d0 (* (pot-value pot) share))))))
                          new)))
    (when pots
      (setf (design-pots copy) (copy-hash-table (design-pots design)))
      (loop for name in pots
            for pot = (gethash name (design-pots copy))
            when pot
              do (setf (pot-pos pot) (coerce (cdr (assoc name pots :test #'string=)) 'double-float))))
    copy))

(defun design-spec-structs (design extra)
  "Every spec the design carries and EXTRA, parsed once."
  (loop for text in (append (design-specs design) extra)
        collect (parse-spec text)))

(defun spec-readings (bench specs)
  "SPECS measured on BENCH: a list of (pass margin measured unit why) each."
  (loop for spec in specs
        collect (handler-case
                    (multiple-value-bind (measured unit why) (measure bench spec)
                      (multiple-value-bind (pass margin) (compare-spec spec measured)
                        (list pass margin measured unit why)))
                  (hotwire-error (failure)
                    (list nil nil nil nil (hotwire-error-detail failure))))))

(defun signed-value (value unit)
  (format nil "~a~a" (if (minusp value) "-" "+") (format-value (abs value) unit)))

;;; --- explain ----------------------------------------------------------------

(defun explain-specs (design extra)
  "What to measure while nudging: the design's specs, or gain and corners."
  (let ((specs (design-spec-structs design extra)))
    (if specs
        specs
        (if (and (design-input design) (design-output design))
            (list (parse-spec "gain_db(1k) > -1000")
                  (parse-spec "f3db_low < 1g")
                  (parse-spec "f3db_high < 1g"))
            (netlist-error 0 "add '*@ spec' lines (gain, corner, voltages) so there is something to explain")))))

(defun explain-effect (spec baseline trial)
  "SPEC's movement from BASELINE to TRIAL, as the words a model reads."
  (let* ((base (third baseline)) (base-unit (fourth baseline))
         (now (third trial)) (unit (fourth trial)))
    (cond ((or (not (numberp base)) (not (numberp now))) nil)
          ((zerop (abs (- now base))) (format nil "~a none" (spec-metric spec)))
          (t (format nil "~a ~a" (spec-metric spec)
                     (signed-value (- now base) (or unit base-unit "")))))))

(defun explain-line (label change baseline trial specs bench)
  "One part's effect on every spec, as the line a model reads."
  (if (not (bench-converged bench))
      (format nil "~a ~a: the operating point no longer converges" label change)
      (let ((effects (remove nil (mapcar #'explain-effect specs baseline trial))))
        (if effects
            (format nil "~a ~a: ~{~a~^; ~}" label change effects)
            (format nil "~a ~a: no measurable effect" label change)))))

(defun explain-text (design extra step)
  "Every part's measured effect on the specs, one line each."
  (let* ((specs (explain-specs design extra))
         (step (if (and (numberp step) (plusp step)) (coerce step 'double-float) 0.1d0))
         (base (bench-of design))
         (baseline (spec-readings base specs)))
    (unless (bench-converged base)
      (netlist-error 0 "the DC operating point does not converge, so nothing can be explained"))
    (with-output-to-string (out)
      (format out "What each part does: ~a (every part nudged ~a%)~%"
              (design-title design) (number-text (* 100 step)))
      (format out "Baseline: ~{~a~^; ~}~%"
              (loop for spec in specs
                    for (pass margin measured unit) in baseline
                    collect (format nil "~a ~a" (spec-metric spec)
                                    (format-value measured (or unit "")))))
      (loop for element in (design-elements design)
            when (and (member (element-kind element) '(:r :c :l) :test #'eq)
                      (not (element-pot element))
                      (plusp (element-value element)))
              do (let* ((moved (* (element-value element) (+ 1d0 step)))
                        (trial (design-with design
                                            :values (list (cons (element-name element) moved))))
                        (bench (bench-of trial))
                        (readings (and (bench-converged bench) (spec-readings bench specs))))
                   (format out "~a~%"
                           (explain-line (element-name element)
                                         (format nil "~a +~a%" (element-name element)
                                                 (number-text (* 100 step)))
                                         baseline readings specs bench))))
      (loop for name being the hash-keys of (design-pots design)
            for pot = (gethash name (design-pots design))
            do (let* ((position (pot-pos pot))
                      (moved (if (<= position 0.9d0) (min 1d0 (+ position 0.1d0)) (- position 0.1d0)))
                      (trial (design-with design :pots (list (cons name moved))))
                      (bench (bench-of trial))
                      (readings (and (bench-converged bench) (spec-readings bench specs))))
                 (format out "~a~%" (explain-line name (format nil "knob ~a->~a"
                                                                             (number-text position)
                                                                             (number-text moved))
                                                   baseline readings specs bench))))
      (format out "A spec a part does not list is one that part does not move. Measure again after a change with hotwire_check.~%"))))

;;; --- tune -------------------------------------------------------------------

(defun spec-scale (spec)
  "The margin that counts as full margin for SPEC, in its own unit."
  (cond ((eq (spec-op spec) :in)
         (max (/ (abs (- (spec-high spec) (spec-low spec))) 2d0) 1d-12))
        ((string= (or (metric-unit (spec-metric spec)) "") "dB") 3d0)
        (t (max (* 0.1d0 (abs (or (spec-low spec) 0d0))) 1d-12))))

(defun tune-score (design specs)
  "How far DESIGN is from passing SPECS: (values violation worst-margin readings)."
  (let ((bench (bench-of design)))
    (unless (bench-converged bench)
      (return-from tune-score (values 10d0 nil nil)))
    (let ((readings (spec-readings bench specs))
          (violation 0d0) (worst nil))
      (loop for spec in specs
            for reading in readings
            do (let ((pass (first reading)) (margin (second reading)))
                 (cond ((or (null margin) (not (numberp margin)))
                        (unless pass (incf violation 10d0)))
                       (t (let ((scaled (/ (coerce margin 'double-float) (spec-scale spec))))
                            (when (minusp scaled) (incf violation (- scaled)))
                            (setf worst (if worst (min worst scaled) scaled)))))))
      (values violation worst readings))))

(defun tune-candidates (value series steps)
  (cons (snap-value value series) (series-neighbours value series steps)))

(defun tune-text (design names extra series rounds steps)
  "Preferred values for NAMES that pass SPECS with the most margin."
  (let* ((specs (design-spec-structs design extra))
         (rounds (max 1 (min 8 (or rounds 3))))
         (steps (max 1 (min 6 (or steps 2)))))
    (when (null specs)
      (netlist-error 0 "nothing to tune for: add '*@ spec' lines or pass specs"))
    (when (null names)
      (netlist-error 0 "name the parts to tune, e.g. vary=['R3', 'C2'] (hotwire_explain shows which parts set each spec)"))
    (let ((elements (loop for name in names
                          for element = (find (string-upcase name) (design-elements design)
                                              :key #'element-name :test #'string=)
                          do (unless element (netlist-error 0 "the netlist has no part called '~a'" name))
                             (unless (member (element-kind element) '(:r :c :l) :test #'eq)
                               (netlist-error 0 "~a is not a resistor, capacitor or inductor; only those can be tuned" name))
                             (when (element-pot element)
                               (netlist-error 0 "~a is half of a pot; turn the knob instead" name))
                          collect element)))
      (let* ((series-of (lambda (element)
                           (if series (series-name series)
                               (if (eq (element-kind element) :r) "E24" "E12"))))
             (start (loop for element in elements
                           collect (cons (element-name element)
                                         (snap-value (element-value element) (funcall series-of element)))))
             (best (design-with design :values start)))
        (multiple-value-bind (before-score before-worst before-readings) (tune-score best specs)
          (declare (ignore before-worst))
          (let ((best-score before-score) (best-readings before-readings) (current (copy-alist start)))
            (loop repeat rounds
                  do (let ((improved nil))
                       (loop for element in elements
                             for name = (element-name element)
                             for current-value = (cdr (assoc name current :test #'string=))
                             do (dolist (candidate (tune-candidates current-value
                                                                     (funcall series-of element) steps))
                                  (when (/= candidate current-value)
                                    (let* ((trial (acons name candidate current))
                                           (trial-design (design-with design :values trial)))
                                      (multiple-value-bind (score worst readings) (tune-score trial-design specs)
                                        (declare (ignore worst))
                                        (when (< score best-score)
                                          (setf best-score score best-readings readings
                                                current trial improved t)))))))
                       (unless improved (return))))
            (with-output-to-string (out)
              (format out "Tune: ~a, ~a preferred values~%" (design-title design)
                      (or (and series (series-name series)) "E24 for resistors, E12 for capacitors"))
              (format out "Before: ~{~a~^; ~}~%"
                      (loop for spec in specs for (pass m measured unit) in before-readings
                            collect (format nil "~a ~a" (spec-metric spec)
                                            (format-value measured (or unit "")))))
              (loop for element in elements
                    for name = (element-name element)
                    for value = (cdr (assoc name current :test #'string=))
                    do (format out "  ~a ~a -> ~a~%" name (format-value (element-value element) "")
                               (format-value value "")))
              (format out "After: ~{~a~^; ~}~%"
                      (loop for spec in specs for (pass m measured unit) in best-readings
                            collect (format nil "~a ~a" (spec-metric spec)
                                            (format-value measured (or unit "")))))
              (format out "~a~%" (if (every #'first best-readings)
                                     "All specs pass with these values."
                                     "Some specs still fail: this topology or these parts cannot reach them; change the circuit, not only the values."))
              (format out "Check the tuned netlist with hotwire_check before building it.~%"))))))))

;;; --- yield ------------------------------------------------------------------

(defun random-unit (state)
  "A deterministic draw in -1..1, and the state after it."
  (let ((next (mod (+ (* 1103515245 state) 12345) 2147483648)))
    (values (- (* 2d0 (/ next 2147483648d0)) 1d0) next)))

(defun yield-text (design extra builds tolerance)
  "How many of BUILDS circuits inside TOLERANCE still meet every spec."
  (let* ((specs (design-spec-structs design extra))
         (builds (max 10 (min 300 (or builds 60))))
         (tolerance (if (and (numberp tolerance) (plusp tolerance) (<= tolerance 0.5d0))
                        (coerce tolerance 'double-float)
                        0.05d0))
         (parts (remove-if-not (lambda (element)
                                 (and (member (element-kind element) '(:r :c :l) :test #'eq)
                                      (not (element-pot element))))
                               (design-elements design))))
    (when (null specs)
      (netlist-error 0 "no specs to test: add '*@ spec' lines or pass specs"))
    (when (null parts)
      (netlist-error 0 "the netlist has no resistors, capacitors or inductors to spread"))
    (let ((passing 0) (failures (make-list (length specs) :initial-element 0))
          (state 20261108))
      (loop repeat builds
            do (let ((values '()))
                 (dolist (element parts)
                   (multiple-value-bind (draw next) (random-unit state)
                     (setf state next)
                     (push (cons (element-name element) (* (element-value element) (+ 1d0 (* tolerance draw))))
                           values)))
                 (let* ((trial (design-with design :values values))
                        (bench (bench-of trial)))
                   (if (not (bench-converged bench))
                       (incf (car failures))
                       (let ((readings (spec-readings bench specs)))
                         (if (every #'first readings)
                             (incf passing)
                             (loop for spec in specs
                                   for (pass margin measured unit) in readings
                                   for index from 0
                                   do (unless pass (incf (nth index failures))))))))))
      (with-output-to-string (out)
        (format out "Yield: ~d/~d builds pass every spec at ~a% part spread~%"
                passing builds (number-text (* 100 tolerance)))
        (format out "~d% of builds would work, ~d would not.~%"
                (round (* 100 (/ passing builds))) (- builds passing))
        (loop for spec in specs for count in failures
              when (plusp count)
                do (format out "  ~a failed in ~d build~:p~%" (spec-text spec) count))
        (format out "Part spread: resistors, capacitors and inductors drawn uniformly inside +/-~a%.~%"
                (number-text (* 100 tolerance)))
        (unless (plusp passing)
          (format out "Nothing survives: measure once with hotwire_check, then widen the spec or change the topology.~%"))))))

;;; --- export -----------------------------------------------------------------

(defparameter +spice-types+
  '((:d . "D") (:npn . "NPN") (:pnp . "PNP") (:opamp . "OPAMP"))
  "The SPICE model type each kind of device is written as.")

(defun spice-number (value)
  "VALUE as a decimal another SPICE reads: the exponent always marked with E."
  (map 'string (lambda (character)
                 (if (member character '(#\d #\D #\e #\E)) #\E character))
       (format nil "~,6e" (coerce value 'double-float))))

(defun spice-model-line (model)
  "MODEL as a .model line: the name, its type and every numeric parameter."
  (let ((params (loop for (key value) on (model-params model) by #'cddr
                      when (numberp value)
                        collect (format nil "~a=~a" (string-upcase (symbol-name key))
                                        (spice-number value)))))
    (format nil ".model ~a ~a(~{~a~^ ~})" (model-name model)
            (or (cdr (assoc (model-type model) +spice-types+)) "D") params)))

(defun spice-element-line (element)
  "ELEMENT as the line a SPICE deck carries."
  (let ((name (element-name element)) (nodes (element-nodes element))
        (value (element-value element)))
    (ecase (element-kind element)
      (:r (format nil "~a ~a ~a ~a" name (first nodes) (second nodes) (spice-number value)))
      (:c (format nil "~a ~a ~a ~a" name (first nodes) (second nodes) (spice-number value)))
      (:l (format nil "~a ~a ~a ~a" name (first nodes) (second nodes) (spice-number value)))
      (:v (format nil "~a ~a ~a DC ~a AC ~a" name (first nodes) (second nodes)
                   (spice-number value) (spice-number (getf (element-params element) :ac 1d0))))
      (:i (format nil "~a ~a ~a DC ~a AC ~a" name (first nodes) (second nodes)
                   (spice-number value) (spice-number (getf (element-params element) :ac 1d0))))
      (:d (format nil "~a ~a ~a ~a" name (first nodes) (second nodes) (element-model element)))
      (:q (format nil "~a ~a ~a ~a ~a" name (first nodes) (second nodes) (third nodes)
                   (element-model element)))
      (:x (format nil "~a ~a ~a ~a ~a ~a OPAMP" name (first nodes) (second nodes)
                   (third nodes) (fourth nodes) (fifth nodes)))
      (:e (format nil "~a ~a ~a ~a ~a ~a" name (first nodes) (second nodes)
                   (third nodes) (fourth nodes) (spice-number value))))))

(defun design-deck (design)
  "DESIGN as a self-contained deck for ngspice or LTspice."
  (with-output-to-string (out)
    (format out "* ~a~%" (design-title design))
    (format out "* exported from an analog circuit check; the '@' annotation lines are comments here~%")
    (format out "* op-amps are ideal: a 100k-gain VCVS with its output against ground, no supply current~%")
    (dolist (line (append (list (format nil "title ~a" (design-title design)))
                           (loop for supply in (reverse (design-supplies design))
                                 collect (format nil "supply ~a" supply))
                           (and (design-input design) (list (format nil "in ~a" (design-input design))))
                           (and (design-output design) (list (format nil "out ~a" (design-output design))))
                           (loop for spec in (reverse (design-specs design))
                                 collect (format nil "spec ~a" spec))))
      (format out "*@ ~a~%" line))
    (loop for name being the hash-keys of (design-pots design)
          do (let ((pot (gethash name (design-pots design))))
               (format out "*@ pot ~a ~a ~a ~a ~a taper=~a pos=~a~%"
                       name (pot-a pot) (pot-w pot) (pot-b pot)
                       (format-value (pot-value pot) "") (pot-taper pot)
                       (number-text (pot-pos pot)))))
    (loop for element in (design-elements design)
          do (format out "~a~%" (spice-element-line element)))
    (loop for name being the hash-keys of (design-models design)
          unless (string= name "OPAMP")
            do (format out "~a~%" (spice-model-line (gethash name (design-models design)))))
    (when (find :x (design-elements design) :key #'element-kind)
      (format out ".subckt OPAMP in+ in- vcc vee out~%E100 out 0 in+ in- 100000~%.ends~%"))
    (when (and (design-input design) (design-output design))
      (format out ".ac dec ~d 1 200k~%" (setting :ac-points)))
    (format out ".op~%.end~%")))

(defun json-string (text)
  "TEXT as one JSON string, its quotes and control characters escaped."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across (or text "")
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (#\Return nil)
               (t (write-char character out))))
    (write-char #\" out)))

(defun json-list (items)
  "ITEMS as a JSON array."
  (format nil "[~{~a~^, ~}]" items))

(defun element-json (element)
  "ELEMENT as one JSON object."
  (format nil "{\"name\": ~a, \"kind\": ~a, \"nodes\": ~a, \"value\": ~a, \"model\": ~a}"
          (json-string (string-downcase (element-name element)))
          (json-string (string-downcase (symbol-name (element-kind element))))
          (json-list (mapcar #'json-string (element-nodes element)))
          (spice-number (element-value element))
          (if (element-model element)
              (json-string (string-downcase (element-model element)))
              "null")))

(defun pot-json (pot)
  "POT as one JSON object."
  (format nil "{\"name\": ~a, \"a\": ~a, \"wiper\": ~a, \"b\": ~a, \"value\": ~a, \"taper\": ~a, \"position\": ~a}"
          (json-string (string-downcase (pot-name pot))) (json-string (pot-a pot))
          (json-string (pot-w pot)) (json-string (pot-b pot))
          (spice-number (pot-value pot)) (json-string (pot-taper pot))
          (spice-number (pot-pos pot))))

(defun result-json (result)
  "RESULT as one JSON object."
  (format nil "{\"spec\": ~a, \"pass\": ~a, \"measured\": ~a}"
          (json-string (spec-text (result-spec result)))
          (if (result-pass result) "true" "false")
          (if (numberp (result-measured result))
              (spice-number (result-measured result))
              (json-string (format-value (result-measured result) "")))))

(defun rule-json (issue)
  "ISSUE as one JSON string."
  (json-string (format nil "~(~a~): ~a" (getf issue :level) (getf issue :message))))

(defun design-json (design extra)
  "DESIGN and the verdict it gets as one JSON object."
  (multiple-value-bind (verdict summary bench results issues) (check-design design extra)
    (declare (ignore bench))
    (format nil "{\"title\": ~a, \"input\": ~a, \"output\": ~a, \"supplies\": ~a, \"specs\": ~a, \"elements\": ~a, \"pots\": ~a, \"verdict\": ~a, \"summary\": ~a, \"results\": ~a, \"rules\": ~a}"
            (json-string (design-title design))
            (if (design-input design) (json-string (design-input design)) "null")
            (if (design-output design) (json-string (design-output design)) "null")
            (json-list (mapcar (lambda (supply) (json-string (string-downcase supply)))
                                (design-supplies design)))
            (json-list (mapcar #'json-string (design-specs design)))
            (json-list (mapcar #'element-json (design-elements design)))
            (json-list (loop for name being the hash-keys of (design-pots design)
                              collect (pot-json (gethash name (design-pots design)))))
            (json-string verdict) (json-string summary)
            (json-list (mapcar #'result-json results))
            (json-list (mapcar #'rule-json issues)))))

(defun export-text (design extra format)
  "DESIGN as FORMAT: a SPICE deck, or JSON with its verdict."
  (let ((format (string-downcase (string-trim " " (or format "spice")))))
    (cond ((string= format "spice") (design-deck design))
          ((string= format "json") (design-json design extra))
          (t (netlist-error 0 "format must be spice or json, not '~a'" format)))))
