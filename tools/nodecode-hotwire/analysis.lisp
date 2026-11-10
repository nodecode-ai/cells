;;;; analysis.lisp --- measurable specs, the verdict, the report a model reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The grammar is the original's: 'gain_db(1k) > 18', 'fc in 960..1160 Hz',
;;;; 'v(out) in 4.3..4.7', 'region(Q1) == active', 'zin(1k) > 250k'. Every
;;;; result carries what was measured, what was wanted, the margin, and one
;;;; plain sentence, so a failure explains itself without reading code.

(in-package #:nodecode-hotwire)

(declaim (ftype function setting))

(defconstant +epsilon+ 1d-30)

(defstruct (spec (:constructor make-spec (text metric arguments op low high)))
  text metric arguments op low high)

(defstruct (result (:constructor make-result (spec pass measured target text margin)))
  spec pass measured target text margin)

(defstruct (bench (:constructor make-bench (design)))
  design op converged method steps sweep)

(defparameter +metric-aliases+
  '(("bias" . "region") ("gain" . "gain_db") ("f3db" . "fc")
    ("i_supply" . "supply_current") ("current" . "supply_current"))
  "The names the original also accepted, read as the metric they mean.")

(defparameter +metrics+
  '(("gain_db" "dB") ("phase" "deg") ("fc" "Hz") ("f3db_low" "Hz") ("f3db_high" "Hz")
    ("peak_db" "dB") ("peak_f" "Hz") ("v" "V") ("i" "A") ("p" "W")
    ("supply_current" "A") ("zin" "ohm") ("region" ""))
  "Every metric this cell measures, and its unit.")

(defun metric-unit (metric)
  (second (assoc metric +metrics+ :test #'string=)))

(defun split-on (separator text)
  (let ((parts '()) (start 0))
    (loop for at = (search separator text :start2 start)
          do (if at
                 (progn (push (string-trim " " (subseq text start at)) parts) (setf start (+ at (length separator))))
                 (progn (push (string-trim " " (subseq text start)) parts) (return (nreverse parts)))))))

(defun spec-number (text metric)
  "TEXT as a number: a percentage, a unit the metric itself carries, or a value."
  (declare (ignore metric))
  (let* ((text (string-trim " " text))
         (percent (and (plusp (length text)) (char= (char text (1- (length text))) #\%))))
    (declare (ignore percent))
    (setf text (string-right-trim "%" text))
    (dolist (unit '("deg" "dB" "db" "Hz" "hz" "°"))
      (when (and (> (length text) (length unit))
                 (string-equal unit (subseq text (- (length text) (length unit)))))
        (return (setf text (string-right-trim " " (subseq text 0 (- (length text) (length unit))))))))
    (parse-value text)))

(defun parse-spec (text)
  "TEXT as a SPEC, or a HOTWIRE-ERROR saying how to write one."
  (let* ((text (string-trim " " text))
         (open (position #\( text))
         (close (and open (position #\) text)))
         (space (position-if (lambda (character) (member character '(#\Space #\Tab))) text))
         (metric (string-downcase (string-trim " " (if open (subseq text 0 open)
                                                     (subseq text 0 (or space (length text)))))))
         (arguments (if (and open close) (split-on "," (subseq text (1+ open) close)) '()))
         (rest (string-trim " " (cond (close (subseq text (1+ close)))
                                      ((and space (null open)) (subseq text space))
                                      (t "")))))
    (setf metric (or (cdr (assoc metric +metric-aliases+ :test #'string=)) metric))
    (unless (assoc metric +metrics+ :test #'string=)
      (netlist-error 0 "unknown spec metric '~a' (known: ~{~a~^, ~})" metric (mapcar #'first +metrics+)))
    (cond
      ((or (zerop (length rest)) (string-equal rest "in"))
       (or (and arguments (make-spec text metric arguments :in (spec-number (first arguments) metric) nil))
           (netlist-error 0 "spec '~a' wants a target, e.g. 'gain_db(1k) > 18' or 'fc in 960..1160 Hz'" text)))
      ((and (<= 3 (length rest)) (string-equal "in" (subseq rest 0 2)) (not (alphanumericp (char rest 2))))
       (let ((range (split-on ".." (string-trim " " (subseq rest 2)))))
         (unless (= 2 (length range))
           (netlist-error 0 "spec '~a': 'in' wants a range, e.g. 'fc in 960..1160 Hz'" text))
         (make-spec text metric arguments :in (spec-number (first range) metric) (spec-number (second range) metric))))
      (t
       (let* ((operator (cond ((and (<= 2 (length rest)) (string= "<=" (subseq rest 0 2))) :<=)
                              ((and (<= 2 (length rest)) (string= ">=" (subseq rest 0 2))) :>=)
                              ((and (<= 2 (length rest)) (string= "==" (subseq rest 0 2))) :==)
                              ((char= (char rest 0) #\<) :<)
                              ((char= (char rest 0) #\>) :>)
                              ((char= (char rest 0) #\=) :==)
                              (t nil)))
              (body (string-trim " " (if operator (subseq rest (if (member operator '(:<= :>= :==) :test #'eq) 2 1)) rest))))
         (unless operator
           (netlist-error 0 "spec '~a' has no comparison: write '>', '<', '>=' or '<=' a value, or 'in lo..hi'" text))
         (if (string-equal metric "region")
             (make-spec text metric arguments :== (string-downcase body) nil)
             (make-spec text metric arguments operator (spec-number body metric) nil)))))))

(defun element-current (design solution name)
  "The DC current through element NAME, in the SPICE sign."
  (let* ((*design* design)
         (element (find (string-upcase name) (design-elements design)
                        :key #'element-name :test #'string=)))
    (unless element
      (netlist-error 0 "no element named ~a" name))
    (let* ((nodes (element-nodes element))
           (a (node-voltage solution (first nodes)))
           (b (node-voltage solution (second nodes)))
           (value (element-value element)))
      (case (element-kind element)
        (:r (/ (- a b) (if (zerop value) 1d-12 value)))
        ((:v :l :e :x) (aref solution (1- (element-row element))))
        (:c 0d0)
        (:i value)
        (:d (let* ((saturation (parameter design element :is 1d-14))
                   (emission (parameter design element :n 1d0))
                   (thermal (* emission +thermal+)))
              (* saturation (- (exp (min (/ (- a b) thermal) 40d0)) 1d0))))
        (:q (let* ((polarity (if (eq (model-kind design (element-model element)) :pnp) -1d0 1d0))
                   (saturation (parameter design element :is 1d-14))
                   (reverse-beta (parameter design element :br 1d0))
                   (base (* polarity (- (node-voltage solution (second nodes))
                                        (node-voltage solution (third nodes)))))
                   (collector (* polarity (- (node-voltage solution (second nodes)) a)))
                   (forward (* saturation (- (exp (min (/ base +thermal+) 40d0)) 1d0)))
                   (reverse (* saturation (- (exp (min (/ collector +thermal+) 40d0)) 1d0))))
              (* polarity (- forward (* reverse (+ 1d0 (/ 1d0 reverse-beta)))))))))))

(defun bench-of (design)
  "A BENCH: the design, its operating point, and an empty sweep."
  (let ((bench (make-bench design)))
    (multiple-value-bind (solution converged method steps) (operating-point design)
      (setf (bench-op bench) solution (bench-converged bench) converged
            (bench-method bench) method (bench-steps bench) steps))
    bench))

(defun sweep-of (bench)
  "The AC sweep: (values frequencies decibels responses), computed once."
  (or (bench-sweep bench)
      (let* ((design (bench-design bench))
             (output (design-output design))
             (per-decade (setting :ac-points))
             (points (max 8 (round (* per-decade (log 200000d0 10d0)))))
             (frequencies (logspace 1d0 200000d0 points))
             (db (make-array points :element-type 'double-float))
             (responses (make-array points)))
        (unless (and (design-input design) output)
          (netlist-error 0 "AC specs need '*@ in <source>' and '*@ out <node>' in the netlist"))
        (dotimes (index points)
          (let ((response (ac-point design (bench-op bench) (aref frequencies index) output)))
            (setf (aref responses index) response
                  (aref db index) (if response (decibels response) -300d0))))
        (setf (bench-sweep bench) (list frequencies db responses)))))

(defun corner-frequency (bench high-pass)
  "The -3 dB corner of the sweep, interpolated in decades: (values Hz or NIL)."
  (destructuring-bind (frequencies db responses) (sweep-of bench)
    (declare (ignore responses))
    (let* ((count (length frequencies))
           (peak (loop for index below count maximize (aref db index)))
           (target (- peak 3d0)))
      (labels ((crossing (from to step)
                 (loop for index from from to to by step
                       for previous = (aref db (- index step))
                       for current = (aref db index)
                       when (and (< previous target) (>= current target))
                         do (return (* (aref frequencies (- index step))
                                       (expt 10d0 (* (/ (- target previous) (- current previous))
                                                     (log (/ (aref frequencies index)
                                                             (aref frequencies (- index step)))
                                                          10d0))))))))
        (if high-pass
            (crossing 1 (1- count) 1)
            (loop for index from 0 below (1- count)
                  when (and (>= (aref db index) target) (< (aref db (1+ index)) target))
                    do (let ((previous (aref db index)) (current (aref db (1+ index))))
                         (return (* (aref frequencies index)
                                    (expt 10d0 (* (/ (- previous target) (- previous current))
                                                  (log (/ (aref frequencies (1+ index))
                                                          (aref frequencies index))
                                                       10d0))))))))))))

(defun band-peak (bench)
  "The highest gain in the band and where it is."
  (destructuring-bind (frequencies db responses) (sweep-of bench)
    (declare (ignore responses))
    (let ((best 0))
      (loop for index from 1 below (length frequencies)
            do (when (> (aref db index) (aref db best)) (setf best index)))
      (values (aref db best) (aref frequencies best)))))

(defun transistor-region (design bench name)
  "NAME's operating region at idle: cutoff, active, saturation or reverse."
  (let* ((*design* design)
         (element (find (string-upcase name) (design-elements design)
                       :key #'element-name :test #'string=)))
    (unless (and element (eq (element-kind element) :q))
      (netlist-error 0 "region(~a): no transistor named ~a" name name))
    (let* ((solution (bench-op bench))
           (nodes (element-nodes element))
           (polarity (if (eq (model-kind design (element-model element)) :pnp) -1d0 1d0))
           (vbe (* polarity (- (node-voltage solution (second nodes)) (node-voltage solution (third nodes)))))
           (vbc (* polarity (- (node-voltage solution (second nodes)) (node-voltage solution (first nodes))))))
      (cond ((and (< vbe 0.45d0) (< vbc 0.45d0)) "cutoff")
            ((and (>= vbe 0.45d0) (< vbc 0.4d0)) "active")
            ((>= vbc 0.4d0) "saturation")
            (t "reverse")))))

(defun supply-current (design bench)
  "The current the circuit draws from its supplies, in amps."
  (let* ((*design* (bench-design bench))
         (solution (bench-op bench))
         (names (or (design-supplies design)
                    (loop for element in (design-elements design)
                          when (and (eq (element-kind element) :v)
                                    (not (equal (element-name element) (design-input design)))
                                    (plusp (abs (element-value element))))
                            collect (element-name element)))))
    (loop for name in names
          sum (- (element-current design solution name)))))

(defun measure (bench spec)
  "SPEC measured on BENCH: (values value-or-string or NIL unit why)."
  (let* ((*design* (bench-design bench))
         (design (bench-design bench))
         (metric (spec-metric spec))
         (arguments (spec-arguments spec))
         (frequency (and arguments (ignore-errors (parse-value (first arguments))))))
    (labels ((response (f)
               (let ((value (ac-point design (bench-op bench) f (design-output design))))
                 (unless value (netlist-error 0 "the AC matrix is singular at ~a Hz" f))
                 value))
             (gain-at (f) (decibels (response f))))
      (cond
        ((string= metric "gain_db")
         (unless frequency (netlist-error 0 "gain_db wants a frequency, e.g. gain_db(1k) > 18"))
         (values (gain-at frequency) "dB" nil))
        ((string= metric "phase")
         (unless frequency (netlist-error 0 "phase wants a frequency, e.g. phase(1k) > -10 deg"))
         (values (atan (imagpart (response frequency)) (realpart (response frequency))) "deg" nil))
        ((string= metric "fc")
         (let ((low-pass (corner-frequency bench nil)))
           (if low-pass (values low-pass "Hz" nil)
               (values (corner-frequency bench t) "Hz" nil))))
        ((string= metric "f3db_low") (values (corner-frequency bench t) "Hz" nil))
        ((string= metric "f3db_high") (values (corner-frequency bench nil) "Hz" nil))
        ((string= metric "peak_db")
         (values (band-peak bench) "dB" nil))
        ((string= metric "peak_f")
         (values (nth-value 1 (band-peak bench)) "Hz" nil))
        ((string= metric "v")
         (values (node-voltage (bench-op bench) (string-downcase (first arguments))) "V" nil))
        ((string= metric "i")
         (values (element-current design (bench-op bench) (first arguments)) "A" nil))
        ((string= metric "p")
         (let* ((current (element-current design (bench-op bench) (first arguments)))
                (element (find (string-upcase (first arguments)) (design-elements design)
                               :key #'element-name :test #'string=))
                (nodes (element-nodes element))
                (voltage (abs (- (node-voltage (bench-op bench) (first nodes))
                                  (node-voltage (bench-op bench) (second nodes))))))
           (values (* (abs current) voltage) "W" nil)))
        ((string= metric "supply_current")
         (values (supply-current design bench) "A" nil))
        ((string= metric "zin")
         (unless frequency (netlist-error 0 "zin wants a frequency, e.g. zin(1k) > 250k"))
         (let* ((solution (ac-solution design (bench-op bench) frequency))
                (branch (gethash (design-input design) (design-rows design))))
           (unless (and solution branch)
             (netlist-error 0 "zin needs '*@ in <source>' naming a voltage source"))
           (values (/ 1d0 (max (abs (aref solution (1- branch))) 1d-30)) "ohm" nil)))
        ((string= metric "region")
         (values (transistor-region design bench (first arguments)) "" nil))
        (t (values nil "" (format nil "~a is not measured by this cell" metric)))))))

(defun number-text (value)
  "VALUE written with three decimals, trailing zeros taken off."
  (let ((text (format nil "~,3f" (coerce value 'double-float))))
    (setf text (string-right-trim "0" text))
    (if (or (string= text "") (string= text "-")) "0" (string-right-trim "." text))))

(defun format-value (value unit)
  "VALUE in engineering notation, with UNIT."
  (cond ((null value) "-")
        ((stringp value) value)
        (t (let* ((value (if (complexp value) (abs value) (coerce value 'double-float)))
                  (magnitude (abs value))
                  (scaled (cond ((>= magnitude 1d6) (list (/ value 1d6) "M"))
                                ((>= magnitude 1d3) (list (/ value 1d3) "k"))
                                ((and (plusp magnitude) (< magnitude 1d-3)) (list (* value 1d3) "m"))
                                (t (list value ""))))
                  (prefix (second scaled)))
             (format nil "~a~@[~a~]~@[ ~a~]" (number-text (first scaled))
                     (unless (string= prefix "") prefix)
                     (unless (string= unit "") unit))))))

(defun compare-spec (spec measured)
  "MEASURED against SPEC: (values pass margin target-text)."
  (let ((op (spec-op spec)) (low (spec-low spec)) (high (spec-high spec)))
    (cond ((stringp measured)
           (values (and (eq op :==) (string-equal measured (string low)))
                   nil (format nil "== ~a" low)))
          ((null measured) (values nil nil "unmeasured"))
          ((eq op :in)
           (values (and (>= measured low) (<= measured high))
                   (min (- measured low) (- high measured))
                   (format nil "in ~a..~a" (format-value low (metric-unit (spec-metric spec)))
                           (format-value high (metric-unit (spec-metric spec))))))
          (t (let ((target (spec-low spec)))
               (values (ecase op (:> (> measured target)) (:>= (>= measured target))
                          (:< (< measured target)) (:<= (<= measured target))
                          (:== (= measured target)))
                       (ecase op (:> (- measured target)) (:>= (- measured target))
                          (:< (- target measured)) (:<= (- target measured)) (:== 0d0))
                       (format nil "~a ~a" (ecase op (:> ">") (:>= ">=") (:< "<") (:<= "<=") (:== "=="))
                               (format-value target (metric-unit (spec-metric spec))))))))))

(defun evaluate-specs (bench extra)
  "Every spec of the design and EXTRA, measured: a list of RESULTS."
  (loop for text in (append (bench-specs bench) extra)
        collect (let* ((spec (parse-spec text))
                       (unit (metric-unit (spec-metric spec))))
                  (handler-case
                      (multiple-value-bind (measured measured-unit why) (measure bench spec)
                        (declare (ignore why))
                        (multiple-value-bind (pass margin target) (compare-spec spec measured)
                          (make-result spec pass measured target
                                       (format nil "~a wants ~a; measured ~a~@[ (~a)~]"
                                               (spec-metric spec) target
                                               (format-value measured (or measured-unit unit))
                                               (and margin (format nil "margin ~a"
                                                                    (format-value margin unit))))
                                       margin)))
                    (hotwire-error (failure)
                      (make-result spec nil nil "unmeasured"
                                   (format nil "~a: ~a" text (hotwire-error-detail failure)) nil))))))

(defun bench-specs (bench)
  "The '*@ spec' lines the design carries."
  (design-specs (bench-design bench)))

(defun check-design (design extra-specs &key (name nil))
  "Run the whole check on DESIGN: (values verdict summary text results issues)."
  (declare (ignore name))
  (let* ((issues (check-rules design))
         (errors (remove-if-not (lambda (issue) (eq (getf issue :level) :error)) issues))
         (bench (bench-of design))
         (results '()))
    (cond
      (errors
       (values "fail"
               (format nil "~d wiring error(s) before simulation: ~a" (length errors)
                       (getf (first errors) :message))
               nil nil issues))
      ((not (bench-converged bench))
       (values "fail"
               "the DC operating point did not converge: check for a node with no DC path, a transistor with no bias, or a supply wired backwards"
               nil nil issues))
      (t
       (setf results (evaluate-specs bench extra-specs))
       (let* ((failed (remove-if #'result-pass results))
              (warnings (remove-if-not (lambda (issue) (eq (getf issue :level) :warning)) issues))
              (verdict (cond (failed "fail")
                             ((plusp (length warnings)) "pass_with_warnings")
                             (t "pass")))
              (passing (count-if #'result-pass results))
              (summary (format nil "~a~@[. Failing: ~{~a~^; ~}~]~@[. Draws ~a mA~]"
                               (if results (format nil "~d/~d specs pass" passing (length results))
                                   "no specs given (add '*@ spec' lines)")
                               (mapcar #'result-text (subseq failed 0 (min 3 (length failed))))
                               (and (plusp (length (design-supplies design)))
                                    (number-text (* 1000 (supply-current design bench)))))))
         (values verdict (format nil "~a~a." (string-upcase (subseq summary 0 1)) (subseq summary 1))
                 bench results issues))))))

(defun spec-line (result)
  "One spec of the report, as the model reads it: what was asked, what came back."
  (format nil "~a  ~a  ->  ~a" (if (result-pass result) "PASS" "FAIL")
          (spec-text (result-spec result)) (result-text result)))

(defun describe-op (bench)
  "The idle voltages and the bias of every transistor, one line each."
  (let* ((*design* (bench-design bench))
         (design (bench-design bench))
         (solution (bench-op bench))
         (lines '()))
    (loop for index from 1 to (design-node-count design)
          for node = (aref (design-names design) (1- index))
          do (push (format nil "~a ~a" node (format-value (node-voltage solution node) "V")) lines))
    (dolist (element (design-elements design))
      (when (eq (element-kind element) :q)
        (push (format nil "~a ~a" (element-name element) (transistor-region design bench (element-name element)))
              lines)))
    (format nil "~{~a~^, ~}" (nreverse lines))))
