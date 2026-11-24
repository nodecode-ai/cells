;;;; sim.lisp --- the simulator: modified nodal analysis, DC, small-signal AC.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One unknown per node (node 0, ground, is not in the matrix) and one per
;;;; branch of every voltage source, inductor and voltage-controlled source.
;;;; A capacitor is open at DC and j*omega*C in the small-signal sweep; an
;;;; inductor is a short at DC and j*omega*L in series in the sweep. Diodes and
;;;; transistors are exponential, so the operating point is Newton's method on
;;;; the linearised circuit, damped while it is far from the answer. The AC
;;;; sweep linearises the same devices at that operating point, which is exactly
;;;; the Jacobian the last Newton step built.

(in-package #:nodecode-hotwire)

(defvar *design* nil "The design being stamped.")
(defvar *element* nil "The element being stamped.")

(defparameter +thermal+ 0.025852d0 "kT/q at 300 K, in volts.")
(defparameter +op-amp-gain+ 1d6 "The gain of the ideal op-amp this cell models.")

(defun node-index (design node)
  "NODE's row, 0 when it is ground."
  (gethash node (design-index design) 0))

(defun node-voltage (vector node)
  "NODE's voltage in VECTOR, 0 for ground. *DESIGN* names the circuit, so a
reading needs no second argument at every call site."
  (let ((row (gethash node (design-index *design*) 0)))
    (if (zerop row) 0d0 (aref vector (1- row)))))

(defun voltage-at (vector index)
  "The voltage INDEX's row holds in VECTOR; row 0 is ground and reads 0."
  (if (zerop index) 0d0 (aref vector (1- index))))

(defun element-row-of (element)
  (element-row element))

(defun add-entry (matrix row column value)
  "MATRIX[row, column] += VALUE, skipping ground (row or column 0)."
  (when (and (plusp row) (plusp column))
    (incf (aref matrix (1- row) (1- column)) value)))

(defun inject (rhs row amount)
  (when (plusp row) (incf (aref rhs (1- row)) amount)))

(defun stamp-admittance (matrix a b y)
  (add-entry matrix a a y)
  (add-entry matrix b b y)
  (add-entry matrix a b (- y))
  (add-entry matrix b a (- y)))

(defun stamp-source (matrix rhs positive negative row value)
  "A voltage source (or a short) of VALUE between POSITIVE and NEGATIVE, its
current the branch unknown ROW."
  (add-entry matrix positive row 1d0)
  (add-entry matrix negative row -1d0)
  (add-entry matrix row positive 1d0)
  (add-entry matrix row negative -1d0)
  (inject rhs row value))

(defun stamp-controlled (matrix positive negative control-positive control-negative row gain)
  "A voltage-controlled voltage source: v(+)-v(-) = GAIN * (vc+ - vc-)."
  (add-entry matrix positive row 1d0)
  (add-entry matrix negative row -1d0)
  (add-entry matrix row positive 1d0)
  (add-entry matrix row negative -1d0)
  (add-entry matrix row control-positive (- gain))
  (add-entry matrix row control-negative gain))

(defun stamp-row-of-currents (matrix rhs guess node terminals current row)
  "Stamp one terminal's current: the row of its Jacobian, and the constant part
left over when the linearisation is written as I = J*v + constant. NODE is the
terminal the current flows into, TERMINALS the nodes the row differentiates by."
  (let ((constant (- current
                     (loop for other in terminals for slope in row
                           sum (* slope (voltage-at guess other))))))
    (loop for other in terminals for slope in row
          do (add-entry matrix node other slope))
    (inject rhs node (- constant))))

(defun stamp-diode (matrix rhs guess a b gmin)
  "A junction between A (anode) and B (cathode), linearised at GUESS."
  (let* ((element *element*)
         (saturation (parameter *design* element :is 1d-14))
         (emission (parameter *design* element :n 1d0))
         (thermal (* emission +thermal+))
         (voltage (- (voltage-at guess a) (voltage-at guess b)))
         (exponential (exp (min (/ (max voltage -2d0) thermal) 40d0)))
         (conductance (+ (* (/ saturation thermal) exponential) gmin))
         (current (+ (* saturation (- exponential 1d0)) (* gmin voltage)))
         (constant (- current (* conductance voltage))))
    (stamp-admittance matrix a b conductance)
    (inject rhs a (- constant))
    (inject rhs b constant)))

(defun stamp-transistor (matrix rhs guess collector base emitter gmin)
  "A PNP or NPN transistor in the Ebers-Moll form, linearised at GUESS."
  (declare (ignore gmin))
  (let* ((element *element*)
         (polarity (if (eq (model-kind *design* (element-model element)) :pnp) -1d0 1d0))
         (saturation (parameter *design* element :is 1d-14))
         (forward-beta (parameter *design* element :bf 100d0))
         (reverse-beta (parameter *design* element :br 1d0))
         (forward-emission (parameter *design* element :nf 1d0))
         (reverse-emission (parameter *design* element :nr 1d0))
         (forward-thermal (* forward-emission +thermal+))
         (reverse-thermal (* reverse-emission +thermal+))
         (vbe (* polarity (- (voltage-at guess base) (voltage-at guess emitter))))
         (vbc (* polarity (- (voltage-at guess base) (voltage-at guess collector))))
         (forward (exp (min (/ vbe forward-thermal) 40d0)))
         (reverse (exp (min (/ vbc reverse-thermal) 40d0)))
         (iforward (* saturation (- forward 1d0)))
         (ireverse (* saturation (- reverse 1d0)))
         (gforward (* (/ saturation forward-thermal) forward))
         (greverse (* (/ saturation reverse-thermal) reverse))
         (transport (- iforward (* ireverse (+ 1d0 (/ 1d0 reverse-beta)))))
         (base-current (+ (/ iforward forward-beta) (/ ireverse reverse-beta)))
         (collector-current (* polarity transport))
         (emitter-current (- (+ collector-current (* polarity base-current))))
         (base-current (* polarity base-current))
         (d-collector (list (- gforward (* (+ 1d0 (/ 1d0 reverse-beta)) greverse))
                            (* (+ 1d0 (/ 1d0 reverse-beta)) greverse)
                            (- gforward)))
         (d-base (list (+ (/ gforward forward-beta) (/ greverse reverse-beta))
                       (- (/ greverse reverse-beta))
                       (- (/ gforward forward-beta))))
         (d-emitter (list (- (+ (first d-collector) (first d-base)))
                          (- (+ (second d-collector) (second d-base)))
                          (- (+ (third d-collector) (third d-base))))))
    (stamp-row-of-currents matrix rhs guess collector (list base collector emitter)
                           collector-current d-collector)
    (stamp-row-of-currents matrix rhs guess base (list base collector emitter) base-current d-base)
    (stamp-row-of-currents matrix rhs guess emitter (list base collector emitter)
                           emitter-current d-emitter)))

(defun build-system (design guess &key omega (gmin 0d0))
  "The MNA matrix and right-hand side of DESIGN at the solution GUESS.
OMEGA NIL builds the DC system; a number builds the small-signal system at
that frequency, linearised at GUESS, with the input source at 1 V."
  (let* ((complex (not (null omega)))
         (type (if complex '(complex double-float) 'double-float))
         (zero (if complex #C(0d0 0d0) 0d0))
         (one (if complex #C(1d0 0d0) 1d0))
         (size (+ (design-node-count design) (design-branch-count design)))
         (matrix (make-array (list size size) :element-type type :initial-element zero))
         (rhs (make-array size :element-type type :initial-element zero))
         (*design* design))
    (dolist (element (design-elements design))
      (let* ((*element* element)
             (nodes (element-nodes element))
             (a (node-index design (first nodes)))
             (b (node-index design (second nodes)))
             (value (element-value element)))
        (case (element-kind element)
          (:r (stamp-admittance matrix a b (/ 1d0 (if (or (null value) (zerop value)) 1d-12 value))))
          (:c (when complex (stamp-admittance matrix a b (* #C(0d0 1d0) omega value))))
          (:l (stamp-source matrix rhs a b (element-row element) zero)
              (when complex
                (add-entry matrix (element-row element) (element-row element)
                            (* #C(0d0 -1d0) omega value))))
          (:v (stamp-source matrix rhs a b (element-row element)
                             (if complex
                                 (if (equal (element-name element) (design-input design)) one zero)
                                 (coerce (or value 0d0) 'double-float))))
          (:i (let ((current (if complex (* one (or (getf (element-params element) :ac) 0d0))
                                 (coerce (or value 0d0) 'double-float))))
                (inject rhs a (- current))
                (inject rhs b current)))
          (:e (stamp-controlled matrix a b
                                 (node-index design (third nodes)) (node-index design (fourth nodes))
                                 (element-row element) value))
          (:x (stamp-controlled matrix (node-index design (fifth nodes)) 0 a b
                                 (element-row element) +op-amp-gain+))
          (:d (stamp-diode matrix rhs guess a b gmin))
          (:q (stamp-transistor matrix rhs guess a b (node-index design (third nodes)) gmin)))))
    (values matrix rhs)))

(defun solve-linear (matrix rhs)
  "MATRIX x = RHS by Gaussian elimination with partial pivoting, or NIL."
  (let* ((size (array-dimension matrix 0))
         (type (array-element-type matrix))
         (zero (coerce 0 type))
         (factors (make-array (list size size) :element-type type :initial-element zero))
         (answer (make-array size :element-type type :initial-element zero)))
    (dotimes (row size)
      (dotimes (column size)
        (setf (aref factors row column) (aref matrix row column)))
      (setf (aref answer row) (aref rhs row)))
    (dotimes (k size)
      (let ((pivot k) (best (abs (aref factors k k))))
        (loop for row from (1+ k) below size
              do (when (> (abs (aref factors row k)) best)
                   (setf best (abs (aref factors row k)) pivot row)))
        (when (< best 1d-300) (return-from solve-linear nil))
        (unless (= pivot k)
          (dotimes (column size) (rotatef (aref factors k column) (aref factors pivot column)))
          (rotatef (aref answer k) (aref answer pivot)))
        (let ((pivot-value (aref factors k k)))
          (loop for row from (1+ k) below size
                for factor = (/ (aref factors row k) pivot-value)
                unless (eql factor zero)
                  do (setf (aref factors row k) zero)
                     (loop for column from (1+ k) below size
                           do (decf (aref factors row column) (* factor (aref factors k column))))
                     (decf (aref answer row) (* factor (aref answer k)))))))
    (loop for row from (1- size) downto 0
          do (let ((sum (aref answer row)))
               (loop for column from (1+ row) below size
                     do (decf sum (* (aref factors row column) (aref answer column))))
               (setf (aref answer row) (/ sum (aref factors row row)))))
    answer))

(defun newton (design &key (gmin 0d0) start (limit 120))
  "Newton's method from START (or zero) at GMIN: (values solution converged-p steps).
While the step is larger than half a volt the update is damped, so a supply
switching on does not throw the exponentials of the junctions off the scale."
  (let* ((size (+ (design-node-count design) (design-branch-count design)))
         (guess (or start (make-array size :element-type 'double-float :initial-element 0d0)))
         (converged nil)
         (steps 0))
    (loop for step from 1 to limit
          while (not converged)
          do (multiple-value-bind (matrix rhs) (build-system design guess :gmin gmin)
               (let ((next (solve-linear matrix rhs)))
                 (setf steps step)
                 (cond
                   ((null next) (loop-finish))
                   (t (let* ((largest (loop for row below size maximize (abs (aref next row))))
                             (change (loop for row below size maximize
                                           (abs (- (aref next row) (aref guess row))))))
                        (cond ((< change (+ 1d-9 (* 1d-6 (max largest 1d0))))
                               (setf guess next converged t))
                              ((> change 0.5d0)
                               (let ((alpha (/ 0.5d0 change)))
                                 (dotimes (row size)
                                   (setf (aref guess row)
                                         (+ (aref guess row)
                                            (* alpha (- (aref next row) (aref guess row))))))))
                              (t (setf guess next)))))))))
    (values guess converged steps)))

(defun operating-point (design)
  "The DC operating point: (values solution converged-p method steps)."
  (multiple-value-bind (solution converged steps) (newton design)
    (if converged
        (values solution t :newton steps)
        (let ((warm solution))
          (dolist (gmin '(1d-3 1d-4 1d-5 1d-6 1d-7 1d-8 1d-9))
            (multiple-value-bind (next ok more) (newton design :gmin gmin :start warm)
              (declare (ignore more))
              (setf warm next)
              (when ok (return (values next t :gmin steps)))))
          (values warm nil :failed steps)))))

(defun ac-solution (design guess frequency)
  "The small-signal solution at FREQUENCY hertz, the input source driven at 1 V."
  (multiple-value-bind (matrix rhs) (build-system design guess :omega (* 2d0 pi frequency))
    (solve-linear matrix rhs)))

(defun ac-point (design guess frequency node)
  "NODE's complex response at FREQUENCY, or NIL when the matrix is singular."
  (let ((*design* design))
    (let ((solution (ac-solution design guess frequency)))
      (and solution (node-voltage solution node)))))

(defun logspace (start stop count)
  (let ((from (log start 10d0)) (to (log stop 10d0)))
    (if (< count 2)
        (vector (expt 10d0 from))
        (let ((points (make-array count :element-type 'double-float)))
          (dotimes (index count points)
            (setf (aref points index)
                  (expt 10d0 (+ from (* (- to from) (/ (coerce index 'double-float) (1- count)))))))))))

(defun decibels (magnitude)
  (* 20d0 (log (max (abs magnitude) 1d-30) 10d0)))
