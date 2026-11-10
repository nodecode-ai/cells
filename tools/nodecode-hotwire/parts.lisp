;;;; parts.lisp --- preferred values and the parts list a builder buys off.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original snapped every resistor to E24 and every capacitor to E6 or
;;;; E12, grouped the equal ones, named a package for each, and added the
;;;; hardware a 9 V pedal needs. What is here is that list, and the value
;;;; tables hotwire_tune searches over. Nothing here simulates.

(in-package #:nodecode-hotwire)

;;; --- preferred values -------------------------------------------------------

(defparameter +e-series+
  '(("E6" 10 15 22 33 47 68)
    ("E12" 10 12 15 18 22 27 33 39 47 56 68 82)
    ("E24" 10 11 12 13 15 16 18 20 22 24 27 30 33 36 39 43 47 51 56 62 68 75 82 91))
  "The preferred-value series: the mantissas of each decade.")

(defun series-name (name)
  "NAME as a series' canonical name, refusing anything else."
  (let ((row (assoc (string-upcase (string-trim " " (or name "E24"))) +e-series+ :test #'string=)))
    (or (first row)
        (netlist-error 0 "series must be one of ~{~a~^, ~}" (mapcar #'first +e-series+)))))

(defun series-mantissas (name)
  (mapcar (lambda (digit) (coerce digit 'double-float))
          (cdr (assoc (series-name name) +e-series+ :test #'string=))))

(defun decade-of (value)
  (floor (log (max value 1d-30) 10d0)))

(defun snap-value (value series)
  "VALUE moved to the nearest value of SERIES, in its decade or either side."
  (let* ((table (series-mantissas series))
         (best value) (best-distance most-positive-double-float))
    (loop for decade from (1- (decade-of value)) to (1+ (decade-of value))
          do (dolist (mantissa table)
               (let* ((candidate (* mantissa (expt 10d0 decade)))
                      (distance (abs (log (/ candidate value) 10d0))))
                 (when (< distance best-distance)
                   (setf best-distance distance best candidate)))))
    best))

(defun series-neighbours (value series steps)
  "The values of SERIES within STEPS places of VALUE's nearest one, itself left out."
  (let* ((table (series-mantissas series))
         (snapped (snap-value value series))
         (flat (loop for decade from (1- (decade-of value)) to (+ 1 (decade-of value))
                     append (mapcar (lambda (mantissa) (* mantissa (expt 10d0 decade))) table)))
         (position (position snapped flat :test #'=)))
    (loop for offset from (- steps) to steps
          for index = (+ position offset)
          when (and (plusp offset) (< index (length flat)))
            collect (nth index flat))))

;;; --- what a part is, on a bench ---------------------------------------------

(defparameter +packages+
  '((:d . "DO-35 axial") (:npn . "TO-92") (:pnp . "TO-92") (:opamp . "DIP-8, or SOIC-8"))
  "The through-hole package each kind of semiconductor comes in.")

(defun element-part (element)
  "ELEMENT's description, package and any note a buyer needs: three values."
  (let* ((built-in (and (member (element-kind element) '(:d :q :x) :test #'eq)
                         (lookup-part (element-model element))))
         (kind (and built-in (second built-in)))
         (description (or (fourth built-in)
                          (format nil "~(~a~) ~a" (element-kind element)
                                  (or (element-model element) "custom model"))))
         (package (or (cdr (assoc kind +packages+)) "as the datasheet's")))
    (values description
            (if built-in package "as the datasheet says")
            (unless built-in "custom .model: pick a real part with these parameters"))))

(defun bom-rating (design name)
  "The voltage NAME is rated for, when the netlist says so."
  (let ((pair (assoc name (design-ratings design) :test #'string=)))
    (and pair (second pair))))

(defun capacitance-text (value)
  "VALUE in farads the way a parts list writes it: pF, nF or uF."
  (let ((magnitude (abs (coerce value 'double-float))))
    (cond ((>= magnitude 1d-6) (format nil "~a uF" (number-text (* value 1d6))))
          ((>= magnitude 1d-9) (format nil "~a nF" (number-text (* value 1d9))))
          (t (format nil "~a pF" (number-text (* value 1d12)))))))

(defun cap-kind (value rating)
  "One capacitor's dielectric, and the voltage it must be bought for."
  (let ((kind (if (>= value 1d-6) "electrolytic, watch the + lead" "film or C0G")))
    (if rating (format nil "~a, ~a or more" kind rating) kind)))

(defun bom-rows (design series)
  "DESIGN's parts as rows, equal values grouped: (values rows hardware-p)."
  (let ((groups '()) (hardware nil))
    (flet ((add (key ref row)
             (let ((group (assoc key groups :test #'equal)))
               (if group
                   (progn (push ref (getf (cdr group) :refs)) (incf (getf (cdr group) :qty)))
                   (push (cons key (list* :refs (list ref) :qty 1 row)) groups)))))
      (loop for element in (design-elements design)
            for name = (element-name element)
            for kind = (element-kind element)
            for value = (element-value element)
            unless (or (element-pot element)
                       (member kind '(:v :i) :test #'eq)
                       (member name (design-supplies design) :test #'string=))
              do (cond
                   ((eq kind :r)
                    (let* ((snapped (snap-value value series))
                           (nonstandard (and (plusp value) (> (abs (- snapped value)) (* 0.005 value)))))
                      (add (list :r snapped) name
                           (list :kind "resistor" :value (format-value snapped "ohm")
                                 :part "1/4 W carbon or metal film, 0.4 in lead spacing"
                                 :note (and nonstandard
                                            (format nil "the netlist says ~a, which is not a ~a value"
                                                    (format-value value "ohm") (series-name series))))))
                    (when (>= value 100d0) (setf hardware t)))
                   ((eq kind :c)
                    (let* ((snapped (snap-value value (if (>= value 1d-6) "E6" "E12")))
                           (nonstandard (> (abs (- snapped value)) (* 0.005 (max value 1d-15)))))
                      (add (list :c snapped) name
                           (list :kind "capacitor" :value (capacitance-text snapped)
                                 :part (cap-kind value (bom-rating design name))
                                 :note (and nonstandard
                                            (format nil "the netlist says ~a; the nearest standard value is ~a"
                                                    (capacitance-text value) (capacitance-text snapped))))))
                    (when (>= value 1d-6) (setf hardware t)))
                   ((eq kind :l)
                    (add (list :l value) name
                         (list :kind "inductor" :value (format-value value "H")
                               :part (format nil "~a audio inductor" (format-value value "H"))
                               :note "large audio inductors are sold as wah or filter inductors")))
                   ((member kind '(:d :q :x) :test #'eq)
                    (multiple-value-bind (description package note) (element-part element)
                      (add (list kind (element-model element)) name
                           (list :kind (case kind (:d "diode") (:q "transistor") (t "op-amp"))
                                 :value (string-downcase (element-model element))
                                 :part description :package package :note note))))
                   (t (setf hardware t)))))
    (values (mapcar #'cdr (nreverse groups)) hardware)))

(defun pedal-hardware-p (design)
  (or (plusp (hash-table-count (design-pots design)))
      (some (lambda (supply)
              (let ((element (find supply (design-elements design) :key #'element-name :test #'string=)))
                (and element (>= (element-value element) 7d0))))
            (design-supplies design))))

(defun bom-text (design series)
  "The parts list as the text a builder reads."
  (let ((name (series-name series)))
    (multiple-value-bind (rows hardware) (bom-rows design name)
      (declare (ignore hardware))
      (with-output-to-string (out)
        (format out "Parts list: ~a (~a preferred values)~%" (design-title design) name)
        (if (null rows)
            (format out "  the netlist has no parts to buy: sources and knobs only~%")
            (dolist (row rows)
              (format out "  ~2d  ~a: ~a ~a - ~a~@[  (~a)~]~%"
                      (getf row :qty)
                      (format nil "~{~a~^, ~}" (reverse (getf row :refs)))
                      (getf row :kind) (getf row :value) (getf row :part) (getf row :note))
              (when (plusp (length (getf row :package)))
                (format out "      ~a~%" (getf row :package)))))
        (loop for name being the hash-keys of (design-pots design)
              do (let ((pot (gethash name (design-pots design))))
                   (format out "  1  ~a: potentiometer ~a - ~a taper, with a knob~%"
                           name (format-value (pot-value pot) "ohm")
                           (string-downcase (pot-taper pot)))))
        (when (pedal-hardware-p design)
          (format out "Hardware: 2x 1/4 in mono jacks; 1x DPDT footswitch; 1x 5 mm LED and its series resistor; 1x die-cast enclosure; 1x 9 V battery snap~%"))))))
