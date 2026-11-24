;;;; netlist.lisp --- the SPICE subset, its parts, its *@ annotations, the ERC.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A netlist is ordinary SPICE with a few comment annotations that other
;;;; simulators ignore, exactly as the original wrote them:
;;;;
;;;;   *@ title Two-transistor fuzz
;;;;   *@ in VIN                    the signal source
;;;;   *@ out out                   the node the user hears
;;;;   *@ supply VCC                a supply source, repeatable
;;;;   *@ pot RV1 a w b 100k taper=log pos=0.5
;;;;   *@ spec gain_db(1k) > 18     a measurable requirement, repeatable
;;;;   *@ rating C3 16V             a part's rating, for the stress check
;;;;
;;;; Elements: R C L V I D Q (and E, X for a voltage-controlled source and an
;;;; ideal op-amp).  Ground is node 0 (or gnd).  Values may be written 4.7k,
;;;; 100n, 2meg, 1e-9 or 4k7.  Nothing in a netlist makes this cell read a file.

(in-package #:nodecode-hotwire)

;;; --- one condition, in words ------------------------------------------------

(define-condition hotwire-error (error)
  ((line :initarg :line :initform 0 :reader hotwire-error-line)
   (detail :initarg :detail :reader hotwire-error-detail))
  (:report (lambda (condition stream)
             (let ((line (hotwire-error-line condition)))
               (if (plusp line)
                   (format stream "line ~d: ~a" line (hotwire-error-detail condition))
                   (format stream "~a" (hotwire-error-detail condition))))))
  (:documentation "A netlist or a circuit this cell cannot read, in plain words."))

(defun netlist-error (line control &rest arguments)
  "Signal a HOTWIRE-ERROR at LINE with CONTROL formatted."
  (error 'hotwire-error :line line :detail (apply #'format nil control arguments)))

;;; --- reading numbers ---------------------------------------------------------

(defparameter +suffixes+
  '(("meg" . 1d6) ("mil" . 25.4d-6) ("r" . 1d0) ("f" . 1d-15) ("p" . 1d-12) ("n" . 1d-9)
    ("u" . 1d-6) ("µ" . 1d-6) ("m" . 1d-3) ("k" . 1d3) ("g" . 1d9) ("t" . 1d12))
  "What follows a number in a SPICE value, longest first. M is milli, MEG is mega.")

(defun suffix-multiplier (rest)
  "The multiplier REST's leading suffix names, or NIL when it names none."
  (loop for (suffix . multiplier) in +suffixes+
        when (and (<= (length suffix) (length rest))
                  (string-equal suffix (subseq rest 0 (length suffix))))
          return multiplier))

(defun number-prefix (text)
  "TEXT's leading decimal number, as a double, and the rest of it."
  (let ((end 0) (length (length text)) (marks '(#\+ #\-)))
    (when (and (plusp length) (member (char text 0) marks)) (incf end))
    (loop while (and (< end length) (digit-char-p (char text end))) do (incf end))
    (when (and (< end length) (char= (char text end) #\.))
      (incf end)
      (loop while (and (< end length) (digit-char-p (char text end))) do (incf end)))
    (when (and (< end length) (member (char text end) '(#\e #\E)))
      (let ((before end))
        (incf end)
        (when (and (< end length) (member (char text end) marks)) (incf end))
        (if (and (< end length) (digit-char-p (char text end)))
            (loop while (and (< end length) (digit-char-p (char text end))) do (incf end))
            (setf end before))))
    (if (and (plusp end) (or (> end 1) (not (member (char text 0) marks))))
        (values (let ((*read-default-float-format* 'double-float))
                  (coerce (read-from-string (subseq text 0 end)) 'double-float))
                (subseq text end))
        (values nil text))))

(defun rkm-value (text)
  "TEXT as 4k7 / 2r2 / 4n7 means it, or NIL."
  (let ((point (position-if-not #'digit-char-p text)))
    (when (and point (plusp point) (< (1+ point) (length text)))
      (let* ((head (subseq text 0 point))
             (suffix (subseq text point (1+ point)))
             (tail (subseq text (1+ point)))
             (multiplier (cdr (assoc suffix +suffixes+ :test #'string-equal))))
        (when (and multiplier (every #'digit-char-p tail))
          (* multiplier (coerce (read-from-string (format nil "~a.~a" head tail)) 'double-float)))))))

(defun parse-value (text &optional (line 0))
  "TEXT as a number: 10k, 4.7uF, 2meg, 1e-9, 4k7, 100mV."
  (let ((text (string-downcase (string-trim " " (string text)))))
    (dolist (unit '("ohm" "Ω"))
      (let ((at (search unit text)))
        (when at (setf text (concatenate 'string (subseq text 0 at) (subseq text (+ at (length unit))))))))
    (multiple-value-bind (value rest) (number-prefix text)
      (unless value
        (netlist-error line "'~a' is not a number (examples: 10k, 4.7u, 100n, 2meg)" text))
      (if (plusp (length rest))
          (* value (or (suffix-multiplier rest) 1d0))
          value))))

;;; --- the parts this cell knows by name ---------------------------------------

(defparameter *parts*
  '(("1N4148" :d (:is 2.52d-9 :n 1.752) "small-signal silicon switching diode (~0.6 V)")
    ("1N914" :d (:is 2.52d-9 :n 1.752) "small-signal silicon switching diode (~0.6 V)")
    ("1N34A" :d (:is 2d-7 :n 1.3) "germanium point-contact diode (~0.3 V)")
    ("1N5817" :d (:is 1d-5 :n 1.05) "schottky diode (~0.3 V)")
    ("1N4001" :d (:is 1d-8 :n 1.6) "rectifier diode")
    ("LED_RED" :d (:is 93.2d-12 :n 3.73) "5 mm red LED (~1.85 V at 20 mA)")
    ("LED_GREEN" :d (:is 66d-12 :n 3.4) "5 mm green LED")
    ("2N3904" :npn (:is 6.734d-15 :bf 134d0 :br 0.7371d0) "NPN, the pedal workhorse")
    ("2N2222" :npn (:is 1.4d-14 :bf 200d0 :br 1d0) "NPN switching transistor")
    ("BC549C" :npn (:is 1.4d-14 :bf 500d0 :br 1d0) "low-noise NPN")
    ("2N5088" :npn (:is 1.4d-14 :bf 400d0 :br 1d0) "high-gain NPN")
    ("MPSA18" :npn (:is 8d-15 :bf 600d0 :br 1d0) "very high-gain NPN")
    ("2N3906" :pnp (:is 1.41d-15 :bf 180.7d0 :br 4.977d0) "PNP complement of the 2N3904")
    ("BC559C" :pnp (:is 1.4d-14 :bf 400d0 :br 1d0) "low-noise PNP")
    ("TL071" :opamp () "single op-amp")
    ("TL072" :opamp () "dual op-amp")
    ("NE5532" :opamp () "low-noise dual op-amp")
    ("LM358" :opamp () "single-supply dual op-amp")
    ("RC4558" :opamp () "dual op-amp")
    ("OPAMP_IDEAL" :opamp () "ideal op-amp"))
  "The parts a netlist may name with no .model line, and the parameters used.")

(defun lookup-part (name)
  "The built-in part NAME describes, or NIL."
  (or (assoc (string-upcase name) *parts* :test #'string=)
      (assoc (cdr (assoc (string-upcase name) '(("LED" . "LED_RED") ("OPAMP" . "OPAMP_IDEAL"))
                          :test #'string=))
             *parts* :test #'string=)))

;;; --- the circuit ------------------------------------------------------------

(defstruct (model (:constructor make-model (name type params)))
  name type params)

(defstruct (element (:constructor make-element (name kind nodes &key (value 0d0) model params pot (line 0))))
  name kind nodes value model params line row pot)

(defstruct (pot (:constructor make-pot (name a w b value taper pos label)))
  name a w b value taper pos label)

(defstruct (design (:constructor %design))
  title elements models pots input output supplies specs ratings source
  names index rows node-count branch-count)

(defun split-tokens (text)
  "TEXT's fields, on whitespace."
  (let ((fields '()) (start nil))
    (loop for index from 0 below (length text)
          for space = (member (char text index) '(#\Space #\Tab #\Return))
          do (cond (space (when start (push (subseq text start index) fields) (setf start nil)))
                   ((null start) (setf start index))))
    (when start (push (subseq text start) fields))
    (nreverse fields)))

(defun split-lines (text)
  (let ((lines '()) (start 0))
    (loop for index from 0 below (length text)
          when (char= (char text index) #\Newline)
            do (push (string-trim '(#\Return) (subseq text start index)) lines)
               (setf start (1+ index)))
    (push (string-trim '(#\Return) (subseq text start)) lines)
    (nreverse lines)))

(defun normal-node (token)
  "A node name as the circuit keeps it."
  (let ((name (string-downcase (or token "0"))))
    (if (member name '("gnd" "ground") :test #'string=) "0" name)))

(defun model-parameters (design name)
  "The parameter list of the model NAME."
  (let ((model (gethash (string-upcase (or name "")) (design-models design))))
    (and model (model-params model))))

(defun model-kind (design name)
  (let ((model (gethash (string-upcase (or name "")) (design-models design))))
    (and model (model-type model))))

(defun parameter (design element key &optional default)
  "ELEMENT's model parameter KEY, as a double."
  (let ((value (getf (model-parameters design (element-model element)) key)))
    (if value (coerce value 'double-float) default)))

(defun pot-fraction (position taper)
  "The fraction of a pot's resistance between terminal A and the wiper."
  (let ((position (max 0d0 (min 1d0 (coerce position 'double-float)))))
    (cond ((member (string-downcase (or taper "lin")) '("log" "a" "audio") :test #'string=)
           (/ (- (expt 11d0 position) 1d0) 10d0))
          ((member (string-downcase (or taper "lin")) '("revlog" "rev" "c") :test #'string=)
           (- 1d0 (/ (- (expt 11d0 (- 1d0 position)) 1d0) 10d0)))
          (t position))))

(defun finalize-design (design)
  "Number the nodes and the branch unknowns, once, in the order they appear."
  (let ((index (make-hash-table :test #'equal)) (names (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (element (design-elements design))
      (unless (eq (element-kind element) :pot)
        (dolist (node (element-nodes element))
          (when (and (not (equal node "0")) (not (gethash node index)))
            (setf (gethash node index) (1+ (length names)))
            (vector-push-extend node names)))))
    (setf (design-index design) index (design-names design) names
          (design-node-count design) (length names))
    (let ((rows (make-hash-table :test #'equal)) (count 0))
      (dolist (element (design-elements design))
        (when (member (element-kind element) '(:v :l :x :e) :test #'eq)
          (setf (element-row element) (+ (design-node-count design) (incf count))
                (gethash (element-name element) rows) (element-row element))))
      (setf (design-rows design) rows (design-branch-count design) count)))
  design)

;;; --- reading the netlist ----------------------------------------------------

(defun assignment (token)
  "TOKEN as (KEY . VALUE) when it is k=v, else NIL."
  (let ((equals (position #\= token)))
    (and equals (plusp equals)
         (cons (string-upcase (subseq token 0 equals)) (subseq token (1+ equals))))))

(defun read-model-line (design tokens line)
  "A .model line: name, type and its (k=v ...) parameters."
  (let ((name (string-upcase (second tokens)))
        (body (format nil "~{~a~^ ~}" (cddr tokens))))
    (let* ((open (position #\( body))
           (type (string-upcase (string-trim " " (subseq body 0 (or open (length body))))))
           (kind (assoc type '(("D" . :d) ("NPN" . :npn) ("PNP" . :pnp) ("OPAMP" . :opamp))
                         :test #'string=)))
      (unless kind
        (netlist-error line "model type '~a' is not simulated here (D, NPN, PNP, OPAMP)" type))
      (let ((params '()))
        (dolist (token (split-tokens (if open (subseq body (1+ open)) "")))
          (let ((pair (assignment token)))
            (when pair
              (push (intern (car pair) :keyword)
                    params)
              (push (ignore-errors (parse-value (cdr pair) line)) params))))
        (setf (gethash name (design-models design))
              (make-model name (cdr kind) (nreverse params)))))))

(defun read-source (design name kind tokens line)
  "A V or I source: DC value, AC magnitude, and the waveform keywords ignored."
  (let ((value 0d0) (ac 0d0)
        (rest (cddr tokens)))
    (loop while rest
          for token = (string-downcase (pop rest))
          do (cond ((string= token "dc") (setf value (parse-value (pop rest) line)))
                   ((string= token "ac") (setf ac (if (and rest (ignore-errors (parse-value (first rest) line)))
                                                      (parse-value (pop rest) line)
                                                      1d0)))
                   ((or (string= token "sin") (string= token "pulse") (string= token "sine")
                        (search "(" token))
                    (setf rest nil))
                   (t (let ((number (ignore-errors (parse-value token line))))
                        (when number (setf value number))))))
    (make-element name kind (list (normal-node (second tokens)) (normal-node (third tokens)))
                  :value value :params (list :ac ac) :line line)))

(defun read-pot (design tokens line)
  "A *@ pot line: two resistors, one for each half of the pot."
  (when (< (length tokens) 5)
    (netlist-error line "pot needs: *@ pot RV1 a wiper b 100k [taper=log] [pos=0.5]"))
  (let* ((name (string-upcase (first tokens)))
         (a (normal-node (second tokens))) (w (normal-node (third tokens))) (b (normal-node (fourth tokens)))
         (value (parse-value (fifth tokens) line))
         (options (cddr (cddddr tokens)))
         (taper (or (cdr (assoc "taper" (mapcar #'assignment options) :test #'string-equal)) "lin"))
         (pos (or (ignore-errors (parse-value (cdr (assoc "pos" (mapcar #'assignment options)
                                                          :test #'string-equal)))) 0.5d0))
         (fraction (pot-fraction pos taper))
         (ra (max 1d0 (* value fraction)))
         (rb (max 1d0 (* value (- 1d0 fraction)))))
    (when (<= value 0)
      (netlist-error line "pot ~a needs a positive value" name))
    (setf (gethash name (design-pots design)) (make-pot name a w b value taper pos name))
    (push (make-element (concatenate 'string name "_A") :r (list a w) :value ra :line line :pot name)
          (design-elements design))
    (push (make-element (concatenate 'string name "_B") :r (list w b) :value rb :line line :pot name)
          (design-elements design))))

(defun read-annotation (design body line)
  "One *@ line: title, in, out, supply, spec, rating or tol."
  (let* ((space (position #\Space body))
         (key (string-downcase (if space (subseq body 0 space) body)))
         (argument (string-trim " " (if space (subseq body space) "")))
         (first-token (first (split-tokens argument))))
    (cond ((string= key "title") (setf (design-title design) argument))
          ((string= key "in") (setf (design-input design) (string-upcase first-token)))
          ((string= key "out") (setf (design-output design) (normal-node first-token)))
          ((string= key "supply") (dolist (name (split-tokens argument))
                                     (push (string-upcase name) (design-supplies design))))
          ((string= key "spec") (when (plusp (length argument))
                                  (push argument (design-specs design))))
          ((string= key "pot") (read-pot design (split-tokens argument) line))
          ((string= key "rating")
           (let ((tokens (split-tokens argument)))
             (when (= (length tokens) 2)
               (push (cons (string-upcase (first tokens)) (second tokens)) (design-ratings design)))))
          (t nil))))

(defun read-element (design line line-number)
  "One element line of the netlist."
  (let* ((tokens (split-tokens line))
         (name (string-upcase (first tokens)))
         (kind (let ((dot (position #\. name :from-end t)))
                 (char (if dot (subseq name (1+ dot)) name) 0))))
    (labels ((nodes (count)
               (when (< (length tokens) (1+ count))
                 (netlist-error line-number "~a needs ~d nodes" name count))
               (loop for index from 1 to count collect (normal-node (nth index tokens))))
             (positive (index what)
               (let ((value (and (nth index tokens) (parse-value (nth index tokens) line-number))))
                 (unless (and value (plusp value))
                   (netlist-error line-number "~a needs a positive ~a" name what))
                 value)))
      (case kind
        ((#\R #\C #\L)
         (make-element name (ecase kind (#\R :r) (#\C :c) (#\L :l))
                       (nodes 2) :value (positive 3 "value") :line line-number))
        ((#\V #\I)
         (read-source design name (if (char= kind #\V) :v :i) tokens line-number))
        (#\D
         (unless (nth 3 tokens) (netlist-error line-number "~a needs anode, cathode and a model" name))
         (make-element name :d (nodes 2) :model (string-upcase (nth 3 tokens)) :line line-number))
        (#\Q
         (unless (nth 4 tokens) (netlist-error line-number "~a needs collector, base, emitter and a model" name))
         (make-element name :q (nodes 3) :model (string-upcase (nth 4 tokens)) :line line-number))
        (#\X
         (unless (and (nth 5 tokens) (lookup-part (nth 6 tokens)))
           (netlist-error line-number "~a: '~a' is not a part this cell knows; an op-amp takes 5 pins then a part: '~a in+ in- vcc vee out TL072'"
                 name (or (nth 6 tokens) "") name))
         (make-element name :x (nodes 5) :model (string-upcase (nth 6 tokens)) :line line-number))
        (#\E
         (unless (nth 5 tokens) (netlist-error line-number "~a needs n+ n- nc+ nc- gain" name))
         (make-element name :e (nodes 4) :value (parse-value (nth 5 tokens) line-number) :line line-number))
        (t (netlist-error line-number "'~a' is not an element this cell simulates (R C L V I D Q X E)" (first tokens)))))))

(defun resolve-models (design)
  "Give every D, Q and X element a model: its .model line, else a built-in part."
  (dolist (element (design-elements design))
    (let ((name (element-model element)))
      (when name
        (unless (gethash name (design-models design))
          (let ((part (lookup-part name)))
            (unless part
              (netlist-error (element-line element) "~a: unknown part '~a'. Add a .model line or use a built-in part"
                    (element-name element) name))
            (setf (gethash name (design-models design))
                  (make-model name (second part) (copy-list (third part))))))
        (let ((wanted (case (element-kind element) (:d :d) (:q '(:npn :pnp)) (:x :opamp)))
              (actual (model-type (gethash name (design-models design)))))
          (unless (if (listp wanted) (member actual wanted) (eq wanted actual))
            (netlist-error (element-line element) "~a: '~a' is a ~(~a~) model, which does not fit a ~(~a~) element"
                  (element-name element) name actual (element-kind element))))))))

(defun parse-netlist (text)
  "TEXT as a DESIGN, or a HOTWIRE-ERROR naming the line and what to fix."
  (when (zerop (length (string-trim '(#\Space #\Tab #\Newline) (or text ""))))
    (netlist-error 0 "the netlist is empty"))
  (let ((design (%design :title "Untitled circuit" :elements '() :models (make-hash-table :test #'equal)
                          :pots (make-hash-table :test #'equal) :supplies '() :specs '() :ratings '())))
    (loop for line in (split-lines text)
          for line-number from 1
          for trimmed = (string-trim " " line)
          do (cond
               ((zerop (length trimmed)) nil)
               ((and (< 1 (length trimmed)) (char= (char trimmed 0) #\*) (char= (char trimmed 1) #\@))
                (let ((body (string-trim " " (subseq trimmed 2))))
                  (when (plusp (length body))
                    (read-annotation design body line-number))))
               ((char= (char trimmed 0) #\*) nil)
               ((char= (char trimmed 0) #\.)
                (let ((directive (string-downcase (first (split-tokens trimmed)))))
                  (cond ((string= directive ".model") (read-model-line design (split-tokens trimmed) line-number))
                        ((member directive '(".include" ".inc" ".lib") :test #'string=)
                         (netlist-error line-number "~a is refused: paste the model lines in, so no netlist makes this cell read a file" directive))
                        ((string= directive ".end") (loop-finish))
                        (t nil))))
               (t (push (read-element design trimmed line-number) (design-elements design)))))
    (setf (design-elements design) (nreverse (design-elements design)))
    (when (null (design-elements design))
      (netlist-error 0 "the netlist has no components"))
    (resolve-models design)
    (finalize-design design)
    (let ((names (loop for element in (design-elements design) collect (element-name element))))
      (when (and (design-input design) (not (member (design-input design) names :test #'string=)))
        (netlist-error 0 "*@ in names '~a', but there is no source with that name" (design-input design)))
      (when (and (design-output design)
                 (not (gethash (design-output design) (design-index design))))
        (netlist-error 0 "*@ out names node '~a', which nothing connects to" (design-output design)))
      (dolist (supply (design-supplies design))
        (unless (member supply names :test #'string=)
          (netlist-error 0 "*@ supply names '~a', but there is no source with that name" supply))))
    design))

;;; --- the electrical rule check -----------------------------------------------

(defun issue (level message) (list :level level :message message))

(defun dc-pairs (element)
  "The node pairs ELEMENT connects at DC, for 'is there a DC path to ground'."
  (let ((nodes (element-nodes element)))
    (case (element-kind element)
      ((:r :l :v :d :e) (list (list (first nodes) (second nodes))))
      (:q (list (list (first nodes) (second nodes)) (list (second nodes) (third nodes))))
      (:x (list (list (fifth nodes) (third nodes)) (list (fifth nodes) (fourth nodes))
                (list (third nodes) (fourth nodes))))
      (t nil))))

(defun dc-path-issues (design found)
  "Every node with no DC path to ground is one error."
  (let ((parent (make-hash-table :test #'equal)))
    (labels ((root (node)
               (let ((up (gethash node parent)))
                 (if (or (null up) (equal up node)) node (root up))))
             (join (a b)
               (let ((ra (root a)) (rb (root b)))
                 (unless (equal ra rb) (setf (gethash ra parent) rb)))))
      (setf (gethash "0" parent) "0")
      (dolist (element (design-elements design))
        (dolist (pair (dc-pairs element))
          (join (first pair) (second pair))))
      (loop for index from 1 to (design-node-count design)
            for node = (aref (design-names design) (1- index))
            unless (equal (root node) (root "0"))
              do (push (issue :error (format nil "node '~a' has no DC path to ground (it is isolated by capacitors). Add a resistor to ground or to a bias point" node))
                       found))))
  found)

(defun check-rules (design)
  "The mistakes that make the simulator or the board fail, as a list of issues."
  (let ((found '()) (pins (make-hash-table :test #'equal)))
    (dolist (element (design-elements design))
      (dolist (node (element-nodes element))
        (push (element-name element) (gethash node pins))))
    (dolist (element (design-elements design))
      (let ((nodes (element-nodes element)))
        (when (and (member (element-kind element) '(:r :c :l :v :i :d) :test #'eq)
                   (equal (first nodes) (second nodes)))
          (push (issue (if (eq (element-kind element) :v) :error :warning)
                       (format nil "~a has both ends on node '~a', so it does nothing"
                               (element-name element) (first nodes)))
                found))
        (when (and (eq (element-kind element) :r) (< (element-value element) 0.5d0)
                   (null (element-pot element)))
          (push (issue :warning (format nil "~a is ~,4g mΩ. In SPICE 'M' means milli: write 1meg for 1 MΩ"
                                        (element-name element) (* 1000 (element-value element))))
                found))
        (when (and (eq (element-kind element) :x)
                   (equal (third nodes) (fourth nodes)))
          (push (issue :error (format nil "~a's V+ and V- pins are the same node, so it has no supply"
                                      (element-name element)))
                found))))
    (loop for node being the hash-keys of pins
          when (and (not (equal node "0")) (not (equal node (design-output design)))
                    (= 1 (length (gethash node pins))))
            do (push (issue :warning (format nil "node '~a' only connects to ~a; that pin goes nowhere"
                                             node (first (gethash node pins))))
                     found))
    (setf found (dc-path-issues design found))
    (let ((parent (make-hash-table :test #'equal)))
      (labels ((root (node) (let ((up (gethash node parent))) (if (or (null up) (equal up node)) node (root up)))))
        (dolist (element (design-elements design))
          (when (member (element-kind element) '(:v :l) :test #'eq)
            (let* ((a (first (element-nodes element))) (b (second (element-nodes element)))
                   (ra (root a)) (rb (root b)))
              (if (equal ra rb)
                  (push (issue :error (format nil "~a closes a loop of voltage sources and inductors with nothing to limit the current"
                                              (element-name element)))
                        found)
                  (setf (gethash ra parent) rb)))))))
    (unless (loop for element in (design-elements design)
                  thereis (member "0" (element-nodes element) :test #'equal))
      (push (issue :error "nothing connects to ground (node 0 or gnd)") found))
    (nreverse found)))
