;;;; tools.lisp --- the ten tools, their declarations, and the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Each tool's arguments are declared once with NLK:DEFINE-CONTRACT, so the
;;;; schema the model reads and the record the verb takes cannot disagree; a
;;;; call that leaves out a required member is refused at the boundary, before
;;;; any verb runs. Every verb answers one JSON string built from the tables in
;;;; data.lisp, plus the section's units, currency, waste and markup -- the
;;;; original plugin's four knobs, which are settings here rather than numbers
;;;; buried in the code.

(in-package #:nodecode-fashion-design)

;;; --- the declarations -------------------------------------------------------

(nlk:define-contract design-brief-call
  (:doc "Turn a loose concept into a structured design brief: silhouette, colour palette, fabric suggestions and the key details. Reads the section's season and price tier for its suggestions.")
  ("concept" :text :required t
   :doc "the core concept or inspiration for the design")
  ("target_audience" :string
   :doc "the customer segment the design is for")
  ("season" :string
   :doc "the season or collection, e.g. SS25 or FW24")
  ("price_point" :string :options '("budget" "mid" "premium" "luxury")
   :doc "the price tier the finishing is costed at"))

(nlk:define-contract tech-pack-call
  (:doc "Create a technical pack for a garment: flat-sketch description, a measurement table over the garment's points of measure, construction, materials, grading rules and labelling.")
  ("garment_type" :text :required t
   :doc "the garment, e.g. t-shirt, shirt, dress, pants, skirt, jacket, coat")
  ("measurements" :object
   :doc "the measurements that are already set, by point of measure; a point left out answers TBD")
  ("construction_details" :string
   :doc "construction and stitch details")
  ("fabric" :string
   :doc "the main fabric")
  ("trims" :string
   :doc "trims, buttons, zippers"))

(nlk:define-contract size-sheet-call
  (:doc "Generate a size chart from a base size across a size range: every point of measure graded by the standard increment, with garment ease on the circumferences.")
  ("garment_type" :text :required t
   :doc "the garment, e.g. t-shirt, dress, pants")
  ("base_size" :string
   :doc "the size the increments start from, M by default")
  ("size_range" :array :of :string
   :doc "the sizes to chart, XS to XXL by default")
  ("measurement_points" :array :of :string
   :doc "the points of measure to chart; the garment's own set by default"))

(nlk:define-contract bom-call
  (:doc "Generate a bill of materials for a garment: every component with quantity, unit, supplier and cost, and the material cost per unit.")
  ("garment_type" :text :required t
   :doc "the garment the bill is for")
  ("fabric" :string
   :doc "the main fabric to name on the first line")
  ("trims" :string
   :doc "the trims to name on the trims line")
  ("hardware" :string
   :doc "the hardware to name on the hardware line"))

(nlk:define-contract fabric-yardage-call
  (:doc "Estimate the fabric yardage one garment of a type needs, and for a whole run, from the fabric width and the size, with the section's cutting waste added.")
  ("garment_type" :text :required t
   :doc "the garment, e.g. t-shirt, dress, coat")
  ("fabric_width" :number
   :doc "the fabric width in inches, 45 by default")
  ("size" :string
   :doc "the size, M by default")
  ("quantity" :integer
   :doc "how many garments, 1 by default"))

(nlk:define-contract cost-sheet-call
  (:doc "Cost a garment per unit and for a run: fabric, labour and trims, 10 % overhead, the total, and the retail price the section's markup asks.")
  ("garment_type" :text :required t
   :doc "the garment being costed")
  ("fabric_cost" :number
   :doc "fabric cost per unit")
  ("labor_cost" :number
   :doc "labour cost per unit")
  ("trims_cost" :number
   :doc "trims cost per unit")
  ("quantity" :integer
   :doc "the production quantity, 1 by default"))

(nlk:define-contract construction-guide-call
  (:doc "Step-by-step construction instructions for a garment, with a tip for the skill level and a rough time estimate.")
  ("garment_type" :text :required t
   :doc "the garment, e.g. t-shirt, dress, pants, jacket")
  ("fabric_type" :string
   :doc "the fabric's behaviour: woven, knit, denim, silk, wool")
  ("skill_level" :string :options '("beginner" "intermediate" "advanced")
   :doc "who is sewing it"))

(nlk:define-contract color-palette-call
  (:doc "Suggest a coordinated five-colour story for a season, with hex codes and Pantone references, and how to balance them.")
  ("season" :string
   :doc "the season, e.g. Spring/Summer 2025")
  ("trend" :string
   :doc "a trend keyword the story answers")
  ("mood" :string
   :doc "the mood or feeling the collection wants"))

(nlk:define-contract measurement-chart-call
  (:doc "Standard body measurements for a size, a gender and an age range, in the section's units.")
  ("size" :text :required t
   :doc "the size, e.g. M, or a numeric size")
  ("gender" :string :options '("women" "men" "unisex" "kids")
   :doc "whose table to read, women by default")
  ("age_range" :string
   :doc "adult, teen, child 6-8: carried into the answer as given"))

(nlk:define-contract collection-plan-call
  (:doc "Plan a collection: its pieces in a balanced order, with the colour and fabric story.")
  ("season" :string
   :doc "the season the collection is for")
  ("number_of_pieces" :integer
   :doc "how many pieces, 6 by default")
  ("theme" :string
   :doc "the collection's theme"))

;;; --- reading the tables -----------------------------------------------------

(defun trimmed (text)
  "TEXT without its surrounding whitespace; an empty string for anything that
is not a string."
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun text-or (value fallback)
  "VALUE when it is a non-empty string, else FALLBACK."
  (let ((text (trimmed value)))
    (if (plusp (length text)) text fallback)))

(defun garment-key (value)
  "The table key a garment name reads as."
  (string-downcase (trimmed value)))

(defun table-value (table key &optional default)
  "CDR of KEY's entry in the alist TABLE, or DEFAULT."
  (let ((entry (assoc key table :test #'equal)))
    (if entry (cdr entry) default)))

(defun strings-of (value)
  "VALUE, a contract's array of strings, as a list; NIL when it is empty."
  (let ((out (loop for item across (or value #()) when (stringp item) collect item)))
    out))

(defun rounded (value places)
  "VALUE rounded to PLACES decimals, as a float."
  (let ((scale (expt 10.0d0 places)))
    (/ (round (* (coerce value 'double-float) scale)) scale)))

(defun units-name ()
  "The length unit the section asks for."
  (if (equal (setting :units) "cm") "cm" "in"))

(defun in-units (inches)
  "INCHES as the section's unit, to one decimal."
  (let ((value (coerce inches 'double-float)))
    (rounded (if (equal (units-name) "cm") (* value 2.54d0) value) 1)))

(defun currency ()
  "The currency the section's money is in."
  (string (setting :currency)))

(defun body-row (gender size)
  "The point-of-measure alist for GENDER and SIZE, or NIL."
  (let ((rows (rest (assoc gender +body-measurements+ :test #'equal))))
    (rest (assoc size rows :test #'equal))))

(defun point-value (row point)
  "POINT's measurement in ROW, or NIL."
  (let ((entry (assoc point row :test #'equal)))
    (and entry (second entry))))

(defun alias-point (point)
  "POINT under the name the women's table uses for it: a women's row carries
BUST where a men's carries CHEST, so a t-shirt's chest is read as the bust."
  (if (equal point "chest") "bust" point))

(defun points-for (garment)
  "The points of measure GARMENT's pack and chart carry."
  (or (rest (assoc (garment-key garment) +points-by-garment+ :test #'equal))
      '("chest" "waist" "hip" "length")))

(defun season-key (season)
  "The palette table's key for SEASON, read the way a collection names it:
the first season word anywhere in it (so Fall/Winter is fall, Spring/Summer
is spring, Autumn 2026 is fall), spring when it names none."
  (let ((text (string-downcase (trimmed season))))
    (or (loop for (word . key) in '(("spring" . "spring") ("summer" . "summer")
                                    ("autumn" . "fall") ("fall" . "fall")
                                    ("winter" . "winter"))
              when (search word text) return key)
        "spring")))

(defun palette-of (key)
  "The five-colour story of KEY, or spring's."
  (rest (assoc key +color-palettes+ :test #'equal)))

(defun color-palette-json (entries)
  "ENTRIES as the JSON array of named colours."
  (coerce (loop for (name hex pantone) in entries
                collect (nlk:json-object "name" name "hex" hex "pantone" pantone))
          'vector))

(defun steps-json (steps)
  "STEPS as the JSON array of numbered instructions."
  (coerce (loop for instruction in steps
                for number from 1
                collect (nlk:json-object "step" number "instruction" instruction))
          'vector))

;;; --- the ten verbs ----------------------------------------------------------

(defun design-brief (call)
  "The design_brief tool: a structured brief built around the concept."
  (let* ((concept (design-brief-call-concept call))
         (target (text-or (design-brief-call-target-audience call) "general"))
         (season (text-or (design-brief-call-season call) "all-season"))
         (price (string-downcase (text-or (design-brief-call-price-point call) "mid")))
         (key (season-key season))
         (palette (palette-of key))
         (fabrics (rest (assoc key +fabrics-by-season+ :test #'equal))))
    (nlk:encode-json-object
     (nlk:json-object
      "concept" concept
      "target_audience" target
      "season" season
      "price_point" price
      "silhouette" (table-value +silhouettes-by-price+ price
                                 "balanced silhouettes mixing classic and trend")
      "color_palette" (coerce (loop for entry in (subseq palette 0 (min 4 (length palette)))
                                     collect (format nil "~a ~a" (first entry) (second entry)))
                               'vector)
      "fabric_suggestions" (coerce (or fabrics '("cotton" "poly blend" "jersey")) 'vector)
      "key_details" (coerce '("signature stitching" "branded hardware"
                               "functional pockets" "adjustable fit elements")
                             'vector)
      "design_notes" (format nil "Build around the concept '~a' for ~a; keep ~a-tier finishing."
                              concept target price)))))

(defun tech-pack (call)
  "The tech_pack tool: the pack, its measurement table filled from what the
caller set."
  (let* ((garment (tech-pack-call-garment-type call))
         (points (points-for garment))
         (given (tech-pack-call-measurements call))
         (table (make-hash-table :test #'equal)))
    (dolist (point points)
      (setf (gethash point table)
            (or (and given (gethash point given)) "TBD")))
    (nlk:encode-json-object
     (nlk:json-object
      "garment_type" garment
      "flat_sketch_description" (format nil "Front and back flat sketch of ~a with labeled points of measure."
                                         garment)
      "measurements" table
      "construction" (text-or (tech-pack-call-construction-details call)
                               "Standard construction; see construction_guide.")
      "materials" (nlk:json-object
                   "main_fabric" (text-or (tech-pack-call-fabric call) "TBD")
                   "trims" (text-or (tech-pack-call-trims call) "TBD")
                   "thread" "matching, 100% polyester"
                   "interfacing" "as required")
      "grading" "Grade from the base size with standard increments; see size_sheet."
      "labeling" (nlk:json-object
                  "care_label" "Machine wash cold, tumble dry low"
                  "size_label" "Printed size"
                  "brand_label" "Woven main label"
                  "country_of_origin" "TBD")
      "points_of_measure" (coerce points 'vector)))))

(defun size-sheet (call)
  "The size_sheet tool: one row per size, each point graded from the base."
  (let* ((garment (size-sheet-call-garment-type call))
         (base (string-upcase (text-or (size-sheet-call-base-size call) "M")))
         (sizes (or (strings-of (size-sheet-call-size-range call))
                    '("XS" "S" "M" "L" "XL" "XXL")))
         (points (or (strings-of (size-sheet-call-measurement-points call))
                     (points-for garment)))
         (row (body-row "women" base))
         (increments (rest (assoc "women" +grade-increments+ :test #'equal)))
         (count (length sizes))
         (base-index (or (loop for size in sizes
                               for index from 0
                               when (equal (string-upcase size) base) return index)
                         (floor count 2)))
         (chart (make-hash-table :test #'equal)))
    (loop for size in sizes
          for index from 0
          do (let ((delta (- index base-index)))
               (setf (gethash (string-upcase size) chart)
                     (apply #'nlk:make-json-object
                            (loop for point in points
                                  for base-value = (or (point-value row (alias-point point))
                                                       (point-value row "chest")
                                                       36)
                                  for ease = (if (member point '("chest" "bust" "hip")
                                                         :test #'equal)
                                                 4 0)
                                  for step = (or (table-value increments (alias-point point)) 1)
                                  append (list point
                                                (in-units (+ base-value ease (* delta step)))))))))
    (nlk:encode-json-object
     (nlk:json-object
      "garment_type" garment
      "base_size" base
      "size_range" (coerce (mapcar #'string-upcase sizes) 'vector)
      "measurement_points" (coerce points 'vector)
      "grade_increments" (apply #'nlk:make-json-object
                                (loop for (point . step) in increments
                                      append (list point (in-units step))))
      "size_chart" chart
      "units" (units-name)
      "notes" "Garment measurements include ease. Adjust per fabric and fit."))))

(defun bill-of-materials (call)
  "The bom tool: the seven standard components, the three the caller may name
replaced, and the material cost per unit."
  (let* ((garment (bom-call-garment-type call))
         (replaced (list (cons "Main fabric" (text-or (bom-call-fabric call) ""))
                         (cons "Lining" "")
                         (cons "Thread" "")
                         (cons "Trims" (text-or (bom-call-trims call) ""))
                         (cons "Hardware" (text-or (bom-call-hardware call) ""))
                         (cons "Labels" "")
                         (cons "Packaging" "")))
         (total 0.0d0)
         (items (loop for (item description quantity unit supplier cost) in +bom-lines+
                      for named = (cdr (assoc item replaced :test #'equal))
                      do (incf total cost)
                      collect (nlk:json-object
                               "item" item
                               "description" (if (plusp (length named)) named description)
                               "quantity" quantity
                               "unit" unit
                               "supplier" supplier
                               "cost" (rounded cost 2)))))
    (nlk:encode-json-object
     (nlk:json-object
      "garment_type" garment
      "bill_of_materials" (coerce items 'vector)
      "total_material_cost_per_unit" (rounded total 2)
      "currency" (currency)))))

(defun fabric-yardage (call)
  "The fabric_calculator tool: yards per garment and for the run, the
section's waste included."
  (let* ((garment (fabric-yardage-call-garment-type call))
         (width (let ((given (fabric-yardage-call-fabric-width call)))
                  (if (and given (numberp given) (plusp given)) given 45)))
         (size (string-upcase (text-or (fabric-yardage-call-size call) "M")))
         (quantity (max 1 (or (fabric-yardage-call-quantity call) 1)))
         (base (or (table-value +yardage-base+ (garment-key garment)) 1.5))
         (step (or (table-value +size-factor+ size) 1.0))
         (waste (setting :waste-percent))
         (per (* base step (coerce (/ 45.0d0 width) 'double-float)))
         (with-waste (* per (+ 1 (coerce (/ waste 100.0d0) 'double-float)))))
    (nlk:encode-json-object
     (nlk:json-object
      "garment_type" garment
      "fabric_width_inches" width
      "size" size
      "quantity" quantity
      "yardage_per_garment" (rounded with-waste 3)
      "total_yardage" (rounded (* with-waste quantity) 3)
      "waste_percent" waste
      "units" "yards"
      "notes" "Includes the section's cutting waste. Verify with a marker or lay plan for production."))))

(defun cost-sheet (call)
  "The cost_sheet tool: the three cost lines, 10 % overhead, the run's total
and the retail price the markup asks."
  (let* ((garment (cost-sheet-call-garment-type call))
         (fabric (or (cost-sheet-call-fabric-cost call) 0))
         (labor (or (cost-sheet-call-labor-cost call) 0))
         (trims (or (cost-sheet-call-trims-cost call) 0))
         (quantity (max 1 (or (cost-sheet-call-quantity call) 1)))
         (per (+ fabric labor trims))
         (overhead (* per 0.10d0))
         (unit (+ per overhead))
         (markup (setting :retail-markup-percent)))
    (nlk:encode-json-object
     (nlk:json-object
      "garment_type" garment
      "cost_breakdown_per_unit" (nlk:json-object
                                 "fabric" (rounded fabric 2)
                                 "labor" (rounded labor 2)
                                 "trims" (rounded trims 2)
                                 "overhead_10pct" (rounded overhead 2)
                                 "total_cost_per_unit" (rounded unit 2))
      "quantity" quantity
      "total_production_cost" (rounded (* unit quantity) 2)
      "retail_markup_percent" markup
      "suggested_retail_price" (rounded (* unit (coerce (/ markup 100.0d0) 'double-float)) 2)
      "currency" (currency)))))

(defun construction-guide (call)
  "The construction_guide tool: the garment's steps, a tip for the skill
level, and a rough estimate."
  (let* ((garment (construction-guide-call-garment-type call))
         (fabric (text-or (construction-guide-call-fabric-type call) "woven"))
         (skill (string-downcase (text-or (construction-guide-call-skill-level call)
                                          "intermediate")))
         (steps (or (rest (assoc (garment-key garment) +construction-steps+ :test #'equal))
                    +generic-steps+)))
    (nlk:encode-json-object
     (nlk:json-object
      "garment_type" garment
      "fabric_type" fabric
      "skill_level" skill
      "steps" (steps-json steps)
      "tips" (table-value +skill-tips+ skill
                           "Use the needle and presser foot the fabric asks for.")
      "estimated_time_hours" (max 2 (length steps))))))

(defun color-palette (call)
  "The color_palette tool: one five-colour story for the season."
  (let* ((season (text-or (color-palette-call-season call) "spring"))
         (trend (text-or (color-palette-call-trend call) ""))
         (mood (text-or (color-palette-call-mood call) ""))
         (key (season-key season)))
    (nlk:encode-json-object
     (nlk:json-object
      "season" season
      "trend" trend
      "mood" mood
      "palette" (color-palette-json (palette-of (or key "spring")))
      "usage_notes" "Use 1-2 hero colors, 2 supporting, 1 neutral for balance."))))

(defun measurement-chart (call)
  "The measurement_chart tool: one row of body measurements, in the section's
units; a size the table does not carry is approximated from the base."
  (let* ((size (string-upcase (text-or (measurement-chart-call-size call) "M")))
         (gender (string-downcase (text-or (measurement-chart-call-gender call) "women")))
         (age (text-or (measurement-chart-call-age-range call) "adult"))
         (key (if (and (plusp (length gender)) (char= (char gender 0) #\m)) "men" "women"))
         (exact (body-row key size))
         (row (or exact (body-row key "M")))
         (shift (if exact 0 (- (mod (char-code (char size 0)) 5) 2))))
    (nlk:encode-json-object
     (nlk:json-object
      "size" size
      "gender" gender
      "age_range" age
      "body_measurements" (apply #'nlk:make-json-object
                                 (loop for (point value) in row
                                       append (list point (in-units (+ value shift)))))
      "units" (units-name)
      :when (not exact) "approximated_from" "the base size"
      "notes" (if exact
                   "Standard body measurements; adjust for target fit and brand standards."
                   "No table row for that size: the base size is shown with a small offset.")))))

(defun collection-plan (call)
  "The collection_plan tool: the pieces in a balanced order, with the colour
and fabric story."
  (let* ((season (text-or (collection-plan-call-season call) "Spring/Summer"))
         (count (max 1 (or (collection-plan-call-number-of-pieces call) 6)))
         (theme (text-or (collection-plan-call-theme call) "core collection"))
         (types (coerce +collection-types+ 'vector)))
    (nlk:encode-json-object
     (nlk:json-object
      "season" season
      "theme" theme
      "number_of_pieces" count
      "collection_overview" (coerce (loop for index below count
                                           collect (nlk:json-object
                                                    "piece" (1+ index)
                                                    "name" (format nil "Piece ~d" (1+ index))
                                                    "garment_type" (aref types (mod index (length types)))
                                                    "color" (if (< index 2) "hero" "supporting")))
                                     'vector)
      "color_story" "2 hero colors, 2 supporting, 1 neutral"
      "fabric_story" "Mix of natural fibers and performance blends"
      "notes" "Balance tops and bottoms; include at least one outerwear piece."))))

;;; --- the command ------------------------------------------------------------

(defparameter +verbs+
  '(("design-brief" :key "concept" :required t :contract design-brief-call
     :function design-brief
     :summary "silhouette, palette, fabrics and details from a concept")
    ("tech-pack" :key "garment_type" :required t :contract tech-pack-call
     :function tech-pack
     :summary "the pack, with a measurement table over the garment's points of measure")
    ("size-sheet" :key "garment_type" :required t :contract size-sheet-call
     :function size-sheet
     :summary "one graded row per size from a base size")
    ("bom" :key "garment_type" :required t :contract bom-call
     :function bill-of-materials
     :summary "the seven components, with cost and the total per unit")
    ("fabric-calculator" :key "garment_type" :required t :contract fabric-yardage-call
     :function fabric-yardage
     :summary "yards per garment and for the run, waste included")
    ("cost-sheet" :key "garment_type" :required t :contract cost-sheet-call
     :function cost-sheet
     :summary "unit cost with overhead, the run's total and a retail price")
    ("construction-guide" :key "garment_type" :required t :contract construction-guide-call
     :function construction-guide
     :summary "numbered steps and a tip for the skill level")
    ("color-palette" :key "season" :contract color-palette-call
     :function color-palette
     :summary "a five-colour story with hex and Pantone references")
    ("measurement-chart" :key "size" :required t :contract measurement-chart-call
     :function measurement-chart
     :summary "standard body measurements for one size")
    ("collection-plan" :key "theme" :contract collection-plan-call
     :function collection-plan
     :summary "the pieces, the colour story and the fabric story"))
  "The ten verbs as the command reads them: the tool's name, the one argument
the command fills from its line, whether that argument is required, the verb
it calls, and the line the overview prints.")

(defun overview-text ()
  "The answer of a bare /fashion-design: what the ten verbs are, and how to
run one."
  (format nil "fashion-design: ten verbs, each also a tool of the same name.~%~
/fashion-design VERB ARGUMENT runs one, e.g. /fashion-design tech-pack jacket.~%~%~
~{  ~a ~a -- ~a~%~}~%~
From eval: (fashion-design:measurement-chart (fashion-design::measurement-chart-call \"M\"))."
          (loop for (name . plist) in +verbs+
                append (list (format nil "~va" 20 name)
                             (format nil "~va" 14 (getf plist :key))
                             (getf plist :summary)))))

(defun verb-entry (name)
  "The +VERBS+ entry called NAME, with underscores read as dashes."
  (let ((wanted (substitute #\- #\_ (string-downcase (trimmed name)))))
    (assoc wanted +verbs+ :test #'equal)))

(defun run-verb (entry argument)
  "ENTRY's verb on ARGUMENT, as the command runs it: the declared contract is
the same one the tool call comes through."
  (let* ((name (first entry))
         (key (getf (rest entry) :key))
         (required (getf (rest entry) :required))
         (function (getf (rest entry) :function))
         (text (trimmed argument)))
    (if (and required (zerop (length text)))
        (format nil "fashion-design: /fashion-design ~a needs ~a, e.g. /fashion-design ~a ~a"
                name key name (if (equal name "measurement-chart") "M" "jacket"))
        (handler-case
            (let* ((declared (nlk:contract (getf (rest entry) :contract)))
                   (call (nlk:contract-decode declared (nlk:make-json-object key text))))
              (funcall function call))
          (nlk:contract-refusal (refusal)
            (format nil "fashion-design: /fashion-design ~a refused that call: ~a"
                    name refusal))))))

(defun run-slash (args session)
  "/fashion-design: the overview, or one verb on the line's argument."
  (declare (ignore session))
  (let* ((text (trimmed args))
         (space (position #\Space text)))
    (if (zerop (length text))
        (overview-text)
        (let* ((name (if space (subseq text 0 space) text))
               (argument (if space (trimmed (subseq text (1+ space))) ""))
               (entry (verb-entry name)))
          (if entry
              (run-verb entry argument)
              (format nil "fashion-design: no verb called ~a.~%~a" name (overview-text)))))))

;;; --- the manual -------------------------------------------------------------

(defparameter *manual*
  "fashion-design: the ten drafting verbs of a small fashion studio.

Each is a tool the model calls and a name the command runs:
  /fashion-design design-brief CONCEPT      silhouette, palette, fabrics, details
  /fashion-design tech-pack GARMENT          the pack with its points of measure
  /fashion-design size-sheet GARMENT         every point graded from a base size
  /fashion-design bom GARMENT                the seven components and their cost
  /fashion-design fabric-calculator GARMENT  yards per garment and for the run
  /fashion-design cost-sheet GARMENT         unit cost, the run's total, retail
  /fashion-design construction-guide GARMENT numbered steps and a tip
  /fashion-design color-palette SEASON       a five-colour story, hex and Pantone
  /fashion-design measurement-chart SIZE     standard body measurements
  /fashion-design collection-plan THEME      the pieces and the two stories

Every verb answers JSON. The tables are in data.lisp and the section's four
settings shape them: units (in or cm) for the measurements the cell computes,
currency for the money, waste_percent for the yardage, retail_markup_percent
for the retail price. Nothing here reaches the network, and no key is needed.

(fashion-design:measurement-chart (fashion-design::measurement-chart-call \"M\"))
  => the body measurements of one size
(fashion-design:size-sheet (fashion-design::size-sheet-call \"dress\"))
  => the graded chart"
  "The (help :fashion-design) text.")
