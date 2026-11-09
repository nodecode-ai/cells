;;;; cell-test.lisp --- lifecycle, the ten verbs, the section's four knobs, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches the network or the clock: every verb is a pure read
;;;; of data.lisp through the section's settings, so the tests are the calls
;;;; the model makes -- the tool found by name in the table, its arguments
;;;; decoded through the declaration it registered with -- and the answers
;;;; decoded back from JSON.

(in-package #:nodecode.test)

;;; --- the slice ---------------------------------------------------------------

(define-test-slice "fashion-design" "FASHION-DESIGN-CELL-"
    :start nodecode-fashion-design:start-cell
  :defaults (("units" "in") ("currency" "USD")
             ("waste_percent" 12) ("retail_markup_percent" 250)))

(defparameter +tool-names+
  '("design_brief" "tech_pack" "size_sheet" "bom" "fabric_calculator"
    "cost_sheet" "construction_guide" "color_palette" "measurement_chart"
    "collection_plan")
  "The ten tools the original plugin registers, by the names it used.")

(defmacro with-fashion-design ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (its defaults when none),
stopped on unwind: every verb reads the settings of a running cell."
  `(with-temp-store ()
     (with-cell-stop ((fashion-design-start ,@pairs))
       ,@body)))

(defun fashion-tool (name &rest pairs)
  "The tool called NAME as the engine calls it: the arguments built from PAIRS
and decoded through the contract the tool registered with."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun fashion-json (name &rest pairs)
  "The tool called NAME's answer, decoded."
  (nlk:decode-json (apply #'fashion-tool name pairs)))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "fashion-design"
  (:help :fashion-design)
  (:command "fashion-design")
  (:running
   (is (= 10 (count-if #'nle::find-tool +tool-names+)) "all ten tools are registered")
   (is (equal "in" (nodecode-fashion-design::setting :units)) "the section's unit is bound")
   (is (= 250 (nodecode-fashion-design::setting :retail-markup-percent)) "and its markup"))
  (:stopped
   (is (null (nle::find-tool "design_brief")) "stop takes the first tool out")
   (is (null (nle::find-tool "collection_plan")) "and the last"))
  (:refused ("units" "fathoms") ("waste_percent" -1)))

;;; --- every tool answers ------------------------------------------------------------

(deftest fashion-design-cell-each-tool-answers-its-own-shape ()
  (with-fashion-design ()
    (dolist (case '(("design_brief" ("concept" "washed linen summer") "silhouette")
                    ("tech_pack" ("garment_type" "dress") "measurements")
                    ("size_sheet" ("garment_type" "t-shirt") "size_chart")
                    ("bom" ("garment_type" "jacket") "bill_of_materials")
                    ("fabric_calculator" ("garment_type" "skirt") "yardage_per_garment")
                    ("cost_sheet" ("garment_type" "shirt") "total_production_cost")
                    ("construction_guide" ("garment_type" "pants") "steps")
                    ("color_palette" ("season" "Fall/Winter 2026") "palette")
                    ("measurement_chart" ("size" "M") "body_measurements")
                    ("collection_plan" ("theme" "harbour") "collection_overview")))
      (destructuring-bind (name pairs key) case
        (let ((answer (nlk:decode-json (apply #'fashion-tool name pairs))))
          (is (hash-table-p answer) (format nil "~a answers one object" name))
          (is (gethash key answer) (format nil "~a answers ~a" name key)))))))

(deftest fashion-design-cell-a-brief-reads-the-season-and-the-tier ()
  (with-fashion-design ()
    (let ((brief (fashion-json "design_brief" "concept" "soft tailoring"
                                "season" "Fall/Winter 2025" "price_point" "luxury")))
      (is (equal "luxury" (gethash "price_point" brief)) "the tier is carried back")
      (is (search "couture" (gethash "silhouette" brief)) "and picks the silhouette")
      (is (= 4 (length (gethash "color_palette" brief))) "four colours, not five")
      (is (find "wool blend" (gethash "fabric_suggestions" brief) :test #'equal) "the season's fabrics are the fall ones"))))

(deftest fashion-design-cell-a-size-sheet-grades-from-its-base ()
  (with-fashion-design ()
    (let* ((sheet (fashion-json "size_sheet" "garment_type" "t-shirt"
                                 "base_size" "M" "size_range" #("XS" "S" "M" "L")))
           (chart (gethash "size_chart" sheet)))
      (is (= 40 (gethash "chest" (gethash "M" chart))) "the base size takes the ease, not the measurement")
      (is (= 36 (gethash "chest" (gethash "XS" chart))) "two sizes down is two increments")
      (is (= 42 (gethash "chest" (gethash "L" chart))) "and one up is one"))))

(deftest fashion-design-cell-a-measurement-chart-follows-the-units ()
  (with-fashion-design ()
    (let ((inches (fashion-json "measurement_chart" "size" "M" "gender" "women")))
      (is (equal "in" (gethash "units" inches)) "inches by default")
      (is (= 36 (gethash "bust" (gethash "body_measurements" inches))))))
  (with-fashion-design ("units" "cm")
    (let ((metric (fashion-json "measurement_chart" "size" "M" "gender" "women")))
      (is (equal "cm" (gethash "units" metric)) "the section says centimetres")
      (is (= 91.4 (gethash "bust" (gethash "body_measurements" metric))) "and the measurement is converted"))))

(deftest fashion-design-cell-a-size-the-table-lacks-is-approximated ()
  (with-fashion-design ()
    (let ((answer (fashion-json "measurement_chart" "size" "8" "gender" "women")))
      (is (hash-table-p (gethash "body_measurements" answer)) "a row is still answered")
      (is (equal "the base size" (gethash "approximated_from" answer)) "and says what it was read from"))))

(deftest fashion-design-cell-the-fabric-calculator-adds-the-waste ()
  (with-fashion-design ()
    (let ((plain (fashion-json "fabric_calculator" "garment_type" "dress"
                                "fabric_width" 45 "size" "M" "quantity" 2)))
      (is (= 2.8 (gethash "yardage_per_garment" plain)) "2.5 yards plus 12 %")
      (is (= 5.6 (gethash "total_yardage" plain)) "and twice that for two garments")))
  (with-fashion-design ("waste_percent" 0)
    (let ((bare (fashion-json "fabric_calculator" "garment_type" "dress"
                               "fabric_width" 45 "quantity" 1)))
      (is (= 2.5 (gethash "yardage_per_garment" bare)) "no waste when the section says none")))
  (with-fashion-design ()
    (let ((wide (fashion-json "fabric_calculator" "garment_type" "coat" "fabric_width" 60)))
      (is (= 2.52 (gethash "yardage_per_garment" wide)) "a 60-inch width takes three quarters of the 45-inch yardage"))))

(deftest fashion-design-cell-the-cost-sheet-uses-the-section-markup ()
  (with-fashion-design ()
    (let ((sheet (fashion-json "cost_sheet" "garment_type" "shirt"
                                "fabric_cost" 8 "labor_cost" 5 "trims_cost" 2
                                "quantity" 10)))
      (is (= 16.5 (gethash "total_cost_per_unit" (gethash "cost_breakdown_per_unit" sheet))) "the cost with 10 % overhead")
      (is (= 165.0 (gethash "total_production_cost" sheet)) "ten units")
      (is (= 41.25 (gethash "suggested_retail_price" sheet)) "and 250 % of the unit cost")
      (is (equal "USD" (gethash "currency" sheet)) "in the section's currency")))
  (with-fashion-design ("currency" "EUR" "retail_markup_percent" 300)
    (let ((sheet (fashion-json "cost_sheet" "garment_type" "shirt"
                                "fabric_cost" 8 "labor_cost" 5 "trims_cost" 2)))
      (is (equal "EUR" (gethash "currency" sheet)) "the currency follows the section")
      (is (= 49.5 (gethash "suggested_retail_price" sheet)) "and so does the markup"))))

(deftest fashion-design-cell-the-bill-of-materials-names-what-it-is-given ()
  (with-fashion-design ()
    (let ((bill (fashion-json "bom" "garment_type" "jacket" "fabric" "waxed cotton"
                               "trims" "horn buttons")))
      (is (= 7 (length (gethash "bill_of_materials" bill))) "seven components")
      (is (= 17.25 (gethash "total_material_cost_per_unit" bill)) "and their total")
      (let ((first (aref (gethash "bill_of_materials" bill) 0)))
        (is (equal "waxed cotton" (gethash "description" first)) "the fabric is named")))))

(deftest fashion-design-cell-the-guide-and-the-plan-answer-their-lists ()
  (with-fashion-design ()
    (let ((guide (fashion-json "construction_guide" "garment_type" "pants"
                                "skill_level" "beginner")))
      (is (= 8 (length (gethash "steps" guide))) "the pants come with their eight steps")
      (is (search "basting" (gethash "tips" guide)) "and the beginner's tip"))
    (let ((plan (fashion-json "collection_plan" "theme" "harbour" "number_of_pieces" 3)))
      (is (= 3 (length (gethash "collection_overview" plan))) "three pieces")
      (is (equal "t-shirt"
                 (gethash "garment_type" (aref (gethash "collection_overview" plan) 0))) "filled from the collection's own order"))))

(deftest fashion-design-cell-a-call-without-its-required-member-is-refused ()
  (with-fashion-design ()
    (is (signals-error nlk:contract-refusal (fashion-tool "design_brief" "season" "SS25")) "a brief needs its concept")
    (is (signals-error nlk:contract-refusal (fashion-tool "measurement_chart")) "a chart needs its size")
    (is (signals-error nlk:contract-refusal (fashion-tool "design_brief" "concept" "x"
                                                          "price_point" "expensive")) "a price tier outside the declared four is refused")))

;;; --- the command ------------------------------------------------------------------

(deftest fashion-design-cell-the-command-runs-one-verb ()
  (with-fashion-design ()
    (multiple-value-bind (text dialog)
        (cell-entry "nodecode-fashion-design" "fashion-design" "tech-pack jacket" "s1")
      (is (null dialog) "a plain answer, no panel")
      (let ((pack (nlk:decode-json text)))
        (is (equal "jacket" (gethash "garment_type" pack)) "the verb ran on the argument")
        (is (gethash "measurements" pack) "and answered the pack")))
    (let ((metric (nlk:decode-json (cell-entry "nodecode-fashion-design" "fashion-design"
                                                "measurement-chart L" "s1"))))
      (is (equal "L" (gethash "size" metric)) "a dashes verb and a size answer one row"))))

(deftest fashion-design-cell-the-command-explains-itself ()
  (with-fashion-design ()
    (let ((bare (cell-entry "nodecode-fashion-design" "fashion-design" "" "s1")))
      (dolist (verb '("design-brief" "tech-pack" "size-sheet" "bom" "fabric-calculator"
                      "cost-sheet" "construction-guide" "color-palette"
                      "measurement-chart" "collection-plan"))
        (is (search verb bare) (format nil "the overview lists ~a" verb))))
    (let ((unknown (cell-entry "nodecode-fashion-design" "fashion-design" "wardrobe coat" "s1"))
          (short (cell-entry "nodecode-fashion-design" "fashion-design" "cost-sheet" "s1")))
      (is (search "no verb called wardrobe" unknown) "an unknown verb says so")
      (is (search "needs garment_type" short) "and a verb without its argument says which"))))
