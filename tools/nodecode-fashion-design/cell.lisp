;;;; cell.lisp --- the cell: ten tools, one command, one manual topic.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original Hermes plugin registers ten tools and a bundled skill, and
;;;; nothing else: no hooks, no state, no network. This is the same ten tools
;;;; and the same knowledge, declared once with NLE:DEFINE-CELL, so a start
;;;; installs all of it and a stop takes exactly that back. The section adds
;;;; the four knobs the original had as constants: units, currency, cutting
;;;; waste and retail markup.

(in-package #:nodecode-fashion-design)

(nlk:define-peripheral fashion-design :not-running t)

(nle:define-cell fashion-design
  (:section ("fashion-design")
    (:guide "nothing here is required; units picks the length unit the composed measurements are in, currency labels the money, waste_percent feeds the fabric calculator and retail_markup_percent the cost sheet")
    ("units" :choice :options '("in" "cm") :default "in"
     :doc "Length unit for the body and garment measurements the size sheet and the measurement chart compute.")
    ("currency" :choice :options '("USD" "EUR" "GBP" "JPY" "INR") :default "USD"
     :doc "The currency the bill of materials and the cost sheet are labelled in.")
    ("waste_percent" :integer :default 12 :min 0
     :doc "Cutting waste the fabric calculator adds to the yardage, in percent.")
    ("retail_markup_percent" :integer :default 250 :min 100
     :doc "The cost sheet's suggested retail price, as a percentage of the unit cost."))
  (:tool "design_brief" (nlk:contract 'design-brief-call) #'design-brief)
  (:tool "tech_pack" (nlk:contract 'tech-pack-call) #'tech-pack)
  (:tool "size_sheet" (nlk:contract 'size-sheet-call) #'size-sheet)
  (:tool "bom" (nlk:contract 'bom-call) #'bill-of-materials)
  (:tool "fabric_calculator" (nlk:contract 'fabric-yardage-call) #'fabric-yardage)
  (:tool "cost_sheet" (nlk:contract 'cost-sheet-call) #'cost-sheet)
  (:tool "construction_guide" (nlk:contract 'construction-guide-call) #'construction-guide)
  (:tool "color_palette" (nlk:contract 'color-palette-call) #'color-palette)
  (:tool "measurement_chart" (nlk:contract 'measurement-chart-call) #'measurement-chart)
  (:tool "collection_plan" (nlk:contract 'collection-plan-call) #'collection-plan)
  (:command "fashion-design" #'run-slash
            :description "Draft a design brief, tech pack, size sheet, BOM, palette or cost sheet"
            :argument-hint "VERB ARGUMENT")
  (:help :fashion-design
         "fashion-design: drafting verbs - briefs, tech packs, size sheets, BOMs, yardage, costing"
         *manual*))
