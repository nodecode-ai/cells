;;;; package.lisp --- the cell's package, and the ethics data every file shares.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The six modules, the six constitution presets and the three strictness
;;;; levels are data, as the original ships them in modules/*.md and
;;;; constitution/templates.json: a layer or the operator can rebind them, and
;;;; the engine never carries a second copy of a rubric.

(defpackage #:nodecode-ethics-filter
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*ethics-filter*
           #:ethics-filter-error #:ethics-filter-error-detail))

(in-package #:nodecode-ethics-filter)

(nlk:define-peripheral ethics-filter :not-running t)

;;; --- the six modules -------------------------------------------------------

(defparameter +modules+
  '((:name "environmental"
     :title "Environmental Stewardship"
     :description "Impact on the natural world: carbon, waste, water, biodiversity, resource use, lifecycle and circularity."
     :question "What is the impact of this decision on the natural world, and is there a more environmentally sound alternative?"
     :relevant-when ("physical resources" "manufacturing" "transport" "energy" "waste"
                      "packaging" "supply chain" "travel" "raw materials" "chemicals"
                      "emissions" "water" "land use" "construction" "product design"
                      "sourcing" "fossil fuels" "renewable" "shipping" "fleet"
                      "facilities" "office" "hardware" "food production" "agriculture"
                      "forestry" "mining" "carbon" "footprint" "recycling" "plastic"
                      "sustainable" "eco" "environment" "climate" "co2" "green" "pollution")
     :exclude-when nil
     :criteria (("Direct Environmental Impact"
                  "Severe, irreversible damage (toxic contamination, deforestation, habitat destruction)"
                  "Significant negative impact (high emissions, excessive waste, water depletion)"
                  "Minor negative impact with mitigation measures"
                  "Neutral or positive environmental contribution")
                 ("Lifecycle Thinking"
                  "No consideration of full lifecycle (source -> use -> disposal)"
                  "Partial consideration with gaps"
                  "Reasonable lifecycle assessment considered"
                  "Full circularity: designed for regeneration, reuse, or minimal end-of-life impact")
                 ("Resource Efficiency"
                  "Excessive resource use, planned obsolescence, single-use"
                  "Standard efficiency with room for improvement"
                  "Above-average efficiency, use of renewables"
                  "Leading efficiency, regenerative resource loops, zero waste")
                 ("Biodiversity & Ecosystem Health"
                  "Active harm to biodiversity and ecosystems"
                  "Neglects biodiversity considerations"
                  "Considers biodiversity with mitigation"
                  "Actively enhances biodiversity and ecosystem health")))
    (:name "fairness"
     :title "Fairness & Stakeholder Impact"
     :description "Who is affected and how: equitable distribution of benefits and burdens, worker welfare, community impact, supply chain equity."
     :question "Who does this decision affect, how are they affected, and is the distribution of benefits and burdens equitable?"
     :relevant-when nil
     :exclude-when ("purely personal preference"
                     "trivial internal decision affecting only the decision-maker"
                     "no external stakeholders" "no other people affected"
                     "affects only the decision-maker")
     :criteria (("Stakeholder Inclusion"
                  "Stakeholders ignored or actively harmed without consideration"
                  "Only powerful stakeholders considered"
                  "Most stakeholders identified and considered"
                  "All stakeholders actively included, including marginalised voices")
                 ("Distribution of Benefits and Burdens"
                  "Benefits concentrated, burdens externalised onto vulnerable groups"
                  "Uneven distribution without adequate compensation"
                  "Reasonably equitable distribution"
                  "Benefits shared broadly, burdens owned by those who can bear them")
                 ("Worker/Producer Welfare"
                  "Exploitation: unsafe conditions, below-living wages, forced labour risk"
                  "Compliance with minimum legal standards only"
                  "Above-minimum standards, fair wages, reasonable conditions"
                  "Thriving: living wages, ownership opportunities, development pathways")
                 ("Community Impact"
                  "Extracts value from community, causes harm, no reinvestment"
                  "Neutral: neither helps nor harms local community"
                  "Positive contribution to local community"
                  "Regenerative: actively strengthens community resilience and wellbeing")
                 ("Supply Chain Equity"
                  "Known exploitation in supply chain, no due diligence"
                  "Minimal supply chain oversight"
                  "Active supply chain monitoring with improvement programmes"
                  "Full supply chain transparency with equitable partnerships")))
    (:name "transparency"
     :title "Transparency & Accountability"
     :description "Would you publish this decision: publicity test, traceability, accountability, values alignment, conflicts of interest."
     :question "Would you be comfortable if every aspect of this decision -- your reasoning, the trade-offs, the outcomes -- were made public?"
     :relevant-when nil
     :exclude-when ("trivial personal preference without consequences"
                     "purely personal preference" "no external stakeholders"
                     "no deeper implications" "no wrong choice")
     :criteria (("The Publicity Test"
                  "Would be embarrassed or harmed if this decision was published"
                  "Would prefer it not be public, but could defend it"
                  "Would be fine with it being public"
                  "Would actively welcome public scrutiny as a validation of integrity")
                 ("Traceability"
                  "No record of decision-making process exists"
                  "Ad hoc or incomplete documentation"
                  "Clear, documented decision trail"
                  "Full audit trail with reasoning, alternatives considered, and trade-offs")
                 ("Accountability Assignment"
                  "No one is accountable for the decision or its outcomes"
                  "Accountability unclear or diffuse"
                  "Clear accountability assigned"
                  "Named accountability with mechanisms for redress if harm occurs")
                 ("Alignment with Stated Values"
                  "Decision contradicts stated values without acknowledgment"
                  "Inconsistency between values and action"
                  "Generally aligned with stated values"
                  "Exemplary: action actively demonstrates values in practice")
                 ("Conflict of Interest Check"
                  "Undisclosed conflict of interest present"
                  "Potential conflict not fully examined"
                  "Conflicts declared and managed"
                  "No conflicts; or conflicts declared, managed, and mitigated")))
    (:name "conscious-leadership"
     :title "Conscious Leadership & Values Alignment"
     :description "Above or below the line: ownership vs blame, values alignment, long-term vs short-term orientation, growth orientation."
     :question "Does this decision come from above the line (curiosity, commitment, ownership) or below the line (defensiveness, blame, denial)?"
     :relevant-when ("values" "purpose" "long-term" "leadership" "culture" "team"
                      "integrity" "dilemma" "difficult choice" "character" "role model"
                      "conflict" "change management" "strategy" "hiring" "firing"
                      "conversation" "accountability" "growth" "relationship" "family"
                      "friend" "community" "care" "apology" "amends" "promotion"
                      "career" "ethical" "morals" "principle" "responsibility" "trust")
     :exclude-when nil
     :criteria (("Above/Below the Line Assessment"
                  "Decision clearly comes from below the line (fear, control, blame, avoidance)"
                  "Mixed: some below-the-line dynamics present"
                  "Predominantly above the line with minor concerns"
                  "Clear above-the-line decision-making: ownership, curiosity, commitment")
                 ("Values Alignment"
                  "Decision contradicts the stated values of the individual/organization"
                  "Values not explicitly considered in the decision"
                  "Values considered and mostly honoured"
                  "Decision directly embodies and demonstrates core values")
                 ("Long-Term vs Short-Term Orientation"
                  "Sacrifices long-term integrity for short-term gain"
                  "Short-term focus with some long-term consideration"
                  "Balanced short and long-term thinking"
                  "Decisions guided by long-term vision, patient capital thinking")
                 ("Personal Growth Orientation"
                  "Defensive: refuses feedback, avoids learning, blames others"
                  "Open to feedback but doesn't act on it"
                  "Actively seeks feedback and adjusts"
                  "Decision framed as learning opportunity regardless of outcome")))
    (:name "ethical-framework"
     :title "Ethical Decision Framework"
     :description "The meta-ethical lens: utilitarian, rights, fairness, common good, and virtue. Always relevant when enabled."
     :question "What do different ethical traditions say about this decision, and can I defend this choice using rigorous ethical reasoning?"
     :relevant-when nil
     :exclude-when nil
     :criteria (("Utilitarian: greatest good"
                  "Which option produces the greatest good for the greatest number of people? Consider all stakeholders, weigh benefits against harms, and do not hide a minority harm behind the majority.")
                 ("Rights: dignity and autonomy"
                  "Does this decision respect the fundamental rights and dignity of all involved? Human rights, informed consent, privacy, autonomy, fair process.")
                 ("Fairness/justice: equity and desert"
                  "Is this decision fair, equitable and just? Distributive justice (benefits and burdens shared), procedural justice (the process was fair), corrective justice (harm has redress).")
                 ("Common good: community and belonging"
                  "Does this decision contribute to the common good? Strengthens social bonds, supports shared resources and institutions, considers the least advantaged.")
                 ("Virtue: character and integrity"
                  "What would a person of strong moral character do here? Honesty, compassion, courage, integrity, humility; is there a gap between who you are and who you want to be?")))
    (:name "compliance"
     :title "Compliance & Certification Guard"
     :description "Legal, regulatory and certification obligations: certification risk, legal compliance, audit readiness, traceability, reporting."
     :question "Does this decision risk any existing certification, breach any regulation, or create unacceptable legal/compliance exposure?"
     :relevant-when ("legal" "regulation" "certification" "contract" "audit" "tax"
                      "reporting" "license" "permit" "standard" "policy" "law"
                      "compliance" "governance" "board" "disclosure" "filing" "statutory"
                      "obligation" "b corp" "iso" "organic" "fair trade" "accredited"
                      "employment" "privacy" "gdpr" "financial" "industry standard"
                      "illegal" "crime" "unlawful" "toxic" "hazardous" "inspection"
                      "regulatory" "statute" "liability" "negligence" "due diligence"
                      "fiduciary" "oversight" "safety violation" "structural risk"
                      "building code")
     :exclude-when nil
     :criteria (("Certification Risk"
                  "Decision would likely violate certification requirements (B Corp, Fair Trade, organic)"
                  "Decision creates material risk of non-compliance with certification standards"
                  "Decision is compliant but with minor concerns to monitor"
                  "Decision is fully compliant with all active certifications and standards")
                 ("Legal & Regulatory Compliance"
                  "Decision violates laws or regulations"
                  "Decision tests legal boundaries or operates in grey areas"
                  "Decision is compliant with applicable law"
                  "Decision exceeds minimum legal requirements in alignment with best practice")
                 ("Audit Readiness"
                  "No documentation trail would survive an audit"
                  "Incomplete documentation, gaps in evidence"
                  "Adequate documentation for standard audit"
                  "Exemplary documentation: audit would confirm best practice")
                 ("Supply Chain/Traceability"
                  "No supply chain visibility or due diligence"
                  "Partial visibility with gaps"
                  "Reasonable supply chain monitoring"
                  "Full traceability with documented ethical sourcing")
                 ("Timing & Reporting Obligations"
                  "Decision creates reporting failures or deadline breaches"
                  "Timing will be tight with some reporting risk"
                  "Adequate time for all reporting obligations"
                  "Decision enhances reporting capability or creates buffer"))))
  "The six modules, in the original's order, each with the rubric bands its
model scores against: 0-25, 26-50, 51-75, 76-100.")

(defparameter +bands+ '("0-25" "26-50" "51-75" "76-100")
  "The four rubric bands, in a module's criteria order.")

(defun module-info (name)
  "The module called NAME, or NIL."
  (find name +modules+ :key (lambda (module) (getf module :name)) :test #'string=))

(defun module-names ()
  "Every module name, in the original's order."
  (mapcar (lambda (module) (getf module :name)) +modules+))

;;; --- the constitution presets ------------------------------------------------

(defparameter +presets+
  '((:id "personal-reflection" :name "Personal Reflection"
     :description "For individuals making significant life decisions -- career, relationships, purchases, ethics"
     :modules (("environmental" . nil) ("fairness" . t) ("transparency" . t)
               ("conscious-leadership" . t) ("ethical-framework" . t) ("compliance" . nil))
     :strictness "moderate")
    (:id "small-business-ethical" :name "Small Ethical Business"
     :description "For small to medium enterprises that want to operate with integrity"
     :modules (("environmental" . t) ("fairness" . t) ("transparency" . t)
               ("conscious-leadership" . t) ("ethical-framework" . t) ("compliance" . t))
     :strictness "moderate")
    (:id "corporate-governance" :name "Corporate Governance"
     :description "For larger organizations embedding ethics into decision-making pipelines"
     :modules (("environmental" . t) ("fairness" . t) ("transparency" . t)
               ("conscious-leadership" . nil) ("ethical-framework" . t) ("compliance" . t))
     :strictness "strict")
    (:id "startup-quick" :name "Startup Quick Check"
     :description "For early-stage startups that need a lightweight ethics check without heavy compliance focus"
     :modules (("environmental" . t) ("fairness" . t) ("transparency" . t)
               ("conscious-leadership" . t) ("ethical-framework" . nil) ("compliance" . nil))
     :strictness "relaxed")
    (:id "maximalist" :name "Maximalist (All Modules, Strict)"
     :description "Full ethics filter at maximum strictness -- for decisions with high stakes"
     :modules (("environmental" . t) ("fairness" . t) ("transparency" . t)
               ("conscious-leadership" . t) ("ethical-framework" . t) ("compliance" . t))
     :strictness "strict")
    (:id "minimal-safe" :name "Minimal Safe Baseline"
     :description "Just the essentials -- fairness, transparency, and compliance"
     :modules (("environmental" . nil) ("fairness" . t) ("transparency" . t)
               ("conscious-leadership" . nil) ("ethical-framework" . nil) ("compliance" . t))
     :strictness "moderate"))
  "The preset constitutions: which modules are in play, and how strict the
verdict is by default.")

(defparameter +strictness-levels+
  '((:id "relaxed" :red-below 30 :green-at-or-above 70)
    (:id "moderate" :red-below 50 :green-at-or-above 80)
    (:id "strict" :red-below 70 :green-at-or-above 90))
  "What a strictness level means: below RED-BELOW is a block, at or above
GREEN-AT-OR-ABOVE is a pass, and between the two is a flag for a person.")

(defun preset-ids ()
  "Every preset's id, for the section and the tool's own choices."
  (mapcar (lambda (preset) (getf preset :id)) +presets+))

(defparameter +strictness-ids+ '("relaxed" "moderate" "strict"))

(defun preset-info (id)
  "The preset called ID, or NIL."
  (find id +presets+ :key (lambda (preset) (getf preset :id)) :test #'string=))

(defun strictness-info (id)
  "The strictness level called ID, or NIL."
  (find id +strictness-levels+ :key (lambda (level) (getf level :id)) :test #'string=))
