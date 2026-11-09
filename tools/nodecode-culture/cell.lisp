;;;; cell.lisp --- the section, the three tools, /culture, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original Hermes plugin registers three deterministic tools, a
;;;; /culture command and a practitioner skill, all computed from two bundled
;;;; research files. This cell gives a model the same three tools, the
;;;; operator the same command, and puts the playbook's doctrine on
;;;; (help :culture) instead of a skill, since a manual topic is the one text
;;;; every request of this image already knows how to carry.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "culture": {"enabled": true,
;;;;               "home_country": "Germany",
;;;;               "max_gaps": 5,
;;;;               "max_highlights": 5,
;;;;               "include_hofstede": true}
;;;; Nothing is required: with an empty section the tools answer, and only a
;;;; gap analysis asked without a home country asks for one.

(in-package #:nodecode-culture)

;;; --- the boundary: what a model call carries --------------------------------------

(nlk:define-contract culture-brief-request
  (:doc "A country's business-culture brief: how trust is built there, how decisions are
really made, how contracts and time are treated, communication style, etiquette and
taboos, the field notes that say where the country differs from its region, and an
optional task-specific hint. Use it before a cross-border negotiation, a first
meeting, a management action, an email to foreign counterparts, or when a
cross-cultural deal has stalled.")
  ("country" :text :required t
   :doc "the country, e.g. Japan; aliases work: USA, UK, UAE, Korea, Holland, Czech Republic, Burma, Lao PDR, PRC, HK, KSA")
  ("task" :string
   :doc "optional context -- negotiation, first meeting, management, communication, payment, trip (or a word from one: deal, email, hiring, travel); it picks the hint"))

(nlk:define-contract culture-gap-request
  (:doc "The cultural distance between the country you work from and a counterpart's
country: the eight Meyer scales ranked by the gap that matters most for the task,
the misread each gap risks, the adaptation move that answers it, and the Hofstede
dimensions that differ most. Deterministic arithmetic over research data: the tool
that prevents the 'they said yes but nothing moves' failure.")
  ("host_country" :text :required t
   :doc "the counterpart's country, e.g. Japan; aliases work")
  ("home_country" :string
   :doc "the country you or your user work from; the section's home_country answers when a call names none")
  ("task" :string
   :doc "optional context -- negotiation, first meeting, management, communication, payment, trip; it weights which gaps matter most"))

(nlk:define-contract culture-countries-request
  (:doc "The countries these tools cover, optionally narrowed to a word from a country
name or a region. Ask it instead of guessing whether a country is covered.")
  ("query" :string
   :doc "a word to filter by, e.g. asia, europe, united; empty answers every country covered"))

;;; --- the tool bodies --------------------------------------------------------------

(defun brief-tool (request)
  "The culture_brief tool: one country's brief, the task hint included."
  (brief (culture-brief-request-country request)
         :task (culture-brief-request-task request)))

(defun gap-tool (request)
  "The culture_gap_analysis tool: the ranked gaps between home and host."
  (gap-analysis (culture-gap-request-host-country request)
                :home (culture-gap-request-home-country request)
                :task (culture-gap-request-task request)))

(defun countries-tool (request)
  "The culture_countries tool: the coverage list, narrowed by the query."
  (countries (culture-countries-request-query request)))

;;; --- the words the model reads ----------------------------------------------------

(defparameter *primer*
  "Business culture is available through the nodecode-culture cell: bundled research
data on 57 economies -- Meyer's eight Culture Map scales, Hofstede's six
dimensions, GLOBE clusters, the Lewis type and practitioner field notes -- as three
model tools whose answers are arithmetic over that data, never a guess, and the same
calls in the CULTURE package from eval.
  culture_brief {country, task}
      How trust is built there, how decisions are really made, contracts, time,
      communication, etiquette, taboos, the country's own highlights, and a task
      hint when a task is named. Reach for it before a cross-border negotiation, a
      first meeting, a management action or an email to foreign counterparts.
  culture_gap_analysis {host_country, home_country, task}
      The eight Meyer scale gaps between your country and theirs, ranked biggest
      first, each with the misread it risks and the move that answers it, plus the
      Hofstede dimensions that differ most. Call it whenever you or your user will
      act across cultures; it is what keeps a polite yes from being read as
      agreement.
  culture_countries {query}
      What is covered. Ask rather than guess, and say so when a country is not.
Doctrine. These are directional positions distilled from published research, not
measurements of an individual: name the framework when a brief drives a decision,
and calibrate to the person in front of you. Country data does not excuse a
stereotype, and a homogenized one -- ASEAN is not one culture, nor Latin America,
nor the Gulf -- is the mistake the highlights exist to prevent. A gap is between
two cultures, so a gap analysis wants both ends: pass home_country, or set
culture.home_country once and every call after it knows.
ERROR: CULTURE-ERROR names the problem."
  "What (help :culture) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell culture
  (:section ("culture")
    (:guide "nothing here is required; home_country is the country you work from, used when a gap analysis names no home_country; max_gaps and max_highlights bound how long an answer runs")
    ("home_country" :string :default ""
                    :doc "the country you work from, e.g. Germany; culture_gap_analysis uses it when a call names no home_country")
    ("max_gaps" :integer :default 5 :min 1
                :doc "how many ranked gaps a gap analysis answers, biggest first")
    ("max_highlights" :integer :default 5 :min 1
                      :doc "how many highlights, etiquette points and taboos a brief carries")
    ("include_hofstede" :boolean :default t
                        :doc "whether a brief carries Hofstede's six dimension numbers"))
  (:tool "culture_brief" (nlk:contract 'culture-brief-request) 'brief-tool)
  (:tool "culture_gap_analysis" (nlk:contract 'culture-gap-request) 'gap-tool)
  (:tool "culture_countries" (nlk:contract 'culture-countries-request) 'countries-tool)
  (:command "culture" 'run-slash
            :description "Business-culture brief for a country"
            :argument-hint "COUNTRY [TASK]")
  (:help :culture "culture: a country's business-culture brief, and the gap to yours - (culture:brief \"Japan\")" *primer*))
