;;;; verbs.lisp --- the server's six tools as six Lisp verbs, and /dyscalculia.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original offered six read-only MCP tools. Each is one verb here,
;;;; answering the server's own text -- the figures, the citations, the chart
;;;; rows exactly as dyscalculia.io publishes them -- and the model calls the
;;;; same six names as tools. Nothing is scored, summarised or invented here.

(in-package #:nodecode-dyscalculia)

;;; --- the six ------------------------------------------------------------------------

(define-verb get-prevalence (&key study age min-pct max-pct)
  "Published dyscalculia prevalence figures, one per study or guideline, with
sample size, year, age and the reference key. STUDY is one entry id (e.g.
\"uk-morsanyi-2018\"), AGE a whole year that the entry's stated age range must
include, MIN-PCT and MAX-PCT a percent window. Filters combine; with none,
all entries come back."
  (call-tool "get_prevalence"
              (arguments "study" study "age" age "min_pct" min-pct "max_pct" max-pct)))

(define-verb list-studies (&key topic)
  "The sources the statistics page cites: key, citation, DOI or PMID, link,
kind and the topics each supports. TOPIC narrows to one section of the page
(prevalence, persistence, math-anxiety, interventions, by-country, sex-ratio,
comorbidity, key-figures, family, official-counts, method)."
  (call-tool "list_studies" (arguments "topic" topic)))

(define-verb get-reference (key)
  "One reference's full record by KEY (e.g. \"shalev2000\"): citation, DOI
and PMID where the site prints them, url, kind, the topics that cite it and
the prevalence entries built from it."
  (unless (and (stringp key) (plusp (length (nlk:trimmed key))))
    (fail "a reference key is required, e.g. shalev2000"))
  (call-tool "get_reference" (arguments "key" (nlk:trimmed key))))

(define-verb list-chart-data (&key id)
  "The chart and table datasets behind the statistics page, with their CSV,
JSON and SVG addresses; ID (chart-1, chart-6, chart-8, chart-9, table-2,
table-4, table-5) also answers that one dataset's rows."
  (call-tool "list_chart_data" (arguments "id" id)))

(define-verb explain-screening-test (&key game include-items)
  "The five number-skills screening games, what each measures and how they
are scored. GAME narrows to one key; INCLUDE-ITEMS adds each game's item
set. A screening, not a diagnosis: it scores no one."
  (call-tool "explain_screening_test"
              (arguments "game" game "include_items" (and include-items t))))

(define-verb get-citation ()
  "How to cite and reuse the statistics compilation: the licence, the citeAs
text, the credit link, the download addresses and the site's own limits."
  (call-tool "get_citation" (arguments)))

;;; --- the tools the model calls ----------------------------------------------------

(defun prevalence-tool (request)
  "The get_prevalence tool."
  (get-prevalence :study (prevalence-query-study request)
                  :age (prevalence-query-age request)
                  :min-pct (prevalence-query-min-pct request)
                  :max-pct (prevalence-query-max-pct request)))

(defun studies-tool (request)
  "The list_studies tool."
  (list-studies :topic (study-query-topic request)))

(defun reference-tool (request)
  "The get_reference tool."
  (get-reference (reference-query-key request)))

(defun chart-tool (request)
  "The list_chart_data tool."
  (list-chart-data :id (chart-query-id request)))

(defun screening-tool (request)
  "The explain_screening_test tool."
  (explain-screening-test :game (screening-query-game request)
                          :include-items (screening-query-include-items request)))

(defun citation-tool (request)
  "The get_citation tool: it takes no arguments."
  (declare (ignore request))
  (get-citation))

;;; --- /dyscalculia -----------------------------------------------------------------

(defparameter +usage+
  "/dyscalculia prevalence [AGE] | studies [TOPIC] | reference KEY | charts [ID] | test [GAME] | citation"
  "What /dyscalculia answers for anything it does not know.")

(defun age-argument (text)
  "TEXT, an age typed after /dyscalculia prevalence, as a whole number, or NIL."
  (and text (plusp (length text))
       (every #'digit-char-p text)
       (parse-integer text)))

(defun run-slash (args session-id)
  "/dyscalculia: the same six answers, typed by the operator."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (c) (member c '(#\Space #\Tab))) text))
         (head (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond
      ((member head '("" "help" "?") :test #'string=)
       (values +usage+
               (nle:list-dialog
                :title "Dyscalculia"
                :summary "Cited figures and references from dyscalculia.io, read through its hosted server."
                :rows (list (nle:run-row "How common is it, by study" "/dyscalculia prevalence"
                                          :detail "prevalence entries with study, age and sample size")
                            (nle:run-row "The sources behind a figure" "/dyscalculia studies"
                                          :detail "key, citation, DOI or PMID per source")
                            (nle:run-row "One full reference" "/dyscalculia reference shalev2000"
                                          :detail "replace the key with any of the site's")
                            (nle:run-row "Charts and tables" "/dyscalculia charts"
                                           :detail "persistence, maths anxiety, interventions, by-country")
                            (nle:run-row "What the screening test measures" "/dyscalculia test"
                                          :detail "five number-skills games, and the site's own limits")
                            (nle:run-row "How to cite the data" "/dyscalculia citation"
                                          :detail "CC BY 4.0 compilation, credit link")))))
      ((string= head "prevalence") (get-prevalence :age (age-argument rest)))
      ((string= head "studies") (list-studies :topic (and (plusp (length rest)) rest)))
      ((string= head "reference")
       (if (plusp (length rest)) (get-reference rest)
           "dyscalculia: /dyscalculia reference KEY, e.g. /dyscalculia reference shalev2000"))
      ((string= head "charts") (list-chart-data :id (and (plusp (length rest)) rest)))
      ((string= head "test") (explain-screening-test :game (and (plusp (length rest)) rest)))
      ((string= head "citation") (get-citation))
      (t +usage+))))
