;;;; cell-test.lisp --- lifecycle, the three tools, /culture, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and the arithmetic over its
;;;; bundled data: that a name resolves through exact, alias, containment and
;;;; close-spelling alike, that a brief carries its country's scales and its
;;;; region's playbook, that a gap analysis ranks the biggest weighted gap
;;;; first and reads the section's home country when a call names none, that
;;;; max_gaps and include_hofstede change the answer, and that every
;;;; unanswered question names itself. No network and no clock: the answers
;;;; are the same text every run.

(in-package #:nodecode.test)

(define-test-slice "culture" "CULTURE-CELL-" :start nodecode-culture:start-cell)

(define-cell-lifecycle-tests "culture"
  (:help :culture)
  (:command "culture")
  (:running
   (is (nle::find-tool "culture_brief") "culture_brief is registered")
   (is (nle::find-tool "culture_gap_analysis") "culture_gap_analysis is registered")
   (is (nle::find-tool "culture_countries") "culture_countries is registered")
   (is (= 5 (nodecode-culture::setting :max-gaps)) "the five biggest gaps by default")
   (is (eq t (nodecode-culture::setting :include-hofstede)) "Hofstede numbers are on by default"))
  (:stopped
   (is (null (nle::find-tool "culture_brief")) "stop unregisters culture_brief")
   (is (null (nle::find-tool "culture_countries")) "stop unregisters culture_countries"))
  (:idle nodecode-culture:culture-error
         (nodecode-culture:brief "Japan")
         (nodecode-culture:gap-analysis "Japan" :home "Germany"))
  (:refused
   ("max_gaps" 0)
   ("max_highlights" 0)))

;;; --- the fixture --------------------------------------------------------------

(defmacro with-culture ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS as its section, stopped on unwind."
  `(with-cell-stop ((culture-start ,@pairs))
     ,@body))

(defun culture-tool (name arguments)
  "NAME called through the wire, as a provider sends it, with ARGUMENTS a JSON
object string; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- a brief --------------------------------------------------------------------

(deftest culture-cell-a-brief-carries-the-country-and-its-scales ()
  (with-culture ()
    (let ((text (culture-tool "culture_brief" "{\"country\":\"Japan\"}")))
      (is (search "Japan -- East Asia" text) text)
      (is (search "Lewis: reactive" text))
      (is (search "Communicating 90 (high-context" text) "the Meyer position, with the pole it leans to")
      (is (search "Power Distance 54" text) "the Hofstede numbers")
      (is (search "Trust:" text))
      (is (search "Country highlights:" text))
      (is (search "Taboos:" text)))))

(deftest culture-cell-a-task-hint-rides-a-brief ()
  (with-culture ()
    (let ((text (culture-tool "culture_brief" "{\"country\":\"Japan\",\"task\":\"negotiation\"}")))
      (is (search "Task hint (negotiation):" text) text)
      (is (search "Anchor patiently" text) "the negotiation paragraph"))
    (let ((text (culture-tool "culture_brief" "{\"country\":\"Japan\",\"task\":\"an email to their boss\"}")))
      (is (search "Task hint (communication):" text) "a word from a family picks the family"))))

(deftest culture-cell-a-name-resolves-through-alias-and-spelling ()
  (with-culture ()
    (is (search "Netherlands --" (culture-tool "culture_brief" "{\"country\":\"Holland\"}")) "an alias resolves")
    (is (search "Japan --" (culture-tool "culture_brief" "{\"country\":\"Japon\"}")) "a close spelling resolves")
    (is (search "Singapore --" (culture-tool "culture_brief" "{\"country\":\"Singa\"}")) "a containment resolves")))

(deftest culture-cell-a-country-not-covered-answers-with-suggestions ()
  (with-culture ()
    (let ((text (culture-tool "culture_brief" "{\"country\":\"Atlantis\"}")))
      (is (search "Not found: Atlantis" text) text)
      (is (search "Try: " text) "and what to try instead"))))

(deftest culture-cell-hofstede-numbers-can-be-turned-off ()
  (with-culture ("include_hofstede" nil)
    (let ((text (culture-tool "culture_brief" "{\"country\":\"Japan\"}")))
      (is (search "Communicating 90" text) "the Meyer scales stay")
      (is (null (search "Hofstede:" text)) "and the Hofstede line is gone"))))

;;; --- the gaps ------------------------------------------------------------------

(deftest culture-cell-the-biggest-weighted-gap-is-ranked-first ()
  (with-culture ()
    (let ((text (culture-tool "culture_gap_analysis"
                               "{\"home_country\":\"United States\",\"host_country\":\"Japan\",\"task\":\"negotiation\"}")))
      (is (search "Culture gap: United States -> Japan (negotiation)" text) text)
      (is (search "1. Trusting 25 -> 70 (host higher" text) text)
      (is (search "Risk: Host builds trust through meals" text) "the misread it risks")
      (is (search "Adapt: Invest relationship time" text) "and the move that answers it")
      (is (search "Hofstede flags: " text)))))

(deftest culture-cell-the-section-s-home-country-answers-when-a-call-names-none ()
  (with-culture ("home_country" "United States")
    (let ((text (culture-tool "culture_gap_analysis" "{\"host_country\":\"Japan\"}")))
      (is (search "Culture gap: United States -> Japan (general)" text) text))
    (let ((text (culture-tool "culture_gap_analysis"
                               "{\"host_country\":\"Japan\",\"home_country\":\"Germany\"}")))
      (is (search "Germany -> Japan" text) "a call's own home country wins"))))

(deftest culture-cell-max-gaps-bounds-the-ranked-list ()
  (with-culture ("max_gaps" 2)
    (let ((text (culture-tool "culture_gap_analysis"
                               "{\"home_country\":\"United States\",\"host_country\":\"Japan\"}")))
      (is (search "Ranked gaps (2 of 8" text) text)
      (is (null (search "3. " text)) "the third gap is cut"))))

(deftest culture-cell-a-gap-analysis-that-cannot-run-says-why ()
  (with-culture ()
    (let ((text (culture-tool "culture_gap_analysis" "{\"host_country\":\"Japan\"}")))
      (is (search "ERROR: CULTURE-ERROR" text) text)
      (is (search "needs a home country" text) "and asks for the one thing it needs"))
    (let ((text (culture-tool "culture_gap_analysis"
                               "{\"home_country\":\"Japan\",\"host_country\":\"Japan\"}")))
      (is (search "the same country" text) "one country is no gap"))))

;;; --- the coverage list and the command -------------------------------------------

(deftest culture-cell-the-coverage-list-counts-and-filters ()
  (with-culture ()
    (let ((text (culture-tool "culture_countries" "{}")))
      (is (search "57 economies covered, 57 matched" text) text))
    (let ((text (culture-tool "culture_countries" "{\"query\":\"asia\"}")))
      (is (search "Japan" text) text)
      (is (null (search "Germany" text)) "a query narrows the list"))))

(deftest culture-cell-the-command-answers-what-the-tool-answers ()
  (with-culture ()
    (let ((text (cell-entry "nodecode-culture" "culture" "Japan negotiation" "s1")))
      (is (search "Japan -- East Asia" text) text)
      (is (search "Task hint (negotiation):" text) "the task is the second word"))
    (let ((text (cell-entry "nodecode-culture" "culture" "" "s1")))
      (is (search "culture: type a country" text) "typed alone, it says how to use it"))))
