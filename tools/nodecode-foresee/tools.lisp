;;;; tools.lisp --- the two tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin reached two tools on Foresee's hosted MCP server:
;;;; quote_insurance (instant estimates) and live_carrier_quotes (Foresee's
;;;; agents walk the carriers' own quote forms and read the page premium
;;;; back). The names are the server's, unchanged, and so are the argument
;;;; names; the transport is what changed (see NOTICE). Each tool is one
;;;; NLK:DEFINE-CONTRACT, so the shape the model is shown and the shape the
;;;; body reads are one declaration.
;;;;
;;;; PROFILE, LINES and IDENTITY are handed to the server as the objects the
;;;; model wrote: Foresee's own schema is the authority on their members, and
;;;; the skills that describe it are the manual topic, (help :foresee).

(in-package #:nodecode-foresee)

(defparameter +profile-doc+
  "the rating facts, in Foresee's own field names: core zip_code and age (or dob); for auto, auto.vehicles[] with each year, make and model, and auto.drivers[] with accidents and violations as lists (an empty list is a clean record); property for home or renters (year_built, construction_type, roof_type, replacement_cost). Optional facts sharpen the estimate: marital_status, gender, credit_range, home_ownership_status, prior_insurance, companion_policies, military_affiliation.")

(defparameter +lines-doc+
  "one key per line to price (auto, home, renters), each that line's coverage options as actual numbers -- nothing is priced at a server default. auto: bi (\"100/300\"), pd (100), coll_deductible, comp_deductible, optional um and medpay. home: coverage_a, coverage_e, coverage_f, aop_deductible (the carrier's own package comes back under sel as ratios of A). renters: coverage_c, coverage_e, coverage_f, deductible, optional coverage_d. Several keys at once price as a bundle.")

;;; --- quote_insurance ---------------------------------------------------------

(nlk:define-contract quote-insurance-call
  (:doc "Instant insurance quote estimates from every carrier Foresee prices: a monthly point
estimate and confidence interval per carrier, the cost of each coverage, a price ladder per lever,
and the carrier's own quote url. Estimates only - Foresee does not bind or take payment; the
purchase finishes on the carrier's site.")
  ("profile" :object :required t :doc +profile-doc+)
  ("lines" :object :required t :doc +lines-doc+)
  ("carriers" :array :of :string
   :doc "only these carriers, by key (geico, progressive, statefarm, allstate, libertymutual, mercury, farmers, travelers, usaa); every carrier Foresee prices when omitted"))

;;; --- live_carrier_quotes -----------------------------------------------------

(nlk:define-contract live-carrier-quotes-call
  (:doc "Foresee's agents complete the carriers' own online quote flows and read back the
premium the page printed, so an estimate is confirmed with proof it holds up on the carrier's
site. Commission it once, then call again with the same arguments to collect; a carrier that
declined is an answer, not an error. Ask the user first and pass their go-ahead verbatim:
the carriers may make a soft credit-based insurance score check and may email or phone.")
  ("profile" :object :required t :doc +profile-doc+)
  ("lines" :object :required t
   :doc "one key per line to walk (auto, home, renters), each that line's ask, or null: auto is required (bi, pd, coll_deductible, comp_deductible); home takes null, because the carrier's own form prices its package; renters is required (coverage_c, coverage_e, coverage_f, deductible). Several keys commission bundled walks.")
  ("user_authorization" :text :required t
   :doc "the user's in-chat go-ahead, verbatim, as they said it (for example \"yes, go ahead\"); it is stored as the consent record before any carrier sees the details")
  ("identity" :object :slot identity-facts
   :doc "the person the quotes are for, usually required by the carriers: first_name, last_name, dob, street, city, email (the carriers send the quote there); optional unit, phone, and zip_code only when it differs from the profile's. Never an SSN. A driver's licence number goes on that driver's auto.drivers[].drivers_license_number, and only when a carrier's needs entry asks for it.")
  ("carriers" :array :of :string
   :doc "only these carriers, by key; every carrier with a validated walk when omitted"))

;;; --- what the model reads back -------------------------------------------------

(defun arguments (&rest pairs)
  "A JSON object from PAIRS (KEY VALUE ...), leaving out every NIL VALUE: a
call carries only what the model asked for."
  (loop for (key value) on pairs by #'cddr
        when value append (list key value) into kept
        finally (return (apply #'nlk:make-json-object kept))))

(defun items (vector)
  "VECTOR -- a contract's array -- when it has something in it, else NIL."
  (and vector (plusp (length vector)) vector))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) "")
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters]"
                   (subseq text 0 limit) (- (length text) limit)))))

(defun answer-text (result)
  "RESULT -- a tools/call answer -- as the text the model reads: its text
content, else the structured content it carries instead."
  (let ((text (content-text result))
        (structured (gethash "structuredContent" result)))
    (cond ((plusp (length text)) text)
          ((hash-table-p structured) (nlk:encode-json-object structured))
          (t "(the server answered nothing)"))))

(defun tool-answer (name arguments)
  "tools/call NAME and answer what the model reads, a refusal included."
  (let* ((limit (setting :max-chars))
         (result (call-tool name arguments))
         (text (clip (answer-text result) limit)))
    (if (gethash "isError" result)
        (format nil "ERROR: ~a" text)
        text)))

(defun tool-quote (call)
  "quote_insurance: Foresee's instant estimates."
  (tool-answer "quote_insurance"
                (arguments "profile" (quote-insurance-call-profile call)
                           "lines" (quote-insurance-call-lines call)
                           "carriers" (items (quote-insurance-call-carriers call)))))

(defun tool-live-quotes (call)
  "live_carrier_quotes: Foresee's agents on the carriers' own forms."
  (tool-answer "live_carrier_quotes"
                (arguments "profile" (live-carrier-quotes-call-profile call)
                           "lines" (live-carrier-quotes-call-lines call)
                           "user_authorization" (live-carrier-quotes-call-user-authorization call)
                           "identity" (live-carrier-quotes-call-identity-facts call)
                           "carriers" (items (live-carrier-quotes-call-carriers call)))))
