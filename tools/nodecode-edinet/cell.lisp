;;;; cell.lisp --- the primer, the section, the three tools, /edinet.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin shipped three tools and a skill that said to reach for them
;;;; before a web search. The tools are registered here under their own
;;;; names; the skill is +PRIMER+, which the model reads with
;;;; (help :edinet) and whose one line rides every request. Nothing hooks the
;;;; provider path: no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "edinet": {"api_key": "...", "base_url": "https://api.edinet-fsa.go.jp/api/v2",
;;;;              "timeout_seconds": 30, "max_calendar_days": 31, "pace_seconds": 1}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-edinet)

(defparameter +primer+
  "Japan's corporate filings are available through the nodecode-edinet cell: three tools over
EDINET API v2, the Financial Services Agency's disclosure system, which carries every listed
company's 有価証券報告書 (annual securities report) and the rest of its disclosures.
  edinet_search_filings   find filings by company_name, sec_code (7203 or 72030),
                          edinet_code (E02144) or corporate_number over date_from/date_to,
                          the FILE dates, not the fiscal period; doc_types narrows the kind
                          (yuho, quarterly, semiannual, extraordinary, amendment). Answers one
                          line per filing with its doc id and what it carries.
  edinet_financials       one filing's headline figures from its CSV: revenue, operating
                          income, net income, total assets, net assets, operating cash flow,
                          with the unit and the accounting standard (Japan GAAP, IFRS, US GAAP).
  edinet_save_document    save the PDF (type 2), the submission ZIP (type 1) or the CSV ZIP
                          (type 5) under the Nodecode home.
Ask for a company's latest annual report by its securities code, not its name, and give a
file-date range around when it is filed (Japanese annual reports cluster in late June). A date
range longer than max_calendar_days is refused, not shortened: step forward a month at a time.
A figure edinet_financials reports missing was not in a consolidated current-year CSV row; the
PDF holds it, so save that and read it. Cite what you answer from: every result ends with the
EDINET filing page and the PDL 1.0 licence EDINET's terms ask for. The operator can run one
search from the keyboard: /edinet 7203 [YYYY-MM-DD [YYYY-MM-DD]]."
  "What (help :edinet) answers while the cell runs: the plugin's skill, in one topic.")

(defun status-text ()
  "What this cell is connected to, without ever printing the key."
  (format nil "edinet: key ~a; ~a; file dates are Japan time, today ~a~%~
Search with /edinet 7203 [YYYY-MM-DD [YYYY-MM-DD]] — a securities code or a company name, ~
then the file dates."
          (if (api-key) "set" "MISSING (set \"api_key\" in the edinet section, or EDINET_API_KEY)")
          (base-url)
          (token-day (today-jst))))

(defun date-word-p (word)
  "Whether WORD is written as a YYYY-MM-DD file date."
  (and (stringp word)
       (= 10 (length word))
       (char= (char word 4) #\-)
       (char= (char word 7) #\-)
       (every #'digit-char-p (remove #\- word))))

(defun search-pairs (words)
  "SEARCH-CALL pairs for the command's WORDS: a company or a code, then up to two file dates."
  (let* ((query (first words))
         (dates (remove-if-not #'date-word-p (rest words)))
         (today (token-day (today-jst)))
         (from (or (first dates) today))
         (to (or (second dates) from)))
    (append (if (every #'digit-char-p query)
                (list "sec_code" query)
                (list "company_name" query))
            (list "date_from" from "date_to" to))))

(defun words-of (text)
  "TEXT split on whitespace, empty pieces dropped."
  (remove "" (split-fields (or text "") #\Space) :test #'string=))

(defun run-command (args session-id)
  "One /edinet invocation: alone, what the cell is connected to; else a search."
  (declare (ignore session-id))
  (let ((words (words-of args)))
    (if (null words)
        (status-text)
        (search-filings (decode-search-call
                         (apply #'nlk:make-json-object (search-pairs words)))))))

(nle:define-cell edinet
  (:section ("edinet")
    (:guide "get a free EDINET API key from FSA's EDINET API materials page (WEEK0060), then paste it in api_key, or export EDINET_API_KEY and name it in api_key_env; next step: /edinet shows the connection")
    ("api_key" :secret
     :doc "the EDINET API key, free from FSA's EDINET API materials page")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (EDINET_API_KEY is read either way)")
    ("base_url" :string
     :doc "the API base: https://api.edinet-fsa.go.jp/api/v2 unless you point it elsewhere")
    ("timeout_seconds" :integer :default 30 :min 5
     :doc "one file-date list or one document's ceiling, in seconds")
    ("max_calendar_days" :integer :default 31 :min 1
     :doc "the widest file-date range one search may cover, 31 at most: EDINET's list API is one day per request")
    ("pace_seconds" :integer :default 1 :min 0
     :doc "the gap between two list requests, the second EDINET's terms ask for"))
  (:help :edinet
         "edinet: Japan's EDINET corporate filings, searchable, with headline financials and documents to save - (help :edinet)"
         +primer+)
  (:command "edinet" 'run-command
            :description "EDINET filings: one search, or the connection"
            :argument-hint "[CODE-OR-NAME] [DATE_FROM] [DATE_TO]"
            :session nil)
  (:tool "edinet_search_filings" (nlk:contract 'search-call) #'search-filings)
  (:tool "edinet_financials" (nlk:contract 'financial-call) #'financials)
  (:tool "edinet_save_document" (nlk:contract 'save-call) #'save-document)
  (:start (lambda ()
            (check-config))))
