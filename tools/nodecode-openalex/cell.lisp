;;;; cell.lisp --- the primer, the section, the /openalex command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original was ten registry tools, two skills, a CLI and a slash command.
;;;; The tools are tools.lisp, the two skills are +PRIMER+ -- which the model
;;;; reads with (help :openalex), and whose one line rides every request -- and
;;;; the command is /openalex below. Nothing hooks the provider path: no model
;;;; is served here.
;;;;
;;;; A key is optional. Without one every call runs on OpenAlex's anonymous
;;;; tier, which works; with one the account tool reads the live allowance from
;;;; the API and adopts the price table the API reports.

(in-package #:nodecode-openalex)

;;; --- the skills, in one topic -----------------------------------------------------

(defparameter +primer+
  "OpenAlex indexes about 250 million scholarly works, with their authors, venues, topics
and citation graph, through one REST API that bills by call shape. The nodecode-openalex cell
puts it behind ten tools, and the price of a call is known before it is sent.
FREE, and they keep working when the budget is gone:
  openalex_resolve    a name to an id. Use it first: searching an author or a journal by name
                      is expensive and imprecise, while filtering on the resolved id is free
                      and exact. Answers id, hint, work count and the filter key to use next.
  openalex_get        records by id, DOI, ORCID, ROR, ISSN or PMID, up to 20 at once.
                      Several id lookups are free; one search that returns them is not.
  openalex_account    the allowance, the price table and what this run has spent.
  openalex_fields     the valid filter and select names, from disk. Ask rather than guess: a
                      rejected query still costs a list call.
CHEAP, $0.0001:
  openalex_count      how many, and how they split: group_by by year, type, institution,
                      journal, country or open-access status. It answers most real questions.
  openalex_related    the citation graph from one work: cited_by, references, related.
THE REST, when the records themselves are wanted:
  openalex_search     $0.001. Ask first whether a count answers it. On works the plain
                      search parameter is FULL TEXT, far broader than most people expect;
                      set search_field=title_and_abstract for bibliographic search.
  openalex_harvest    cursor-paged harvest, $0.0001 a page, checked against the budget first.
  openalex_classify   $0.01 a call, off unless the section allows it. Only for text that is
                      not in OpenAlex; a work with a DOI already has its topics, for free.
  openalex_fulltext   free to ask whether OpenAlex holds the full text, $0.01 to download it
                      on confirm=true. Roughly 54 million works have content, so absence is
                      normal, and a PDF comes back as a url rather than as bytes.
How to work: resolve before searching by name, count before searching for records, and fetch by
id rather than searching for a record that already has one. A filter is the precise tool: a
comma is AND, a pipe is OR within one field, ! negates, a range is 2020-2024 or
cited_by_count:>100, and a sort is field:desc (the -field form the API documents 400s).
Citation counts favour older work and coverage is uneven outside Crossref-indexed journals, so
say what the data is rather than what it suggests. Id lookups cost nothing, so the operator can
ask by hand: /openalex <id|doi|name|budget>."
  "What (help :openalex) answers while the cell runs: the original's ten tool descriptions
and its two skills in one topic.")

(defparameter +usage+
  "**/openalex**: scholarly lookup
```
/openalex W2741809807             fetch a work by id          free
/openalex doi:10.7717/peerj.4375  fetch by DOI                free
/openalex bengio                  resolve a name to an id     free
/openalex budget                  this run's spend, prices    free
```
Counts and searches cost money, so ask for those in the conversation and the model picks the
cheapest call that answers the question.")

;;; --- the section -------------------------------------------------------------------

(defun parse-budget (text)
  "TEXT as dollars, written either as a JSON number or as a string. A value that is not a
non-negative number refuses, naming the member the operator can fix."
  (let* ((raw (if (stringp text) (nlk:trimmed text) text))
         (value (typecase raw
                  (real (coerce raw 'double-float))
                  (string (handler-case
                              (multiple-value-bind (number end)
                                  (read-from-string raw nil nil)
                                (and number (eql end (length raw))
                                     (realp number) (coerce number 'double-float)))
                            (error () nil)))
                  (t nil))))
    (unless (and value (not (minusp value)))
      (error 'nlk:config-refusal
             :detail (format nil "openalex: budget_usd is ~s, which is not an amount of ~
dollars; write it as \"0.05\" for five cents, or 0 for free calls only" text)))
    value))

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's values, with the key resolved out of
its variable and the budget parsed out of its string."
  (declare (ignore table))
  (list :api-key (nonempty (getf values :api-key))
        :api-key-env (getf values :api-key-env)
        :base-url (getf values :base-url)
        :verbosity (getf values :verbosity)
        :budget-usd (parse-budget (getf values :budget-usd))
        :per-page (getf values :per-page)
        :allow-text-classification (getf values :allow-text-classification)
        :timeout-seconds (getf values :timeout-seconds)
        :max-result-chars (getf values :max-result-chars)))

(defun check-config ()
  "Refuse a section whose base address is not an address, and say once that no key is set:
the anonymous tier is a fact worth knowing before a call is refused rather than after."
  (let ((base (setting :base-url)))
    (when (and (nonempty base)
               (not (or (eql 0 (search "https://" base))
                        (eql 0 (search "http://" base)))))
      (fail "openalex: base_url must be an http or https address, not ~s" base)))
  (unless (api-key)
    (nle:notice (format nil "openalex: no key, so every call runs on OpenAlex's anonymous ~
tier ($~,2f a day); a free key from openalex.org/settings/api raises it to $1.00 a day"
                        +anon-daily-usd+)
                :level :info)))

;;; --- the operator's command ----------------------------------------------------------

(defun identifier-p (text)
  "Whether TEXT is something to fetch rather than a name to resolve: an OpenAlex id, an
external id with its prefix, a bare DOI, or the url form of any of them."
  (let ((text (nlk:trimmed text)))
    (or (and (> (length text) 1)
             (digit-char-p (char text 1))
             (find (char-upcase (char text 0)) "WASITCPFK"))
        (some (lambda (prefix) (eql 0 (search prefix (string-downcase text))))
              '("openalex:" "doi:" "pmid:" "pmcid:" "mag:" "orcid:" "ror:" "issn:"
                "http://" "https://"))
        (and (eql 0 (search "10." text)) (find #\/ text)))))

(defun status-text ()
  "What the cell is pointed at, without ever printing the key."
  (format nil "openalex: ~a, key ~a, verbosity ~a; $~,4f of $~,2f spent this run~%~
Resolve a name or fetch an id from here: /openalex <name|id|doi>."
          (root) (if (api-key) "set" "MISSING") (setting :verbosity)
          (ledger-spent *ledger*) (budget-usd)))

(defun run-command (args session-id)
  "One /openalex invocation: an id fetches, a name resolves, budget reports. Free either
way -- the two costly tools are left to the conversation, where the model picks them."
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (cond
      ((zerop (length argument)) +usage+)
      ((member (string-downcase argument) '("help" "-h" "--help" "?") :test #'equal)
       +usage+)
      ((member (string-downcase argument) '("budget" "cost" "account" "status") :test #'equal)
       (tool-account nil))
      ((identifier-p argument)
       (tool-get (decode-get-call (nlk:json-object "id" argument "verbosity" "detail"))))
      (t (tool-resolve (decode-resolve-call (nlk:json-object "query" argument)))))))

;;; --- the cell ------------------------------------------------------------------------

(nle:define-cell openalex
  (:section ("openalex")
    (:guide "a key is optional: without one every call runs on the anonymous tier. Make a free key at openalex.org/settings/api and paste it in api_key, or export OPENALEX_API_KEY (or name the variable in api_key_env). budget_usd is this cell's ceiling and the free calls keep working past it; next step: /openalex budget shows the allowance and the prices")
    ("api_key" :secret
     :doc "the OpenAlex API key, sent as a bearer token")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (OPENALEX_API_KEY is read either way)")
    ("base_url" :string
     :doc "the API address: https://api.openalex.org unless you go through a proxy of your own")
    ("verbosity" :choice :options '("summary" "detail" "raw") :default "summary"
     :doc "how much of each record a tool answers: summary, detail or raw")
    ("budget_usd" :string :default "0.05"
     :doc "this cell's spend ceiling in dollars, written as a string: 0.05 is five cents, 0 is free calls only")
    ("per_page" :integer :default 25 :min 1
     :doc "how many records a search or a graph walk returns unless the call names its own count")
    ("allow_text_classification" :boolean :default nil
     :doc "allow openalex_classify, which costs $0.01 a call")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one request's ceiling")
    ("max_result_chars" :integer :default 24000 :min 500
     :doc "how much text one tool answer carries before it is cut"))
  (:settings #'read-settings)
  (:help :openalex
         "openalex: 250 million scholarly works, their authors, venues and citation graph - (help :openalex)"
         +primer+)
  (:command "openalex" #'run-command
            :description "OpenAlex: fetch an id, resolve a name, or say what is left of the budget"
            :session nil)
  (:tool "openalex_resolve" (nlk:contract 'resolve-call) #'tool-resolve)
  (:tool "openalex_get" (nlk:contract 'get-call) #'tool-get)
  (:tool "openalex_count" (nlk:contract 'count-call) #'tool-count)
  (:tool "openalex_search" (nlk:contract 'search-call) #'tool-search)
  (:tool "openalex_related" (nlk:contract 'related-call) #'tool-related)
  (:tool "openalex_account" +account-schema+ #'tool-account)
  (:tool "openalex_fields" (nlk:contract 'fields-call) #'tool-fields)
  (:tool "openalex_classify" (nlk:contract 'classify-call) #'tool-classify)
  (:tool "openalex_harvest" (nlk:contract 'harvest-call) #'tool-harvest)
  (:tool "openalex_fulltext" (nlk:contract 'fulltext-call) #'tool-fulltext)
  (:start (lambda ()
            (setf *ledger* (make-ledger))
            (check-config))))
