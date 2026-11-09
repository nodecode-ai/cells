;;;; tools.lisp --- the ten tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's ten tools, with its names and its argument names
;;;; unchanged (see NOTICE). Each is one NLK:DEFINE-CONTRACT, so the shape the
;;;; model is shown and the shape the body reads are one declaration.
;;;;
;;;; The descriptions do one job above all others: route the model to the
;;;; cheapest call that answers the question. A lookup by id is free, a list
;;;; call is $0.0001, a search ten times that, a text classification or a
;;;; full-text download a hundred times that, and the anonymous daily
;;;; allowance is $0.10. Each answer therefore carries what it cost.

(in-package #:nodecode-openalex)

;;; --- helpers -------------------------------------------------------------------

(defun entity-for (given)
  "GIVEN as one of the collections, and works when it is not one."
  (let ((entity (and (stringp given) (string-downcase (nlk:trimmed given)))))
    (if (member entity +entities+ :test #'equal) entity "works")))

(defun verbosity-for (given)
  "The verbosity the call asked for, else the section's."
  (let ((level (and (stringp given) (string-downcase (nlk:trimmed given)))))
    (if (member level '("summary" "detail" "raw") :test #'equal)
        level
        (setting :verbosity))))

(defun id-list (text)
  "TEXT as at most twenty identifiers, in the order given."
  (let ((parts (remove-if (lambda (part) (zerop (length part)))
                          (loop for start = 0 then (1+ end)
                                for end = (position #\, text :start start)
                                collect (nlk:trimmed (subseq text start end))
                                while end))))
    (subseq parts 0 (min 20 (length parts)))))

(defun plain-length (vector)
  "How many entries a JSON array holds, or zero."
  (if (vectorp vector) (length vector) 0))

(defun cost-line (class)
  "What a call of CLASS cost, and what is left of the ceiling: every answer
says so, because the model's choice of tool is what spends the budget."
  (format nil "cost: ~a~@[ · spent $~,4f of $~,2f this run~]"
          (if (zerop (price class))
              "free"
              (format nil "$~,4f (~a)" (price class) class))
          (ledger-spent *ledger*)
          (budget-usd)))

(defun search-split (search field)
  "The two ways OpenAlex exposes searching, kept apart. Values: the API
params, and a filter fragment. The bare search parameter is FULL TEXT on
works, roughly twice as broad as most callers expect, while the *.search:
filters are how title and abstract searching is asked for."
  (let ((query (and (stringp search) (nlk:trimmed search)))
        (where (if (stringp field) (string-downcase (nlk:trimmed field)) "default")))
    (if (zerop (length (or query "")))
        (values nil nil)
        (cond
          ((member where '("" "default" "fulltext") :test #'equal)
           (values (list (cons "search" query)) nil))
          ((member where '("title" "abstract" "title_and_abstract") :test #'equal)
           ;; Commas and pipes are filter syntax, so they cannot appear raw here.
           (values nil (format nil "~a.search:~a" where
                               (substitute #\Space #\, (substitute #\Space #\| query)))))
          (t (values (list (cons "search" query)) nil))))))

(defun merge-filter (base extra)
  "Two filter expressions as one, comma being AND."
  (let ((parts (remove-if #'null (list (and (stringp base) (nlk:trimmed base))
                                       (and (stringp extra) (nlk:trimmed extra))))))
    (and parts (join parts ","))))

(defun results-of (body)
  "The results array of an answer, as a vector."
  (or (nlk:json-value body :array "results") #()))

(defun meta-count (body)
  "How many records the whole set holds, or NIL."
  (let ((meta (object body "meta")))
    (number-of meta "count")))

;;; --- openalex_resolve (free) ----------------------------------------------------

(nlk:define-contract resolve-call
  (:doc "Turn a name into an OpenAlex id you can filter on. FREE, and it keeps working when
the budget is gone. Use it first, always: searching for an author, journal or institution by
name is expensive and imprecise, while resolving the name to an id and filtering on that id is
free and exact. 'Papers by a named author' should be resolve then filter, never a text search.
Answers the id, a disambiguating hint, the work count, and the filter key to use next.")
  ("query" :text :required t
   :doc "name or partial name, e.g. 'bengio', 'nature neuro', 'max planck'")
  ("entity" :string :options +entities+
   :doc "which kind of thing to resolve; omit to search across all types"))

(defun tool-resolve (call)
  "openalex_resolve: /autocomplete, which is free."
  (let* ((query (nlk:trimmed (resolve-call-query call)))
         (entity (and (resolve-call-entity call)
                      (member (resolve-call-entity call) +entities+ :test #'equal)
                      (resolve-call-entity call)))
         (body (request (if entity (format nil "autocomplete/~a" entity) "autocomplete")
                        (list (cons "q" query))))
         (rows (results-of body)))
    (with-output-to-string (out)
      (if (zerop (plain-length rows))
          (format out "No match for ~s. Try a shorter or differently spelled query.~%" query)
          (format out "~d matches for ~s (free)~%" (plain-length rows) query))
      (loop for row across rows
            when (hash-table-p row)
              do (format out "  ~a  ~a~@[  ~d works~]~@[  ~d citations~]~@[  filter ~a~]~@[  - ~a~]~%"
                         (or (short-id (text-of row "id")) "?")
                         (or (text-of row "display_name") "(unnamed)")
                         (number-of row "works_count")
                         (number-of row "cited_by_count")
                         (text-of row "filter_key")
                         (text-of row "hint")))
      (when (plusp (plain-length rows))
        (format out "~%Filter on the id rather than searching the name: ~
openalex_count with filter='authorships.author.id:<A...>' and group_by='publication_year'. ~
The filter_key above names the filter to use.~%")))))

;;; --- openalex_get (free) ----------------------------------------------------------

(nlk:define-contract get-call
  (:doc "Fetch records by id, DOI, ORCID, ROR, ISSN or PMID. FREE, at any budget. Accepts an
OpenAlex id (W2741809807), a DOI, an ORCID, a ROR, an ISSN, a PMID, or the URL form of any of
them, and normalizes whichever you pass. Because it costs nothing, prefer fetching several
records by id over one search that returns them: ten id lookups are free.")
  ("id" :text :required t
   :doc "the identifier, or a comma-separated list of up to 20")
  ("entity" :string :options +entities+
   :doc "the collection; usually inferable from the id, and works by default")
  ("verbosity" :string :options '("summary" "detail" "raw")
   :doc "'detail' adds the abstract, topics and keywords; 'raw' is everything OpenAlex holds"))

(defun tool-get (call)
  "openalex_get: one singleton fetch per identifier, each free."
  (let ((wanted (id-list (get-call-id call)))
        (fallback (entity-for (get-call-entity call)))
        (verbosity (verbosity-for (get-call-verbosity call)))
        (found '())
        (missing '()))
    (dolist (original wanted)
      (let* ((identifier (normalize-id original))
             (entity (entity-of-id identifier fallback))
             (select (select-for entity verbosity)))
        (handler-case
            (push (list original entity
                        (request (format nil "~a/~a" entity identifier)
                                 (and select (list (cons "select" select)))))
                  found)
          (openalex-not-found ()
            (push original missing)))))
    (with-output-to-string (out)
      (dolist (entry (nreverse found))
        (destructuring-bind (original entity record) entry
          (unless (string= (string-downcase (nlk:trimmed original))
                           (string-downcase (normalize-id original)))
            (format out "queried as ~a~%" original))
          (write-string (if (string= verbosity "raw")
                            (raw-block record)
                            (record-block entity record verbosity))
                        out)))
      (when missing
        (format out "not found: ~a~%Check the identifier's form: an external id needs its ~
prefix (doi:, pmid:, orcid:, ror:, issn:), and a bare DOI finds nothing.~%"
                (join missing ", ")))
      (format out "~a~%" (cost-line "free")))))

;;; --- openalex_count (cheap) --------------------------------------------------------

(nlk:define-contract count-call
  (:doc "Count and group records: a tenth of the price of a search, and roughly five hundred
times less data. COSTS $0.0001 against $0.001 for openalex_search. Reach for it first: how
many papers on a topic, how the count splits by year, which institutions or journals or
countries dominate, how open access breaks down, which topics an author works on. Adding a
group_by is what keeps a query at list price even when a search term rides along.")
  ("entity" :string :options +entities+ :doc "what to count; works by default")
  ("filter" :string
   :doc "filter expression: a comma is AND, a pipe is OR within one field, ! negates, > and < compare, e.g. 'publication_year:>2020,is_oa:true'")
  ("search" :string :slot search-text :doc "free text to combine with the grouping")
  ("group_by" :string
   :doc "field to break the count down by, e.g. publication_year, type, open_access.oa_status, authorships.institutions.country_code; omit for a bare total"))

(defun tool-count (call)
  "openalex_count: one list-priced request, grouped or not."
  (let* ((entity (entity-for (count-call-entity call)))
         (group (and (stringp (count-call-group-by call))
                     (nlk:trimmed (count-call-group-by call))))
         (params nil))
    (multiple-value-bind (search-params search-filter)
        (search-split (count-call-search-text call) "default")
      (let ((filter (merge-filter (count-call-filter call) search-filter)))
        (setf params (append search-params
                              (and filter (list (cons "filter" filter)))
                              (if (and group (plusp (length group)))
                                  ;; select does not work beside group_by, and a
                                  ;; grouped answer carries no records anyway.
                                  (list (cons "group_by" group))
                                  ;; An ungrouped count still returns records,
                                  ;; so it asks for the smallest possible one.
                                  (list (cons "per_page" 1) (cons "select" "id"))))))
      (let* ((body (request entity params))
             (total (meta-count body))
             (groups (nlk:json-value body :array "group_by")))
        (with-output-to-string (out)
          (format out "~a: ~@[~d matching~]~@[ · filter ~a~]~@[ · search ~s~]~%"
                  entity total
                  (cdr (assoc "filter" params :test #'equal))
                  (cdr (assoc "search" params :test #'equal)))
          (when (and group (plusp (length group)))
            (format out "~%by ~a:~%" group)
            (write-string (group-lines groups) out)
            (when (= (plain-length groups) 200)
              (format out "~%  (the API caps group_by at 200 groups, so this is the head of the ~
distribution rather than all of it; narrow the filter to see the tail)~%")))
          (format out "~%~a~%" (cost-line "list")))))))
;;; --- openalex_search (expensive) ---------------------------------------------------

(nlk:define-contract search-call
  (:doc "Return records, at ten times the price of a count. COSTS $0.001. Before calling it,
ask whether you need the records themselves: if the question is how many, which years, which
institutions, which journals or which topics, openalex_count answers it for a tenth of the
price. IMPORTANT: on works the plain search parameter searches FULL TEXT, far broader than most
people expect; set search_field='title_and_abstract' for conventional bibliographic search.
Filtering by an id resolved through openalex_resolve is freer and more precise than searching
by name.")
  ("entity" :string :options +entities+ :doc "what to search; works by default")
  ("search" :string :slot search-text :doc "free-text query")
  ("search_field" :string :options '("default" "title" "abstract" "title_and_abstract" "fulltext")
   :doc "which field to search; 'default' means full text on works")
  ("filter" :string :doc "filter expression, combined with the search")
  ("sort" :string :slot sort-order :doc "'field:desc' or 'field:asc'; the -field form the API documents is rewritten")
  ("per_page" :integer :doc "1 to 200, 25 by default")
  ("page" :integer :doc "1-indexed; page times per_page stays under 10000")
  ("verbosity" :string :options '("summary" "detail" "raw")
   :doc "'detail' adds the abstract, topics and keywords"))

(defun tool-search (call)
  "openalex_search: one search-priced request, and the records it returns."
  (let ((entity (entity-for (search-call-entity call)))
        (verbosity (verbosity-for (search-call-verbosity call)))
        (per-page (clamp-per-page (search-call-per-page call) (setting :per-page)))
        (page (max 1 (or (search-call-page call) 1))))
    (multiple-value-bind (search-params search-filter)
        (search-split (search-call-search-text call) (search-call-search-field call))
      (let ((filter (merge-filter (search-call-filter call) search-filter)))
        (cond
          ((and (null search-params) (null filter))
           "Give a search term or a filter: an unbounded list of every work in OpenAlex is not an answer.")
          ((> (* page per-page) 10000)
           (format nil "page ~d at per_page ~d passes OpenAlex's 10,000-record ceiling. ~
Narrow the filter, or use openalex_harvest, which pages with a cursor." page per-page))
          (t
           (let* ((params (append search-params
                                   (and filter (list (cons "filter" filter)))
                                   (list (cons "per_page" per-page) (cons "page" page))
                                   (let ((sort (normalize-sort (search-call-sort-order call))))
                                     (and sort (list (cons "sort" sort))))
                                   (let ((select (select-for entity verbosity)))
                                     (and select (list (cons "select" select))))))
                  (body (request entity params))
                  (rows (results-of body))
                  (total (meta-count body)))
             (with-output-to-string (out)
               (format out "~a: ~@[~d matching~] · page ~d · ~d returned~%"
                       entity total page (plain-length rows))
               (write-string (results-text entity rows verbosity
                                           :limit (setting :max-result-chars))
                             out)
               (when (and total (> total (plain-length rows)))
                 (format out "~%more exist; each page is billed again, and openalex_count with ~
a group_by costs a tenth as much for the shape of the set.~%"))
               (format out "~a~%" (cost-line "search"))))))))))

;;; --- openalex_related (cheap) -----------------------------------------------------

(defparameter +related-filters+ '(("cited_by" . "cites") ("references" . "cited_by")
                                  ("related" . "related_to"))
  "Which filter walks the citation graph in which direction.")

(nlk:define-contract related-call
  (:doc "Walk the citation graph from one work, at list price: COSTS $0.0001, ten times less
than finding the same papers by searching. mode='cited_by' finds works citing this one,
mode='references' finds works it cites, mode='related' finds OpenAlex's related-works list.
Use it to walk a literature forwards or backwards from a known paper.")
  ("id" :text :required t :doc "the work to traverse from")
  ("mode" :string :options '("cited_by" "references" "related") :doc "direction of travel")
  ("per_page" :integer :doc "1 to 200, 25 by default")
  ("sort" :string :slot sort-order :doc "e.g. cited_by_count:desc, publication_date:desc")
  ("verbosity" :string :options '("summary" "detail" "raw")))

(defun tool-related (call)
  "openalex_related: a works list filtered on the citation graph."
  (let* ((identifier (normalize-id (related-call-id call)))
         (mode (let ((given (and (related-call-mode call)
                                 (string-downcase (nlk:trimmed (related-call-mode call))))))
                 (if (assoc given +related-filters+ :test #'equal) given "cited_by")))
         (verbosity (verbosity-for (related-call-verbosity call)))
         (per-page (clamp-per-page (related-call-per-page call) (setting :per-page)))
         (select (select-for "works" verbosity))
         (body (request "works"
                         (append (list (cons "filter" (format nil "~a:~a"
                                                                (cdr (assoc mode +related-filters+
                                                                             :test #'equal))
                                                                identifier))
                                       (cons "per_page" per-page))
                                 (let ((sort (normalize-sort (related-call-sort-order call))))
                                   (and sort (list (cons "sort" sort))))
                                 (and select (list (cons "select" select))))))
         (rows (results-of body)))
    (with-output-to-string (out)
      (format out "~a of ~a: ~@[~d matching · ~]~d returned~%"
              mode identifier (meta-count body) (plain-length rows))
      (write-string (results-text "works" rows verbosity :limit (setting :max-result-chars)) out)
      (format out "~a~%" (cost-line "list")))))

;;; --- openalex_account (free) ------------------------------------------------------

(defparameter +account-schema+
  ;; The one tool with no arguments: a contract types a payload, and this one carries none,
  ;; so the schema is the plist REGISTER-TOOL takes when no declaration fits.
  (list :description "Report the budget, the prices and this run's spend. FREE. The account's daily allowance and what is left of it, plus what this run has spent by call type. Call it before starting anything broad, or when a call fails in a way that might be budget related."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "The argument-free account tool's schema.")

(defun tool-account (call)
  "openalex_account: the local ledger, and the live allowance when a key is set."
  (declare (ignore call))
  (let ((key (api-key)))
    (with-output-to-string (out)
      (format out "openalex ~a · verbosity ~a · key ~a~%"
              (root) (setting :verbosity) (if key "set" "MISSING"))
      (format out "this run: $~,4f of $~,2f · ~d calls (~d free)~@[ · by class ~{~a~^, ~}~]~%"
              (ledger-spent *ledger*) (budget-usd) (ledger-calls *ledger*)
              (ledger-free *ledger*)
              (loop for (class . count) in (ledger-classes *ledger*)
                    collect (format nil "~a ~d" class count)))
      (format out "prices: ~{~a~^ · ~}~%"
              (loop for (class . usd) in *prices* collect (format nil "~a $~,4f" class usd)))
      (if key
          (handler-case
              (let* ((body (request "rate-limit" nil))
                     (info (rate-limit-report body))
                     (changed (set-prices (live-prices info))))
                (format out "account: ~@[~,4f remaining of ~]~@[~,4f a day~]~@[ · resets in ~d seconds~]~%"
                        (number-of info "daily_remaining_usd")
                        (number-of info "daily_budget_usd")
                        (number-of info "resets_in_seconds"))
                (when changed
                  (format out "prices refreshed from the account: ~{~a~^, ~}~%" changed)))
            (openalex-error (condition)
              (format out "account: ~a~%" (openalex-error-detail condition))))
          (format out "no key, so the anonymous tier: $~,2f a day. A free key from ~
openalex.org/settings/api raises it to $~,2f. Id lookups and name resolution are free either way.~%"
                  +anon-daily-usd+ 1.00d0)))))

;;; --- openalex_fields (free, local) -------------------------------------------------

(nlk:define-contract fields-call
  (:doc "List the valid filter and select field names, from disk: FREE, no API call, and it
cannot fail on budget. Use it when you are unsure whether a filter exists rather than
guessing, because a rejected query still costs a list call. The filter set and the select set
are not the same, so check the one you need.")
  ("entity" :string :options +entities+ :doc "whose fields to list; works by default")
  ("kind" :string :options '("filter" "select" "both") :doc "which field set; they differ"))

(defun tool-fields (call)
  "openalex_fields: the shipped vocabulary, and no request at all."
  (let* ((entity (entity-for (fields-call-entity call)))
         (kind (let ((given (and (fields-call-kind call)
                                 (string-downcase (nlk:trimmed (fields-call-kind call))))))
                 (if (member given '("filter" "select" "both") :test #'equal) given "both"))))
    (with-output-to-string (out)
      (format out "~a fields (free, served locally)~%" entity)
      (when (member kind '("filter" "both") :test #'equal)
        (format out "~%filter fields~%")
        (dolist (family (filter-fields entity))
          (format out "  ~a: ~a~%" (first family) (join (rest family) ", ")))
        (format out "~%group_by reads best on: ~a~%" (join +groupable-hints+ ", ")))
      (when (and (member kind '("select" "both") :test #'equal)
                 (string= entity "works"))
        (format out "~%select fields: ~a~%" (join +work-select+ ", "))
        (format out "select takes top-level names only, so a nested path such as ~
authorships.author.id is refused there and is a filter, not a select.~%"))
      (format out "~%vocabularies~%")
      (dolist (vocabulary +vocabularies+)
        (format out "  ~a: ~a~%" (first vocabulary) (join (rest vocabulary) ", ")))
      (format out "~%the filter language~%")
      (dolist (line +syntax-lines+)
        (format out "  ~a~%" line)))))
;;; --- openalex_classify (the most expensive call) ---------------------------------

(nlk:define-contract classify-call
  (:doc "Classify text into OpenAlex topics. COSTS $0.01 PER CALL, a hundred times a list
call, and ten calls exhaust the anonymous day. Off by default and enabled only in the section.
Before using it, check whether the work already exists in OpenAlex: openalex_get returns its
topics for free AND more accurately, because a classifier reading a title alone has been seen to
misread it. So this is for text genuinely not in OpenAlex, such as an unpublished abstract or a
grant proposal. For anything with a DOI, resolve and fetch instead.")
  ("title" :string :doc "title of the text to classify")
  ("abstract" :string :doc "abstract or body text"))

(defun topic-line (node &key (prefix "  "))
  "One topic as a line: the label, the score, and where it sits in the taxonomy."
  (when (hash-table-p node)
    (format nil "~a~a~@[ (~a)~]~@[ · ~a~]~@[ / ~a~]~@[ / ~a~]~%"
            prefix
            (or (text-of node "display_name") "-")
            (number-of node "score")
            (text-of (object node "subfield") "display_name")
            (text-of (object node "field") "display_name")
            (text-of (object node "domain") "display_name"))))

(defun tool-classify (call)
  "openalex_classify: /text/topics, text-priced and off unless the section allows it."
  (let ((title (and (classify-call-title call) (nlk:trimmed (classify-call-title call))))
        (abstract (and (classify-call-abstract call) (nlk:trimmed (classify-call-abstract call)))))
    (when (and (zerop (length (or title ""))) (zerop (length (or abstract ""))))
      (fail "openalex_classify needs a title, an abstract, or both."))
    (unless (setting :allow-text-classification)
      (fail "openalex_classify is off: text classification costs $0.01 a call, a hundred times a ~
list call, and ten calls exhaust the anonymous day. If the text is a published work, ~
openalex_get returns its topics for free. To enable it anyway, set allow_text_classification ~
to true in the openalex section."))
    (let* ((body (request "text/topics"
                          (list (cons "title" (and title (plusp (length title)) title))
                                (cons "abstract" (and abstract (plusp (length abstract)) abstract)))))
           (topics (nlk:json-value body :array "topics")))
      (with-output-to-string (out)
        (format out "primary topic~%")
        (write-string (or (topic-line (object body "primary_topic")) "  none~%") out)
        (when (plusp (plain-length topics))
          (format out "~%other topics~%")
          (loop for node across topics
                for index from 0
                when (< index 5)
                  do (write-string (or (topic-line node) "") out)))
        (format out "~a~%" (cost-line "text"))))))

;;; --- openalex_harvest -------------------------------------------------------------

(nlk:define-contract harvest-call
  (:doc "Cursor-pages a large result set, billed once per page. Each page is a separate
billable call, so the cost is roughly records divided by per_page, times $0.0001, and the whole
run is checked against the budget before the first page rather than stopping halfway with
partial data and a spent wallet. For anything above a few hundred thousand records the bulk
snapshot at s3://openalex is free and faster; use this for the middle ground.")
  ("entity" :string :options +entities+ :doc "what to page; works by default")
  ("filter" :string :doc "filter expression defining the set")
  ("search" :string :slot search-text :doc "optional search; makes every page search-priced")
  ("max_records" :integer :doc "hard ceiling on records retrieved, 200 by default")
  ("per_page" :integer :doc "1 to 200; higher means fewer billed calls")
  ("verbosity" :string :options '("summary" "detail" "raw")))

(defun tool-harvest (call)
  "openalex_harvest: a cursor walk, bounded before the first page is sent."
  (let* ((entity (entity-for (harvest-call-entity call)))
         (verbosity (verbosity-for (harvest-call-verbosity call)))
         (filter (merge-filter (harvest-call-filter call) nil))
         (search (and (harvest-call-search-text call)
                      (nlk:trimmed (harvest-call-search-text call))))
         (per-page (clamp-per-page (harvest-call-per-page call) 200))
         (ceiling (max 1 (or (harvest-call-max-records call) 200))))
    (when (and (zerop (length (or filter ""))) (zerop (length (or search ""))))
      (fail "openalex_harvest needs a filter or a search to define the set; harvesting all of ~
OpenAlex through the API is neither possible nor sensible."))
    (let* ((pages (ceiling ceiling per-page))
           (class (if search "search" "list"))
           (estimate (* pages (price class))))
      ;; The whole run is checked up front: stopping halfway leaves partial data.
      (when (> estimate (+ (budget-left) 1d-9))
        (fail "openalex_harvest would fetch ~d pages at $~,4f each, $~,4f in all, and only ~
$~,4f of the $~,2f budget is left. Lower max_records, or raise budget_usd."
              pages (price class) estimate (max 0d0 (budget-left)) (budget-usd)))
      (let ((records '()) (total nil) (fetched 0) (cursor "*"))
        (loop while (and cursor (< (length records) ceiling))
              do (let* ((select (select-for entity verbosity))
                        (params (append (and filter (list (cons "filter" filter)))
                                        (and search (list (cons "search" search)))
                                        (list (cons "per_page" per-page) (cons "cursor" cursor))
                                        (and select (list (cons "select" select)))))
                        (body (request entity params)))
                   (incf fetched)
                   (when (null total) (setf total (meta-count body)))
                   (let ((batch (results-of body)))
                     (loop for raw across batch
                           while (< (length records) ceiling)
                           when (hash-table-p raw)
                             do (push raw records))
                     (setf cursor (text-of (object body "meta") "next_cursor"))
                     (when (zerop (plain-length batch)) (setf cursor nil)))))
        (let ((rows (coerce (nreverse records) 'vector)))
          (with-output-to-string (out)
            (format out "~a: ~@[~d matching · ~]~d retrieved in ~d page~:p of ~d records each~%"
                    entity total (length rows) fetched per-page)
            (write-string (results-text entity rows verbosity :limit (setting :max-result-chars)) out)
            (format out "~a~%" (cost-line class))))))))

;;; --- openalex_fulltext ------------------------------------------------------------

(nlk:define-contract fulltext-call
  (:doc "Report whether OpenAlex holds the full text, free, and download it only on confirm.
Call it WITHOUT confirm first: that costs nothing and tells you whether the full text exists,
in which formats, and what downloading would cost. Call it again with confirm=true only when
the user has asked for the full text of the paper: the download COSTS $0.01, a hundred times a
list call. OpenAlex holds content for roughly 54 million works, a fraction of the index, so
absence is normal; when there is no content, or only a PDF, an url comes back and nothing is
spent.")
  ("id" :text :required t :doc "work identifier: OpenAlex id, DOI, PMID or their URL form")
  ("confirm" :boolean :doc "false checks availability for free; true downloads and costs $0.01")
  ("format" :string :slot format-mode :options '("text" "pdf") :doc "'text' reads the TEI; 'pdf' hands back the link only"))

(defun tei-text (xml &optional (limit 0))
  "XML -- GROBID TEI -- flattened into the prose a model can read: tags dropped,
the entities that matter unfolded, runs of whitespace collapsed."
  (let ((out (make-string-output-stream)) (inside nil) (space nil) (any nil))
    (loop for character across xml
          do (cond
               ((char= character #\<) (setf inside t))
               ((char= character #\>) (setf inside nil space t))
               (inside)
               ((member character '(#\Space #\Tab #\Newline #\Return))
                (setf space t))
               (t (when (and space any) (write-char #\Space out))
                  (setf space nil any t)
                  (write-char character out))))
    (let ((text (nlk:trimmed (get-output-stream-string out))))
      (dolist (pair '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"")))
        (setf text (replace-string (car pair) (cdr pair) text)))
      (if (plusp limit) (clip text limit) text))))

(defun tool-fulltext (call)
  "openalex_fulltext: a free availability check, and a content-priced download on confirm."
  (let* ((identifier (normalize-id (fulltext-call-id call)))
         (record (request (format nil "works/~a" identifier)
                          (list (cons "select" "id,display_name,has_content,content_urls,open_access"))))
         (available (object record "has_content"))
         (urls (object record "content_urls"))
         (open-access (object record "open_access"))
         (formats (loop for key in '("grobid_xml" "pdf")
                        when (and available (nlk:json-value available :boolean key))
                          collect key)))
    (with-output-to-string (out)
      (format out "~a ~a~%  available: ~a~@[ · open access: ~a~]~%"
              (or (short-id (text-of record "id")) identifier)
              (or (text-of record "display_name") "")
              (if formats (join formats ", ") "no full text")
              (text-of open-access "oa_url"))
      (cond
        ((null formats)
         (format out "OpenAlex holds no full text for this work, which is common: it has content ~
for a fraction of the index. If an open-access url is above, the paper is still readable there.~%"))
        ((not (fulltext-call-confirm call))
         (format out "Nothing was downloaded and nothing was spent. Downloading costs $~,2f, a ~
hundred times a list call; call again with confirm=true only if the user asked for the full ~
text. If they only want to read it, the open-access url above is free.~%" (price "content")))
        ((or (string= (fulltext-call-format-mode call) "pdf")
             (not (and available (nlk:json-value available :boolean "grobid_xml"))))
         (format out "pdf: ~a~%A PDF is binary, so nothing was spent: fetch that url with your ~
own web tools and read it there.~%" (text-of urls "pdf")))
        (t (let ((text (tei-text (get-content (text-of urls "grobid_xml")))))
             (format out "~d characters of ~a~%~a~%" (length text) (text-of urls "grobid_xml")
                     (clip text (setting :max-result-chars))))))
      (format out "~a~%" (cost-line "free")))))
