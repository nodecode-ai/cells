;;;; cell.lisp --- Caduceus: the four tools, the command, the section, the manual.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :caduceus) answers the primer below, and every request's help
;;;; section carries one line naming it. Nothing hooks the provider path and
;;;; nothing dials: the only things this cell reads are the files the user
;;;; imported and the vault under the folder they named.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "caduceus": {"data_dir": "a folder of your own", "privacy_level": "open",
;;;;                "hidden_categories": [], "unlocked_categories": []}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Rewritten from electricsheephq's Hermes Agent plugin (see NOTICE): its
;;;; four tools and what they answer, with the vault as plain files and the
;;;; evidence pack in this folder instead of a CPIC export and a research
;;;; watch.

(in-package #:nodecode-caduceus)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Caduceus is a private vault on this machine for the user's own DNA raw data, Apple Health
numbers, medications and notes, through the nodecode-caduceus cell: four tools and a /caduceus
command. Nothing is uploaded; the tools read the vault and answer slices of it, and those
slices enter the conversation, so the model provider the user chose sees them.
  caduceus_capture  {op, path, drug, brand, event, dose, date, text, topics, confirm}
      op=import   copy and parse a file the user names by absolute path: 23andMe or
                  AncestryDNA raw data (.txt, one line per marker), a Promethease report
                  table (.tsv or .zip), an unzipped Apple Health export.xml, or a FHIR
                  Bundle of Observations (.json). Never open those files yourself; pass
                  the path. A .zip needs the unzip command the machine already has.
      op=status   what is imported: markers, records, sample days, the folder.
      op=medication  record a start, dose change or stop. Show the user the details and
                  set confirm=true only after they agree.
      op=journal  store a note, with optional topics.
      op=settings show the privacy level and categories. Only the user changes them.
      op=brief    write a local HTML health brief and answer its path.
      op=sources  list the files imported so far.
  caduceus_genome  {op, rsids, gene, drug, topic, min_magnitude, repute, limit}
      op=lookup   the user's call at each rsid, with the pack's finding when it has one.
      op=gene     every packed finding and gene-drug entry for a gene symbol.
      op=pgx      a drug name: gene-drug evidence with each marker called, absent or not
                  on the chip. Consumer chips miss rare variants; say so.
      op=derived  APOE and the enzymes a chip can combine, with a phenotype guess.
      op=summary  what is imported and the strongest findings.
      op=filter   findings by magnitude, repute or a topic word.
      op=report   rebuild the local HTML variant report and answer its path.
  caduceus_search  {query, kinds, ref, limit}
      Find records in the vault by words; fetch one in full by the ref a hit returned.
  caduceus_trend  {op, metric, lab, start, end, bucket, pivot, drug, before_days, after_days}
      op=metrics  what the vault holds, with units and coverage.
      op=series   one metric bucketed by day, week or month.
      op=compare  before against after a date, or a medication's start.
      op=labs     one measurement over time, by name or part of one.
      op=check    the latest day against the user's own 5th to 95th percentile.

A missing day is a day the export held nothing for, not a zero. Every genotype answer says
the consumer-chip limits; every gene-drug answer says it is not a dosing recommendation.
Quote numbers with their units, and never change a medication on the strength of one."
  "What (help :caduceus) answers while the cell runs.")

(defparameter +limits+
  "Information with limits, not medical advice."
  "What the original's every answer ends with, kept.")

;;; --- answers, and the one condition ---------------------------------------------

(define-condition refusal (error)
  ((text :initarg :text :reader refusal-text))
  (:documentation "A call that cannot go on, carrying the words the model reads."))

(defun refuse (format-control &rest args)
  "Stop the call with the words the model reads: ANSWERING turns it into the
call's failure, so nothing after it runs."
  (error 'refusal :text (apply #'format nil format-control args)))

(defmacro answering (&body body)
  "BODY's JSON text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (caduceus-error (condition)
       (nle:failure (format nil "ERROR: ~a" (caduceus-error-detail condition))))
     (refusal (condition)
       (nle:failure (format nil "ERROR: ~a" (refusal-text condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Caduceus: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- reading the section ---------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with data_dir expanded to a namestring and the two
category lists checked against the categories this cell knows: a name nothing
types would hide nothing, so it refuses here instead."
  (declare (ignore table))
  (let ((raw (getf values :data-dir))
        (plist (copy-list values)))
    (unless (nonblank (if (pathnamep raw) (namestring raw) raw))
      (nlk:config-error "no data_dir is set: set it to a folder this cell may keep its vault in (for example a folder of your own inside your home), then restart the cell"))
    (setf (getf plist :data-dir)
          (namestring (uiop:ensure-directory-pathname
                       (nlk:expand-home (if (pathnamep raw) (namestring raw) raw)))))
    (dolist (key '(:hidden-categories :unlocked-categories))
      (let ((names '()))
        (dolist (name (getf plist key))
          (let ((name (string-downcase (princ-to-string name))))
            (unless (member name +categories+ :test #'string=)
              (nlk:config-error "~s is not a category this cell knows; it knows ~{~a~^, ~}"
                                name +categories+))
            (push name names)))
        (setf (getf plist key) (nreverse names))))
    plist))

;;; --- search ---------------------------------------------------------------------

(defun annotation-docs ()
  "One search document per packed marker the chip called."
  (loop for call in (annotation-calls)
        collect (list :kind "annotation"
                      :category (getf call :category)
                      :ref (format nil "annotation:~a" (getf call :rsid))
                      :title (format nil "~a (~a)" (getf call :title) (getf call :gene))
                      :body (format nil "~a Genotype ~a at ~a.~@[ Magnitude ~a.~] ~a"
                                    (getf call :finding) (or (getf call :genotype) "not called")
                                    (getf call :rsid) (getf call :magnitude) +chip-limits+)
                      :ts "")))

(defun vault-docs ()
  "Every searchable record: what is imported, what is noted, what is packed."
  (append (annotation-docs) (all-records)))

(defun score-doc (doc terms)
  "DOC's score for TERMS, and how many of TERMS it carries: a title hit
counts four, a body hit one."
  (let ((title (string-downcase (or (getf doc :title) "")))
        (body (string-downcase (or (getf doc :body) "")))
        (score 0) (hits 0))
    (dolist (term terms)
      (let ((in-title (search term title))
            (in-body (search term body)))
        (when (or in-title in-body) (incf hits))
        (when in-title (incf score 4))
        (when in-body (incf score))))
    (values score hits)))

(defun snippet-of (doc terms)
  "The part of DOC's body a hit is about."
  (let* ((body (or (getf doc :body) ""))
         (lower (string-downcase body))
         (at (loop for term in terms when (search term lower) return (search term lower))))
    (if (and at (> at 0))
        (format nil "…~a" (subseq body (max 0 (- at 40)) (min (length body) (+ at 160))))
        (subseq body 0 (min (length body) 200)))))

(defun doc-json (doc terms)
  (nlk:json-object
   "ref" (getf doc :ref)
   "kind" (getf doc :kind)
   "title" (getf doc :title)
   "snippet" (snippet-of doc terms)
   "category" (getf doc :category)
   "date" (getf doc :ts)))

(defun search-hits (queries kinds)
  "Every doc carrying every one of QUERIES (or, when none carries all,
any of them), best first: a list of (SCORE . DOC)."
  (let ((scores (make-hash-table :test #'equal))
        (all '()))
    (dolist (doc (vault-docs))
      (when (or (null kinds) (member (getf doc :kind) kinds :test #'string=))
        (multiple-value-bind (score hits) (score-doc doc queries)
          (when (plusp hits)
            (setf (gethash (getf doc :ref) scores) (list score hits doc))
            (push (getf doc :ref) all)))))
    (let* ((rows (mapcar (lambda (ref) (gethash ref scores)) (nreverse all)))
           (full (remove-if-not (lambda (row) (= (second row) (length queries))) rows))
           (chosen (or full rows)))
      (values (sort chosen #'> :key #'first)
              (if full "all_terms" "any_term")))))

(defun search-vault (request)
  "caduceus_search: ranked hits the privacy policy allows, or one record in full."
  (answering
   (let* ((policy (policy))
          (ref (nonblank (search-request-ref request))))
     (if ref
         (let ((doc (find ref (vault-docs) :key (lambda (doc) (getf doc :ref)) :test #'string=)))
           (cond ((null doc) (refuse "no record with the ref ~a" ref))
                 ((not (visible-p policy (getf doc :category) :kind (getf doc :kind)))
                  (nlk:encode-json-object
                   (nlk:json-object "ref" ref "withheld" t
                                    "reason" "hidden by your privacy settings")))
                 (t (nlk:encode-json-object
                     (nlk:json-object "ref" ref "kind" (getf doc :kind)
                                      "title" (getf doc :title) "body" (getf doc :body)
                                      "category" (getf doc :category) "date" (getf doc :ts))))))
         (let* ((query (nonblank (search-request-query request)))
                (terms (and query (words-of query)))
                (limit (max 1 (min 25 (search-request-limit request)))))
           (unless terms (refuse "give a query, or a ref to fetch"))
           (multiple-value-bind (rows match) (search-hits terms (search-request-kinds request))
             (let ((results '()) (withheld 0))
               (dolist (row rows)
                 (let ((doc (third row)))
                   (if (visible-p policy (getf doc :category) :kind (getf doc :kind))
                       (when (< (length results) limit)
                         (push (doc-json doc terms) results))
                       (incf withheld))))
               (nlk:encode-json-object
                (nlk:json-object
                 "query" query
                 "match" match
                 "results" (coerce (nreverse results) 'vector)
                 "withheld" withheld
                 "note" (cond (results
                                (if (plusp withheld)
                                    "some records are hidden by your privacy settings; quote the ref when you want the whole record"
                                    "quote the ref when you want the whole record"))
                               ((plusp withheld)
                                "everything that matched is hidden by your privacy settings; say so, never what it was")
                               (t "nothing matched: try fewer or different words, or import more data with caduceus_capture op=import")))))))))))

;;; --- the genome tool -------------------------------------------------------------

(defun finding-json (call)
  (nlk:json-object "rsid" (getf call :rsid) "gene" (getf call :gene)
                   "title" (getf call :title) "category" (getf call :category)
                   "magnitude" (getf call :magnitude) "repute" (getf call :repute)
                   "genotype" (or (getf call :genotype) :null)
                   "effect_alleles" (or (getf call :effect-alleles) :null)
                   "state" (getf call :state) "finding" (getf call :finding)))

(defun genome-summary ()
  (let* ((policy (policy))
         (calls (annotation-calls))
         (shown (loop for call in calls
                      when (visible-p policy (getf call :category) :kind "annotation")
                        collect call)))
    (nlk:json-object
     "markers_called" (genotype-count)
     "packed_markers" (length +annotations+)
     "findings" (length calls)
     "sources" (coerce (mapcar (lambda (source) (getf source :kind)) (all-sources)) 'vector)
     "privacy" (getf policy :level)
     "top_findings" (coerce (loop for call in (subseq shown 0 (min 5 (length shown)))
                                    collect (finding-json call))
                             'vector)
     "note" (if (plusp (genotype-count))
                 (format nil "~a Findings follow the privacy level: ~a." +chip-limits+ (getf policy :level))
                 "no DNA raw data is imported yet: import a 23andMe or AncestryDNA .txt with caduceus_capture op=import"))))

(defun genome-lookup (rsids)
  (unless rsids
    (refuse "op=lookup wants at least one rsid"))
  (let ((policy (policy)))
    (nlk:json-object
     "markers" (coerce
                (loop for rsid across (coerce rsids 'vector)
                      for entry = (annotation-entry rsid)
                      for call = (and entry (annotation-call entry))
                      collect (if (and entry (visible-p policy (getf call :category) :kind "annotation"))
                                  (finding-json call)
                                  (call-object rsid)))
                'vector)
     "disclaimer" +chip-limits+)))

(defun genome-gene (gene)
  (unless (nonblank gene)
    (refuse "op=gene wants a gene symbol, for example SLCO1B1"))
  (let* ((policy (policy))
         (calls (remove-if-not (lambda (call) (string-equal gene (getf call :gene)))
                                (annotation-calls)))
         (entries (remove-if-not (lambda (entry) (string-equal gene (getf entry :gene)))
                                 +pgx-entries+)))
    (nlk:json-object
     "gene" (string-upcase gene)
     "findings" (coerce (loop for call in calls
                                when (visible-p policy (getf call :category) :kind "annotation")
                                  collect (finding-json call))
                         'vector)
     "gene_drug_entries" (coerce (loop for entry in entries
                                        collect (nlk:json-object
                                                 "id" (getf entry :id) "gene" (getf entry :gene)
                                                 "drugs" (coerce (getf entry :drugs) 'vector)
                                                 "finding" (getf entry :finding)
                                                 "evidence" (getf entry :evidence)))
                                  'vector)
     "note" (if (or calls entries)
                +chip-limits+
                "this pack has no finding for that gene; it covers the common chip markers, not every gene"))))

(defun genome-filter (request)
  (let* ((policy (policy))
         (repute (let ((repute (genome-request-repute request)))
                   (if (string= repute "any") nil repute)))
         (calls (annotation-calls :min-magnitude (genome-request-min-magnitude request)
                                  :repute repute
                                  :topic (nonblank (genome-request-topic request))
                                  :gene (nonblank (genome-request-gene request))))
         (limit (max 1 (min 100 (genome-request-limit request))))
         (shown (loop for call in calls
                      when (visible-p policy (getf call :category) :kind "annotation")
                        collect call)))
    (nlk:json-object
     "count" (length shown)
     "findings" (coerce (loop for call in (subseq shown 0 (min limit (length shown)))
                                collect (finding-json call))
                         'vector)
     "withheld" (- (length calls) (length shown))
     "note" (if shown
                 "use caduceus_genome op=pgx with a drug name when a finding is about a drug"
                 "nothing matched that filter"))))

(defun enzyme-json (call)
  (nlk:json-object
   "gene" (getf call :gene)
   "phenotype" (or (getf call :phenotype) (getf call :function) "")
   "detail" (nlk:json-object
             "no_function_alleles" (if (null (getf call :no-function)) :null (getf call :no-function))
             "increased_alleles" (if (null (getf call :increased)) :null (getf call :increased))
             "reduced_alleles" (if (null (getf call :reduced))
                                    (if (null (getf call :decreased)) :null (getf call :decreased))
                                    (getf call :reduced))
             "expressing_alleles" (if (null (getf call :expressing-alleles))
                                       :null (getf call :expressing-alleles)))
   "note" (or (getf call :note) "")))

(defun derived-answer ()
  (nlk:json-object
   "calls" (coerce (append (list (apoe-report))
                            (mapcar #'enzyme-json
                                    (remove nil (list (cyp2c19-result) (slco1b1-result)
                                                      (cyp2c9-result) (cyp3a5-result)))))
                    'vector)
   "note" "a phenotype from a consumer chip is a guess from the common alleles: a clinical test confirms it"
   "disclaimer" +chip-limits+))

(defun read-genome (request)
  "caduceus_genome: the op the request names, answered from the vault."
  (answering
   (let ((op (genome-request-op request)))
     (cond ((string= op "summary") (nlk:encode-json-object (genome-summary)))
           ((string= op "lookup") (nlk:encode-json-object (genome-lookup (genome-request-rsids request))))
           ((string= op "gene") (nlk:encode-json-object (genome-gene (genome-request-gene request))))
           ((string= op "filter") (nlk:encode-json-object (genome-filter request)))
           ((string= op "pgx")
            (unless (nonblank (genome-request-drug request))
              (refuse "op=pgx wants a drug name, for example clopidogrel"))
            (nlk:encode-json-object (pgx-check (genome-request-drug request))))
           ((string= op "derived") (nlk:encode-json-object (derived-answer)))
           ((string= op "report")
            (let ((report (write-variant-report)))
              (nlk:encode-json-object
               (nlk:json-object "path" (getf report :path)
                                "file_url" (format nil "file://~a" (getf report :path))
                                "entries" (getf report :entries)
                                "note" "a local HTML file; nothing left the machine"))))
           (t (refuse "unknown op ~a" op))))))

;;; --- the trend tool --------------------------------------------------------------

(defun metric-matching (text)
  "The metric whose name carries TEXT, or NIL, and the candidates when
several do."
  (let* ((wanted (string-downcase (nlk:trimmed text)))
         (exact (find wanted (sample-metrics) :test #'string=))
         (partial (remove-if-not (lambda (metric) (search wanted metric :test #'char-equal))
                                  (sample-metrics))))
    (values (or exact (first (sort partial #'string<)))
            (sort partial #'string<))))

(defun read-trend (request)
  "caduceus_trend: the op the request names, answered from the samples."
  (answering
   (let* ((op (trend-request-op request))
          (start (iso-day-p (trend-request-start request)))
          (end (iso-day-p (trend-request-end request))))
     (cond
       ((string= op "metrics") (nlk:encode-json-object (metrics-object)))
       ((string= op "check") (nlk:encode-json-object (check-object)))
       ((string= op "series")
        (let ((metric (nonblank (trend-request-metric request))))
          (unless metric (refuse "op=series wants a metric name; op=metrics lists them"))
          (nlk:encode-json-object (series-object metric start end (trend-request-bucket request)))))
       ((string= op "labs")
        (let ((lab (nonblank (trend-request-lab request))))
          (unless lab (refuse "op=labs wants a lab name or part of one, for example glucose"))
          (multiple-value-bind (metric candidates) (metric-matching lab)
            (unless metric
              (refuse "no measurement matches ~s; the vault holds ~{~a~^, ~}" lab (sample-metrics)))
            (let ((series (series-object metric start end "day")))
              (nlk:encode-json-object
               (nlk:copy-json-object series
                                     "asked_as" lab
                                     "candidates" (coerce candidates 'vector)
                                     "note" (if (> (length candidates) 1)
                                                 "more than one measurement matches; say which one you used"
                                                 "the measurement over time, oldest day first")))))))
       ((string= op "compare")
        (let* ((drug (nonblank (trend-request-drug request)))
               (pivot (or (iso-day-p (trend-request-pivot request))
                          (and drug (medication-start-day drug))))
               (metric (nonblank (trend-request-metric request))))
          (unless metric (refuse "op=compare wants a metric name"))
          (unless pivot
            (refuse "op=compare wants a pivot date, or a drug whose start is recorded: ~@[~s has no recorded start~]"
                    drug))
          (nlk:encode-json-object
           (nlk:copy-json-object (compare-object metric pivot
                                                 (trend-request-before-days request)
                                                 (trend-request-after-days request))
                                 "because" (if drug "a recorded medication start" "the pivot you gave")))))
       (t (refuse "unknown op ~a" op))))))

;;; --- the capture tool ------------------------------------------------------------

(defun text-category (text)
  "The category a line of report text points at, or `general'."
  (let ((text (string-downcase (or text ""))))
    (cond ((or (search "alzheimer" text) (search "dementia" text) (search "apoe" text)) "alzheimers")
          ((or (search "brca" text) (search "breast cancer" text) (search "lynch" text)
               (search "colorectal cancer" text)) "cancer_risk")
          ((search "carrier" text) "carrier")
          ((or (search "depress" text) (search "bipolar" text) (search "schizo" text)
               (search "anxiety" text) (search "adhd" text) (search "autis" text))
           "mental_health")
          ((or (search "pregnan" text) (search "infertil" text) (search "pcos" text)
               (search "endometrio" text)) "reproductive")
          (t "general"))))

(defun promethease-field (row fields name)
  "ROW's field NAME, found by the header's own name, or NIL."
  (let ((position (position name fields :test #'string=)))
    (and position (< position (length row)) (nlk:trimmed (nth position row)))))

(defun promethease-rows (lines)
  "The rows of a Promethease report table, keyed by the header's own names."
  (let ((header (find-if (lambda (line)
                           (let ((lower (string-downcase line)))
                             (and (search "magnitude" lower)
                                  (search "repute" lower))))
                         lines)))
    (unless header
      (fail "that file has no Promethease header (it has no Magnitude and Repute columns)"))
    (let* ((fields (mapcar (lambda (field) (string-downcase (nlk:trimmed field)))
                           (uiop:split-string header :separator '(#\Tab))))
           (rows '()))
      (dolist (line lines)
        (unless (string= line header)
          (let ((row (uiop:split-string line :separator '(#\Tab))))
            (when (>= (length row) (max 4 (length fields)))
              (let ((summary (or (promethease-field row fields "summary")
                                 (promethease-field row fields "text") ""))
                    (repute (or (promethease-field row fields "repute") "neutral"))
                    (magnitude (or (promethease-field row fields "magnitude") "0"))
                    (rsid (or (promethease-field row fields "rs#")
                              (promethease-field row fields "rsid")
                              (promethease-field row fields "snp") "")))
                (push (list :rsid (string-downcase rsid)
                            :summary summary
                            :repute (string-downcase repute)
                            :magnitude (or (ignore-errors (read-from-string magnitude)) 0))
                      rows))))))
      (nreverse rows))))

(defun import-promethease (path)
  "Import a Promethease report table as annotation records."
  (let ((rows (promethease-rows (read-lines path)))
        (count 0))
    (when (null rows)
      (fail "~a holds no Promethease rows this cell recognizes" (file-namestring path)))
    (dolist (row (subseq rows 0 (min (length rows) 2000)))
      (let ((ref (if (plusp (length (getf row :rsid)))
                     (format nil "promethease:~a" (getf row :rsid))
                     (format nil "promethease:row-~d" count))))
        (append-record (list :kind "annotation"
                             :category (text-category (getf row :summary))
                             :ref ref
                             :title (format nil "~@[~a ~]magnitude ~a" (getf row :rsid) (getf row :magnitude))
                             :body (format nil "~a Repute ~a, magnitude ~a, imported from a Promethease report."
                                           (getf row :summary) (getf row :repute) (getf row :magnitude))
                             :ts (iso-now))))
      (incf count))
    (append-source path "Promethease report" count)
    (nlk:json-object "imported" (file-namestring path) "annotations" count
                     "note" "Promethease rows are records, not a second opinion; the pack's own findings stay separate")))

(defun detect-and-import (path request)
  "Import PATH by what it is: an archive is read through unzip, XML is an
Apple Health export, JSON is a FHIR bundle, a table with Magnitude and
Repute is a Promethease report, and a table with an rsid header is raw DNA."
  (declare (ignore request))
  (let* ((name (pathname path))
         (type (string-downcase (or (pathname-type name) "")))
         (head (if (member type '("zip") :test #'string=)
                   ""
                   (with-open-file (in path :direction :input :external-format :utf-8
                                            :if-does-not-exist nil)
                     (if in (let ((line (read-line in nil ""))) (or line "")) "")))))
    (cond
      ((string= type "xml") (import-apple-health path))
      ((string= type "json") (import-fhir path))
      ((and (> (length head) 0) (search "<HealthData" head)) (import-apple-health path))
      ((and (> (length head) 0) (or (search "magnitude" head :test #'char-equal)
                                     (search "Magnitude" head)))
       (import-promethease path))
      ((string= type "zip")
       (let ((first-line (or (first (read-lines path)) "")))
         (if (search "magnitude" first-line :test #'char-equal)
             (import-promethease path)
             (import-raw-data path "raw data"))))
      (t (import-raw-data path "raw data")))))

(defun check-import-path (raw)
  "RAW, a path the user or the model gave, as a namestring of a file that is
there. A relative path is refused: the vault writes what it was given, and a
path is only unambiguous when it is the whole one."
  (let ((text (nonblank raw)))
    (unless text (fail "give the file's absolute path in path"))
    (when (and (plusp (length text)) (char= (char text 0) #\@)
               (search "file:" text))
      (setf text (nlk:trimmed (subseq text (length "@file:")))))
    (let ((pathname (pathname (nlk:expand-home text))))
      (unless (uiop:absolute-pathname-p pathname)
        (fail "give the file's absolute path, not ~s" text))
      (let ((file (probe-file pathname)))
        (unless (and file (not (uiop:directory-pathname-p file)))
          (fail "no file at ~a" (namestring pathname)))
        (namestring file)))))

(defun medication-start-day (drug)
  "The day the vault records DRUG starting, or NIL."
  (let ((wanted (string-downcase (or drug "")))
        (found nil))
    (dolist (record (all-records))
      (when (and (string= (getf record :kind) "medication")
                 (search wanted (string-downcase (or (getf record :title) "")))
                 (search "event: start" (or (getf record :body) "")))
        (let ((at (search "date: " (or (getf record :body) ""))))
          (when at
            (let ((day (subseq (getf record :body) (+ at 6)
                                (min (length (getf record :body)) (+ at 16)))))
              (when (iso-day-p day) (setf found day)))))))
    found))

(defun write-brief ()
  "A local HTML health brief: what is in the vault, the latest numbers, the
medications and the strongest findings."
  (let* ((path (merge-pathnames "health-brief.html" (data-dir)))
         (policy (policy))
         (metrics (if (visible-p policy "wellness" :kind "observation")
                      (sort (sample-metrics) #'string<)
                      '()))
         (medications (remove-if-not (lambda (record) (string= (getf record :kind) "medication"))
                                     (all-records)))
         (findings (loop for call in (annotation-calls)
                         when (visible-p policy (getf call :category) :kind "annotation")
                           collect call)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (format out "<!doctype html><meta charset=\"utf-8\"><title>Caduceus health brief</title>~%")
      (format out "<h1>Health brief</h1><p>Written ~a from the vault on this machine.</p>~%" (html-escape (iso-now)))
      (format out "<h2>What the vault holds</h2><ul><li>~d markers</li><li>~d sample metric-days</li>~%<li>~d records</li></ul>~%"
              (genotype-count) (length (samples)) (length (all-records)))
      (format out "<h2>Latest numbers</h2><table border=\"1\" cellpadding=\"4\"><tr><th>Metric</th><th>Day</th><th>Value</th><th>Unit</th></tr>~%")
      (dolist (metric metrics)
        (let ((rows (remove-if-not (lambda (sample) (string= metric (getf sample :metric)))
                                    (samples))))
          (when rows
            (let ((latest (car (last (sort rows #'string< :key (lambda (sample) (getf sample :day)))))))
              (format out "<tr><td>~a</td><td>~a</td><td>~,2f</td><td>~a</td></tr>~%"
                      (html-escape metric) (html-escape (getf latest :day))
                      (getf latest :value) (html-escape (getf latest :unit)))))))
      (format out "</table><h2>Medications</h2><ul>~%")
      (dolist (record medications)
        (format out "<li>~a — ~a</li>~%" (html-escape (getf record :title))
                (html-escape (getf record :body))))
      (format out "</ul><h2>Findings</h2><ul>~%")
      (dolist (call (subseq findings 0 (min 20 (length findings))))
        (format out "<li><b>~a</b> (~a): ~a</li>~%"
                (html-escape (getf call :title)) (html-escape (getf call :gene))
                (html-escape (getf call :finding))))
      (format out "</ul><p>~a</p>~%" (html-escape +limits+)))
    (list :path (namestring path) :metrics (length metrics) :medications (length medications))))

(defun capture (request)
  "caduceus_capture: the op the request names."
  (answering
   (let ((op (capture-request-op request)))
     (cond
       ((string= op "import")
        (let ((path (check-import-path (capture-request-path request))))
          (nlk:encode-json-object
           (nlk:copy-json-object (detect-and-import path request)
                                 "path" path
                                 "next" "ask what it found with caduceus_genome op=summary or caduceus_trend op=metrics"))))
       ((string= op "status")
        (nlk:encode-json-object
         (nlk:json-object
          "vault" (namestring (data-dir))
          "markers_called" (genotype-count)
          "records" (length (all-records))
          "metrics" (length (sample-metrics))
          "sample_days" (length (remove-duplicates (mapcar (lambda (sample) (getf sample :day))
                                                          (samples))
                                                   :test #'string=))
          "imports" (length (all-sources))
          "next" (if (plusp (genotype-count))
                     "ask caduceus_genome op=derived, or caduceus_search with a word"
                     "import a DNA raw data file, or an Apple Health export.xml, with op=import"))))
       ((string= op "medication")
        (let ((drug (nonblank (capture-request-drug request)))
              (event (capture-request-event request)))
          (unless drug (refuse "op=medication wants the drug's name in drug"))
          (unless (capture-request-confirm request)
            (refuse "not recorded yet: the user has to agree first. Show them ~a ~a~@[ ~a~]~@[ on ~a~], then call again with confirm=true"
                    drug event (nonblank (capture-request-dose request))
                    (iso-day-p (capture-request-date request))))
          (let* ((brand (nonblank (capture-request-brand request)))
                 (dose (nonblank (capture-request-dose request)))
                 (date (or (iso-day-p (capture-request-date request)) (subseq (iso-now) 0 10)))
                 (details (format nil "date: ~a; event: ~a; dose: ~a; brand: ~a"
                                  date event (or dose "") (or brand "")))
                 (category (medication-category (format nil "~a ~@[~a~]" drug brand))))
            (append-record (list :kind "medication" :category category
                                 :ref (next-ref "medication")
                                 :title (format nil "~a ~a~@[ ~a~]~@[ (~a)~]"
                                                drug (string-downcase event) dose brand)
                                 :body details :ts (iso-now)))
            (nlk:encode-json-object
             (nlk:json-object "recorded" t "drug" drug "event" event "date" date
                              "dose" (or dose :null) "category" category
                              "note" "a medication record is a note the trend tool can compare around, not a prescription")))))
       ((string= op "journal")
        (let ((text (nonblank (capture-request-text request))))
          (unless text (refuse "op=journal wants the note in text"))
          (let ((topics (capture-request-topics request)))
            (append-record (list :kind "journal" :category "general"
                                 :ref (next-ref "journal")
                                 :title (subseq text 0 (min 70 (length text)))
                                 :body (if topics (format nil "~a (topics: ~{~a~^, ~})" text (coerce topics 'list)) text)
                                 :ts (iso-now)))
            (nlk:encode-json-object
             (nlk:json-object "stored" t "kind" "journal" "chars" (length text)
                              "topics" (coerce (or topics '()) 'vector))))))
       ((string= op "settings")
        (nlk:encode-json-object (policy-object)))
       ((string= op "sources")
        (nlk:encode-json-object
         (nlk:json-object
          "sources" (coerce (loop for source in (all-sources)
                                   collect (nlk:json-object
                                            "path" (getf source :path) "kind" (getf source :kind)
                                            "rows" (getf source :rows) "at" (getf source :at)))
                             'vector)
          "count" (length (all-sources)))))
       ((string= op "brief")
        (let ((brief (write-brief)))
          (nlk:encode-json-object
           (nlk:json-object "path" (getf brief :path)
                            "file_url" (format nil "file://~a" (getf brief :path))
                            "metrics" (getf brief :metrics)
                            "medications" (getf brief :medications)
                            "note" "a local HTML file; open it in a browser, nothing left the machine"))))
       ((string= op "watch_run")
        (nlk:encode-json-object
         (nlk:json-object
          "ran" nil
          "note" "the research watch is not part of this rewrite: it queried Europe PMC and NCBI, and a cell that always dials would surprise. The pack's own entries carry their CPIC sources instead.")))
       (t (refuse "unknown op ~a" op))))))

;;; --- /caduceus ------------------------------------------------------------------

(defun status-text ()
  (format nil "Caduceus vault: ~a~%~d markers, ~d records, ~d metrics over ~d days; privacy level ~a.~%~a"
          (namestring (data-dir)) (genotype-count) (length (all-records))
          (length (sample-metrics))
          (length (remove-duplicates (mapcar (lambda (sample) (getf sample :day)) (samples))
                                      :test #'string=))
          (getf (policy) :level)
          (if (plusp (genotype-count))
              "Ask for a gene, a drug, a marker or a trend, or type /caduceus pgx <drug>."
              "Import a DNA raw data file or an Apple Health export.xml first: ask the agent to do it.")))

(defun caduceus-command (args session-id)
  "One /caduceus invocation: status, a search, or a gene-drug answer."
  (declare (ignore session-id))
  (handler-case
      (let* ((text (or args ""))
             (words (words-of text))
             (head (first words)))
        (cond
          ((null head) (status-text))
          ((member head '("status" "help") :test #'string=) (status-text))
          ((string= head "pgx")
           (let ((drug (format nil "~{~a~^ ~}" (rest words))))
             (if (zerop (length drug))
                 "Usage: /caduceus pgx <drug>, for example /caduceus pgx clopidogrel"
                 (pgx-text drug))))
          ((string= head "search")
           (let ((terms (rest words)))
             (if (null terms)
                 "Usage: /caduceus search <words>"
                 (search-text terms))))
          (t (search-text words))))
    (caduceus-error (condition)
      (format nil "Caduceus: ~a" (caduceus-error-detail condition)))
    (error (condition)
      (format nil "Caduceus error: ~a" (nlk:one-line (princ-to-string condition))))))

(defun search-text (terms)
  (multiple-value-bind (rows match) (search-hits terms nil)
    (declare (ignore match))
    (if (null rows)
        (format nil "Nothing in the vault matches ~{~a~^ ~}. Try fewer words, or import more data."
                terms)
        (with-output-to-string (out)
          (format out "~d hit~:p for ~{~a~^ ~}:~%" (length rows) terms)
          (dolist (row (subseq rows 0 (min 6 (length rows))))
            (let ((doc (third row)))
              (format out "  ~a [~a] ~a~%" (getf doc :ref) (getf doc :kind) (getf doc :title))))))))

(defun pgx-text (drug)
  (let* ((answer (pgx-check drug))
         (entries (nlk:json-array answer "entries")))
    (with-output-to-string (out)
      (format out "~a:~%" (nlk:json-value answer :string "drug"))
      (loop for index from 0 below (length entries)
            for entry = (aref entries index)
            do (format out "  ~a — ~a~%    ~a~%"
                       (nlk:json-value entry :string "gene")
                       (nlk:json-value entry :string "allele")
                       (nlk:json-value entry :string "finding")))
      (format out "~a~%" (nlk:json-value answer :string "disclaimer")))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell caduceus
  (:section ("caduceus")
    (:guide "data_dir is the folder the vault lives in and the only required setting: set it first, then restart the cell. privacy_level open shows every category; standard keeps Alzheimer's risk, cancer predisposition, carrier status, mental health and reproductive findings hidden until unlocked_categories names them. hidden_categories are never shown at any level. Then import a DNA raw data file or an Apple Health export.xml with caduceus_capture op=import, and ask a question.")
    ("data_dir" :path
     :doc "the folder this cell keeps its vault in: records, genotypes, samples and sources, as plain tab-separated files")
    ("privacy_level" :choice :options '("open" "standard") :default "open"
     :doc "open shows every category unless you hid it; standard keeps the sensitive categories hidden until you unlock them")
    ("hidden_categories" :list
     :doc "categories never shown at any level: alzheimers, cancer_risk, carrier, mental_health, reproductive, pgx, trait, wellness, records, medication, research, general")
    ("unlocked_categories" :list
     :doc "sensitive categories to show at the standard level"))
  (:settings #'read-settings)
  (:help :caduceus
         "caduceus: a private local vault of your DNA, Apple Health numbers, medications and notes - caduceus_search, caduceus_genome, caduceus_trend, caduceus_capture"
         *manual*)
  (:tool "caduceus_search" (nlk:contract 'search-request) #'search-vault)
  (:tool "caduceus_genome" (nlk:contract 'genome-request) #'read-genome)
  (:tool "caduceus_trend" (nlk:contract 'trend-request) #'read-trend)
  (:tool "caduceus_capture" (nlk:contract 'capture-request) #'capture)
  (:command "caduceus" 'caduceus-command
            :description "Your health vault: status, a search, or a gene-drug answer"
            :argument-hint "status | search WORDS | pgx DRUG")
  (:start (lambda ()
            (let ((dir (data-dir)))
              (nle:notice (format nil "caduceus: vault at ~a, privacy level ~a"
                                   (namestring dir) (getf (policy) :level))
                          :key "nodecode-caduceus")
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-caduceus"))))))
)
