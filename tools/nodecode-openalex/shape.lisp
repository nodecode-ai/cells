;;;; shape.lisp --- turning an OpenAlex record into something a model can read.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two levers, and they work at different layers. SELECT happens
;;;; server-side and costs nothing extra, because OpenAlex prices by call shape
;;;; rather than by bytes: a 25-record page is about 1.5 MB whole and 6.5 KB
;;;; with five fields selected. Like the API, select takes top-level names
;;;; only, so it cannot reach into authorships -- the single biggest field in a
;;;; work, and the one trimmed here. An abstract arrives as an inverted index,
;;;; a term-to-positions map twice the size of the prose it encodes, so
;;;; rebuilding it both halves the bytes and spares the model a puzzle.
;;;;
;;;; The original answered JSON. This answers lines: the same facts, with the
;;;; mapping and the building already done, because a model reading a paper
;;;; list wants the paper list and not a hash table.

(in-package #:nodecode-openalex)

;;; --- small helpers ----------------------------------------------------------

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a [...]" (subseq text 0 limit)))))

(defun join (list separator)
  "LIST's elements as one string, SEPARATOR between them."
  (format nil (format nil "~~{~a~~^~a~~}" "~a" separator) list))

(defun replace-string (old new text)
  "TEXT with every occurrence of OLD replaced by NEW."
  (with-output-to-string (out)
    (loop with size = (length old)
          for start = 0 then (+ at size)
          for at = (search old text :start2 start)
          while at
            do (write-string text out :start start :end at)
               (write-string new out)
          finally (write-string text out :start start))))

(defun object (value key)
  "KEY of VALUE when it is a JSON object, else NIL."
  (and (hash-table-p value) (nlk:json-value value :object key)))

(defun text-of (value key)
  "KEY of VALUE as text, or NIL."
  (and (hash-table-p value) (nlk:json-value value :text key)))

(defun number-of (value key)
  "KEY of VALUE as a number, or NIL."
  (and (hash-table-p value) (nlk:json-value value :number key)))

(defun short-id (value)
  "https://openalex.org/W2741809807 as W2741809807: the bare id is what every
follow-up call wants, and a third of the length."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and (plusp (length text))
         (let ((slash (position #\/ text :from-end t)))
           (if slash (subseq text (1+ slash)) text)))))

(defun bare-doi (value)
  "A DOI without the resolver prefix."
  (let ((text (and (stringp value) value)))
    (and text
         (if (eql 0 (search "https://doi.org/" text))
             (subseq text (length "https://doi.org/"))
             text))))

(defun reconstruct-abstract (inverted &optional (limit 1500))
  "Rebuild prose from OpenAlex's inverted index: each term maps to the
positions it occupies, so inverting it recovers the original word order.
Positions can have gaps, which is why this walks the sorted keys."
  (when (hash-table-p inverted)
    (let ((positions (make-hash-table :test #'eql :size 512)) (any nil))
      (maphash (lambda (term indexes)
                 (when (and (stringp term) (vectorp indexes))
                   (loop for index across indexes
                         when (integerp index)
                           do (setf (gethash index positions) term any t))))
               inverted)
      (when any
        (let ((words (loop for key being the hash-keys of positions
                             using (hash-value term) collect (cons key term))))
          (clip (join (mapcar #'cdr (sort words #'< :key #'car)) " ") limit))))))

(defun entity-of-id (identifier fallback)
  "The entity an identifier belongs to, from its prefix or its leading letter."
  (let* ((text (nlk:trimmed identifier))
         (colon (position #\: text)))
    (cond
      ((and colon (plusp colon))
       (let ((prefix (string-downcase (subseq text 0 colon))))
         (cond ((member prefix '("doi" "pmid" "pmcid" "mag") :test #'equal) "works")
               ((string= prefix "orcid") "authors")
               ((string= prefix "ror") "institutions")
               ((string= prefix "issn") "sources")
               ((string= prefix "openalex")
                (entity-of-id (subseq text (1+ colon)) fallback))
               (t fallback))))
      ((and (>= (length text) 2)
            (digit-char-p (char text 1))
            (assoc (char-upcase (char text 0))
                   '((#\W . "works") (#\A . "authors") (#\S . "sources")
                     (#\I . "institutions") (#\T . "topics") (#\P . "publishers")
                     (#\F . "funders") (#\K . "keywords") (#\C . "concepts"))))
       (cdr (assoc (char-upcase (char text 0))
                   '((#\W . "works") (#\A . "authors") (#\S . "sources")
                     (#\I . "institutions") (#\T . "topics") (#\P . "publishers")
                     (#\F . "funders") (#\K . "keywords") (#\C . "concepts")))))
      (t fallback))))

(defun normalize-id (value)
  "An identifier in the free, prefixed form every call wants. A bare DOI with
no prefix 404s, and the URL form of an id is billed while the prefixed form is
free, so whichever shape the call was given is coerced here."
  (let ((text (nlk:trimmed (or value ""))))
    (cond
      ((zerop (length text)) "")
      ((some (lambda (prefix) (eql 0 (search prefix (string-downcase text))))
             '("openalex:" "doi:" "pmid:" "pmcid:" "mag:" "orcid:" "ror:" "issn:"))
       text)
      ((and (> (length text) 1)
            (digit-char-p (char text 1))
            (find (char-upcase (char text 0)) "WASITCPFK"))
       (string-upcase text))
      (t (or (loop for (host prefix) in '(("doi.org/" "doi:") ("orcid.org/" "orcid:")
                                           ("ror.org/" "ror:") ("openalex.org/" ""))
                   for at = (search host (string-downcase text))
                   when at
                     return (let ((tail (subseq text (+ at (length host)))))
                              (if (plusp (length prefix))
                                  (concatenate 'string prefix tail)
                                  (string-upcase tail))))
             (if (and (eql 0 (search "10." text)) (find #\/ text))
                 (concatenate 'string "doi:" text)
                 text))))))

(defun normalize-sort (value)
  "OpenAlex accepts field:asc and field:desc; its own OpenAPI description
advertises a -field form that 400s, so it is rewritten here."
  (let ((text (nlk:trimmed (or value ""))))
    (cond ((zerop (length text)) nil)
          ((eql 0 (search "-" text)) (concatenate 'string (subseq text 1) ":desc"))
          (t text))))

(defun clamp-per-page (value default)
  "OpenAlex allows 1 to 200 even where its llms.txt says 100."
  (max 1 (min 200 (or (and (integerp value) value) default))))

;;; --- what to ask for ----------------------------------------------------------

(defparameter +work-select-summary+
  '("id" "doi" "display_name" "publication_year" "type" "cited_by_count"
    "authorships" "primary_location" "open_access" "primary_topic" "is_retracted"))

(defparameter +work-select-detail+
  (append +work-select-summary+
          '("abstract_inverted_index" "publication_date" "topics" "keywords"
            "referenced_works_count" "fwci" "biblio" "best_oa_location" "language"
            "sustainable_development_goals" "ids")))

(defparameter +author-select+
  '("id" "orcid" "display_name" "works_count" "cited_by_count" "summary_stats"
    "last_known_institutions" "affiliations"))

(defparameter +source-select+
  '("id" "issn_l" "display_name" "type" "publisher" "host_organization_name"
    "is_oa" "is_in_doaj" "works_count" "cited_by_count" "summary_stats" "apc_usd"
    "country_code"))

(defparameter +institution-select+
  '("id" "ror" "display_name" "country_code" "type" "homepage_url" "works_count"
    "cited_by_count" "summary_stats" "geo"))

(defun select-for (entity verbosity)
  "The select value to send, or NIL at raw verbosity: raw means whatever
OpenAlex holds, and a select list would quietly define that for the caller."
  (unless (string= verbosity "raw")
    (let ((fields (cdr (assoc entity
                               (list (cons "works" (if (string= verbosity "detail")
                                                       +work-select-detail+
                                                       +work-select-summary+))
                                     (cons "authors" +author-select+)
                                     (cons "sources" +source-select+)
                                     (cons "institutions" +institution-select+))
                               :test #'equal))))
      (and fields (join fields ",")))))

;;; --- rendering -----------------------------------------------------------------

(defun authors-block (authorships verbosity)
  "The author list compressed to one line: it is the biggest field in a work."
  (let* ((rows (and (vectorp authorships) (coerce authorships 'list)))
         (total (length rows))
         (cap (if (string= verbosity "detail") 25 10))
         (names (loop for entry in (subseq rows 0 (min cap total))
                      for author = (object entry "author")
                      for name = (text-of author "display_name")
                      when name collect name)))
    (when names
      (format nil "~a~@[ (+~d more of ~d)~]"
              (join names ", ")
              (and (> total cap) (- total cap))
              (and (> total cap) total)))))

(defun work-block (raw verbosity)
  "One work as the lines the model reads."
  (let* ((location (object raw "primary_location"))
         (source (object location "source"))
         (oa (object raw "open_access"))
         (topic (object raw "primary_topic"))
         (field (object topic "field"))
         (doi (bare-doi (text-of raw "doi")))
         (citation (number-of raw "cited_by_count"))
         (title (or (text-of raw "display_name") (text-of raw "title") "(untitled)")))
    (with-output-to-string (out)
      (format out "~a~@[ (~a)~]"
              title
              (or (number-of raw "publication_year") (text-of raw "publication_date")))
      (format out "~%  ~a~@[ · ~a~]~@[ · ~d citations~]~@[ · ~a~]~@[ · RETRACTED~]"
              (or (short-id (text-of raw "id")) "?")
              (text-of raw "type")
              citation
              (text-of oa "oa_status")
              (and (nlk:json-value raw :boolean "is_retracted") t))
      (let ((authors (authors-block (nlk:json-value raw :array "authorships") verbosity)))
        (when authors (format out "~%  authors: ~a" authors)))
      (let ((venue (text-of source "display_name"))
            (venue-id (short-id (text-of source "id"))))
        (when (or venue doi)
          (format out "~%  venue: ~a~@[ (~a)~]~@[ · doi:~a~]"
                  (or venue "-") venue-id doi)))
      (when (or (text-of topic "display_name") (text-of field "display_name"))
        (format out "~%  topic: ~a~@[ (field: ~a)~]"
                (or (text-of topic "display_name") "-")
                (text-of field "display_name")))
      (when (text-of oa "oa_url")
        (format out "~%  open access: ~a" (text-of oa "oa_url")))
      (when (string= verbosity "detail")
        (let ((abstract (reconstruct-abstract (nlk:json-value raw :object "abstract_inverted_index")))
              (keywords (loop for keyword across (or (nlk:json-value raw :array "keywords") #())
                              for name = (text-of keyword "display_name")
                              when name collect name))
              (topics (loop for item across (or (nlk:json-value raw :array "topics") #())
                            for name = (text-of item "display_name")
                            when name collect name)))
          (when abstract (format out "~%  abstract: ~a" abstract))
          (when keywords (format out "~%  keywords: ~a" (join (subseq keywords 0 (min 8 (length keywords))) ", ")))
          (when topics (format out "~%  topics: ~a" (join (subseq topics 0 (min 5 (length topics))) ", ")))
          (when (number-of raw "referenced_works_count")
            (format out "~%  references: ~d" (number-of raw "referenced_works_count")))
          (when (text-of raw "language")
            (format out "~%  language: ~a" (text-of raw "language")))))
      (format out "~%"))))

(defun author-block (raw verbosity)
  "One author as the lines the model reads."
  (declare (ignore verbosity))
  (let* ((stats (object raw "summary_stats"))
         (institutions (loop for item across (or (nlk:json-value raw :array "last_known_institutions") #())
                             for name = (text-of item "display_name")
                             when name collect name)))
    (with-output-to-string (out)
      (format out "~a ~a~@[ · orcid:~a~]"
              (or (text-of raw "display_name") "(unnamed)")
              (or (short-id (text-of raw "id")) "?")
              (short-id (text-of raw "orcid")))
      (format out "~%  ~@[~d works · ~]~@[~d citations~]~@[ · h-index ~d~]~@[ · i10 ~d~]"
              (number-of raw "works_count")
              (number-of raw "cited_by_count")
              (number-of stats "h_index")
              (number-of stats "i10_index"))
      (when institutions
        (format out "~%  institutions: ~a" (join (subseq institutions 0 (min 3 (length institutions))) ", ")))
      (format out "~%"))))

(defun source-block (raw verbosity)
  "One journal, repository or conference as the lines the model reads."
  (declare (ignore verbosity))
  (with-output-to-string (out)
    (format out "~a ~a~@[ · ~a~]~@[ · ~a~]"
            (or (text-of raw "display_name") "(unnamed)")
            (or (short-id (text-of raw "id")) "?")
            (text-of raw "type")
            (text-of raw "host_organization_name"))
    (format out "~%  ~@[issn ~a · ~]~@[~d works · ~]~@[~d citations~]~@[ · in DOAJ~]~@[ · open access~]"
            (text-of raw "issn_l")
            (number-of raw "works_count")
            (number-of raw "cited_by_count")
            (and (nlk:json-value raw :boolean "is_in_doaj") t)
            (and (nlk:json-value raw :boolean "is_oa") t))
    (format out "~%")))

(defun institution-block (raw verbosity)
  "One institution as the lines the model reads."
  (declare (ignore verbosity))
  (with-output-to-string (out)
    (format out "~a ~a~@[ · ~a~]~@[ · ~a~]"
            (or (text-of raw "display_name") "(unnamed)")
            (or (short-id (text-of raw "id")) "?")
            (text-of raw "country_code")
            (text-of raw "type"))
    (format out "~%  ~@[~d works · ~]~@[~d citations~]~@[ · ~a~]"
            (number-of raw "works_count")
            (number-of raw "cited_by_count")
            (text-of raw "homepage_url"))
    (format out "~%")))

(defun generic-block (raw verbosity)
  "Any collection the original did not shape: the name, the id and the counts."
  (declare (ignore verbosity))
  (with-output-to-string (out)
    (format out "~a ~a~@[ · ~a~]"
            (or (text-of raw "display_name") (text-of raw "id") "(unnamed)")
            (or (short-id (text-of raw "id")) "?")
            (text-of raw "type"))
    (format out "~%  ~@[~d works · ~]~@[~d citations~]"
            (number-of raw "works_count")
            (number-of raw "cited_by_count"))
    (format out "~%")))

(defun record-block (entity raw verbosity)
  "RAW -- one record of ENTITY -- as the lines the model reads."
  (funcall (or (cdr (assoc entity
                            '(("works" . work-block) ("authors" . author-block)
                              ("sources" . source-block)
                              ("institutions" . institution-block))
                            :test #'equal))
               #'generic-block)
           raw verbosity))

(defun raw-block (raw)
  "RAW at raw verbosity: everything OpenAlex holds, as compact JSON."
  (format nil "~a~%" (nlk:encode-json-object raw)))

(defun results-text (entity rows verbosity &key (limit 24000))
  "ROWS -- a vector of records -- as the text a tool answers with, clipped to
LIMIT characters in all."
  (let ((text (with-output-to-string (out)
                (loop for raw across (or rows #())
                      when (hash-table-p raw)
                        do (write-string (if (string= verbosity "raw")
                                             (raw-block raw)
                                             (record-block entity raw verbosity))
                                         out)))))
    (clip text limit)))

(defun group-lines (groups &key (limit 50))
  "A group_by response as lines: the label to read and the id to filter on next."
  (let ((rows (and (vectorp groups) (coerce groups 'list))))
    (with-output-to-string (out)
      (loop for group in (subseq rows 0 (min limit (length rows)))
            when (hash-table-p group)
              do (let ((key (text-of group "key"))
                       (label (text-of group "key_display_name")))
                   (format out "  ~a~@[ ~a~]  ~d~%"
                           (or label key "-")
                           (and (stringp key) (eql 0 (search "http" key)) (short-id key))
                           (or (number-of group "count") 0)))))))

(defparameter +price-keys+ '(("singleton" . "free") ("list" . "list") ("search" . "search")
                              ("text" . "text") ("content" . "content"))
  "How the API's own endpoint names map onto this cell's call classes.")

(defun live-prices (info)
  "INFO -- the rate_limit object -- as (CLASS . USD) pairs, one per class the API
prices. The table is authoritative and the constants in wire.lisp are only a
starting point, so it is adopted whenever the account answers."
  (let ((costs (and (hash-table-p info) (nlk:json-value info :object "endpoint_costs_usd"))))
    (when costs
      (loop for (endpoint . class) in +price-keys+
            for usd = (nlk:json-value costs :number endpoint)
            when (realp usd)
              collect (cons class (coerce usd 'double-float))))))

(defun rate-limit-report (body)
  "The budget fields of a /rate-limit answer. The live response nests them
under rate_limit even though the documentation says they are top level, so
both shapes are read."
  (let ((inner (object body "rate_limit")))
    (or inner body)))
