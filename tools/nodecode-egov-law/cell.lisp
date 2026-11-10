;;;; cell.lisp --- the primer, the section, the three tools and /egov-law.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :egov-law) answers the primer below, and every session's help
;;;; section carries one line naming it. The tools answer JSON, as the
;;;; original plugin's did, so a turn can cite source.url and the credit.

(in-package #:nodecode-egov-law)

(defparameter +primer+
  "Japanese statutes are available through the nodecode-egov-law cell, from e-Gov's
Law API (the Digital Agency of Japan's official database). Three tools, each read-only,
no key:
  egov_law_search    find a law by the name a person uses: official title (労働基準法),
      e-Gov abbreviation (労基法), common short name (下請法, 憲法), law number
      (昭和二十二年法律第四十九号) or law ID (322AC0000000049). mode text finds a phrase
      inside law text and answers laws with snippets, no article numbers.
  egov_law_article   read one article: law and article as digits or as written
      (32, 第32条, 三十二条, 32の2, 附則第143条), optional paragraph, as_of
      (YYYY-MM-DD, 2017-04-01 or later) for the wording in force that day, and
      compare_with for whether it reads differently then; max_chars cuts the text.
  egov_law_revisions when amendments take effect: next_amendment, scheduled_amendments,
      in_force_now, earlier_versions. Each row's check pair (as_of, compare_with) is
      what egov_law_article wants to show whether that amendment changes an article.
Show source.attribution and url with any law text you quote, and do not write an article
from memory: e-Gov's terms ask for the credit. If a law name matches several laws, the
answer is an error with the candidates' law IDs; pick one and call again. Bills still in
the Diet are not in e-Gov, so silence about an amendment is not proof there is none.
The operator reads the same data with /egov-law LAW [ARTICLE]."
  "What (help :egov-law) answers while the cell runs.")

;;; --- the schemas the model reads ------------------------------------------------

(defparameter +law-argument+
  (nlk:make-json-object
   "type" "string"
   "description" (concatenate 'string
                               "The law, written any way a person would: official title (労働基準法), "
                               "e-Gov abbreviation (労基法), common short name (下請法, 憲法), law number "
                               "(昭和二十二年法律第四十九号 or 昭和22年法律第49号) or law ID (322AC0000000049). "
                               "An exact title or abbreviation always wins. A partial name that matches "
                               "several laws answers an error listing the candidates' law IDs.")))

(defparameter +search-schema+
  (list :description
        (concatenate 'string
                     "Japan laws (e-Gov): find a statute by name. Read-only. source.url is the e-Gov top page. "
                     "mode title matches names and abbreviations; mode text finds a phrase and answers sentences "
                     "without article numbers, so call egov_law_article next. "
                     "If you did not call this tool, say you did not search and do not invent a law number.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "query" (nlk:make-json-object
                   "type" "string"
                   "description" "Part of a law name, abbreviation, law number or ID; with mode text, a word or phrase inside law text.")
          "mode" (nlk:make-json-object
                  "type" "string" "enum" (vector "title" "text") "default" "title"
                  "description" "title matches names and abbreviations; text matches the body of laws.")
          "limit" (nlk:make-json-object
                   "type" "integer" "minimum" 1 "maximum" 50 "default" 10
                   "description" "How many laws to return (default 10, max 50). Exact titles and Acts come first.")
          "include_repealed" (nlk:make-json-object
                               "type" "boolean" "default" nil
                               "description" "Include repealed laws in a title search (default false: laws in force only)."))
         "required" (vector))))

(defparameter +article-schema+
  (list :description
        (concatenate 'string
                     "Japan laws (e-Gov): read one article's text. Read-only. Answers the wording, source.as_of, "
                     "source.retrieved_at and source.url; show source.attribution with any text you quote. "
                     "Pass the law by name or abbreviation and the article as digits (32, 32-2). "
                     "To compare an amendment, pass as_of and compare_with from egov_law_revisions. "
                     "It reads the main provisions and the law's own 附則, not 別表 or forms. "
                     "If you did not call this tool, say you did not fetch it and do not write the article from memory.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "law" +law-argument+
          "article" (nlk:make-json-object
                     "type" "string"
                     "description" "Article number as digits or as written: 32, 第32条, 三十二条, 32の2. A paragraph may be written in: 第32条第2項. 附則第143条 reads that article of the law's own supplementary provisions; 附則 alone reads one without articles.")
          "paragraph" (nlk:make-json-object
                       "type" "integer" "minimum" 1
                       "description" "Optional paragraph (項) number to return only that paragraph; it overrides a paragraph written inside article.")
          "as_of" (nlk:make-json-object
                    "type" "string"
                    "description" "YYYY-MM-DD on or after 2017-04-01: a past date returns the text in force that day, a future date the text as already-passed amendments will make it read. Leave out for the current text.")
          "compare_with" (nlk:make-json-object
                           "type" "string"
                           "description" "YYYY-MM-DD on or after 2017-04-01: adds comparison, whether the article reads differently on that date than on as_of (today if omitted), and the wording there.")
          "max_chars" (nlk:make-json-object
                        "type" "integer" "minimum" 300 "maximum" 20000 "default" 4000
                        "description" "Cut the text after this many characters (default 4000). If cut, the answer says truncated=true."))
         "required" (vector))))

(defparameter +revisions-schema+
  (list :description
        (concatenate 'string
                     "Japan laws (e-Gov): when an amendment takes effect. Read-only. Answers enforcement dates, "
                     "the law's e-Gov page URL and source.retrieved_at. Each row's check pair (as_of, "
                     "compare_with) is what egov_law_article wants to show whether an article changes. "
                     "Bills still in the Diet, and changes to a separate 施行令 or 施行規則, are not included. "
                     "If you did not call this tool, say you did not fetch the history and do not invent an act number.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "law" +law-argument+
          "limit" (nlk:make-json-object
                   "type" "integer" "minimum" 1 "maximum" 50 "default" 10
                   "description" "Max entries in each list (scheduled amendments, earlier versions); default 10, max 50."))
         "required" (vector))))

;;; --- the operator's command ------------------------------------------------------

(defun article-text (law article)
  "LAW's ARTICLE as the operator reads it on the command line."
  (let ((answer (article law article :max-chars 1200)))
    (format nil "~a~@[（~a）~] ~a~%~a~%~a~%~@[cut: ~a~]~%source: ~a"
            (or (gethash "law_title" answer) law)
            (gethash "law_num" answer)
            (gethash "article" answer)
            (or (gethash "caption" answer) "")
            (gethash "text" answer)
            (and (gethash "truncated" answer)
                 (format nil "at ~a of ~a characters" (setting :max-chars) (gethash "total_chars" answer)))
            (or (gethash "url" answer) +site+))))

(defun search-text (query)
  "The laws whose title matches QUERY, as the operator reads them."
  (let* ((answer (search-laws query :limit 10))
         (results (gethash "results" answer))
         (rows (loop for result across (or results #())
                     collect (format nil "  ~a  [~a]~@[ — ~a~]"
                                      (gethash "law_title" result)
                                      (gethash "law_id" result)
                                      (gethash "law_num" result)))))
    (if rows
        (format nil "~{~a~%~}" rows)
        (format nil "no law in force has a title containing ~a; try a shorter part of the name" query))))

(defun run-command (arguments session-id)
  "One /egov-law LAW [ARTICLE] invocation => the text the shell shows."
  (declare (ignore session-id))
  (let* ((words (remove "" (ppcre:split "\\s+" (nlk:trimmed (or arguments ""))) :test #'string=)))
    (handler-case
        (cond ((null words) (format nil "~a" +primer+))
              ((cdr words) (article-text (first words) (second words)))
              (t (search-text (first words))))
      (egov-law-error (condition)
        (format nil "egov-law: ~a" (egov-law-error-detail condition))))))

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell egov-law
  (:section ("egov-law")
    (:guide "nothing here is required: e-Gov's Law API is public, with no key; endpoint is the API base, timeout_seconds one request's ceiling, max_chars how much of an article the model reads by default")
    ("endpoint" :string :default "https://laws.e-gov.go.jp/api/2"
     :doc "the e-Gov Law API v2 base; leave it alone unless e-Gov moves it")
    ("timeout_seconds" :integer :default 20 :min 5
     :doc "one request's ceiling in seconds")
    ("max_chars" :integer :default 4000 :min 300
     :doc "how much of one article egov_law_article returns by default (300 to 20000)"))
  (:help :egov-law
         "egov-law: read Japanese statutes from e-Gov - egov_law_search, egov_law_article, egov_law_revisions (help :egov-law)"
         +primer+)
  (:command "egov-law" 'run-command
            :description "Japanese law from e-Gov: a law's articles, or search by name"
            :argument-hint "LAW [ARTICLE]")
  (:tool "egov_law_search" +search-schema+ 'egov_law_search)
  (:tool "egov_law_article" +article-schema+ 'egov_law_article)
  (:tool "egov_law_revisions" +revisions-schema+ 'egov_law_revisions))
