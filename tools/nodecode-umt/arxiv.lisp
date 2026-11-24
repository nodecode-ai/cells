;;;; arxiv.lisp --- arXiv: a keyword search, one paper, a category's newest.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One endpoint, https://export.arxiv.org/api/query, answers an Atom feed
;;;; for a search query (all:TERMS), a category (cat:cs.AI) or an id list;
;;;; it needs no key, asks only for a user agent. The feed is read with
;;;; regexps over the entries, which is the whole of what this cell needs
;;;; from it: id, title, summary, authors, categories, dates, and the two
;;;; links.

(in-package #:nodecode-umt)

(defparameter +arxiv-max+ 25 "How many papers one call may answer with.")

(defun arxiv-api-base () (string-right-trim "/" (setting :arxiv-api-base)))

;;; --- one entry -----------------------------------------------------------------

(defun xml-field (body tag)
  "BODY's TAG element as text: whitespace collapsed, entities decoded."
  (multiple-value-bind (whole groups)
      (ppcre:scan-to-strings (format nil "(?s)<~a[^>]*>(.*?)</~a>" tag tag) body)
    (declare (ignore whole))
    (when (and groups (plusp (length (aref groups 0))))
      (let ((text (nlk:trimmed (decode-entities (ppcre:regex-replace-all "\\s+" (aref groups 0) " ")))))
        (and (plusp (length text)) text)))))

(defun xml-attribute (attributes name)
  "NAME's value in an element's ATTRIBUTES, decoded, or NIL."
  (when (stringp attributes)
    (multiple-value-bind (whole groups)
        (ppcre:scan-to-strings (format nil "~a=\"([^\"]*)\"" name) attributes)
      (declare (ignore whole))
      (and groups (aref groups 0) (decode-entities (aref groups 0))))))

(defun arxiv-id (raw)
  "RAW's arXiv identifier, without the abs url around it."
  (multiple-value-bind (whole groups) (ppcre:scan-to-strings "/abs/([^?\\s<]+)" (or raw ""))
    (declare (ignore whole))
    (or (and groups (aref groups 0))
        (and (stringp raw) (nlk:trimmed raw)))))

(defun arxiv-authors (body)
  "BODY's author names, in the feed's order."
  (let ((names '()))
    (ppcre:do-register-groups (name) ("<author[^>]*>\\s*<name>([^<]*)</name>" body)
      (push (nlk:trimmed (decode-entities name)) names))
    (nreverse names)))

(defun arxiv-entry (body)
  "One Atom entry BODY as the plist the renderer reads."
  (let ((links '())
        (categories '()))
    (ppcre:do-register-groups (attributes) ("<link([^>]*)>" body)
      (push attributes links))
    (ppcre:do-register-groups (term) ("<category[^>]*term=\"([^\"]+)\"" body)
      (push (decode-entities term) categories))
    (let* ((links (nreverse links))
           (pdf (find-if (lambda (attributes)
                           (or (equal "pdf" (xml-attribute attributes "title"))
                               (equal "application/pdf" (xml-attribute attributes "type"))))
                         links))
           (abs (find-if (lambda (attributes) (equal "alternate" (xml-attribute attributes "rel")))
                         links))
           (raw-id (or (xml-field body "id") ""))
           (primary (multiple-value-bind (whole groups)
                        (ppcre:scan-to-strings "<arxiv:primary_category([^>]*)>" body)
                      (declare (ignore whole))
                      (and groups (xml-attribute (aref groups 0) "term")))))
      (list :id (arxiv-id raw-id)
            :title (or (xml-field body "title") "Untitled paper")
            :summary (or (xml-field body "summary") "")
            :authors (arxiv-authors body)
            :categories (nreverse categories)
            :primary primary
            :published (xml-field body "published")
            :updated (xml-field body "updated")
            :pdf (and pdf (xml-attribute pdf "href"))
            :abs (or (and abs (xml-attribute abs "href")) (and (plusp (length raw-id)) raw-id))))))

(defun arxiv-feed (parameters)
  "PARAMETERS an alist of (name . value) => the feed's entries as plists."
  (let* ((query (format nil "~{~a~^&~}"
                         (loop for (name . value) in parameters
                               collect (format nil "~a=~a" name (url-encode value)))))
         (url (format nil "~a?~a" (arxiv-api-base) query))
         (xml (http-get url :accept "application/atom+xml, application/xml;q=0.9, text/xml;q=0.8"))
         (papers '()))
    (ppcre:do-register-groups (body) ("(?s)<entry>(.*?)</entry>" xml)
      (push (arxiv-entry body) papers))
    (nreverse papers)))

;;; --- the rendering ---------------------------------------------------------------

(defparameter +arxiv-sorts+
  '(("relevance" . "relevance") ("submitteddate" . "submittedDate")
    ("lastupdateddate" . "lastUpdatedDate"))
  "What a caller may ask a search to sort by, as arXiv spells it.")

(defun arxiv-sort (sort)
  "SORT as arXiv's spelling, or a refusal naming the three."
  (or (cdr (assoc (string-downcase (princ-to-string (or sort "relevance"))) +arxiv-sorts+
                   :test #'equal))
      (fail "sort is relevance, submittedDate or lastUpdatedDate, not ~s" sort)))

(defun render-papers (heading papers &key (summary-limit 500))
  "HEADING, then one paper a block: id and date, title, authors, categories,
the abs and pdf urls, and the summary clipped to SUMMARY-LIMIT."
  (if (null papers)
      (format nil "~@[~a: ~]no papers found." heading)
      (with-output-to-string (out)
        (when heading (format out "~a (~d):~%" heading (length papers)))
        (loop for paper in papers
              for index from 1
              do (format out "~d. ~a~@[ (~a)~]~%" index (getf paper :title)
                         (or (getf paper :published) (getf paper :id)))
                 (format out "   ~a~@[ - ~{~a~^, ~}~]~%" (or (getf paper :id) "no id")
                         (getf paper :authors))
                 (when (getf paper :categories)
                   (format out "   ~{~a~^, ~}~@[ (primary ~a)~]~%"
                           (getf paper :categories) (getf paper :primary)))
                 (when (getf paper :abs) (format out "   ~a~%" (getf paper :abs)))
                 (when (getf paper :pdf) (format out "   ~a~%" (getf paper :pdf)))
                 (when (plusp (length (or (getf paper :summary) "")))
                   (format out "   ~a~%" (nlk:clip (getf paper :summary) summary-limit :ellipsis "...")))))))

;;; --- the verbs --------------------------------------------------------------------

(define-verb search-papers (query &key (max 5) (sort "relevance"))
  "Search arXiv for QUERY across every field: at most MAX papers (5 by
default, 25 at most), sorted by relevance, submittedDate or lastUpdatedDate.
Each answers its id, title, authors, categories, abs and pdf urls and a
summary."
  (let ((query (nlk:trimmed (princ-to-string query))))
    (unless (plusp (length query)) (fail "a search needs a query"))
    (render-papers (format nil "arXiv papers matching ~s" query)
                   (arxiv-feed (list (cons "search_query" (format nil "all:~a" query))
                                     (cons "start" 0)
                                     (cons "max_results" (bounded max 1 +arxiv-max+))
                                     (cons "sortBy" (arxiv-sort sort))
                                     (cons "sortOrder" "descending"))))))

(define-verb paper (id)
  "One arXiv paper by its identifier (2405.12345, or its versioned form):
title, authors, categories, dates, the abs and pdf urls, and the whole
abstract."
  (let ((id (nlk:trimmed (princ-to-string id))))
    (unless (plusp (length id)) (fail "a paper needs an arXiv identifier"))
    (let ((papers (arxiv-feed (list (cons "id_list" id) (cons "max_results" 1)))))
      (unless papers (fail "arXiv has no paper ~s" id))
      (render-papers nil papers :summary-limit 3000))))

(define-verb recent-papers (&key category (max 5))
  "The newest papers arXiv announced in CATEGORY (cs.AI when unset, from the
umt section's default_category): at most MAX papers (5 by default, 25 at
most), newest first."
  (let ((category (nlk:trimmed (princ-to-string (or category (setting :default-category) "cs.AI")))))
    (unless (plusp (length category)) (fail "a category is required, e.g. cs.AI"))
    (render-papers (format nil "Newest arXiv papers in ~a" category)
                   (arxiv-feed (list (cons "search_query" (format nil "cat:~a" category))
                                     (cons "start" 0)
                                     (cons "max_results" (bounded max 1 +arxiv-max+))
                                     (cons "sortBy" "submittedDate")
                                     (cons "sortOrder" "descending"))))))
