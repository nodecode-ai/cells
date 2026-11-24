;;;; sources.lisp --- the registries: Crossref, doi.org, PubMed, arXiv.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call is read-only, metadata-only, and goes to a public scholarly
;;;; API. The only thing sent is the reference itself -- an id, or a title
;;;; with its first author and year -- never conversation text, files, or
;;;; anything about this machine but the honest user agent below. A registry
;;;; that cannot answer is a registry that answered nothing: it never makes a
;;;; reference look fake.

(in-package #:nodecode-citation-guard)

(defparameter +user-agent+
  "citation-guard/0.1.0 (Nodecode cell; +https://github.com/ChienNguyen23/hermes-citation-guard)"
  "What every registry is told this is. Honest, and the same string the
original sent, so the registries that police their polite pool still see one
client.")

(defstruct (client (:constructor make-client (&key mailto timeout (ncbi-key "") (cache nil))))
  (mailto "")
  (timeout 8)
  (ncbi-key "")
  (cache nil))

(defvar *client* nil
  "The client the running cell checks with; NIL outside a start.")

(defun registry-client () (or *client* (make-client)))

;;; --- transport ---------------------------------------------------------------

(defun url-encode (text &key (keep ""))
  "TEXT percent-encoded for a query or a path; the characters in KEEP ride as
  they are."
  (with-output-to-string (out)
    (loop for char across (or text "")
          do (if (or (alphanumericp char) (find char (concatenate 'string "-._~" keep))
                     (> (char-code char) 127))
                 (write-char char out)
                 (format out "%~2,'0X" (char-code char))))))

(defun registry-headers (accept &key key-header)
  `(("user-agent" . ,+user-agent+)
    ("accept" . ,accept)
    ,@(when key-header (list key-header))))

(defun cache (client) (client-cache (or client (registry-client))))

(defun cached (url)
  (let ((table (cache *client*)))
    (when table (gethash url table))))

(defun keep (url body)
  (let ((table (cache *client*)))
    (when (and table body) (setf (gethash url table) body))))

(defun get-text (url &key (accept "application/json") key-header)
  "URL's body when the registry answered 200, NIL when it answered 404 (nothing
is there). Any other status signals: a registry that could not be read is
reported as not checked, never as a fake reference."
  (or (cached url)
      (multiple-value-bind (body status)
          (nlk:http :get url :headers (registry-headers accept :key-header key-header)
                          :timeout (client-timeout (registry-client)))
        (cond ((= status 200) (keep url body) body)
              ((member status '(404 410)) nil)
              (t (error "the registry answered HTTP ~d" status))))))

(defun get-json (url &key key-header)
  "URL decoded as JSON, or NIL when the registry holds nothing there."
  (let ((body (get-text url :key-header key-header)))
    (when body (nlk:decode-json body))))

(defun dig (object &rest path)
  "OBJECT walked by PATH: string keys into objects, integers into arrays."
  (loop for step in path
        do (setf object (cond ((hash-table-p object) (gethash step object))
                              ((and (integerp step) (vectorp object) (not (stringp object))
                                    (< step (length object)))
                               (aref object step))
                              (t nil)))
        finally (return object)))

(defun first-string (value)
  "VALUE when it is a string, else its first element when it is an array."
  (cond ((stringp value) value)
        ((and (vectorp value) (plusp (length value))) (aref value 0))
        (t nil)))

(defun one-line (text &key (cap 400))
  "TEXT on one line, at most CAP characters."
  (let ((text (nlk:one-line (or text "") :cap cap)))
    (string-trim " \t" (substitute #\Space #\Newline text))))

(defun clean-title (text)
  "TEXT as a title a person reads: whitespace collapsed, tags and math taken
out, a trailing period dropped."
  (let ((text (one-line (or text "") :cap 300)))
    (setf text (string-replace-all text "<i>" ""))
    (setf text (string-replace-all text "</i>" ""))
    (setf text (string-replace-all text "<sub>" ""))
    (setf text (string-replace-all text "</sub>" ""))
    (setf text (string-replace-all text "<sup>" ""))
    (setf text (string-replace-all text "</sup>" ""))
    (string-right-trim "." text)))

;;; --- Crossref ------------------------------------------------------------------

(defun crossref-records (items)
  (loop for work across (or items #())
        collect (crossref-record work)))

(defun crossref-record (work)
  "One Crossref work as the plist this cell compares: title, year, first
author, doi, kind."
  (when (hash-table-p work)
    (list :title (clean-title (first-string (dig work "title")))
          :year (crossref-year work)
          :author (dig work "author" 0 "family")
          :author-or-given (or (dig work "author" 0 "family") (dig work "author" 0 "name"))
          :doi (dig work "DOI")
          :kind (dig work "type")
          :container (first-string (dig work "container-title"))
          :in-update (crossref-update-kind work))))

(defun crossref-year (work)
  (loop for key in '("issued" "published-print" "published-online" "published" "created")
        for year = (dig work key "date-parts" 0 0)
        when (integerp year) return year))

(defun crossref-update-kind (work)
  "The kind of update WORK itself is, when it is a notice: retraction and the
like. NIL for an ordinary paper."
  (let ((kind (dig work "type")))
    (and (member kind '("retraction" "withdrawal" "removal" "expression_of_concern")
                 :test #'string=)
         kind)))

(defun crossref-work (doi)
  "The Crossref record for DOI, or NIL when Crossref holds no such DOI."
  (let* ((url (format nil "https://api.crossref.org/works/~a" (url-encode doi :keep "/")))
         (body (get-text url)))
    (when body (crossref-record (dig (nlk:decode-json body) "message")))))

(defun crossref-query-url (parameters)
  (format nil "https://api.crossref.org/works?~{~a~^&~}"
          (append parameters
                  (let ((mailto (client-mailto (registry-client))))
                    (when (plusp (length mailto))
                      (list (format nil "mailto=~a" (url-encode mailto))))))))

(defun crossref-search (text limit)
  "Crossref's closest works to TEXT, best first."
  (let* ((url (crossref-query-url
               (list (format nil "query.bibliographic=~a" (url-encode (one-line text :cap 500)))
                     (format nil "rows=~d" limit)
                     "select=DOI,title,author,issued,type,container-title,update-to")))
         (body (get-text url)))
    (when body (crossref-records (dig (nlk:decode-json body) "message" "items")))))

(defun crossref-updates (doi)
  "The notices Crossref holds that update DOI, as a plist of :kind and :doi."
  (let* ((url (crossref-query-url
               (list (format nil "filter=updates:~a" (url-encode doi :keep "/"))
                     "rows=20" "select=DOI,type,update-to")))
         (body (get-text url)))
    (when body
      (loop for work across (or (dig (nlk:decode-json body) "message" "items") #())
            for notice = (dig work "DOI")
            append (loop for update across (or (dig work "update-to") #())
                         collect (list :kind (dig update "type") :doi notice))))))

;;; --- doi.org, for the agencies Crossref does not index ----------------------------

(defun datacite-work (doi)
  "DOI's metadata from the registration agency that minted it, as a Crossref-
shaped plist, or NIL. DataCite, mEDRA and the rest answer the content
negotiation doi.org serves; a DOI none of them knows is one that does not
exist."
  (let* ((url (format nil "https://doi.org/~a" (url-encode doi :keep "/")))
         (body (get-text url :accept "application/vnd.citationstyles.csl+json")))
    (when body
      (handler-case
          (let ((record (nlk:decode-json body)))
            (when (hash-table-p record)
              (list :title (clean-title (first-string (dig record "title")))
                    :year (or (dig record "issued" "date-parts" 0 0)
                              (let ((year (dig record "issued" "year"))) (and (integerp year) year)))
                    :author (dig record "author" 0 "family")
                    :author-or-given (or (dig record "author" 0 "family")
                                         (dig record "author" 0 "name"))
                    :doi doi :kind (dig record "type") :container (dig record "container-title"))))
        (error () nil)))))

;;; --- PubMed --------------------------------------------------------------------

(defun eutils-url (path parameters)
  (format nil "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/~a?~{~a~^&~}"
          path
          (append parameters
                  (let ((key (client-ncbi-key (registry-client))))
                    (when (plusp (length key))
                      (list (format nil "api_key=~a" (url-encode key))))))))

(defun pubmed-search (text limit)
  "PubMed's closest records to TEXT, best first, as plists."
  (let* ((url (eutils-url "esearch.fcgi"
                          (list "db=pubmed" "retmode=json"
                                (format nil "retmax=~d" limit)
                                (format nil "term=~a" (url-encode (one-line text :cap 400))))))
         (body (get-text url))
         (ids (and body (coerce (or (dig (nlk:decode-json body) "esearchresult" "idlist") #()) 'list))))
    (when ids (pubmed-summaries ids))))

(defun pubmed-summaries (ids)
  (let* ((url (eutils-url "esummary.fcgi"
                          (list "db=pubmed" "retmode=json"
                                (format nil "id=~{~a~^,~}" ids))))
         (body (get-text url))
         (result (and body (dig (nlk:decode-json body) "result"))))
    (when result
      (loop for id in ids
            for record = (dig result id)
            when (hash-table-p record) collect (pubmed-record id record)))))

(defun pubmed-record (pmid record)
  (let ((kinds (coerce (or (dig record "pubtype") #()) 'list)))
    (list :title (clean-title (dig record "title"))
          :year (let ((text (dig record "pubdate"))) (and text (find-year text)))
          :author (let ((name (dig record "authors" 0 "name")))
                    (and name (first (words name))))
          :author-or-given (dig record "authors" 0 "name")
          :pmid pmid :kind (first kinds)
          :container (dig record "source")
          :in-update (cond ((member "Retracted Publication" kinds :test #'string=) "retraction")
                           ((member "Expression of Concern" kinds :test #'string=)
                            "expression_of_concern")))))

(defun pubmed-work (pmid)
  (first (pubmed-summaries (list pmid))))

;;; --- arXiv ---------------------------------------------------------------------

(defun atom-entries (body)
  "BODY's Atom entries, each as ((TAG . TEXT) ...), read without an XML library."
  (let ((entries '()) (start 0))
    (loop
      (let ((at (search "<entry>" body :start2 start)))
        (unless at (return (nreverse entries)))
        (let ((close (search "</entry>" body :start2 at)))
          (unless close (return (nreverse entries)))
          (push (atom-fields (subseq body (+ at 7) close)) entries)
          (setf start (+ close 8)))))))

(defun atom-fields (chunk)
  (loop for tag in '("title" "published" "name" "id")
        for value = (atom-field chunk tag)
        when value collect (cons tag value)))

(defun atom-field (chunk tag)
  "CHUNK's first <TAG> element as text, or NIL."
  (let* ((open (search (format nil "<~a" tag) chunk))
         (from (and open (position #\> chunk :start open)))
         (close (and from (search (format nil "</~a>" tag) chunk :start2 from))))
    (when (and from close)
      (let ((raw (subseq chunk (1+ from) close)))
        (clean-title (string-replace-all (string-replace-all raw "&amp;" "&")
                                         "  " " "))))))

(defun arxiv-record (entry)
  (let ((id (cdr (assoc "id" entry :test #'string=))))
    (list :title (clean-title (cdr (assoc "title" entry :test #'string=)))
          :year (find-year (or (cdr (assoc "published" entry :test #'string=)) ""))
          :author (let ((name (cdr (assoc "name" entry :test #'string=))))
                    (and name (car (last (words name)))))
          :author-or-given (cdr (assoc "name" entry :test #'string=))
          :arxiv (and id (last-path-segment id))
          :kind "preprint")))

(defun last-path-segment (url)
  (let ((slash (position #\/ url :from-end t)))
    (string-trim " " (if slash (subseq url (1+ slash)) url))))

(defun arxiv-work (id)
  "arXiv's record for ID, or NIL."
  (let* ((url (format nil "https://export.arxiv.org/api/query?id_list=~a"
                       (url-encode id :keep "./")))
         (body (get-text url :accept "application/atom+xml")))
    (when body (first (mapcar #'arxiv-record (atom-entries body))))))

(defun arxiv-search (text limit)
  "arXiv's closest records to TEXT, best first."
  (let* ((url (format nil "https://export.arxiv.org/api/query?search_query=~a&max_results=~d"
                       (url-encode (format nil "ti:\"~a\"" (one-line text :cap 200)))
                       limit))
         (body (get-text url :accept "application/atom+xml")))
    (when body (mapcar #'arxiv-record (atom-entries body)))))
