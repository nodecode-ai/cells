;;;; tools.lisp --- the two tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin served Hermes's two web verbs: web_search over the documents
;;;; index, and web_extract over the documents Machine Library holds. The two
;;;; tools here are those two, under names of their own, with the endpoints
;;;; and the answer shapes the provider used (see NOTICE). Each one is an
;;;; NLK:DEFINE-CONTRACT, so the shape the model is shown and the shape the
;;;; body reads are one declaration, and a member the declaration does not
;;;; carry has no reader.
;;;;
;;;; Every tool answers text: the model reads it, and the titles, the urls
;;;; and the identifiers stay where it can cite them.

(in-package #:nodecode-machine-library)

;;; --- what a document is, in one line ---------------------------------------------

(defun canonical-uri (uris)
  "The identifier to cite: the DOI when the document has one, else the first
identifier it carries, as the provider chose it."
  (or (find-if (lambda (uri) (and (stringp uri) (eql 0 (search "doi://" uri)))) uris)
      (find-if #'stringp uris)))

(defun document-uris (body document)
  "The identifiers a document carries: the answer's own list, else its document's."
  (or (remove-if-not #'stringp (object-array body "uris"))
      (remove-if-not #'stringp (object-array document "uris"))))

(defun document-page-url (id uris)
  "The public page of a document: /r/{DOI} when it has a DOI, else /r/{id}."
  (let ((doi (find-if (lambda (uri) (and (stringp uri) (eql 0 (search "doi://" uri)))) uris)))
    (if doi
        (format nil "~a/r/~a" (site) (encode-path (subseq doi 6)))
        (format nil "~a/r/~a" (site) (percent-encode (or id ""))))))

(defun author-line (authors)
  "The first three authors, saying how many more there were."
  (when authors
    (format nil "~{~a~^, ~}~:[~; et al.~]"
            (subseq authors 0 (min 3 (length authors)))
            (> (length authors) 3))))

(defun year-of (document)
  "The year a document was issued in, from its unix issued_at, or NIL."
  (let ((issued (nlk:json-value document :integer "issued_at")))
    (and issued (plusp issued)
         (nth-value 5 (decode-universal-time (+ issued 2208988800) 0)))))

(defun byline (document)
  "The document in one line: its kind, its authors, its publisher, its year."
  (let* ((authors (loop for author in (objects-of document "authors")
                        for name = (object-text author "name")
                        when name collect name))
         (metadata (json-object-of (nlk:json-value document :object "metadata")))
         (kind (object-text document "type"))
         (publisher (or (object-text metadata "publisher")
                        (object-text metadata "container_title")))
         (parts (remove-if #'blank-p
                           (list (and kind (substitute #\Space #\- kind))
                                 (author-line authors)
                                 publisher
                                 (year-of document)))))
    (and parts (format nil "~{~a~^ · ~}" parts))))

;;; --- machine_library_search --------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search Machine Library for papers, books, patents, standards and Wikipedia
articles, each with a stable page to cite. Answers numbered sources: title, page url,
byline, the best-matching passage and the identifier to cite. Use
machine_library_extract to read one.")
  ("query" :text :required t
   :doc "what to look for, in words: a topic, a title, or an author and a year")
  ("limit" :integer
   :doc "how many documents to answer with, 1 to 20; the section's max_results when the call names none"))

(defun snippet-of (hit document)
  "The passage of HIT that matched, else the document's abstract, else the empty string."
  (or (loop for snippet in (objects-of hit "snippets")
            for text = (object-text snippet "text")
            when text return text)
      (object-text document "abstract")
      ""))

(defun format-search (body &key snippet-chars)
  "BODY --- a /v2/search answer --- as the text the model reads."
  (let ((hits (objects-of body "hits")))
    (with-output-to-string (stream)
      (if (null hits)
          (format stream "no Machine Library document matched this query~%")
          (loop for hit in hits
                for index from 1
                do (let* ((document (or (json-object-of (nlk:json-value hit :object "document"))
                                        (nlk:json-object)))
                          (uris (document-uris hit document))
                          (source (canonical-uri uris))
                          (parts (remove-if #'blank-p
                                            (list (byline document)
                                                  (clip (collapse-space (snippet-of hit document))
                                                        snippet-chars))))
                          (description (and parts (format nil "~{~a~^ — ~}" parts))))
                     (format stream "~d. ~a~%   ~a~%~@[   ~a~%~]~@[   source: ~a~%~]"
                             index
                             (or (object-text document "title") source "Untitled document")
                             (document-page-url (object-text hit "id") uris)
                             description
                             source)))))))

(defun tool-search (call)
  "machine_library_search: the API's /v2/search, read as numbered sources."
  (let* ((query (nlk:trimmed (search-call-query call)))
         (limit (let ((asked (search-call-limit call)))
                  (if asked
                      (max 1 (min asked +max-search-limit+))
                      (setting :max-results))))
         (body (request :post "/v2/search/"
                        :content (nlk:encode-json-object
                                  (nlk:make-json-object "query" query "limit" limit)))))
    (format-search body :snippet-chars (setting :snippet-chars))))

;;; --- machine_library_extract -------------------------------------------------------

(nlk:define-contract extract-call
  (:doc "Read documents Machine Library holds: its own /r/ pages, and the source links
it resolves to a stored document --- a DOI, an arXiv id, an ISBN, a standard
identifier. Answers each document's title, page, identifier and text. A url the
library does not hold fails on its own line: use a general page fetch for the open
web.")
  ("urls" :array :of :string :required t
   :doc "the urls to read: https://machinelibrary.ai/r/..., https://doi.org/..., https://arxiv.org/abs/..., or an identifier such as doi://10.1038/nature14539"))

(defun by-uri-path (uri)
  "The API path that resolves URI to a stored document."
  (format nil "/v2/documents/by-uri/~a?max_chars=~d"
          (percent-encode (nlk:trimmed uri))
          (setting :max-chars)))

(defun token-limit ()
  "The token count that bounds one by-id fetch: four characters per token is a
safe upper bound, as the original counted it."
  (max 1 (floor (setting :max-chars) 4)))

(defun split-url (text)
  "TEXT as an absolute url => (values SCHEME HOST PATH), the path without its
query or fragment; NIL for a relative one."
  (let* ((text (nlk:trimmed (or text "")))
         (mark (search "://" text)))
    (when mark
      (let* ((scheme (string-downcase (subseq text 0 mark)))
             (rest (subseq text (+ 3 mark)))
             (cut (or (position-if (lambda (character)
                                     (member character '(#\/ #\? #\#)))
                                   rest)
                      (length rest)))
             (host (subseq rest 0 cut))
             (path (subseq rest cut))
             (end (or (position-if (lambda (character)
                                     (member character '(#\? #\#)))
                                   path)
                      (length path))))
        (values scheme host (subseq path 0 end))))))

(defun machine-library-host-p (host)
  "True when HOST is the public site, with or without its www., as the provider read it."
  (member (string-downcase (or host "")) '("machinelibrary.ai" "www.machinelibrary.ai")
          :test #'string=))

(defun prefix-p (prefix text)
  "True when TEXT begins with PREFIX."
  (and (>= (length text) (length prefix))
       (string= prefix text :end2 (length prefix))))

(defun document-path (url)
  "The API path that fetches URL, or a refusal naming what Machine Library can hold."
  (multiple-value-bind (scheme host path) (split-url url)
    (if (and scheme
             (member scheme '("http" "https") :test #'string=)
             (machine-library-host-p host))
        (let ((path (percent-decode path)))
          (unless (prefix-p "/r/" path)
            (fail "~a is not a Machine Library document page: give ~a/r/... or a DOI, arXiv, ISBN or standard identifier"
                  (nlk:trimmed url) (site)))
          (let* ((target (subseq path 3))
                 (document-id (subseq target 0 (or (position #\/ target) (length target)))))
            (cond ((prefix-p "10." target)
                   (by-uri-path (concatenate 'string "doi://" target)))
                  ((zerop (length document-id))
                   (fail "~a names no document: give ~a/r/... or an identifier"
                         (nlk:trimmed url) (site)))
                  (t (format nil "/v2/documents/~a?max_tokens=~d"
                             (percent-encode document-id) (token-limit))))))
        (by-uri-path url))))

(defun format-document (stream url body)
  "Write URL's document, as BODY holds it, to STREAM."
  (let* ((document (or (json-object-of (nlk:json-value body :object "document"))
                        (nlk:json-object)))
         (uris (document-uris body document))
         (source (canonical-uri uris))
         (title (object-text document "title"))
         (abstract (nlk:trimmed (or (object-text document "abstract") "")))
         (stored (nlk:trimmed (or (object-text document "content") "")))
         (total (nlk:json-value document :integer "content_length"))
         (page (document-page-url (object-text body "id") uris))
         (description (byline document)))
    (format stream "url: ~a~%" url)
    (when (and page (plusp (length page))) (format stream "page: ~a~%" page))
    (when source (format stream "source: ~a~%" source))
    (when description (format stream "byline: ~a~%" description))
    (format stream "~%")
    (when title (format stream "# ~a~%" title))
    (when (plusp (length abstract)) (format stream "~a~%~%" abstract))
    (format stream "~a~%"
            (clip (if (plusp (length stored)) stored "(the stored text of this document is empty)")
                  (setting :max-chars)))
    (when (and total (> total (length stored)))
      (format stream "[the stored document is ~d characters long; this is its beginning]~%" total))))

(defun format-one (stream url)
  "Write URL's document, or the reason it has none, to STREAM: one url's
failure never takes the others with it, as in the original."
  (handler-case
      (let ((body (request :get (document-path url))))
        (if (null body)
            (format stream "url: ~a~%  no Machine Library document for this url; use a general page fetch for the open web~%"
                    url)
            (format-document stream url body)))
    (machine-library-error (condition)
      (format stream "url: ~a~%  ~a~%" url (machine-library-error-detail condition)))))

(defun tool-extract (call)
  "machine_library_extract: one fetch per url, and every url answered on its own."
  (let ((urls (coerce (or (extract-call-urls call) #()) 'list)))
    (unless urls
      (fail "machine_library_extract: urls is empty; name at least one url"))
    (with-output-to-string (stream)
      (loop for url in urls
            for first = t then nil
            do (progn (unless first (terpri stream))
                      (format-one stream url))))))
