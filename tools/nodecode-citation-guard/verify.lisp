;;;; verify.lisp --- what a reference is worth: does it exist, does it match,
;;;; is it still standing.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A reference names a work by an id, or by its own shape. An id is
;;;; resolved first and the registry's title compared with what the text
;;;; claims; only a reference with no id is searched by title. Nothing is
;;;; judged from a registry that did not answer.

(in-package #:nodecode-citation-guard)

(defstruct (check (:constructor make-check (&key raw index status issues record closest)))
  raw index status (issues '()) record closest)

(defparameter +severe+ '("retraction" "withdrawal" "removal" "partial_retraction")
  "The Crossref update kinds that take a work out of the literature.")

(defparameter +status-label+
  '((:verified              . "✓ verified")
    (:metadata-mismatch     . "⚠ details differ from the registry")
    (:concern               . "⚠ expression of concern")
    (:retracted             . "✗ retracted")
    (:wrong-id              . "✗ identifier points to a different work")
    (:id-not-found          . "✗ identifier does not exist")
    (:not-found             . "? not found in Crossref, PubMed or arXiv")
    (:unchecked             . "· not checked (a registry did not answer)")))

(defparameter +problem-statuses+
  '(:metadata-mismatch :concern :retracted :wrong-id :id-not-found :not-found)
  "The statuses worth a footer, a flag, and a person's eyes.")

(defun status-label (status) (or (cdr (assoc status +status-label+)) (string-downcase (string status))))

(defun problem-p (check) (member (check-status check) +problem-statuses+))

(defun shorten (text &key (cap 110))
  (let ((text (one-line (or text "") :cap (1+ cap))))
    (if (> (length text) cap) (concatenate 'string (subseq text 0 (1- cap)) "...") text)))

;;; --- one reference ---------------------------------------------------------------

(defun registry-matches-p (record text)
  "Whether RECORD's title is the title TEXT carries: the registry title's own
words, at least half of them, stand in the cited text. A reference list
quotes its title, so a real match clears this and a glued-on id does not."
  (let ((title (getf record :title)))
    (or (null title)
        (< (length (content-words title)) 3)
        (>= (coverage (content-words title) (words text)) 0.5))))

(defun retraction-of (record)
  "The notice that takes RECORD out of the literature, as a plist, or NIL."
  (or (let ((kind (getf record :in-update)))
        (and kind (list :kind kind :doi (or (getf record :doi) (getf record :pmid)) :source "the record")))
      (let ((doi (getf record :doi)))
        (when (and (stringp doi) (plusp (length doi)))
          (find-if (lambda (update) (member (getf update :kind) '("retraction" "withdrawal" "removal"
                                                                  "partial_retraction" "expression_of_concern")
                                             :test #'string=))
                   (crossref-updates doi))))))

(defun judge-record (citation record label id)
  "CITATION judged against the RECORD an identifier resolved to."
  (let ((raw (getf citation :raw)) (index (getf citation :index)))
    (if (null record)
        (make-check :raw raw :index index :status :id-not-found
                    :issues (list (format nil "~a ~a does not exist" label id)))
        (let ((notes '()) (wrong nil))
          (unless (registry-matches-p record (or (getf citation :text) raw))
            (setf wrong t)
            (push (format nil "~a ~a resolves to a different work: ~a" label id (shorten (getf record :title)))
                  notes))
          (let ((cited (getf citation :year)) (registry (getf record :year)))
            (when (and cited registry (/= cited registry))
              (push (format nil "year: cited ~d, registry ~d" cited registry) notes)))
          (let ((cited (getf citation :author)) (registry (getf record :author)))
            (when (and (stringp cited) (stringp registry)
                       (plusp (length cited)) (plusp (length registry))
                       (not (string-equal cited registry)))
              (push (format nil "first author: cited ~a, registry ~a" cited registry) notes)))
          (let ((notice (retraction-of record))
                (notes (nreverse notes)))
            (cond
              (wrong (make-check :raw raw :index index :status :wrong-id :issues notes :record record))
              ((and notice (member (getf notice :kind) +severe+ :test #'string=))
               (make-check :raw raw :index index :status :retracted :record record
                           :issues (cons (format nil "~a notice ~a (~a)"
                                                 (getf notice :kind)
                                                 (or (getf notice :doi) "unknown")
                                                 (or (getf notice :source) "the registry"))
                                         notes)))
              ((and notice (string= (getf notice :kind) "expression_of_concern"))
               (make-check :raw raw :index index :status :concern :record record
                           :issues (cons (format nil "an expression of concern stands against ~a"
                                                 (or (getf notice :doi) id))
                                         notes)))
              (notes (make-check :raw raw :index index :status :metadata-mismatch
                                 :issues notes :record record))
              (t (make-check :raw raw :index index :status :verified :record record))))))))

(defun check-identifier (citation record label id)
  (judge-record citation record label id))

(defun check-by-title (citation)
  "CITATION, which names no identifier, searched for by its own words."
  (let* ((raw (or (getf citation :text) (getf citation :raw)))
         (index (getf citation :index))
         (candidates (append (or (crossref-search raw 5) '())
                             (or (pubmed-search raw 5) '())
                             (or (arxiv-search raw 5) '())))
         (scored (sort (loop for record in candidates
                             for score = (coverage (content-words (getf record :title)) (words raw))
                             when (>= score 0.85) collect (cons score record))
                       #'> :key #'car))
         (best (cdr (first scored))))
    (if best
        (judge-record citation best "" "")
        (make-check :raw (getf citation :raw) :index index :status :not-found
                    :closest (cdr (first candidates))
                    :issues (list (if candidates
                                      (format nil "no matching record (closest: ~a)" (shorten (getf (cdr (first candidates)) :title)))
                                      "no matching record in Crossref, PubMed or arXiv"))))))

(defun check-citation (citation)
  "CITATION checked against the registries; a call that fails is `unchecked`,
never a fake reference."
  (handler-case
      (let ((doi (getf citation :doi)) (pmid (getf citation :pmid)) (arxiv (getf citation :arxiv)))
        (cond
          (doi (check-identifier citation
                                  (or (crossref-work doi) (datacite-work doi))
                                  "DOI" doi))
          (pmid (check-identifier citation (pubmed-work pmid) "PMID" pmid))
          (arxiv (check-identifier citation (arxiv-work arxiv) "arXiv id" arxiv))
          (t (check-by-title citation))))
    (error (condition)
      (make-check :raw (getf citation :raw) :index (getf citation :index) :status :unchecked
                  :issues (list (format nil "could not be checked: ~a" (one-line (princ-to-string condition) :cap 120)))))))

(defun check-all (text &key (max-items 40) (check-retractions t))
  "TEXT's references, at most MAX-ITEMS, each checked. => the checks."
  (declare (ignore check-retractions))
  (mapcar #'check-citation (citations-from text :max-items max-items)))

;;; --- the report -------------------------------------------------------------------

(defun count-status (checks status)
  (count status checks :key #'check-status))

(defun report-summary (checks)
  (format nil "~d reference~:p checked: ~d problem~:p, ~d verified~@[, ~d not checked~]."
          (length checks)
          (count-if #'problem-p checks)
          (count-status checks :verified)
          (let ((unchecked (count-status checks :unchecked)))
            (and (plusp unchecked) unchecked))))

(defun report-text (checks &key (title "Citation check"))
  "The full report: one block per reference, its verdict, and why."
  (if (null checks)
      (format nil "~a: no checkable references found (a reference needs a DOI, a PMID, an arXiv id, or a line with authors, a year and a title)." title)
      (with-output-to-string (out)
        (format out "~a: ~a~%" title (report-summary checks))
        (dolist (check checks)
          (let ((record (check-record check))
                (status (check-status check)))
            (format out "  ~a ~@[~d. ~]~a~%"
                    (cond ((problem-p check) "✗") ((eq status :verified) "✓") (t "·"))
                    (1+ (or (check-index check) 0))
                    (shorten (check-raw check) :cap 96))
            (format out "      ~a~%" (status-label status))
            (dolist (issue (check-issues check))
              (format out "        - ~a~%" issue))
            (when record
              (format out "        registry: ~a~@[ (~d)~]~@[ ~a~]~%"
                      (shorten (if (getf record :title) (getf record :title) "no title")
                               :cap 80)
                      (getf record :year)
                      (or (getf record :doi) (getf record :pmid) (getf record :arxiv))))))
        (format out "_Do not rely on a flagged reference until you have checked it yourself._~%"))))
