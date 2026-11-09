;;;; cell.lisp --- the section, the verify_citations tool, /citecheck, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin's three surfaces were a tool, a reply-guard hook and a
;;;; /citecheck command; this cell keeps the tool and the command, and the
;;;; model reads the manual line in every request, so it checks a bibliography
;;;; before presenting one without a hook at all.

(in-package #:nodecode-citation-guard)

;; The cell's own condition and FAIL, its settings variable and
;; RUNNING-SETTINGS; NLE:DEFINE-CELL below declares everything it contributes.
(nlk:define-peripheral citation-guard :not-running t)

(defparameter +primer+
  "citation-guard checks references against the registries that issue their
identifiers: doi.org, Crossref, PubMed and arXiv. Deterministic, no model calls.

  verify_citations text [max_items] [check_retractions]
      every DOI, PMID, arXiv id and full reference entry in TEXT, each one
      verified, flagged, or reported as not checked. Call it BEFORE presenting a
      list of papers, a bibliography or a reference list.
  /citecheck <path or pasted references>
      the same check for a .md, .txt, .bib or .docx file (absolute, or ~/...),
      or for text typed after the command; the full report goes to the operator.

What a verdict means: verified (title, year and first author agree); metadata
mismatch (year or first author differs); id_not_found (the DOI/PMID/arXiv id
does not exist); wrong_id (the id resolves to a different work); retracted or
concern (a notice stands against it); not_found (nothing in any registry: a
fabricated reference, or one from a book, a report, a local journal); unchecked
(a registry did not answer -- never reported as fake).

Settings: citation-guard.max_items bounds one call (40 by default),
request_timeout_s one registry request (8), and the optional contact_email and
ncbi_api_key are courtesy values sent to Crossref/NCBI and NCBI alone.
(help :citation-guard) answers this while the cell runs."
  "What (help :citation-guard) answers while the cell runs.")

;;; --- the tool's boundary ---------------------------------------------------------

(nlk:define-contract verify-citations
  (:doc "Check references against the scholarly registries (doi.org, Crossref,
PubMed, arXiv). For each reference it reports whether the DOI/PMID/arXiv id
exists, whether it points to the cited title, whether the year and first author
match, and whether the work was retracted. Call this BEFORE presenting any list
of papers, citations or a bibliography to the user, and when the user asks
whether references are real. Input is plain text: a reference list, a draft with
a References section, or BibTeX. Deterministic; no model involved.")
  ("text" :text :required t
          :doc "the references: one entry per line, a References section, or BibTeX entries")
  ("max_items" :integer :default 40
                :doc "how many references to check, at most 200")
  ("check_retractions" :boolean :default t
                        :doc "also look for retraction and withdrawal notices"))

(defun verify-tool (request)
  "The model's call: a report it can read and act on."
  (let* ((text (verify-citations-text request))
         (asked (or (verify-citations-max-items request) (setting :max-items)))
         (max-items (max 1 (min 200 asked)))
         (checks (check-all text :max-items max-items
                                 :check-retractions (not (null (verify-citations-check-retractions request))))))
    (report-text checks :title "verify_citations")))

;;; --- the operator's command -------------------------------------------------------

(defparameter +max-file-bytes+ (* 10 1024 1024)
  "How big a manuscript /citecheck reads; past that, paste the list instead.")

(defun absolute-p (path)
  #+win32 (let ((text (uiop:native-namestring path)))
            (and (> (length text) 1) (alpha-char-p (char text 0)) (char= (char text 1) #\:)))
  #-win32 (uiop:absolute-pathname-p path))

(defun resolve-path (text)
  "TEXT as a path: `~/' reads against the operator's home, a relative one
against the folder this Nodecode runs in."
  (let ((path (nlk:expand-home text)))
    (if (absolute-p path) path (merge-pathnames path (uiop:getcwd)))))

(defun strip-tags (xml)
  "XML without its elements, one line per paragraph."
  (with-output-to-string (out)
    (let ((start 0))
      (loop
        (let ((open (search "<" xml :start2 start)))
          (unless open (write-string xml out :start start) (return))
          (write-string xml out :start start :end open)
          (let ((close (position #\> xml :start open)))
            (unless close (write-string xml out :start open) (return))
            (let ((tag (subseq xml (1+ open) close)))
              (cond ((member tag '("/w:p" "/w:tr") :test #'string=) (terpri out))
                    ((member tag '("w:tab" "w:br") :test #'string=) (write-char #\Tab out)))
              (setf start (1+ close)))))))))

(defun docx-text (path)
  "PATH's document.xml, as text, through unzip."
  (let ((xml (uiop:run-program (list "unzip" "-p" (uiop:native-namestring path) "word/document.xml")
                                :output :string :error-output nil :ignore-error-status t)))
    (when xml
      (setf xml (string-replace-all xml "&amp;" "&"))
      (setf xml (string-replace-all xml "&lt;" "<"))
      (setf xml (string-replace-all xml "&gt;" ">"))
      (strip-tags xml))))

(defun document-text (path)
  "PATH read as the text /citecheck checks; => (values TEXT ERROR)."
  (handler-case
      (let ((size (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))))
        (if (> size +max-file-bytes+)
            (values nil "is larger than 10 MB; paste the reference list instead")
            (values (if (string-equal (or (pathname-type path) "") "docx")
                        (or (docx-text path) "")
                        (uiop:read-file-string path))
                    nil)))
    (error (condition) (values nil (one-line (princ-to-string condition) :cap 160)))))

(defun read-document (path)
  (document-text path))

(defun run-citecheck (args session-id)
  "The whole report for a file or pasted references."
  (declare (ignore session-id))
  (let ((arg (nlk:trimmed (or args ""))))
    (cond
      ((zerop (length arg))
       (format nil "Usage: /citecheck <a .md/.txt/.bib/.docx file>  or  /citecheck <pasted references>~%Checks every reference against doi.org, Crossref, PubMed and arXiv."))
      ((and (< (length arg) 1024)
            (member (string-downcase (or (pathname-type (pathname (string-trim "'\"" arg))) ""))
                    '("md" "txt" "bib" "docx" "markdown" "text") :test #'string=)
            (null (probe-file (ignore-errors (resolve-path (string-trim "'\"" arg)))))
            (not (find #\Space arg)))
       (format nil "File not found: ~a~%A path is read as it is typed: an absolute one, or ~~/... for your home."
               (ignore-errors (resolve-path (string-trim "'\"" arg)))))
      (t
       (let ((path (and (< (length arg) 1024) (ignore-errors (resolve-path (string-trim "'\"" arg))))))
         (if (and path (probe-file path))
             (multiple-value-bind (text error) (document-text path)
               (if error
                   (format nil "Could not read ~a: ~a" (file-namestring path) error)
                   (report-text (check-all text :max-items 200)
                                :title (format nil "Citation check: ~a" (file-namestring path)))))
             (report-text (check-all arg :max-items (setting :max-items)) :title "Citation check")))))))

;;; --- the cell --------------------------------------------------------------------

(defun start ()
  (setf *client* (make-client :mailto (setting :contact-email)
                               :timeout (setting :request-timeout-s)
                               :ncbi-key (setting :ncbi-api-key)
                               :cache (make-hash-table :test #'equal)))
  (nle:on-stop (lambda () (setf *client* nil))))

(nle:define-cell citation-guard
  (:section ("citation-guard")
    (:guide "nothing here is required; contact_email and ncbi_api_key are optional courtesy settings for Crossref and NCBI")
    ("contact_email" :string :default ""
                     :doc "optional: sent to Crossref and NCBI as the polite-pool contact address, and to nothing else")
    ("ncbi_api_key" :string :default ""
                    :doc "optional: a free NCBI key, sent only to NCBI, so its rate limit counts requests per key")
    ("max_items" :integer :default 40 :min 1
                 :doc "how many references one check reads")
    ("request_timeout_s" :integer :default 8 :min 1
                          :doc "how long one registry request may take"))
  (:tool "verify_citations" (nlk:contract 'verify-citations) 'verify-tool)
  (:command "citecheck" 'run-citecheck
            :description "Check every reference in a file or pasted text against Crossref, PubMed and arXiv"
            :argument-hint "<file.md|.txt|.bib|.docx or pasted references>")
  (:help :citation-guard
         "citation-guard: verify_citations and /citecheck check every cited DOI, PMID, arXiv id or reference entry"
         +primer+)
  (:start #'start))
