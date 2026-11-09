;;;; cell-test.lisp --- lifecycle, every verdict, both surfaces, each refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not the registries: the
;;;; section and its refusal, the tool and the command through the real
;;;; dispatch seam against a canned Crossref/PubMed/arXiv, that a missing
;;;; identifier is reported as missing and a retracted work as retracted, and
;;;; that a registry which did not answer is never called a fake reference.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "citation-guard"
  (:fixture with-stub)
  (:config "max_items" 40)
  (:help :citation-guard)
  (:command "citecheck")
  (:running
   (is (= 40 (nodecode-citation-guard:setting :max-items)) "the section's own limit is what the cell runs on")
   (is-present (entry (nle::find-tool "verify_citations")) "the tool is registered"))
  (:stopped
   (is (null nodecode-citation-guard:*client*) "the registry client goes with the stop")
   (is (null (nle::find-tool "verify_citations")) "and the tool comes off"))
  (:refused
   ("max_items" 0)))

;;; --- what a person cites ---------------------------------------------------------

(defparameter +bibliography+
  (format nil "Vaswani A, et al. Attention is all you need. NeurIPS. 2017. doi:~a~%
Smith J, Lee K. Deep learning for early detection of sepsis in emergency departments. Nat Med. 2022. doi:~a~%
Mehra MR, Desai SS, Ruschitzka F, Patel AN. ~a. Lancet. 2020. doi:~a"
          +verified-doi+ +missing-doi+ +retracted-title+ +retracted-doi+))

(defun verify-arguments (text)
  (nlk:encode-json-object (nlk:make-json-object "text" text)))

;;; --- the tool -------------------------------------------------------------------

(deftest citation-guard-cell-the-tool-reports-each-verdict
  (with-guard ("max_items" 40)
    (let ((text (call-tool "verify_citations" (verify-arguments +bibliography+))))
      (is (search "3 references checked" text))
      (is (search "verified" text) "the real reference is verified")
      (is (search "Attention is all you need" text))
      (is (search "does not exist" text) "the invented DOI is named")
      (is (search +missing-doi+ text))
      (is (search "retraction" text) "the retracted paper is flagged")
      (is (search "10.1016/notice-one" text) "with the notice that retracted it"))))

(deftest citation-guard-cell-the-ids-are-what-reaches-the-registries
  (with-guard ("max_items" 40)
    (call-tool "verify_citations" (verify-arguments +bibliography+))
    (is (some (lambda (url) (search (format nil "api.crossref.org/works/~a" +verified-doi+) url)) (urls-asked)) "the DOI is resolved at Crossref")
    (is (some (lambda (url) (search "filter=updates:" url)) (urls-asked)) "retraction notices are asked for by the DOI that was resolved")
    (is (some (lambda (url) (search "api.crossref.org/works/" url)) (urls-asked)) "an unknown id is asked of the registration agency too")
    (is (equal "citation-guard/0.1.0 (Nodecode cell; +https://github.com/ChienNguyen23/hermes-citation-guard)"
               (cdr (assoc "user-agent" (second (car *requests*)) :test #'string-equal))) "the registry is told what this is")))

(deftest citation-guard-cell-a-registry-that-did-not-answer-is-not-a-fake
  (with-guard ()
    (setf *registry-down* t)
    (let ((text (call-tool "verify_citations" (verify-arguments +bibliography+))))
      (is (search "not checked" text))
      (is (search "could not be checked" text))
      (is (null (search "does not exist" text)) "a failed call never reads as a missing identifier"))))

;;; --- the operator's command ------------------------------------------------------

(deftest citation-guard-cell-the-command-reports-pasted-text
  (with-guard ()
    (let ((text (run-command "citecheck" +bibliography+)))
      (is (search "Citation check" text))
      (is (search "does not exist" text))
      (is (search "retraction" text)))))

(deftest citation-guard-cell-the-command-reads-the-file-it-is-given
  (with-guard ()
    (uiop:with-temporary-file (:pathname path :type "txt" :keep t :direction :output
                               :stream out)
      (write-string +bibliography+ out)
      :close-stream
      (let ((text (run-command "citecheck" (uiop:native-namestring path))))
        (is (search "Citation check:" text))
        (is (search (file-namestring path) text) "the report names the file it read")
        (is (search "3 references checked" text))))))

(deftest citation-guard-cell-the-command-says-what-it-asked-for
  (with-guard ()
    (is (search "Usage: /citecheck" (run-command "citecheck")))
    (is (search "File not found" (run-command "citecheck" "/nonexistent/references.md")))))

;;; --- the boundary ---------------------------------------------------------------

(deftest citation-guard-cell-a-call-without-text-is-refused
  (with-stub
    (let ((condition (signals-error nlk:contract-refusal
                        (nlk:contract-decode (nlk:contract 'nodecode-citation-guard::verify-citations)
                                             (nlk:make-json-object "max_items" 5)))))
      (is (search "text" (princ-to-string condition)) "the refusal names the member to fill in"))))

(deftest citation-guard-cell-a-reference-is-read-out-of-prose-too
  (with-stub
    (let ((citations (nodecode-citation-guard:extract-citations +bibliography+)))
      (is (= 3 (length citations)) "three references, three entries")
      (is (equal +missing-doi+ (getf (second citations) :doi)) "the second one's DOI")
      (is (= 2020 (getf (third citations) :year)) "and the third one's year"))))
