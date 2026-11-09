;;;; support.lisp --- the four registries, canned, and no network.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one seam that reaches outside is NLK:HTTP. WITH-STUB replaces it
;;;; with a tiny registry server -- Crossref by DOI and by query, doi.org,
;;;; PubMed's esearch and esummary, arXiv's Atom feed -- and records every
;;;; URL, so a test asserts on what the cell asked and nothing dials.

(in-package #:nodecode.test)

(define-test-slice "citation-guard" "CITATION-GUARD-CELL-"
  :start nodecode-citation-guard:start-cell)

(defparameter +verified-doi+ "10.1038/real-one")
(defparameter +retracted-doi+ "10.1016/retracted-one")
(defparameter +missing-doi+ "10.9999/nothing")

(defparameter +verified-json+
  "{\"status\":\"ok\",\"message\":{\"DOI\":\"10.1038/real-one\",\"title\":[\"Attention is all you need\"],\"author\":[{\"family\":\"Vaswani\"}],\"issued\":{\"date-parts\":[[2017]]},\"type\":\"journal-article\",\"container-title\":[\"NeurIPS\"]}}")

(defparameter +retracted-title+
  "Hydroxychloroquine or chloroquine with or without a macrolide for treatment of COVID-19: a multinational registry analysis")

(defparameter +retracted-json+
  (format nil "{\"status\":\"ok\",\"message\":{\"DOI\":\"~a\",\"title\":[\"~a\"],\"author\":[{\"family\":\"Mehra\"}],\"issued\":{\"date-parts\":[[2020]]},\"type\":\"journal-article\"}}"
          +retracted-doi+ +retracted-title+))

(defparameter +updates-json+
  (format nil "{\"status\":\"ok\",\"message\":{\"items\":[{\"DOI\":\"10.1016/notice-one\",\"type\":\"retraction\",\"update-to\":[{\"type\":\"retraction\",\"DOI\":\"~a\"}]}]}}"
          +retracted-doi+))

(defparameter +empty-items-json+ "{\"status\":\"ok\",\"message\":{\"items\":[]}}")

(defparameter +esummary-json+
  "{\"result\":{\"uids\":[\"12345678\"],\"12345678\":{\"title\":\"Deep learning for early detection of sepsis in emergency departments\",\"pubdate\":\"2022 Mar\",\"authors\":[{\"name\":\"Smith J\"}],\"pubtype\":[\"Journal Article\"],\"source\":\"Nat Med\"}}}")

(defparameter +esearch-json+ "{\"esearchresult\":{\"idlist\":[\"12345678\"]}}")

(defparameter +arxiv-xml+
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\"><entry><id>https://arxiv.org/abs/2401.00001v1</id><published>2024-01-02T00:00:00Z</published><title>Scaling laws for sparse retrieval</title><author><name>Ada Lovelace</name></author></entry></feed>")

(defvar *requests* '()
  "Every URL the cell asked for, newest first, as (URL HEADERS).")

(defvar *registry-down* nil
  "When true every registry answers 500: a cell that could not check.")

(defun registry-stub (method url &rest arguments)
  "The stubbed NLK:HTTP."
  (declare (ignore method))
  (push (list url (getf arguments :headers)) *requests*)
  (when *registry-down* (return-from registry-stub (values "upstream is down" 500 nil)))
  (cond
    ((search "filter=updates:" url)
     (values (if (search +retracted-doi+ url) +updates-json+ +empty-items-json+) 200 nil))
    ((search "api.crossref.org/works?" url) (values +empty-items-json+ 200 nil))
    ((search (format nil "api.crossref.org/works/~a" +verified-doi+) url)
     (values +verified-json+ 200 nil))
    ((search (format nil "api.crossref.org/works/~a" +retracted-doi+) url)
     (values +retracted-json+ 200 nil))
    ((search "api.crossref.org/works/" url) (values "{\"status\":\"error\"}" 404 nil))
    ((search "doi.org/" url) (values "{}" 404 nil))
    ((search "esearch.fcgi" url) (values +esearch-json+ 200 nil))
    ((search "esummary.fcgi" url) (values +esummary-json+ 200 nil))
    ((search "export.arxiv.org" url) (values +arxiv-xml+ 200 nil))
    (t (values "{}" 404 nil))))

(defmacro with-stub (&body body)
  "Run BODY with NLK:HTTP stubbed as the four registries, and the cell's own
state cleared."
  `(with-saved-globals (*requests* *registry-down* nodecode-citation-guard:*client*)
     (setf *requests* '() *registry-down* nil)
     (with-stubbed-fdefinition (nlk:http (method url &rest arguments)
                                 (apply #'registry-stub method url arguments))
       ,@body)))

(defmacro with-guard ((&rest pairs) &body body)
  "Run BODY with the cell started on SECTION PAIRS, its registries stubbed,
and stopped on unwind."
  `(with-stub
     (with-cell-stop ((citation-guard-start ,@pairs))
       ,@body)))

(defun call-tool (name &optional (arguments "{}"))
  "NAME called as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun urls-asked () (mapcar #'car *requests*))

(defun run-command (name &optional (argument ""))
  "NAME driven the way the shell drives a slash command; => its text."
  (let ((entry (nle::find-registered-command name)))
    (and entry (funcall (nle::slash-command-headless entry) argument "citation-guard-session"))))
