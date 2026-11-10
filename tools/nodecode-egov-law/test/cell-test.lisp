;;;; cell-test.lisp --- the egov-law cell against a stubbed e-Gov Law API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP answering a small document shaped
;;;; like the API's, so nothing reaches laws.e-gov.go.jp. What is proved is
;;;; the cell's own wiring: the section (a healthy start, a wrong value
;;;; refused), the three tools through their registered names and arguments,
;;;; the element name an article is read by, and the operator's command.

(in-package #:nodecode.test)

(define-test-slice "egov-law" "EGOV-LAW-CELL-" :start egov-law:start-cell)

(define-cell-lifecycle-tests "egov-law"
  (:help :egov-law)
  (:refused ("max_chars" 10)))

;;; --- fixtures ------------------------------------------------------------------

(defparameter +laws+
  "{\"total_count\": 2, \"laws\": [
     {\"law_info\": {\"law_id\": \"322AC0000000050\", \"law_num\": \"昭和二十二年法律第五十号\", \"law_type\": \"Act\"},
      \"revision_info\": {\"law_title\": \"労働基準法施行規則\", \"abbrev\": \"労基則\", \"category\": \"労働\"}},
     {\"law_info\": {\"law_id\": \"322AC0000000049\", \"law_num\": \"昭和二十二年法律第四十九号\", \"law_type\": \"Act\"},
      \"revision_info\": {\"law_title\": \"労働基準法\", \"abbrev\": \"労基法\", \"category\": \"労働\"}}]}")

(defparameter +article+
  "{\"law_info\": {\"law_id\": \"322AC0000000049\", \"law_num\": \"昭和二十二年法律第四十九号\"},
     \"revision_info\": {\"law_title\": \"労働基準法\", \"amendment_enforcement_date\": \"2026-07-17\",
                         \"amendment_law_title\": \"労働者災害補償保険法等の一部を改正する法律\",
                         \"amendment_law_num\": \"令和八年法律第六十号\",
                         \"current_revision_status\": \"CurrentEnforced\"},
     \"law_full_text\": {\"tag\": \"Article\", \"attr\": {\"Num\": \"32\"}, \"children\": [
        {\"tag\": \"ArticleCaption\", \"attr\": {}, \"children\": [\"（労働時間）\"]},
        {\"tag\": \"ArticleTitle\", \"attr\": {}, \"children\": [\"第三十二条\"]},
        {\"tag\": \"Paragraph\", \"attr\": {\"Num\": \"1\"}, \"children\": [
           {\"tag\": \"ParagraphNum\", \"attr\": {}, \"children\": []},
           {\"tag\": \"ParagraphSentence\", \"attr\": {}, \"children\": [
              {\"tag\": \"Sentence\", \"attr\": {}, \"children\": [\"使用者は、労働者に、休憩時間を除き一週間について四十時間を超えて、労働させてはならない。\"]}]}]},
        {\"tag\": \"Paragraph\", \"attr\": {\"Num\": \"2\"}, \"children\": [
           {\"tag\": \"ParagraphNum\", \"attr\": {}, \"children\": []},
           {\"tag\": \"ParagraphSentence\", \"attr\": {}, \"children\": [
              {\"tag\": \"Sentence\", \"attr\": {}, \"children\": [\"使用者は、一週間の各日については、労働者に、休憩時間を除き一日について八時間を超えて、労働させてはならない。\"]}]}]}]}}")

(defparameter +revisions+
  "{\"law_info\": {\"law_id\": \"322AC0000000049\", \"law_num\": \"昭和二十二年法律第四十九号\",
                 \"promulgation_date\": \"1947-04-07\"},
     \"revisions\": [
       {\"law_revision_id\": \"322AC0000000049_20990401_508AC0000000061\", \"law_title\": \"労働基準法\",
        \"amendment_scheduled_enforcement_date\": \"2099-04-01\",
        \"amendment_law_title\": \"労働基準法等の一部を改正する法律\",
        \"amendment_law_num\": \"令和十年法律第六十一号\", \"repeal_status\": \"None\",
        \"current_revision_status\": \"UnEnforced\"},
       {\"law_revision_id\": \"322AC0000000049_20000101_000000000000000\", \"law_title\": \"労働基準法\",
        \"amendment_enforcement_date\": \"2000-01-01\", \"repeal_status\": \"None\",
        \"current_revision_status\": \"CurrentEnforced\"}]}")

;;; --- the wire --------------------------------------------------------------------

(defvar *asked* '()
  "Every URL the stubbed NLK:HTTP was asked for, newest first.")

(defmacro with-egov ((pages) &body body)
  "BODY with NLK:HTTP answering PAGES — (FRAGMENT . ANSWER), FRAGMENT searched
in the url, ANSWER a document string, or (STRING STATUS)."
  `(let ((*asked* '()))
     (with-stubbed-fdefinition (nlk:http (method url &rest arguments)
                                 
                                 (is (eq :get method) "the API is read, never written")
                                 (push url *asked*)
                                 (let ((answer (cdr (find-if (lambda (entry) (search (car entry) url)) ,pages))))
                                   (cond ((stringp answer) (values answer 200 (make-hash-table :test 'equal)))
                                         (answer (values (first answer) (second answer) (make-hash-table :test 'equal)))
                                         (t (values "{\"code\": \"404004\", \"message\": \"not found\"}" 404
                                                    (make-hash-table :test 'equal))))))
       ,@body)))

(defun egov-law-section (&rest pairs)
  "A whole shared config holding the `egov-law' section PAIRS make."
  (nlk:json-object "egov-law" (apply #'nlk:make-json-object pairs)))

(defmacro with-egov-cell ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS make."
  `(with-cell-stop ((egov-law:start-cell (egov-law-section ,@pairs)))
     ,@body))

(defun answer (text)
  "TEXT — a tool's JSON — as a decoded object."
  (nlk:decode-json text))

(defun asked-url (&optional (index 0))
  "The INDEXth url asked for, oldest first."
  (nth index (reverse *asked*)))

;;; --- a law found by the name a person uses ----------------------------------------

(deftest egov-law-cell-searches-by-title ()
  (with-egov ((list (cons "laws?" +laws+)))
    (with-egov-cell ()
      (let* ((result (answer (egov-law:egov_law_search (nlk:make-json-object "query" "労基法"))))
             (first (aref (gethash "results" result) 0)))
        (is (equal "title" (gethash "mode" result))
            (format nil "ANSWER ~a" (nlk:encode-json-object result)))
        (is (equal "322AC0000000049" (gethash "law_id" first)) "the abbreviation's own law, not the longer title that also contains the name")
        (is (equal "abbreviation" (gethash "match" first)))
        (is (equal "https://laws.e-gov.go.jp/law/322AC0000000049" (gethash "url" first)))
        (is (search "laws.e-gov.go.jp" (gethash "attribution" (gethash "source" result))) "the credit e-Gov asks for travels with the answer")
        (is (search (quri:url-encode "労基法" :encoding :utf-8) (asked-url)) "the name is percent-encoded on the wire")
        (is (search "repeal_status=None" (asked-url)) "laws in force only, unless asked")))))

(deftest egov-law-cell-reads-an-article ()
  (with-egov ((list (cons "law_data/322AC0000000049" +article+) (cons "laws?" +laws+)))
    (with-egov-cell ()
      (let ((result (answer (egov-law:egov_law_article
                             (nlk:make-json-object "law" "労基法" "article" "第32条")))))
        (is (equal "労働基準法" (gethash "law_title" result)))
        (is (equal "第三十二条" (gethash "article" result)))
        (is (equal "（労働時間）" (gethash "caption" result)))
        (is (search "四十時間を超えて" (gethash "text" result)) "the paragraph's own wording")
        (is (search "２　使用者は" (gethash "text" result)) "the second paragraph numbered in full width")
        (is (equal 2 (gethash "paragraphs" result)))
        (is (equal "2026-07-17" (gethash "in_force_since" result)))
        (is (equal "https://laws.e-gov.go.jp/law/322AC0000000049" (gethash "url" result)))
        (is (search "elm=MainProvision-Article_32" (car *asked*)) "the element name the API reads by")
        (is (search "attribution" (nlk:encode-json-object (gethash "source" result))))))))

(deftest egov-law-cell-reads-revisions ()
  (with-egov ((list (cons "law_revisions/322AC0000000049" +revisions+)
               (cons "laws?" +laws+)))
    (with-egov-cell ()
      (let ((result (answer (egov-law:egov_law_revisions (nlk:make-json-object "law" "労基法")))))
        (is (equal "2099-04-01" (gethash "in_force_since" (gethash "next_amendment" result))))
        (is (equal 1 (length (gethash "scheduled_amendments" result))))
        (is (equal "2000-01-01" (gethash "in_force_since" (gethash "in_force_now" result))))
        (is (equal "2099-03-31" (gethash "as_of" (gethash "check" (gethash "next_amendment" result)))) "the day before, for reading the article as it read then")
        (is (equal "https://laws.e-gov.go.jp/law/322AC0000000049" (gethash "url" result)))))))

(deftest egov-law-cell-refuses-an-unknown-law ()
  (with-egov ((list (cons "laws?" "{\"total_count\": 0, \"laws\": []}")))
    (with-egov-cell ()
      (let ((result (answer (egov-law:egov_law_article
                             (nlk:make-json-object "law" "架空法" "article" "1")))))
        (is (equal "law_not_found" (gethash "error_type" result))
            (format nil "a tool answers the refusal; got ~a" (nlk:encode-json-object result)))
        (is (search "egov_law_search" (gethash "next_step" result)))))))

(deftest egov-law-cell-command-reads-an-article ()
  (with-egov ((list (cons "law_data/322AC0000000049" +article+) (cons "laws?" +laws+)))
    (with-egov-cell ()
      (let ((text (egov-law:run-command "労基法 32" nil)))
        (is (search "労働基準法" text))
        (is (search "第三十二条" text))
        (is (search "四十時間を超えて" text))
        (is (search "https://laws.e-gov.go.jp/law/322AC0000000049" text) "the source page, so the reader can check it")))))
