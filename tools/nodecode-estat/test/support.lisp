;;;; support.lisp --- the estat test runner and its scripted e-Stat.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under an
;;;; ESTAT-CELL- prefix, and RUN-ESTAT-TESTS runs exactly that slice, so
;;;; this system's test-op never re-runs the core suite.
;;;;
;;;; The one network call is NODECODE-ESTAT::HTTP; WITH-ESTAT stubs that
;;;; name, records every request and answers the canned e-Stat replies, so no
;;;; test ever opens a socket.

(in-package #:nodecode.test)

(define-test-slice "estat" "ESTAT-CELL-" :start nodecode-estat:start-cell)

;;; --- config and lifecycle fixtures --------------------------------------------

(defparameter +key+ "estat-test-key"
  "The application ID a test's section carries.")

(defun estat-start-with (&rest pairs)
  "ESTAT-START with the test's ID unless PAIRS name one."
  (apply #'estat-start
         (append (unless (member "app_id" pairs :test #'string=)
                   (list "app_id" +key+))
                 (copy-list pairs))))

(defmacro with-estat ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its one wire stubbed, stopped on
unwind."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-estat::http (url timeout)
                                (stub-http url timeout))
       (with-cell-stop ((estat-start-with ,@pairs))
         ,@body))))

;;; --- the canned e-Stat -------------------------------------------------------

(defun estat-object (&rest specs)
  "One JSON object out of alternating keys and text values."
  (apply #'nlk:make-json-object specs))

(defun estat-json (&rest specs)
  "A JSON text out of alternating keys and values."
  (nlk:encode-json-object (apply #'nlk:make-json-object specs)))

(defparameter +list-answer+
  (estat-json
   "GET_STATS_LIST"
   (estat-object
    "RESULT" (estat-object "STATUS" "0" "ERROR_MSG" "")
    "DATA_LIST_INF"
    (estat-object
     "DATA_LIST"
     (vector
      (estat-object "@id" "0003445078"
                     "@statName" "人口推計"
                     "@title" "人口推計 2020年10月1日現在"
                     "@govOrg" "総務省"
                     "@surveyDate" "2020"
                     "@areaLevel" "都道府県"
                     "@frequency" "1年")
      (estat-object "@id" "0003410379"
                     "@statName" "国勢調査"
                     "@title" "令和2年国勢調査 人口等基本集計"
                     "@govOrg" "総務省"
                     "@surveyDate" "2020"
                     "@areaLevel" "市区町村"
                     "@frequency" "5年")))))
  "What the stubbed getStatsList answers: two tables.")

(defparameter +meta-answer+
  (estat-json
   "GET_META_INFO"
   (estat-object
    "RESULT" (estat-object "STATUS" "0" "ERROR_MSG" "")
    "METADATA_INF"
    (estat-object
     "TABLE_INF" (estat-object "@id" "0003445078"
                                 "TITLE" (estat-object "@no" "1" "$" "人口推計 2020年10月1日現在")
                                 "GOV_ORG" (estat-object "@code" "00200" "$" "総務省")
                                 "STATISTICS_NAME" "人口推計"
                                 "SURVEY_DATE" "2020")
     "CLASS_INF"
     (estat-object
      "CLASS_OBJ"
      (vector
       (estat-object "@id" "area" "@name" "地域"
                      "CLASS" (vector
                               (estat-object "@code" "00000" "@name" "全国" "@level" "1")
                               (estat-object "@code" "13000" "@name" "東京都" "@level" "2")
                               (estat-object "@code" "13206" "@name" "府中市" "@level" "3")
                               (estat-object "@code" "34207" "@name" "府中市" "@level" "3")))
       (estat-object "@id" "time" "@name" "時点"
                      "CLASS" (vector
                               (estat-object "@code" "2020000000" "@name" "2020年")
                               (estat-object "@code" "2021000000" "@name" "2021年")))
       (estat-object "@id" "cat01" "@name" "男女"
                      "CLASS" (vector
                               (estat-object "@code" "000" "@name" "総数")
                               (estat-object "@code" "001" "@name" "男")
                               (estat-object "@code" "002" "@name" "女"))))))))
  "What the stubbed getMetaInfo answers: one table, three dimensions.")

(defun data-answer (&rest rows)
  "A getStatsData answer carrying ROWS, each one a plist of @area, @time,
@cat01, $ and @unit."
  (estat-json
   "GET_STATS_DATA"
   (estat-object
    "RESULT" (estat-object "STATUS" "0" "ERROR_MSG" "")
    "STATISTICAL_DATA"
    (estat-object
     "DATA_INF"
     (estat-object
      "VALUE"
      (coerce (loop for row in rows
                     collect (apply #'estat-object row))
              'vector))))))

(defparameter +data-answer+
  (data-answer (list "@area" "13000" "@time" "2020000000" "@cat01" "000"
                     "$" "14047594" "@unit" "人"))
  "What the stubbed getStatsData answers: Tokyo's population.")

;;; --- the stub -----------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, oldest first: (:url :timeout).")

(defvar *answer* nil
  "A function of the url answering (values BODY STATUS), or NIL to answer
the canned reply the URL's endpoint names.")

(defun stub-http (url timeout)
  (push (list :url url :timeout timeout) *calls*)
  (let ((url (princ-to-string url)))
    (if *answer*
        (funcall *answer* url)
        (cond ((search "getStatsList" url) (values +list-answer+ 200))
              ((search "getMetaInfo" url) (values +meta-answer+ 200))
              ((search "getStatsData" url) (values +data-answer+ 200))
              (t (values (estat-json "ERROR" (estat-object "STATUS" "0")) 200))))))

;;; --- reading the request a call made ------------------------------------------

(defun params-of (url)
  "URL's query string as an alist of decoded (NAME . VALUE) pairs."
  (loop with query = (quri:uri-query (quri:uri url))
        for pair in (and query (uiop:split-string query :separator '(#\&)))
        for equals = (position #\= pair)
        collect (cons (quri:url-decode (subseq pair 0 equals) :encoding :utf-8)
                      (quri:url-decode (subseq pair (1+ equals)) :encoding :utf-8))))

(defun param (call name)
  "The value CALL's url carries for NAME, or NIL when it carries none."
  (cdr (assoc name (params-of (getf call :url)) :test #'string=)))

(defun endpoint-of (call)
  "The e-Stat endpoint CALL asked: getStatsList, getMetaInfo or getStatsData."
  (first (last (uiop:split-string (quri:uri-path (quri:uri (getf call :url)))
                                   :separator '(#\/)))))

;;; --- calling what the cell contributes -----------------------------------------

(defun estat-decode-data-call (&rest specs)
  "The data tool's declaration, decoded from SPECS: the preflight a call gets."
  (nlk:contract-decode (nlk:contract 'nodecode-estat::estat-data-request)
                        (apply #'nlk:make-json-object specs)))

(defun estat-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))
