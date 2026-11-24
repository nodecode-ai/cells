;;;; client.lisp --- GET JSON from the e-Gov Law API v2, and nothing else.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The API is public and unauthenticated, so a cell needs no key: one host,
;;;; https://laws.e-gov.go.jp, its /api/2 paths, a timeout and a size cap. A
;;;; failure comes back as a refusal whose message names what went wrong and
;;;; whose KIND the tool answers as error_type, so a turn that cannot read a
;;;; law says so instead of inventing one.

(in-package #:nodecode-egov-law)

(defparameter +site+ "https://laws.e-gov.go.jp"
  "The e-Gov site; /law/<law-id> is the human-readable page a result cites.")

(defparameter +attribution+
  "出典：e-Gov法令検索（https://laws.e-gov.go.jp/）のデータを Nodecode の egov-law セルが取得・整形（条単位の抽出等）"
  "e-Gov's terms ask for this credit with any text quoted from it.")

(defparameter +max-bytes+ (* 8 1024 1024)
  "The largest law document this cell reads; a bigger one asks for one article.")

(defvar *error-kind* nil
  "The machine-readable kind of the refusal being raised, which the tool
answers as error_type: not_found, bad_request, upstream, network,
bad_response, bad_argument, law_not_found, ambiguous_law, article_not_found.")

(defun law-fail (kind control &rest args)
  "Refuse with KIND recorded and the formatted CONTROL as the message."
  (setf *error-kind* kind)
  (apply #'fail control args))

(defun endpoint ()
  "The API base the section names, without its trailing slash."
  (string-right-trim "/" (setting :endpoint)))

(defun url-for (path params)
  "PATH under the API base, with PARAMS — an alist whose empty members are
left out — percent-encoded after it."
  (let ((kept (remove-if (lambda (pair)
                           (let ((value (cdr pair)))
                             (or (null value) (and (stringp value) (zerop (length value))))))
                         params)))
    (format nil "~a/~a~@[?~a~]"
            (endpoint) (string-left-trim "/" path)
            (and kept (quri:url-encode-params kept)))))

(defun get-json (path &key params)
  "GET PATH from the e-Gov API as a decoded object. Every failure is a
refusal, so no caller has to know what a socket or a status is."
  (let ((url (url-for path params))
        (body nil)
        (status nil))
    (handler-case
        (multiple-value-setq (body status)
          (nlk:http :get url
                    :headers '(("accept" . "application/json"))
                    :timeout (setting :timeout-seconds)
                    :max-bytes +max-bytes+))
      (error (condition)
        (declare (ignore condition))
        (law-fail "network"
                  "could not reach the e-Gov Law API at ~a; check this machine's network and call again"
                  +site+)))
    (let ((data (ignore-errors (nlk:decode-json body))))
      (when (and (integerp status) (>= status 400))
        (law-fail (case status
                    (404 "not_found")
                    (400 "bad_request")
                    (t "upstream"))
                  "the e-Gov Law API answered HTTP ~d for ~a~@[ (~a)~]"
                  status path (and (hash-table-p data) (nlk:json-value data :text "message"))))
      (unless (hash-table-p data)
        (law-fail "bad_response"
                  "the e-Gov Law API answered ~a with something that is not a JSON object" path))
      data)))

(defun law-page (law-id)
  "The human-readable e-Gov page for LAW-ID (always its current version)."
  (format nil "~a/law/~a" +site+ law-id))

(defun today-jst ()
  "Today in Japan, YYYY-MM-DD, which is the day e-Gov's versions turn over on."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (+ (get-universal-time) (* 9 3600)))
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))
