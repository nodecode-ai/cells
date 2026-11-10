;;;; support.lisp --- the odds radar test runner and its scripted Polymarket.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under an
;;;; ODDSRADAR-CELL- prefix, and RUN-ODDSRADAR-TESTS runs exactly that slice,
;;;; so this system's test-op never re-runs the core suite.
;;;;
;;;; Every network call is NODECODE-ODDSRADAR::HTTP-JSON, the cell's one
;;;; seam: WITH-ODDS-API stubs it, records each call and answers a scripted
;;;; JSON object per URL, so no test opens a socket. Each start gets a
;;;; watchlist file of its own under the scratch home, so no test reads
;;;; another's watches.

(in-package #:nodecode.test)

(defvar *oddsradar-scratch* 0
  "How many scratch watchlist files this run has made.")

(defun scratch-watchlist ()
  "A watchlist path no other test has used, under this run's scratch home."
  (namestring (nlk:home (format nil "test-oddsradar/watchlist-~d.json" (incf *oddsradar-scratch*)))))

(define-test-slice "oddsradar" "ODDSRADAR-CELL-" :start nodecode-oddsradar:start-cell
  :defaults (("data_file" (scratch-watchlist))))

;;; --- calling what the cell contributes -------------------------------------------

(defun oddsradar-tool (name &rest pairs)
  "The registered tool NAME, called the way the engine calls it: the
declaration materializes the record, the body answers the text."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

;;; --- the scripted seam ------------------------------------------------------------

(defun odds-url (base path params)
  "The URL HTTP-JSON would ask for, so a test matches on what was asked."
  (format nil "~a~a~@[?~a~]" base path (nodecode-oddsradar::query-string params)))

(defmacro with-odds-api ((calls &key pages config) &body body)
  "Run BODY with the cell started on CONFIG and HTTP-JSON stubbed: PAGES is a
list of (MATCH . ANSWER), and the first whose MATCH is a substring of the
full URL answers -- ANSWER a JSON value (a hash table or a vector), or a
function of (BASE PATH PARAMS) answering one. Every call is recorded into
CALLS, with PUSH, so the newest is first."
  (let ((pairs (or config '(quote ("sparkline" nil)))))
    `(with-cell-stop ((apply #'oddsradar-start ,pairs))
       (let ((,calls '()) (odds-pages ,pages))
         (with-stubbed-fdefinition (nodecode-oddsradar::http-json (base path params timeout)
                                      (declare (ignore timeout))
                                      (let* ((url (odds-url base path params))
                                             (entry (find-if (lambda (pair) (search (car pair) url))
                                                             odds-pages)))
                                        (push (list url params) ,calls)
                                        (unless entry
                                          (nodecode-oddsradar::fail "no page for ~a" url))
                                        (let ((answer (cdr entry)))
                                          (if (functionp answer)
                                              (funcall answer base path params)
                                              answer))))
           ,@body)))))

;;; --- the JSON the two APIs answer with ---------------------------------------------

(defun odds-array (items)
  "ITEMS as the JSON text Gamma double-encodes an array as."
  (nlk:encode-json-object (coerce items 'vector)))

(defun odds-object (&rest specs)
  "One JSON object built from alternating key/value pairs."
  (apply #'nlk:make-json-object specs))

(defun odds-market (&rest specs)
  "A Gamma market record: the binary market the tests watch, with SPECS laid
over it. Prices, outcomes and token ids arrive as JSON text, as Gamma sends
them."
  (apply #'nlk:make-json-object
         (append (list "slug" "fed-cut-december"
                       "question" "Fed cuts rates in December?"
                       "outcomes" (odds-array '("Yes" "No"))
                       "outcomePrices" (odds-array '("0.40" "0.60"))
                       "clobTokenIds" (odds-array '("111" "222"))
                       "closed" nil
                       "volume24hr" 54200
                       "volumeNum" 1600000
                       "liquidityNum" 98200
                       "oneDayPriceChange" 0.065
                       "oneWeekPriceChange" 0.06
                       "endDate" "2026-12-10T00:00:00Z"
                       "bestBid" 0.39
                       "bestAsk" 0.41
                       "groupItemTitle" "")
                 specs)))

(defun odds-midpoint (mid)
  "The CLOB's midpoint answer."
  (odds-object "mid" mid))

(defun odds-history (&rest prices)
  "The CLOB's price-history answer for PRICES."
  (odds-object "history" (coerce (loop for price in prices
                                        collect (odds-object "t" 0 "p" price))
                                  'vector)))





