;;;; cell-test.lisp --- the cell against a stubbed Polymarket.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Polymarket, no
;;;; key is needed and the network stays out of it. What is proved is the
;;;; cell's own wiring: the section (a healthy start, and a value out of range
;;;; refused), the six tools through the dispatch the model calls them by, what
;;;; each asks the wire for, the watchlist's fire-once-and-re-arm rule, and
;;;; /polymarket.

(in-package #:nodecode.test)

(define-test-slice "polymarket" "POLYMARKET-CELL-" :start nodecode-polymarket:start-cell)

;;; --- fixtures ------------------------------------------------------------------

(defparameter *requests* '()
  "Every URL the stub answered, newest first.")

(defparameter *failure* nil
  "When a number, the HTTP status every request is answered with.")

(defparameter +market-json+
  (format nil "{\"id\":\"1\",\"slug\":\"test-pass\",\"question\":\"Will the test pass by 2027?\",~@
\"outcomes\":\"[\\\"Yes\\\",\\\"No\\\"]\",\"outcomePrices\":\"[\\\"0.62\\\",\\\"0.38\\\"]\",~@
\"clobTokenIds\":\"[\\\"tok-yes\\\",\\\"tok-no\\\"]\",\"volume24hr\":23456,\"volume1wk\":70000,~@
\"volumeNum\":345678,\"liquidityNum\":50000,\"oneDayPriceChange\":0.095,\"spread\":0.07,~@
\"bestBid\":0.61,\"bestAsk\":0.63,\"endDate\":\"2027-01-01T00:00:00Z\",\"closed\":false,\"active\":true}")
  "One Gamma market, as the wire spells it.")

(defun soon-json ()
  "One market resolving tomorrow, as the closing-soon window asks for."
  (format nil "{\"id\":\"2\",\"slug\":\"test-soon\",\"question\":\"Will the test resolve tomorrow?\",\"outcomes\":\"[\\\"Yes\\\",\\\"No\\\"]\",\"outcomePrices\":\"[\\\"0.5\\\",\\\"0.5\\\"]\",\"volume24hr\":90000,\"liquidityNum\":20000,\"oneDayPriceChange\":0.02,\"endDate\":\"~a\",\"closed\":false,\"active\":true}"
          (nodecode-polymarket::iso-time (+ (get-universal-time) 86400))))

(defun stub-body (url)
  "What Polymarket answers URL, as (values BODY STATUS): markets, a search, or a history."
  (cond ((search "/prices-history" url)
         (values (format nil "{\"history\":[{\"t\":~d,\"p\":0.41},{\"t\":~d,\"p\":0.55},{\"t\":~d,\"p\":0.62}]}"
                         (- (get-universal-time) 172800)
                         (- (get-universal-time) 86400)
                         (get-universal-time))
                 200))
        ((search "/public-search" url)
         (values (format nil "{\"events\":[{\"slug\":\"test-event\",\"markets\":[~a]}]}" +market-json+)
                 200))
        ((search "end_date_min" url) (values (format nil "[~a]" (soon-json)) 200))
        ((search "/markets" url) (values (format nil "[~a]" +market-json+) 200))
        (t (values "{}" 404))))

(defun stub-wire (method url &rest options)
  "NLK:HTTP, answering from the request's own URL; nothing leaves the machine."
  (declare (ignore method options))
  (push url *requests*)
  (if *failure*
      (values (format nil "{\"error\":\"upstream ~d\"}" *failure*) *failure*
              (make-hash-table :test #'equal) nil)
      (multiple-value-bind (body status) (stub-body url)
        (values body status (make-hash-table :test #'equal) nil))))

(defmacro with-polymarket-wire (&body body)
  "BODY with NLK:HTTP stubbed and no watchlist left from another test."
  `(let ((*requests* '())
         (*failure* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (apply #'stub-wire method url options))
       ,@body)))

(defmacro with-polymarket-running (&body body)
  "BODY with the cell started on its defaults, stopped on unwind."
  `(with-cell-stop ((polymarket-start))
     (let ((path (ignore-errors (nodecode-polymarket::watch-path))))
       (when (and path (probe-file path)) (ignore-errors (delete-file path))))
     ,@body))

(defun asked-for (fragment)
  "The newest URL the stub answered that holds FRAGMENT, or NIL."
  (find fragment *requests* :test #'search))

(defun tool-answer (name arguments)
  "NAME called the way the model calls it, as the text the model reads."
  (nle::message-content (execute-wire-call "call-1" name arguments)))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "polymarket"
  (:fixture with-polymarket-wire)
  (:help :polymarket)
  (:command "polymarket")
  (:running
   (is (nle::find-tool "polymarket_scan") "the scan tool is registered")
   (is (nle::find-tool "polymarket_search") "and the search")
   (is (nle::find-tool "polymarket_market") "and the deep dive")
   (is (nle::find-tool "polymarket_signals") "and the signals")
   (is (nle::find-tool "polymarket_watch") "and the watchlist")
   (is (nle::find-tool "polymarket_brief") "and the briefing")
   (is (= 6 (count-if (lambda (entry) (eql 0 (search "polymarket_" (first entry))))
                      nle::*tools*)) "six, no others"))
  (:stopped
   (is (null (nle::find-tool "polymarket_scan")) "and stop takes every one back")
   (is (null (nle::find-tool "polymarket_brief"))))
  (:refused ("default_limit" 0)
            ("timeout_seconds" 0)
            ("move_pts" 0)))

;;; --- the wire --------------------------------------------------------------------

(deftest polymarket-cell-scan-answers-trending-through-the-stub (with-polymarket-wire)
  (with-polymarket-running
    (let ((answer (tool-answer "polymarket_scan" "{\"mode\":\"trending\"}")))
      (is (search "Will the test pass by 2027?" answer) "the question is there")
      (is (search "62%" answer) "with its odds")
      (is (search "$23.5K" answer) "and its 24h volume")
      (is (search "polymarket.com/market/test-pass" answer) "and the link to check it")
      (is (search "gamma-api.polymarket.com" (first *requests*)) "asked of Gamma"))))

(deftest polymarket-cell-every-request-stays-on-the-two-hosts (with-polymarket-wire)
  (with-polymarket-running
    (tool-answer "polymarket_market" "{\"market\":\"test-pass\"}")
    (is (= 2 (length *requests*)) "one market, one price history")
    (is (every (lambda (url)
                 (or (uiop:string-prefix-p "https://gamma-api.polymarket.com" url)
                     (uiop:string-prefix-p "https://clob.polymarket.com" url)))
               *requests*) "and nothing else answered")))

(deftest polymarket-cell-a-host-off-the-allowlist-is-refused (with-polymarket-wire)
  (with-polymarket-running
    (is (signals-error nodecode-polymarket:polymarket-error
          (nodecode-polymarket::get-json "https://not-polymarket.test/markets")) "off the allowlist")
    (is (null *requests*) "so no request was made")))

(deftest polymarket-cell-search-passes-the-words-and-the-limit (with-polymarket-wire)
  (with-polymarket-running
    (let ((answer (tool-answer "polymarket_search" "{\"query\":\"fed rate cut\",\"limit\":3}")))
      (is (search "Results for 'fed rate cut'" answer) "the answer names what was searched")
      (is (search "Will the test pass by 2027?" answer) "and carries the match")
      (is (search "q=fed%20rate%20cut" (asked-for "/public-search")) "the words go out encoded")
      (is (search "limit_per_type=3" (asked-for "/public-search"))))))

(deftest polymarket-cell-market-reads-every-outcome-and-the-history (with-polymarket-wire)
  (with-polymarket-running
    (let ((answer (tool-answer "polymarket_market" "{\"market\":\"https://polymarket.com/market/test-pass\"}")))
      (is (search "Yes 62%" answer) "the yes outcome")
      (is (search "No 38%" answer) "and the no")
      (is (search "7 days:" answer) "the price history")
      (is (search "spread" answer) "the bid/ask gap")
      (is (search "bid/ask gap is 7.0 pts" answer) "and the wide-spread flag it earns")
      (is (search "tok-yes" (asked-for "/prices-history")) "history read from the yes token"))))

(deftest polymarket-cell-closing-soon-asks-gamma-for-its-window (with-polymarket-wire)
  (with-polymarket-running
    (let ((answer (tool-answer "polymarket_scan" "{\"mode\":\"closing_soon\",\"hours\":48}")))
      (is (search "Closing soon" answer) "the closing-soon list")
      (is (search "Will the test resolve tomorrow?" answer) "with the market about to close")
      (is (search "ends 24h" answer) "and how long it has")
      (let ((url (asked-for "/markets")))
        (is (search "end_date_min=" url) "the window goes to Gamma as a date")
        (is (search "order=volume24hr" url))))))

(deftest polymarket-cell-signals-say-why-a-market-was-flagged (with-polymarket-wire)
  (with-polymarket-running
    (let ((answer (tool-answer "polymarket_signals" "{}")))
      (is (search "Worth a closer look" answer))
      (is (search "Why:" answer) "each flagged market says why")
      (is (search "moved +9.5 pts" answer) "the big move is named with its size"))))

(deftest polymarket-cell-a-broken-wire-becomes-an-error-line (with-polymarket-wire)
  (with-polymarket-running
    (let ((*failure* 503))
      (let ((answer (tool-answer "polymarket_scan" "{}")))
        (is (search "ERROR" answer) "the tool answers text, not a signal")
        (is (search "HTTP 503" answer) "naming what the wire said")
        (is (>= (length *requests*) 2) "after the one retry")))))

;;; --- the watchlist ----------------------------------------------------------------

(deftest polymarket-cell-a-watch-rule-fires-once-and-re-arms (with-polymarket-wire)
  (with-polymarket-running
    (is (search "Watching: Will the test pass"
                (tool-answer "polymarket_watch"
                             "{\"action\":\"add\",\"market\":\"test-pass\",\"above\":50}")) "a rule at 50")
    (let ((listed (tool-answer "polymarket_watch" "{\"action\":\"list\"}")))
      (is (search "at or above 50%" listed) "and listed with its line"))
    (let ((first (tool-answer "polymarket_watch" "{\"action\":\"check\"}")))

      (is (search "Watchlist alerts" first) "the check fires once")
      (is (search "62% is at or above your 50% line" first) "with the rule's own words"))
    (let ((second (tool-answer "polymarket_watch" "{\"action\":\"check\"}")))

      (is (search "no new alert" second) "and the second check repeats nothing"))))

(deftest polymarket-cell-a-rule-out-of-percent-is-refused (with-polymarket-wire)
  (with-polymarket-running
    (let ((answer (tool-answer "polymarket_watch"
                                "{\"action\":\"add\",\"market\":\"test-pass\",\"above\":140}")))
      (is (search "ERROR" answer) "140 percent is no price line")
      (is (search "between 1 and 99 percent" answer)))))

(deftest polymarket-cell-remove-takes-a-market-off (with-polymarket-wire)
  (with-polymarket-running
    (tool-answer "polymarket_watch" "{\"action\":\"add\",\"market\":\"test-pass\"}")
    (is (search "Removed" (tool-answer "polymarket_watch"
                                         "{\"action\":\"remove\",\"market\":\"test-pass\"}")))
    (is (search "empty" (tool-answer "polymarket_watch" "{\"action\":\"list\"}")) "the list says so")))

;;; --- the briefing and the command ----------------------------------------------------

(deftest polymarket-cell-the-briefing-is-markdown-ready-to-send (with-polymarket-wire)
  (with-polymarket-running
    (let ((answer (tool-answer "polymarket_brief" "{}")))
      (is (search "# Prediction Market Brief" answer))
      (is (search "Biggest 24h movers" answer))
      (is (search "Closing soon" answer))
      (is (search "not financial advice" answer) "with the disclaimer it ships"))))

(deftest polymarket-cell-the-command-answers-and-offers-its-lists (with-polymarket-wire)
  (with-polymarket-running
    (multiple-value-bind (text dialog) (nodecode-polymarket::slash "movers")
      (is (search "Biggest 24h movers" text))
      (is (null dialog) "a named list needs no panel"))
    (multiple-value-bind (text dialog) (nodecode-polymarket::slash "")
      (is (search "# Prediction Market Brief" text) "typed alone, the briefing")
      (is dialog "and a panel over the lists it knows"))
    (multiple-value-bind (text) (nodecode-polymarket::slash "watch add test-pass above 70")
      (is (search "Watching:" text) "the watch subcommand takes its lines"))))
