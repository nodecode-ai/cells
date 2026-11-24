;;;; cell-test.lisp --- the odds radar cell, end to end over a scripted Polymarket.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is the cell's stubbed HTTP-JSON; every watchlist is a file
;;;; of its own under the scratch home. Nothing here opens a socket, and no
;;;; test reads another's watches.

(in-package #:nodecode.test)

;;; --- lifecycle, settings and the boundary ----------------------------------------

(define-cell-lifecycle-tests "oddsradar"
  (:help :oddsradar)
  (:command "oddsradar")
  (:running
   (is (nle::find-tool "odds_search") "the search tool is registered")
   (is (nle::find-tool "odds_watch") "and the watchlist")
   (is (equal 10 (nodecode-oddsradar::setting :timeout-seconds)) "a call waits ten seconds by default")
   (is (= 50 (nodecode-oddsradar::setting :max-watches)) "and the list holds fifty watches")
   (is (nodecode-oddsradar::setting :sparkline) "the sparkline is drawn by default"))
  (:stopped
   (is (null (nle::find-tool "odds_search")) "stop takes every tool back out"))
  (:refused ("max_watches" 0) ("timeout_seconds" 0) ("movers_min_volume" -1))
  (:idle nodecode-oddsradar:oddsradar-error
         (nodecode-oddsradar:search-markets "x")
         (nodecode-oddsradar:movers)))

(deftest oddsradar-cell-refuses-a-call-with-no-market ()
  ;; A boundary the declaration types: a call without the one member the
  ;; contract requires never reaches the body, which is the repair the model
  ;; reads. A setting out of range refuses the start the same way, above.
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-oddsradar::odds-market-call)
                              (nlk:make-json-object "outcome" "Yes"))))
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-oddsradar::odds-watch-call)
                              (nlk:make-json-object "action" "keep")))))

;;; --- search -----------------------------------------------------------------------

(defun fed-search-answer ()
  "Gamma's public-search answer: one event with the one market it is about."
  (odds-object "events"
               (vector (odds-object "title" "Fed decision in December?"
                                     "slug" "fed-decision-in-december"
                                     "volume24hr" 54200
                                     "markets" (vector (odds-market))))))

(deftest oddsradar-cell-searches-active-markets ()
  (with-odds-api (calls :pages (list (cons "/public-search" (fed-search-answer))))
    (let ((text (oddsradar-tool "odds_search" "query" "fed cut")))
      (is (search "Fed decision in December?  (24h vol $54.2K)" text) "the event and its volume lead")
      (is (search "40.0%" text) "the odds print as probabilities")
      (is (search "slug: fed-cut-december" text) "and the slug the next call wants"))
    (let ((params (second (first calls))))
      (is (equal "fed cut" (cdr (assoc "q" params :test #'equal))) "the query travels")
      (is (equal "5" (nodecode-oddsradar::param-text (cdr (assoc "limit_per_type" params :test #'equal)))) "and the limit, as the query string writes it")
      (is (search "gamma-api.polymarket.com/public-search" (first (first calls))) "to the public search endpoint"))))

;;; --- one market -------------------------------------------------------------------

(deftest oddsradar-cell-reads-one-market ()
  (with-odds-api (calls :pages (list (cons "/markets?slug=fed-cut-december" (vector (odds-market)))
                                      (cons "/midpoint" (odds-midpoint "0.55"))
                                      (cons "/prices-history" (odds-history 0.31 0.35 0.40 0.55 0.62 0.60 0.71))))
    (let ((text (oddsradar-tool "odds_market" "market" "fed-cut-december")))
      (is (search "Fed cuts rates in December?" text) "the market's question leads the block")
      (is (search "Yes 55.0%" text) "the first outcome is priced at the live midpoint")
      (is (search "(live mid)" text) "and the block says the price is live")
      (is (search "24h +6.5 pts" text) "the 24h move is in points")
      (is (search "vol 24h $54.2K" text) "the volume is the short form")
      (is (search "ends 2026-12-10" text) "the end date is there")
      (is (search "7d " text) "and the sparkline is drawn")
      (is (search "polymarket.com/event/fed-cut-december" text) "with the page to read it on"))))

(deftest oddsradar-cell-refuses-a-url-that-is-not-polymarket ()
  (with-odds-api (calls :pages '())
    (let ((text (oddsradar-tool "odds_market" "market" "https://example.com/event/x")))
      (is (search "ERROR: Only polymarket.com links are supported." text) "the host is refused in words")
      (is (null calls) "and nothing was asked"))))

;;; --- the watchlist and its alerts -------------------------------------------------

(deftest oddsradar-cell-watches-a-market-and-alerts-on-a-crossing ()
  (let ((mid "0.42"))
    (with-odds-api (calls :pages (list (cons "/markets?slug=fed-cut-december" (vector (odds-market)))
                                        (cons "/midpoint" (lambda (&rest arguments)
                                                             (declare (ignore arguments))
                                                             (odds-midpoint mid)))))
      (let ((added (oddsradar-tool "odds_watch" "action" "add"
                                    "market" "fed-cut-december" "above" 45)))
        (is (search "Watching [" added) "the market is watched")
        (is (search "above 45.0%" added) "with the threshold rule the operator set")
        (is (= 1 (length (nodecode-oddsradar:load-watches))) "and one watch is on the list"))
      (setf mid "0.62")
      (let ((fired (oddsradar-tool "odds_check")))
        (is (search "crossed above 45.0%" fired) "the crossing fires")
        (is (search "42.0% → 62.0%" fired) "naming both prices")
        (is (search "fed-cut-december" fired) "and the market's page"))
      (let ((quiet (oddsradar-tool "odds_check")))
        (is (search "No alerts" quiet) "and a second check is quiet: the rule is edge-triggered")))))

(deftest oddsradar-cell-alerts-on-a-move-and-re-arms ()
  (let ((mid "0.50")
        (file (scratch-watchlist)))
    (with-odds-api (calls :config (list "data_file" file "sparkline" nil)
                            :pages (list (cons "/markets?slug=fed-cut-december" (vector (odds-market)))
                                         (cons "/midpoint" (lambda (&rest arguments)
                                                              (declare (ignore arguments))
                                                              (odds-midpoint mid)))))
      (oddsradar-tool "odds_watch" "action" "add" "market" "fed-cut-december" "move_pts" 5)
      (is (search "±5 pts" (oddsradar-tool "odds_watch" "action" "list")) "the move rule is the five points it was given")
      (setf mid "0.53")
      (is (search "No alerts" (oddsradar-tool "odds_check")) "three points is inside the rule")
      (setf mid "0.58")
      (let ((fired (oddsradar-tool "odds_check")))
        (is (search "▲ +8.0 pts" fired) "eight points fires the rule, with its direction")
        (is (search "50.0% → 58.0%" fired) "from the baseline it moved from"))
      (let ((peek (oddsradar-tool "odds_check" "peek" t)))
        (is (search "No alerts" peek) "the baseline moved, so nothing fires twice")))
    ;; The watchlist outlives the cell: it is a file, not memory.
    (with-odds-api (calls :config (list "data_file" file "sparkline" nil) :pages '())
      (is (= 1 (length (nodecode-oddsradar:load-watches))) "the watch survives a restart"))))
(deftest oddsradar-cell-removes-a-watch ()
  (with-odds-api (calls :pages (list (cons "/markets?slug=fed-cut-december" (vector (odds-market)))
                                      (cons "/midpoint" (odds-midpoint "0.42"))))
    (let* ((added (oddsradar-tool "odds_watch" "action" "add" "market" "fed-cut-december"))
           (id (subseq added (length "Watching [") (position #\] added))))
      (is (search "Removed [" (oddsradar-tool "odds_watch" "action" "remove" "id" id)) "the watch is removed by the id it was given")
      (is (null (nodecode-oddsradar:load-watches)) "and the list is empty again"))))

;;; --- movers and the sparkline -----------------------------------------------------

(deftest oddsradar-cell-ranks-the-movers ()
  (with-odds-api (calls :pages (list (cons "/markets?active="
                                            (vector (odds-market "slug" "fed-cut-december"
                                                                  "oneDayPriceChange" -0.18
                                                                  "volume24hr" 900000)
                                                    (odds-market "slug" "small-move"
                                                                  "question" "Small move?"
                                                                  "oneDayPriceChange" 0.02
                                                                  "volume24hr" 800000)))))
    (let ((text (oddsradar-tool "odds_movers" "limit" 2)))
      (is (search "Biggest 24h moves" text) "the block says what it is")
      (is (< (search "fed-cut-december" text) (search "small-move" text)) "the biggest move leads the list")
      (is (search "-18.0 pts" text) "a fall prints with its sign"))))

(deftest oddsradar-cell-draws-a-sparkline ()
  (let ((line (nodecode-oddsradar::spark '(0.31 0.35 0.40 0.55 0.62 0.60 0.71))))
    (is (= 7 (length line)) "one step per price")
    (is (char= (char line 0) (char nodecode-oddsradar::+bars+ 0)) "the lowest price is the lowest bar")
    (is (char= (char line (1- (length line))) (char nodecode-oddsradar::+bars+ 7)) "and the highest the highest")
    (is (equal "" (nodecode-oddsradar::spark '(0.5))) "one price is no curve")))

;;; --- the command ------------------------------------------------------------------

(deftest oddsradar-cell-command-reads-a-market ()
  (with-odds-api (calls :pages (list (cons "/markets?slug=fed-cut-december" (vector (odds-market)))
                                      (cons "/midpoint" (odds-midpoint "0.44"))))
    (let ((text (cell-entry "nodecode-oddsradar" "oddsradar" "show fed-cut-december" nil)))
      (is (search "Yes 44.0%" text) "show reads the market at its live price")
      (is (search "polymarket.com/event" text) "and prints its page"))))

(deftest oddsradar-cell-command-checks-the-watchlist ()
  (with-odds-api (calls :pages '())
    (is (search "No alerts" (cell-entry "nodecode-oddsradar" "oddsradar" "check" nil)) "an empty watchlist has nothing to alert")
    (is (search "Watchlist is empty" (cell-entry "nodecode-oddsradar" "oddsradar" "list" nil)) "and list says so")))

(deftest oddsradar-cell-bare-command-asks-for-a-market ()
  (with-odds-api (calls :pages '())
    (multiple-value-bind (text dialog) (cell-entry "nodecode-oddsradar" "oddsradar" "" nil)
      (is (search "watchlist" text) "the answer says what /oddsradar does")
      (is (search "Market" (nlk:encode-json-object dialog)) "and offers a row to type one in")
      (is (null calls) "asking costs no request"))))

