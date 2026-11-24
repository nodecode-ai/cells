;;;; cell.lisp --- the five tools, the /oddsradar command and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model reads the market with the odds_* tools, the operator with
;;;; /oddsradar; both answer the same blocks, built by MARKETS.LISP and
;;;; WATCH.LISP. The section of the config is `oddsradar':
;;;;   "oddsradar": {"enabled": true, "max_watches": 50, "timeout_seconds": 10,
;;;;                 "movers_min_volume": 50000, "sparkline": true,
;;;;                 "data_file": ""}
;;;; No key: both Polymarket hosts are public and read-only.

(in-package #:nodecode-oddsradar)

;;; --- the primer ------------------------------------------------------------------

(defparameter +primer+
  "Polymarket odds are reachable through the nodecode-oddsradar cell: five tools the
model calls and one slash command, /oddsradar.
  odds_search(query, limit)   Active prediction markets for a question, with each market's
      odds (prices are probabilities, 0-1). Limit 1-10.
  odds_market(market, outcome)   One market in full: every outcome's odds, the live CLOB
      midpoint, 24h and 7d moves in points, volume, liquidity, end date, resolution, a
      7-day sparkline. MARKET is a polymarket.com link or a slug odds_search returned;
      an event with several markets answers with their slugs to pick from.
  odds_watch(action, market, outcome, move_pts, above, below, id)   The user's persistent
      watchlist: add (needs market and at least one rule), remove (id or market), list.
      move_pts alerts when the odds move that many points from the last alert (5 when no
      other rule is given); above/below alert once when the odds cross that percent, and
      re-arm after a one-point pullback.
  odds_check(peek)   Every watch evaluated now: the alerts that fired (moves, crossings,
      resolutions). Firing resets that watch's baseline; peek=true looks without consuming.
  odds_movers(limit)   The biggest 24h moves among the most-traded markets.
  /oddsradar show <url|slug>     the same market block, for the operator; bare, it offers a
      row to type a market in. Also: /oddsradar <query>, watch, list, unwatch, check, movers.
Read-only: no key, no wallet, no trading. The watchlist is one JSON file in this home and
survives restarts, so odds_check run on a schedule is the recurring alert."
  "What (help :oddsradar) answers while the cell runs.")

;;; --- shared bits ------------------------------------------------------------------

(defun bounded (value low high fallback)
  "VALUE as an integer between LOW and HIGH, FALLBACK when it is not one."
  (if (integerp value) (max low (min high value)) fallback))

(defun nonblank (value)
  "VALUE, trimmed, or NIL when it is absent or only space."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

;;; --- the tools --------------------------------------------------------------------

(nlk:define-contract odds-search-call
  (:doc "Search ACTIVE Polymarket prediction markets and return events with each market's current odds (prices are probabilities, 0-1). Use it when the user asks what the odds or the market's probability of something are: elections, rates, crypto prices, sports, launches. Read-only.")
  ("query" :text :required t
           :doc "keywords, e.g. 'fed december cut' or 'bitcoin 150k'")
  ("limit" :integer :default 5
           :doc "how many events, 1 to 10"))

(defun tool-search (call)
  "The `odds_search' tool: the events a query finds, with their odds."
  (handler-case
      (fmt-search (search-markets (odds-search-call-query call)
                                  :limit (bounded (odds-search-call-limit call) 1 10 5)))
    (oddsradar-error (condition) (format nil "ERROR: ~a" condition))))

(nlk:define-contract odds-market-call
  (:doc "Live detail for one Polymarket market: outcome odds, the live CLOB midpoint, 24h and 7d moves in percentage points, volume, liquidity, end date, resolution status and a 7-day sparkline. If an event has several markets, the refusal lists their slugs: pick one and call again.")
  ("market" :text :required t
            :doc "a polymarket.com event or market URL, or a market slug odds_search returned")
  ("outcome" :string :default ""
             :doc "the outcome to price live; empty takes the first, usually Yes"))

(defun tool-market (call)
  "The `odds_market' tool: one market, as the block /oddsradar show prints."
  (handler-case
      (let* ((market (resolve-market (odds-market-call-market call)))
             (named (nonblank (odds-market-call-outcome call)))
             (outcome (if named
                          (nth (market-outcome-index market named) (market-outcomes market))
                          (first (market-outcomes market)))))
        (fmt-market market
                    (live-price market outcome)
                    (if (setting :sparkline) (sparkline market outcome) "")))
    (oddsradar-error (condition) (format nil "ERROR: ~a" condition))))

(nlk:define-contract odds-watch-call
  (:doc "Manage the user's persistent Polymarket watchlist, which survives sessions. action=add needs market and at least one rule: move_pts (alert when the odds move that many points from the last alert, 5 by default), above or below (alert once when the odds cross that percent, re-armed after a one-point pullback). action=remove takes id or market. action=list shows every watch. Alerts come from odds_check, which the operator can also run on a schedule.")
  ("action" :string :required t :options '("add" "remove" "list")
            :doc "add, remove or list")
  ("market" :string :default ""
            :doc "a polymarket.com link or a slug, for add and remove")
  ("id" :string :default ""
        :doc "the watch id, for remove")
  ("outcome" :string :default ""
             :doc "the outcome to watch; empty takes the first, usually Yes")
  ("move_pts" :number
              :doc "alert on a move of this many points, e.g. 5")
  ("above" :number
           :doc "alert when the odds rise to this percent, e.g. 60")
  ("below" :number
           :doc "alert when the odds fall to this percent, e.g. 40"))

(defun tool-watch (call)
  "The `odds_watch' tool: the watchlist added to, removed from, or listed."
  (handler-case
      (let ((action (string-downcase (odds-watch-call-action call))))
        (cond
          ((equal action "list") (fmt-watches (load-watches)))
          ((equal action "remove")
           (let ((gone (remove-watch (or (nonblank (odds-watch-call-id call))
                                         (nonblank (odds-watch-call-market call))))))
             (if gone
                 (format nil "Removed [~a] ~a" (watch-id gone) (watch-title gone))
                 "No watch matches that id or market.")))
          ((equal action "add")
           (let* ((above (odds-watch-call-above call))
                  (below (odds-watch-call-below call))
                  (move (let ((given (odds-watch-call-move-pts call)))
                          (cond (given given)
                                ((or above below) nil)
                                (t 5d0))))
                  (watch (add-watch (or (nonblank (odds-watch-call-market call)) "")
                                    :outcome (nonblank (odds-watch-call-outcome call))
                                    :move move :above above :below below)))
             (format nil "Watching [~a] ~a~%   now ~a · alert on ~a~%   Run odds_check, or /oddsradar check, to see them."
                     (watch-id watch) (watch-title watch)
                     (pct (watch-baseline watch)) (rules-describe watch))))
          (t (fail "action must be add, remove or list"))))
    (oddsradar-error (condition) (format nil "ERROR: ~a" condition))))

(nlk:define-contract odds-check-call
  (:doc "Evaluate every watched market now and answer the alerts that fired: moves, threshold crossings, resolutions. Firing an alert resets that watch's baseline; peek=true looks without consuming anything. A schedule that runs odds_check (or /oddsradar check) is the recurring alert.")
  ("peek" :boolean :default nil
          :doc "do not reset baselines or consume alerts"))

(defun tool-check (call)
  "The `odds_check' tool: the alerts that fired, or a line saying none did."
  (handler-case
      (let ((peek (odds-check-call-peek call)))
        (multiple-value-bind (alerts errors) (check-watches :commit (not peek))
          (let ((lines (mapcar #'alert-text alerts)))
            (when errors
              (setf lines (append lines (mapcar (lambda (error) (format nil "(skipped ~a)" error))
                                                errors))))
            (cond (lines (format nil "~{~a~^~%~}" lines))
                  (peek "No alerts: every watched market is inside its rules.")
                  (t "No alerts: every watched market is inside its rules.")))))
    (oddsradar-error (condition) (format nil "ERROR: ~a" condition))))

(nlk:define-contract odds-movers-call
  (:doc "The biggest 24h odds moves among the most-traded active Polymarket markets (min $50K 24h volume). Use it for 'what is moving on Polymarket today'.")
  ("limit" :integer :default 8
           :doc "how many, 1 to 20"))

(defun tool-movers (call)
  "The `odds_movers' tool: the markets that moved most in a day."
  (handler-case
      (fmt-movers (movers :limit (bounded (odds-movers-call-limit call) 1 20 8)))
    (oddsradar-error (condition) (format nil "ERROR: ~a" condition))))

;;; --- the command ------------------------------------------------------------------

(defun command-tokens (args)
  "ARGS, as the operator typed them, split into a command and its arguments."
  (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space #\Tab)))

(defun split-options (tokens)
  "TOKENS => (values POSITIONAL OPTIONS), OPTIONS the k=v pairs, keys lowered."
  (let ((positional '()) (options '()))
    (dolist (token tokens)
      (let ((at (and (not (uiop:string-prefix-p "http" token))
                     (position #\= token))))
        (if at
            (push (cons (string-downcase (string-left-trim "-" (subseq token 0 at)))
                        (subseq token (1+ at)))
                  options)
            (push token positional))))
    (values (nreverse positional) (nreverse options))))

(defun option-number (options key &optional (default nil))
  "OPTIONS' KEY as a number: DEFAULT when absent, NIL for off/none/no, and a
refusal for anything else."
  (let ((value (cdr (assoc key options :test #'equal))))
    (cond ((null value) default)
          ((member (string-downcase value) '("" "off" "none" "no") :test #'equal) nil)
          (t (or (val->number value)
                 (fail "~a= needs a number, got ~s" key value))))))

(defun command-market (market)
  "MARKET read, with the sparkline the section allows."
  (fmt-market market (live-price market)
              (if (setting :sparkline) (sparkline market) "")))

(defun run-oddsradar (args session-id)
  "The /oddsradar command: the same engine the tools call, in the conversation."
  (declare (ignore session-id))
  (let ((tokens (command-tokens args)))
    (handler-case
        (if (null tokens)
            (values "Polymarket odds and your watchlist"
                    (nle:list-dialog
                     :title "Odds Radar"
                     :summary "read a market, search, or check your watchlist"
                     :rows (list (nle:ask-row "Read a market" "Market" "/oddsradar show"
                                               :placeholder "a polymarket.com link or a slug"
                                               :verb "show")
                                 (nle:ask-row "Search markets" "Query" "/oddsradar search"
                                               :placeholder "fed rates" :verb "search")
                                 (nle:ask-row "Check the watchlist" "Anything" "/oddsradar check"
                                               :placeholder "leave it empty" :verb "check"))))
            (let ((command (string-downcase (first tokens)))
                  (rest (rest tokens)))
              (cond
                ((member command '("help" "-h" "--help") :test #'equal) +primer+)
                ((equal command "show")
                 (if (null rest)
                     "Usage: /oddsradar show <polymarket url or slug>"
                     (command-market (resolve-market (first rest)))))
                ((equal command "watch")
                 (multiple-value-bind (positional options) (split-options rest)
                   (if (null positional)
                       "Usage: /oddsradar watch <polymarket url or slug> [move=5] [above=60] [below=40] [outcome=Yes]"
                       (let* ((above (option-number options "above"))
                              (below (option-number options "below"))
                              (move (if (or (assoc "above" options :test #'equal)
                                            (assoc "below" options :test #'equal))
                                        (option-number options "move")
                                        (option-number options "move" 5d0)))
                              (watch (add-watch (first positional)
                                                :outcome (cdr (assoc "outcome" options :test #'equal))
                                                :move move :above above :below below)))
                         (format nil "Watching [~a] ~a~%   now ~a · alert on ~a~%   Run check, or odds_check, to see them."
                                 (watch-id watch) (watch-title watch)
                                 (pct (watch-baseline watch)) (rules-describe watch))))))
                ((member command '("list" "ls" "watches") :test #'equal)
                 (fmt-watches (load-watches)))
                ((member command '("unwatch" "rm" "remove") :test #'equal)
                 (if (null rest)
                     "Usage: /oddsradar unwatch <watch id or slug>"
                     (let ((gone (remove-watch (first rest))))
                       (if gone
                           (format nil "Removed [~a] ~a" (watch-id gone) (watch-title gone))
                           (format nil "No watch matches ~s." (first rest))))))
                ((equal command "check")
                 (multiple-value-bind (alerts errors) (check-watches :commit t)
                   (let ((lines (mapcar #'alert-text alerts)))
                     (when errors
                       (setf lines (append lines (mapcar (lambda (error)
                                                           (format nil "(skipped ~a)" error))
                                                         errors))))
                     (if lines (format nil "~{~a~^~%~}" lines)
                         "No alerts: every watched market is inside its rules."))))
                ((equal command "movers") (fmt-movers (movers)))
                ((equal command "search")
                 (fmt-search (search-markets (format nil "~{~a~^ ~}" rest))))
                (t (fmt-search (search-markets (format nil "~{~a~^ ~}" tokens)))))))
      (oddsradar-error (condition) (format nil "Odds Radar: ~a" condition)))))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell oddsradar
  (:section ("oddsradar")
    (:guide "nothing is required: both Polymarket hosts are public and keyless; data_file names where the watchlist lives, and max_watches, timeout_seconds and movers_min_volume bound one check")
    ("max_watches" :integer :default 50 :min 1
                   :doc "the most watches the list may hold")
    ("timeout_seconds" :integer :default 10 :min 1
                       :doc "how long one Polymarket call may wait")
    ("movers_min_volume" :integer :default 50000 :min 0
                         :doc "the 24h volume a market needs before odds_movers ranks it")
    ("sparkline" :boolean :default t
                 :doc "draw the 7-day sparkline in odds_market and /oddsradar show")
    ("data_file" :path
                 :doc "the watchlist file; empty keeps it under this home at oddsradar/watchlist.json"))
  (:tool "odds_search" (nlk:contract 'odds-search-call) 'tool-search)
  (:tool "odds_market" (nlk:contract 'odds-market-call) 'tool-market)
  (:tool "odds_watch" (nlk:contract 'odds-watch-call) 'tool-watch)
  (:tool "odds_check" (nlk:contract 'odds-check-call) 'tool-check)
  (:tool "odds_movers" (nlk:contract 'odds-movers-call) 'tool-movers)
  (:command "oddsradar" 'run-oddsradar
            :description "Polymarket odds, your watchlist and its alerts"
            :argument-hint "[query | show | watch | list | unwatch | check | movers]")
  (:help :oddsradar "oddsradar: Polymarket odds, a persistent watchlist and its alerts" +primer+))
