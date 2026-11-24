;;;; cell.lisp --- the primer, the six tools, /polymarket, and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :polymarket) answers the primer below and every request carries one
;;;; line naming it. The six tools take what the model calls them with, and
;;;; each answers text the model can read back -- plain lines, or markdown for
;;;; the briefing. Nothing here writes anything but the watchlist file, and
;;;; nothing trades: Polymarket's two public endpoints are read, and no more.

(in-package #:nodecode-polymarket)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Read Polymarket prediction markets, through the nodecode-polymarket cell: six
tools, and the Lisp verbs below with the same names in the polymarket package,
called through eval. Every answer is text; a refusal is ERROR: POLYMARKET-ERROR
with the reason. All of it is public, read-only data: no key, no wallet, and
nothing here can trade.
  polymarket_scan      mode trending (busiest), movers (biggest 24h price change)
      or closing_soon; limit 1-15; hours is closing_soon's window.
  polymarket_search    query: the words a person would say. Run it before
      polymarket_market when the operator names a topic, not a link.
  polymarket_market    one market in depth: every outcome's odds, bid/ask,
      liquidity, volume, a 7-day price summary and any flags. Takes a slug, a
      numeric id or a polymarket.com link.
  polymarket_signals   the markets worth a closer look: big 24h moves, volume
      spikes, wide spreads, outcome prices that do not add to 100%, and
      undecided markets about to close. Prompts to look, never predictions.
  polymarket_watch     action add (market, plus any of above/below percent
      lines and a move of N points in 24h), remove, list, or check. check
      answers only the alerts that are new since the last check, so it is safe
      to run on a schedule.
  polymarket_brief     one finished markdown briefing: watchlist alerts, the
      biggest movers, what is worth a look, what closes soon.
A yes price of 0.62 is about a 62% implied chance: a price, not a fact. Points
are percentage points of that price. Always give the operator the market link
so they can check the live price, never present odds as a trade, and report the
numbers as they came back rather than inventing any. The operator can also type
/polymarket for the same lists."
  "What (help :polymarket) answers while the cell runs.")

;;; --- what the model calls --------------------------------------------------------

(nlk:define-contract scan-call
  (:doc "List live Polymarket markets: trending (the busiest by 24h volume), movers (the biggest 24h price changes), or closing_soon (resolving within the next few days). Read-only public data.")
  ("mode" :string :default "trending" :options '("trending" "movers" "closing_soon")
          :doc "which ranking: trending, movers or closing_soon")
  ("limit" :integer :doc "how many markets to return (1-15); the section's default_limit when absent")
  ("hours" :integer :doc "closing_soon only: the window, in hours (the section's closing_hours when absent)"))

(nlk:define-contract search-call
  (:doc "Search Polymarket by keyword -- a person, team, token, event or topic -- and answer the matching open markets with their current odds. Run it before polymarket_market when the operator names a topic instead of giving a market link.")
  ("query" :text :required t :doc "the words to search for, e.g. fed rate cut or bitcoin 150k")
  ("limit" :integer :doc "how many markets to return (1-15)"))

(nlk:define-contract market-call
  (:doc "One Polymarket market in depth: the odds of every outcome, bid/ask, liquidity, volume, time to resolution, a 7-day price summary and any attention flags. Takes a market slug, a numeric id, or a polymarket.com link.")
  ("market" :text :required t :doc "a market slug, a numeric id, or a polymarket.com URL"))

(nlk:define-contract signals-call
  (:doc "Scan the most active Polymarket markets and flag the ones worth a closer look: big 24h moves, volume spikes, wide bid/ask spreads, outcome prices that do not add to 100%, and undecided markets about to close. Heuristic prompts to investigate, not predictions or trade advice.")
  ("limit" :integer :doc "how many flagged markets to return (1-15)"))

(nlk:define-contract watch-call
  (:doc "A personal watchlist of Polymarket markets with alert rules. add needs market, plus any of above/below price lines (percentages: 70 means 70%) and a move trigger (percentage points in 24h); remove, list, and check. check evaluates every rule now and reports only NEW alerts, so it is safe to run on a schedule.")
  ("action" :string :required t :options '("add" "remove" "list" "check")
            :doc "add, remove, list or check")
  ("market" :text :doc "a slug, id or link (add), or a slug or words from its question (remove)")
  ("above" :number :doc "alert when the yes price rises to this percent or more")
  ("below" :number :doc "alert when the yes price falls to this percent or less")
  ("move" :number :doc "alert on a move of this many percentage points in 24h"))

(nlk:define-contract brief-call
  (:doc "Build a whole prediction-market briefing in one call: watchlist alerts, the biggest 24h movers, the markets worth a closer look, and what closes soon. Answers ready-to-send markdown, for a daily message or a scheduled brief.")
  ("limit" :integer :doc "how many markets each list carries (1-10)"))

;;; --- what a number becomes ---------------------------------------------------------

(defun clamp (value low high default)
  "VALUE held between LOW and HIGH, DEFAULT when it is absent."
  (let ((value (or value default)))
    (max low (min high (if (integerp value) value (round value))))))

(defun limit-or (value &optional (high 15))
  "VALUE as a list length: 1 to HIGH, the section's default_limit by default."
  (clamp value 1 high (setting :default-limit)))

(defun pool ()
  "The busiest open markets, a hundred of them: the pool every ranking reads."
  (list-markets :limit 100))

;;; --- the verbs, one per tool ---------------------------------------------------------

(define-verb scan (mode &key limit hours)
  "MODE as a list of markets, in the words the model reads back."
  (let ((mode (if (member mode '("trending" "movers" "closing_soon") :test #'equal)
                  mode "trending"))
        (limit (limit-or limit)))
    (cond ((equal mode "movers")
           (market-list "Biggest 24h movers" (movers (pool) limit)))
          ((equal mode "closing_soon")
           (let* ((window (clamp hours 1 1440 (setting :closing-hours)))
                  (now (get-universal-time))
                  (markets (list-markets
                            :limit 100 :now now
                            :end-date-min (iso-time now)
                            :end-date-max (iso-time (+ now (* 3600 window))))))
             (market-list "Closing soon" (closing-soon markets limit window))))
          (t (let* ((markets (list-markets :limit (max (* limit 3) 30)))
                    (floor (setting :min-volume-24h))
                    (busy (remove-if-not (lambda (m) (>= (getf m :volume-24h) floor))
                                         markets)))
               (market-list "Trending on Polymarket"
                            (subseq busy 0 (min limit (length busy)))))))))

(define-verb search (query &key limit)
  "The markets matching QUERY, as text."
  (let ((query (nlk:trimmed (or query ""))))
    (when (zerop (length query)) (fail "a search needs words to search for"))
    (market-list (format nil "Results for '~a'" query)
                 (search-markets query :limit (limit-or limit)))))

(define-verb market (ref)
  "One market in depth, as text: every outcome, the spread, a 7-day summary, its flags."
  (let ((market (get-market ref)))
    (unless market
      (fail "no Polymarket market found for ~s; polymarket_search finds one by name" ref))
    (let ((lines (list (format nil "**~a**" (getf market :question))
                       (or (getf market :url) "")
                       ""
                       (format nil "Odds: ~{~a~^ · ~}"
                               (loop for name in (getf market :outcomes)
                                     for price in (getf market :prices)
                                     collect (format nil "~a ~a" name (pct price))))
                       (format nil "24h change ~a · volume 24h ~a · total ~a · liquidity ~a · ends ~a"
                               (pts (getf market :change-1d)) (money (getf market :volume-24h))
                               (money (getf market :volume-total)) (money (getf market :liquidity))
                               (ends-in (getf market :hours)))
                       (format nil "Bid ~a · ask ~a · spread ~a"
                               (pct (getf market :bid)) (pct (getf market :ask))
                               (let ((spread (getf market :spread)))
                                 (if spread (format nil "~,1f pts" (* spread 100)) "n/a")))))
          (history (history-of market))
          (flags (compute-signals market)))
      (when history
        (setf lines (append lines (list (format nil "7 days: ~,2f -> ~,2f (high ~,2f, low ~,2f, trend ~a, ~d points)"
                                                 (getf history :start) (getf history :latest)
                                                 (getf history :high) (getf history :low)
                                                 (getf history :trend) (getf history :points))))))
      (dolist (flag flags)
        (setf lines (append lines (list (format nil "- ~a" (third flag))))))
      (format nil "~{~a~^~%~}" (append lines (list (format nil "_~a_" *disclaimer*)))))))

(defun history-of (market)
  "MARKET's yes token's 7-day series summarised, or NIL when there is none."
  (let* ((tokens (getf market :tokens))
         (index (getf market :yes-index)))
    (when (and tokens (< index (length tokens)))
      (let ((series (handler-case (price-history (nth index tokens))
                      (polymarket-error () nil))))
        (history-stats series)))))

(define-verb signals (&key limit)
  "The markets worth a closer look, as text."
  (signal-list "Worth a closer look" (rank-signals (pool) (limit-or limit))))

(define-verb watch (action &key market above below move)
  "The watchlist: add, remove, list or check."
  (let ((action (string-downcase (if (and action (plusp (length (nlk:trimmed action))))
                                     (nlk:trimmed action) "list"))))
    (cond ((equal action "list")
           (let ((items (read-watchlist)))
             (if items
                 (format nil "**Watchlist**~%~{~a~^~%~}"
                         (loop for item in items for n from 1
                               collect (format nil "~d. ~a — ~a" n (getf item :question)
                                               (describe-rule item))))
                 "Your watchlist is empty. Add one with polymarket_watch action=add market=<slug>.")))
          ((equal action "add")
           (let ((ref (nlk:trimmed (or market ""))))
             (when (zerop (length ref)) (fail "a market is needed to add: a slug, an id or a link"))
             (let ((found (or (get-market ref)
                              (fail "no Polymarket market found for ~s" ref))))
               (let ((items (add-rule (read-watchlist) found :above above :below below :move move)))
                 (write-watchlist items)
                 (let ((item (find (getf found :slug) items
                                   :key (lambda (i) (getf i :slug)) :test #'equal)))
                   (format nil "Watching: ~a — ~a. It is ~a now."
                           (getf found :question) (describe-rule item) (pct (getf found :yes))))))))
          ((equal action "remove")
           (multiple-value-bind (items removed) (remove-rule (read-watchlist) (or market ""))
             (if removed
                 (progn (write-watchlist items) "Removed from the watchlist.")
                 (format nil "Nothing on the watchlist matches ~s." (or market "")))))
          ((equal action "check")
           (multiple-value-bind (new active errors) (check-watchlist #'get-market)
             (declare (ignore active))
             (format nil "~@[~a~%~%~]~@[~{_Note: ~a_~%~}~]checked ~d market~:p: ~a."
                     (and new (alert-list "Watchlist alerts" new))
                     errors (length (read-watchlist))
                     (if new (format nil "~d new alert~:p" (length new)) "no new alert"))))
          (t (fail "action is add, remove, list or check, not ~a" action)))))

(define-verb brief (&key limit)
  "One markdown briefing of everything, ready to send."
  (let* ((limit (limit-or limit 10))
         (errors '())
         (alerts (handler-case (multiple-value-bind (new active errors-seen)
                                  (check-watchlist #'get-market)
                                (declare (ignore active))
                                (setf errors errors-seen)
                                new)
                   (error (condition)
                     (setf errors (list (format nil "watchlist check skipped (~a)" condition)))
                     nil)))
         (pool (pool)))
    (brief-markdown alerts (movers pool limit) (rank-signals pool limit)
                    (closing-soon pool (min limit 3)) errors)))

;;; --- the tools ----------------------------------------------------------------

(defun scan-tool (call)
  (scan (scan-call-mode call) :limit (scan-call-limit call) :hours (scan-call-hours call)))

(defun search-tool (call)
  (search (search-call-query call) :limit (search-call-limit call)))

(defun market-tool (call)
  (market (market-call-market call)))

(defun signals-tool (call)
  (signals :limit (signals-call-limit call)))

(defun watch-tool (call)
  (watch (watch-call-action call)
         :market (watch-call-market call)
         :above (watch-call-above call)
         :below (watch-call-below call)
         :move (watch-call-move call)))

(defun brief-tool (call)
  (brief :limit (brief-call-limit call)))

;;; --- /polymarket -------------------------------------------------------------------

(defun dashboard ()
  "What /polymarket shows when it is typed alone: the lists it can answer."
  (nle:list-dialog
   :title "Polymarket"
   :summary "Live odds, movers, signals, a watchlist and a briefing, read-only."
   :rows (list (nle:run-row "The briefing" "/polymarket brief"
                             :detail "alerts, movers, what is worth a look, what closes soon")
               (nle:run-row "Trending" "/polymarket trending" :detail "the busiest markets now")
               (nle:run-row "Movers" "/polymarket movers" :detail "the biggest 24h price changes")
               (nle:run-row "Closing soon" "/polymarket closing" :detail "what resolves in the next days")
               (nle:run-row "Signals" "/polymarket signals" :detail "unusual volume, spreads and pricing")
               (nle:run-row "Watchlist" "/polymarket watch list" :detail "the markets you asked to be told about"))))

(defun slash (text)
  "TEXT after /polymarket as (values TEXT DIALOG)."
  (let* ((words (uiop:split-string (or text "") :separator " "))
         (sub (string-downcase (or (first words) ""))))
    (cond ((member sub '("" "brief" "b") :test #'equal)
           (values (brief) (and (zerop (length (nlk:trimmed text))) (dashboard))))
          ((member sub '("help" "?" "-h" "--help") :test #'equal)
           (values +primer+ (dashboard)))
          ((equal sub "trending") (values (scan "trending") nil))
          ((member sub '("movers" "mover") :test #'equal) (values (scan "movers") nil))
          ((member sub '("closing" "closing_soon" "soon") :test #'equal)
           (values (scan "closing_soon") nil))
          ((member sub '("signals" "signal") :test #'equal) (values (signals) nil))
          ((equal sub "search")
           (if (rest words)
               (values (search (format nil "~{~a~^ ~}" (rest words))) nil)
               (fail "usage: /polymarket search <words>")))
          ((equal sub "market")
           (if (second words)
               (values (market (second words)) nil)
               (fail "usage: /polymarket market <slug or link>")))
          ((equal sub "watch") (values (slash-watch (rest words)) nil))
          (t (fail "unknown command ~a; /polymarket help lists them" sub)))))

(defun slash-watch (words)
  "The watch subcommands, with KEY VALUE pairs after add."
  (let ((action (string-downcase (or (first words) "list"))))
    (if (member action '("add" "remove") :test #'equal)
        (let ((ref (second words))
              (above nil) (below nil) (move nil))
          (loop for (key value) on (cddr words) by #'cddr
                do (cond ((equal key "above") (setf above (ignore-errors (parse-number value))))
                         ((equal key "below") (setf below (ignore-errors (parse-number value))))
                         ((equal key "move") (setf move (ignore-errors (parse-number value))))))
          (watch action :market ref :above above :below below :move move))
        (watch action))))

(defun parse-number (value)
  "VALUE as a real, or NIL."
  (ignore-errors (multiple-value-bind (number end) (read-from-string value nil nil)
                   (and (realp number) (= end (length value)) (float number 1d0)))))

(defun run-slash (args session-id)
  "/polymarket: the briefing alone, or one of the lists it names."
  (declare (ignore session-id))
  (handler-case (slash args)
    (polymarket-error (condition)
      (values (format nil "ERROR: ~a" (polymarket-error-detail condition)) nil))
    (error (condition)
      (values (format nil "Something went wrong (~a). Try again in a moment." condition) nil))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell polymarket
  (:section ("polymarket")
    (:guide "nothing here needs a key: Polymarket's Gamma and CLOB endpoints are public. min_volume_24h ignores quiet markets, move_pts says what counts as a big 24h move, closing_hours sets the closing-soon window, default_limit is how many markets each list shows, and watchlist is where the alert file lives under the home")
    ("min_volume_24h" :integer :default 5000 :min 0
                      :doc "ignore markets quieter than this in the last 24 hours (USD)")
    ("move_pts" :integer :default 8 :min 1
                :doc "a 24h price change of at least this many points is a big move")
    ("closing_hours" :integer :default 72 :min 1
                     :doc "how far ahead closing soon looks")
    ("default_limit" :integer :default 5 :min 1
                     :doc "how many markets each list shows (1-15)")
    ("wide_spread" :integer :default 5 :min 1
                   :doc "a bid/ask gap of at least this many points is wide")
    ("spike_ratio" :integer :default 3 :min 1
                   :doc "24h volume this many times its recent daily average is a spike")
    ("band_low" :integer :default 25 :min 1
                :doc "the lower edge, in percent, of the still-undecided band")
    ("band_high" :integer :default 75 :min 1
                 :doc "the upper edge, in percent, of the still-undecided band")
    ("timeout_seconds" :integer :default 10 :min 1
                       :doc "one Polymarket request's ceiling")
    ("watchlist" :string :default "polymarket/watchlist.sexp"
                 :doc "where the alert file is written, under the home"))
  (:tool "polymarket_scan" (nlk:contract 'scan-call) 'scan-tool)
  (:tool "polymarket_search" (nlk:contract 'search-call) 'search-tool)
  (:tool "polymarket_market" (nlk:contract 'market-call) 'market-tool)
  (:tool "polymarket_signals" (nlk:contract 'signals-call) 'signals-tool)
  (:tool "polymarket_watch" (nlk:contract 'watch-call) 'watch-tool)
  (:tool "polymarket_brief" (nlk:contract 'brief-call) 'brief-tool)
  (:help :polymarket "polymarket: live Polymarket odds, movers, attention signals, a price-alert watchlist and a briefing; /polymarket" +primer+)
  (:command "polymarket" 'run-slash
            :catches 'polymarket-error
            :description "Prediction markets: the briefing, movers, signals and a watchlist"
            :argument-hint "[brief|trending|movers|closing|signals|search <words>|market <slug>|watch ...]")
  (:start (lambda ()
            (forget-cache)
            (nle:on-stop #'forget-cache))))
