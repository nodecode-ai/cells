;;;; markets.lisp --- one market: how Gamma's record reads, and how it prints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's Market dataclass and its pure helpers, in Lisp: a market
;;;; built out of a Gamma object (outcomes and prices arrive double-encoded as
;;;; JSON text, which JLIST and VAL->NUMBER read), a polymarket.com URL or a
;;;; bare slug resolved to the one market it names, the live CLOB midpoint
;;;; with the Gamma snapshot as fallback, and the blocks the tools and /oddsradar
;;;; answer with -- prices as probabilities, moves in points, a text sparkline.

(in-package #:nodecode-oddsradar)

;;; --- the market ------------------------------------------------------------------

(defstruct (market (:constructor make-market) (:copier nil))
  (slug "" :read-only t)
  (question "" :read-only t)
  (outcomes '("Yes" "No") :read-only t)
  (prices '() :read-only t)
  (token-ids '() :read-only t)
  (event-slug "" :read-only t)
  (event-title "" :read-only t)
  (label "" :read-only t)
  (closed nil :read-only t)
  (end-date "" :read-only t)
  (volume-24h 0d0 :read-only t)
  (volume 0d0 :read-only t)
  (liquidity 0d0 :read-only t)
  (change-1d nil :read-only t)
  (change-1w nil :read-only t)
  (best-bid nil :read-only t)
  (best-ask nil :read-only t))

(defun outcome-name (value)
  "VALUE, an outcome as Gamma writes it, as a string."
  (if (stringp value) value (princ-to-string value)))

(defun market-from-gamma (object &optional event)
  "The MARKET OBJECT describes, EVENT its Gamma event when the caller has it.
Outcomes and prices are double-encoded JSON text in Gamma's answer, so they
are read through JLIST; a market with no outcomes is the binary Yes/No."
  (let* ((event (or event
                     (let ((events (jlist (json-field object "events"))))
                       (and events (first events)))))
         (outcomes (mapcar #'outcome-name (jlist (json-field object "outcomes"))))
         (end-date (jtext object "endDate" "")))
    (make-market
     :slug (jtext object "slug" "")
     :question (let ((question (jtext object "question")))
                 (or question (jtext object "title" "") ""))
     :outcomes (or outcomes '("Yes" "No"))
     :prices (remove nil (mapcar #'val->number (jlist (json-field object "outcomePrices"))))
     :token-ids (mapcar #'outcome-name (jlist (json-field object "clobTokenIds")))
     :event-slug (jtext event "slug" "")
     :event-title (jtext event "title" "")
     :label (jtext object "groupItemTitle" "")
     :closed (truthful-p (json-field object "closed"))
     :end-date (subseq end-date 0 (min 10 (length end-date)))
     :volume-24h (or (jnum object "volume24hr") 0d0)
     :volume (or (jnum object "volumeNum") (jnum object "volume") 0d0)
     :liquidity (or (jnum object "liquidityNum") (jnum object "liquidity") 0d0)
     :change-1d (jnum object "oneDayPriceChange")
     :change-1w (jnum object "oneWeekPriceChange")
     :best-bid (jnum object "bestBid")
     :best-ask (jnum object "bestAsk"))))

(defun market-outcome-index (market outcome)
  "OUTCOME's position among MARKET's outcomes, matched without case, or a
refusal naming the outcomes it does have."
  (or (position (string-downcase (or outcome "Yes")) (market-outcomes market)
                :test #'equal :key #'string-downcase)
      (fail "~s is not an outcome of this market (outcomes: ~{~a~^, ~})"
            outcome (market-outcomes market))))

(defun market-price (market outcome)
  "MARKET's snapshot price for OUTCOME, or NIL when Gamma sent none."
  (let ((index (market-outcome-index market outcome)))
    (and (< index (length (market-prices market)))
         (nth index (market-prices market)))))

(defun market-url (market)
  "The polymarket.com page MARKET is read on."
  (let ((event (market-event-slug market))
        (slug (market-slug market)))
    (if (and (plusp (length event)) (not (equal event slug)))
        (format nil "https://polymarket.com/event/~a/~a" event slug)
        (format nil "https://polymarket.com/event/~a" slug))))

(defun market-resolved-outcome (market)
  "On a closed market, the outcome the odds settled on: it trades at 1."
  (let ((prices (market-prices market)))
    (when (and (market-closed market) prices)
      (let* ((index (position (reduce #'max prices) prices))
             (price (nth index prices)))
        (and (>= price 0.98d0) (nth index (market-outcomes market)))))))

;;; --- a URL or a slug -------------------------------------------------------------

(defparameter +slug-scanner+ (cl-ppcre:create-scanner "^[a-z0-9][a-z0-9-]{1,200}$")
  "What a bare Polymarket slug is made of.")

(defun parse-ref (ref)
  "REF, a polymarket.com URL or a bare slug, => (values EVENT-SLUG MARKET-SLUG)."
  (let ((ref (nlk:trimmed (or ref ""))))
    (when (zerop (length ref))
      (fail "Give a Polymarket URL or a market slug."))
    (if (or (search "://" ref) (uiop:string-prefix-p "polymarket.com" ref))
        (let* ((uri (quri:uri (if (search "://" ref) ref (concatenate 'string "https://" ref))))
               (host (string-downcase (or (quri:uri-host uri) "")))
               (parts (remove "" (uiop:split-string (or (quri:uri-path uri) "")
                                                    :separator "/")
                              :test #'equal)))
          (unless (or (equal host "polymarket.com")
                      (uiop:string-suffix-p ".polymarket.com" host))
            (fail "Only polymarket.com links are supported."))
          (unless (and (rest parts) (member (first parts) '("event" "market") :test #'equal))
            (fail "That link does not point to a Polymarket event or market."))
          (values (and (equal (first parts) "event") (second parts))
                  (cond ((equal (first parts) "market") (second parts))
                        ((cddr parts) (third parts)))))
        (progn
          (unless (cl-ppcre:scan +slug-scanner+ ref)
            (fail "~s is neither a Polymarket URL nor a slug." ref))
          (values nil ref)))))

(defun resolve-market (ref)
  "REF resolved to the one MARKET it names. An event with several markets
answers a refusal listing their slugs, which is the call's repair."
  (multiple-value-bind (event-slug market-slug) (parse-ref ref)
    (let ((timeout (setting :timeout-seconds)))
      (let ((record (and market-slug (market-by-slug market-slug timeout))))
        (when record
          (return-from resolve-market (market-from-gamma record)))
        (when (null event-slug)
          (setf event-slug market-slug))
        (let ((event (and event-slug (event-by-slug event-slug timeout))))
          (when (and (null event) event-slug (null market-slug))
            (let ((record (market-by-slug event-slug timeout)))
              (when record
                (return-from resolve-market (market-from-gamma record)))))
          (unless event
            (fail "No Polymarket market or event found for ~s." ref))
          (let* ((records (jlist (json-field event "markets")))
                 (open (remove-if (lambda (record) (truthful-p (json-field record "closed")))
                                  records))
                 (chosen (or open records)))
            (if (= 1 (length chosen))
                (market-from-gamma (first chosen) event)
                (let ((markets (sort (loop for record in chosen
                                           collect (market-from-gamma record event))
                                     #'> :key (lambda (one)
                                                (if (member "Yes" (market-outcomes one)
                                                            :test #'string-equal)
                                                    (or (market-price one "Yes") 0d0)
                                                    0d0)))))
                  (fail "~s has ~d markets; pick one slug:~%~{  ~a~^~%~}"
                        (jtext event "title" ref) (length chosen)
                        (loop for one in (subseq markets 0 (min 12 (length markets)))
                              collect (format nil "~a  (~a, ~a)"
                                              (market-slug one)
                                              (if (plusp (length (market-label one)))
                                                  (market-label one)
                                                  (market-question one))
                                              (pct (first (market-prices one))))))))))))))

(defun live-price (market &optional outcome)
  "MARKET's live price for OUTCOME: the CLOB midpoint while the market is
open, and Gamma's snapshot when the book does not answer."
  (let* ((index (if outcome (market-outcome-index market outcome) 0))
         (name (nth index (market-outcomes market)))
         (tokens (market-token-ids market)))
    (or (and (not (market-closed market))
             (< index (length tokens))
             (handler-case (midpoint (nth index tokens) (setting :timeout-seconds))
               (oddsradar-error () nil)))
        (market-price market name))))

;;; --- search, movers, the sparkline ------------------------------------------------

(define-verb search-markets (query &key (limit 5))
  "The active events QUERY finds, one plist each: the event, its volume, and
up to six of its open markets with their odds, highest first."
  (let ((query (nlk:trimmed (or query ""))))
    (when (zerop (length query))
      (fail "Search needs a query, e.g. 'bitcoin' or 'fed rates'."))
    (let ((events (search-events query limit (setting :timeout-seconds))))
      (loop for event in (subseq events 0 (min limit (length events)))
            collect (let* ((records (remove-if (lambda (record)
                                                 (truthful-p (json-field record "closed")))
                                               (jlist (json-field event "markets"))))
                           (markets (sort (loop for record in records
                                                collect (market-from-gamma record event))
                                          #'> :key (lambda (one)
                                                     (or (first (market-prices one)) 0d0)))))
                      (list :event (jtext event "title" "")
                            :event-slug (jtext event "slug" "")
                            :volume-24h (or (jnum event "volume24hr") 0d0)
                            :markets (loop for one in (subseq markets 0 (min 6 (length markets)))
                                           collect (list :slug (market-slug one)
                                                         :label (if (plusp (length (market-label one)))
                                                                    (market-label one)
                                                                    (market-question one))
                                                         :outcomes (market-outcomes one)
                                                         :prices (market-prices one)))
                            :more-markets (max 0 (- (length markets) 6))))))))

(define-verb movers (&key (limit 8))
  "The biggest 24h moves among the most-traded open markets, by absolute
points, skipping markets already effectively settled."
  (let* ((minimum (float (setting :movers-min-volume) 1d0))
         (pool (loop for record in (top-markets 150 (setting :timeout-seconds))
                     for one = (market-from-gamma record)
                     for prices = (market-prices one)
                     when (and (market-change-1d one)
                               (>= (market-volume-24h one) minimum)
                               (not (market-closed one))
                               prices
                               (< 0.015d0 (first prices) 0.985d0))
                       collect one)))
    (subseq (sort pool #'> :key (lambda (one) (abs (market-change-1d one))))
            0 (min limit (length pool)))))

(defparameter +bars+ "▁▂▃▄▅▆▇█"
  "The eight steps a sparkline is drawn with, the original's own.")

(defun spark (values &key (width 28))
  "VALUES, one price each, as a one-line sparkline of at most WIDTH steps."
  (when (< (length values) 2)
    (return-from spark ""))
  (let ((points (coerce values 'vector)))
    (when (> (length points) width)
      (let* ((step (/ (float (1- (length points)) 1d0) (1- width))))
        (setf points (coerce (loop for index below width
                                   collect (aref points (round (* index step))))
                             'vector))))
    (let ((low (reduce #'min points))
          (high (reduce #'max points)))
      (if (< (- high low) 1d-9)
          (make-string (length points) :initial-element (char +bars+ 3))
          (with-output-to-string (out)
            (loop for point across points
                  do (write-char (char +bars+ (min 7 (floor (* (/ (- point low) (- high low))
                                                              7.999d0))))
                                 out)))))))

(defun sparkline (market &optional outcome)
  "MARKET's 7-day sparkline for OUTCOME, or an empty string when the book
keeps no history for it."
  (let* ((index (if outcome (market-outcome-index market outcome) 0))
         (tokens (market-token-ids market)))
    (if (< index (length tokens))
        (spark (handler-case (price-history (nth index tokens) (setting :timeout-seconds))
                 (oddsradar-error () '())))
        "")))

;;; --- formatting ------------------------------------------------------------------

(defun pct (value)
  "VALUE, a probability, as a percentage: 75.5%, and 0.42% below one."
  (if (null value)
      "n/a"
      (let ((percent (* 100d0 value)))
        (if (or (>= percent 1d0) (zerop percent))
            (format nil "~,1f%" percent)
            (format nil "~,2f%" percent)))))

(defun money (value)
  "VALUE, dollars, as the short form a trader reads: $54.2K, $1.6M."
  (loop for (unit divisor) in '(("B" 1d9) ("M" 1d6) ("K" 1d3))
        when (>= (abs (float value 1d0)) divisor)
          do (return-from money (format nil "$~,1f~a" (/ (float value 1d0) divisor) unit)))
  (format nil "$~,0f" (float value 1d0)))

(defun number-text (value)
  "VALUE as a compact decimal: 5 for 5.0, 2.5 for 2.5."
  (let ((value (float value 1d0)))
    (if (= value (fround value))
        (princ-to-string (round value))
        (format nil "~,1f" value))))

(defun signed (value &optional (digits 1))
  "VALUE, a number of points, with its sign and DIGITS decimals: +6.5, -18.0."
  (let ((value (float value 1d0)))
    (format nil "~a~,vf" (if (minusp value) "-" "+") digits (abs value))))

(defun ago (seconds)
  "SECONDS as the coarse age an alert prints: 42s, 7m, 3h, 2d."
  (let ((seconds (max 0 (floor seconds))))
    (cond ((< seconds 90) (format nil "~ds" seconds))
          ((< seconds 5400) (format nil "~dm" (floor seconds 60)))
          ((< seconds 172800) (format nil "~dh" (floor seconds 3600)))
          (t (format nil "~dd" (floor seconds 86400))))))

(defun fmt-market (market live &optional sparkline)
  "The block /oddsradar show and the odds_market tool answer with."
  (let ((lines (list (if (and (plusp (length (market-label market)))
                               (not (search (market-label market) (market-question market)))
                               (search (market-label market) (market-question market)))
                          (market-question market)
                          (market-question market)))))
    (when (and (plusp (length (market-label market)))
               (not (search (market-label market) (market-question market))))
      (setf (first lines) (format nil "~a -- ~a" (market-question market) (market-label market))))
    (let ((odds (loop for name in (market-outcomes market)
                      for index from 0
                      for price = (if (and (zerop index) live)
                                      live
                                      (nth index (market-prices market)))
                      collect (format nil "~a ~a" name (pct price)))))
      (push (format nil "  ~{~a~^ · ~}~:[~;  (live mid)~]" odds (and live t)) lines))
    (when (or (market-change-1d market) (market-change-1w market)
              (and (market-best-bid market) (market-best-ask market)))
      (push (format nil "  ~{~a~^ · ~}"
                    (remove nil
                            (list (and (market-change-1d market)
                                       (format nil "24h ~a pts" (signed (* 100 (market-change-1d market)))))
                                  (and (market-change-1w market)
                                       (format nil "7d ~a pts" (signed (* 100 (market-change-1w market)))))
                                  (and (market-best-bid market) (market-best-ask market)
                                       (format nil "bid/ask ~a/~a"
                                               (pct (market-best-bid market))
                                               (pct (market-best-ask market)))))))
            lines))
    (push (format nil "  vol 24h ~a · total ~a · liquidity ~a~@[ · ends ~a~]"
                  (money (market-volume-24h market)) (money (market-volume market))
                  (money (market-liquidity market))
                  (and (plusp (length (market-end-date market))) (market-end-date market)))
          lines)
    (when (plusp (length (or sparkline "")))
      (push (format nil "  7d ~a" sparkline) lines))
    (push (format nil "  ~a" (market-url market)) lines)
    (when (market-closed market)
      (push (format nil "  CLOSED~@[ · ~a won~]"
                    (market-resolved-outcome market))
            lines))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun fmt-search (results)
  "The block /oddsradar <query> and the odds_search tool answer with."
  (if (null results)
      "No active Polymarket events match that."
      (with-output-to-string (out)
        (loop for result in results
              for first = t then nil
              do (unless first (terpri out))
                 (format out "~a  (24h vol ~a)~%"
                         (getf result :event) (money (getf result :volume-24h)))
                 (loop for one in (getf result :markets)
                       for name = (or (first (getf one :outcomes)) "Yes")
                       do (format out "  ~6@a  ~@[~a: ~]~a~%         slug: ~a~%"
                                  (pct (first (getf one :prices)))
                                  (and (not (string-equal name "Yes")) name)
                                  (getf one :label) (getf one :slug)))
                 (when (plusp (getf result :more-markets))
                   (format out "  … ~d more markets in this event~%" (getf result :more-markets)))))))

(defun fmt-movers (markets)
  "The block /oddsradar movers and the odds_movers tool answer with."
  (if (null markets)
      "No big movers right now."
      (with-output-to-string (out)
        (format out "Biggest 24h moves among the most-traded Polymarket markets:~%")
        (loop for one in markets
              for name = (format nil "~a~@[ -- ~a~]"
                                 (market-question one)
                                 (and (plusp (length (market-label one)))
                                      (not (search (market-label one) (market-question one)))
                                      (market-label one)))
              do (format out "  ~6@a pts  ~6@a  ~a~%           ~a · 24h vol ~a~%"
                         (signed (* 100 (or (market-change-1d one) 0d0)))
                         (pct (first (market-prices one))) name
                         (market-slug one) (money (market-volume-24h one)))))))
