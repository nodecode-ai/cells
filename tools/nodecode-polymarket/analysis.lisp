;;;; analysis.lisp --- the numbers, and what is worth a second look. No network.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here reads a normalized market (wire.lisp) and answers text or
;;;; a plain value, so the whole of it is testable without a socket. The
;;;; thresholds are the section's settings, not constants in the code.

(in-package #:nodecode-polymarket)

;;; --- saying a number ----------------------------------------------------------------

(defun money (value)
  "VALUE in dollars, short: $1.2M, $12.3K, $840."
  (cond ((null value) "n/a")
        ((>= value 1000000) (format nil "$~,1fM" (/ value 1000000)))
        ((>= value 1000) (format nil "$~,1fK" (/ value 1000)))
        (t (format nil "$~d" (round value)))))

(defun pct (value)
  "VALUE -- a 0-1 probability -- as a percentage: 62%."
  (if (null value)
      "n/a"
      (let ((points (* value 100)))
        (if (or (< points 10) (> points 90))
            (format nil "~,1f%" points)
            (format nil "~d%" (round points))))))

(defun pts (value)
  "VALUE -- a price change in probability -- as percentage points."
  (if (null value)
      "n/a"
      (format nil "~a~,1f pts" (if (plusp value) "+" "") (* value 100))))

(defun ends-in (hours)
  "HOURS from now as a short span: 7h, 4d, 3w."
  (cond ((null hours) "no end date")
        ((<= hours 0) "past end date")
        ((< hours 48) (format nil "~dh" (round hours)))
        ((< (/ hours 24) 21) (format nil "~dd" (round (/ hours 24))))
        (t (format nil "~,0fw" (/ hours 168)))))

;;; --- one market as a line ------------------------------------------------------------

(defun market-line (index market &optional extra)
  "INDEX. QUESTION — odds (change) · volume · ends, then EXTRA and the link."
  (format nil "~d. **~a** — ~a (~a) · ~a 24h · ends ~a~@[~%~%   ~a~]~@[~%~%   ~a~]"
          index (getf market :question) (pct (getf market :yes)) (pts (getf market :change-1d))
          (money (getf market :volume-24h)) (ends-in (getf market :hours))
          extra (let ((url (getf market :url)))
                  (and url (plusp (length url)) url))))

(defun market-list (title markets)
  "TITLE over one line a market, or that nothing matched."
  (if (null markets)
      (format nil "**~a**~%Nothing matched right now." title)
      (format nil "**~a**~%~{~a~^~%~}" title
              (loop for market in markets for i from 1 collect (market-line i market)))))

(defun signal-list (title ranked)
  "TITLE over the flagged markets, each with why it was flagged."
  (if (null ranked)
      (format nil "**~a**~%No markets are flagging right now." title)
      (format nil "**~a**~%~{~a~^~%~}" title
              (loop for row in ranked for i from 1
                    collect (market-line i (getf row :market)
                                         (format nil "Why: ~{~a~^; ~}" (getf row :reasons)))))))

(defun alert-list (title alerts)
  "TITLE over each alert: the market, the rule that fired and the link."
  (if (null alerts)
      (format nil "**~a**~%No watchlist alerts." title)
      (format nil "**~a**~%~{~a~^~%~}" title
              (loop for row in alerts for i from 1
                    for market = (getf row :market)
                    collect (format nil "~d. **~a** — ~a~@[~%~%   ~a~]"
                                    i (getf market :question) (getf row :message)
                                    (let ((url (getf market :url)))
                                      (and url (plusp (length url)) url)))))))

;;; --- the rankings ---------------------------------------------------------------------

(defun movers (markets &optional (limit (setting :default-limit)))
  "MARKETS whose 24h price moved most, biggest first, above the volume floor."
  (let ((pool (remove-if-not (lambda (m)
                               (and (getf m :change-1d)
                                    (>= (getf m :volume-24h) (setting :min-volume-24h))))
                             markets)))
    (subseq (sort pool #'> :key (lambda (m) (abs (getf m :change-1d))))
            0 (min (length pool) limit))))

(defun closing-soon (markets &optional (limit (setting :default-limit)) (hours nil))
  "MARKETS resolving within HOURS (the section's window by default), soonest first."
  (let* ((window (or hours (setting :closing-hours)))
         (pool (remove-if-not (lambda (m)
                                (let ((h (getf m :hours)))
                                  (and h (< 0 h window)
                                       (>= (getf m :volume-24h) (setting :min-volume-24h)))))
                              markets)))
    (subseq (sort pool (lambda (a b)
                         (let ((ha (getf a :hours)) (hb (getf b :hours)))
                           (if (/= ha hb) (< ha hb)
                               (> (getf a :volume-24h) (getf b :volume-24h))))))
            0 (min (length pool) limit))))

(defun compute-signals (market)
  "The attention flags MARKET earns right now, as a list of (TYPE WEIGHT REASON).
Prompts to look closer, never predictions."
  (let ((out '())
        (volume-24h (or (getf market :volume-24h) 0d0))
        (active (>= (or (getf market :volume-24h) 0d0) (setting :min-volume-24h)))
        (move (setting :move-pts)))
    (let ((change (getf market :change-1d)))
      (when (and change active (>= (abs (* change 100)) move))
        (push (list :big-move (* 2.0 (min (/ (abs (* change 100)) move) 3.0))
                    (format nil "moved ~a in 24h on ~a of volume"
                            (pts change) (money volume-24h)))
              out)))
    (let ((week (getf market :volume-1w)))
      (when (and week (plusp week) active)
        (let ((ratio (/ volume-24h (/ week 7.0))))
          (when (>= ratio (setting :spike-ratio))
            (push (list :volume-spike (* 1.5 (min (/ ratio (setting :spike-ratio)) 3.0))
                        (format nil "24h volume is ~,1fx its recent daily average" ratio))
                  out)))))
    (let ((spread (getf market :spread)))
      (when (and spread (>= spread (/ (setting :wide-spread) 100.0))
                 (>= (getf market :liquidity) 1000))
        (push (list :wide-spread 1.0
                    (format nil "bid/ask gap is ~,1f pts, so the shown price may be unreliable"
                            (* spread 100)))
              out)))
    (let ((prices (getf market :prices)))
      (when (and (getf market :binary) (= 2 (length prices)) (every #'realp prices))
        (let ((total (reduce #'+ prices)))
          (when (>= (abs (- total 1)) 0.03)
            (push (list :price-sum-gap 1.0
                        (format nil "outcome prices add up to ~,0f% instead of 100%" (* total 100)))
                  out)))))
    (let ((hours (getf market :hours)) (yes (getf market :yes)))
      (when (and hours (< 0 hours (setting :closing-hours)) yes active
                 (<= (/ (setting :band-low) 100.0) yes (/ (setting :band-high) 100.0)))
        (push (list :closing-undecided 2.0
                    (format nil "resolves in ~a and is still priced at ~a"
                            (ends-in hours) (pct yes)))
              out)))
    (nreverse out)))

(defun rank-signals (markets &optional (limit (setting :default-limit)))
  "The MARKETS worth a closer look, each as (:SCORE :MARKET :REASONS), best first."
  (let ((ranked '()))
    (dolist (market markets)
      (let ((signals (compute-signals market)))
        (when signals
          (push (list :score (reduce #'+ signals :key #'second)
                      :market market
                      :reasons (mapcar #'third signals))
                ranked))))
    (subseq (sort ranked #'> :key (lambda (row) (getf row :score)))
            0 (min (length ranked) limit))))

;;; --- a price series ---------------------------------------------------------------------

(defun history-stats (history)
  "HISTORY -- a list of (TIMESTAMP . PRICE) -- summarised: range, 24h and overall change."
  (when (and history (>= (length history) 2))
    (let* ((sorted (sort (copy-list history) #'< :key #'car))
           (last-row (car (last sorted)))
           (prices (mapcar #'cdr sorted))
           (day-ago (- (car last-row) 86400))
           (base (first prices)))
      (dolist (row sorted)
        (when (<= (car row) day-ago) (setf base (cdr row))))
      (list :points (length sorted)
            :start (first prices)
            :latest (cdr last-row)
            :high (reduce #'max prices)
            :low (reduce #'min prices)
            :change-24h (- (cdr last-row) base)
            :change-period (- (cdr last-row) (first prices))
            :trend (let ((change (- (cdr last-row) (first prices))))
                     (cond ((< (abs change) 0.02) "flat")
                           ((plusp change) "up")
                           (t "down")))))))

;;; --- a watch rule against a market -------------------------------------------------------

(defun evaluate-watch (item market)
  "The rules on ITEM that are true right now, as a list of (KEY . TEXT)."
  (let ((hits '())
        (yes (getf market :yes))
        (above (getf item :above))
        (below (getf item :below))
        (move (getf item :move))
        (change (getf market :change-1d)))
    (when (and above yes (>= yes above))
      (push (cons :above (format nil "price ~a is at or above your ~a line" (pct yes) (pct above)))
            hits))
    (when (and below yes (<= yes below))
      (push (cons :below (format nil "price ~a is at or below your ~a line" (pct yes) (pct below)))
            hits))
    (when (and move change (>= (abs (* change 100)) move))
      (push (cons :move (format nil "moved ~a in 24h (your trigger is ~d pts)" (pts change) (round move)))
            hits))
    (nreverse hits)))
