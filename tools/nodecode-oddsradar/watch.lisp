;;;; watch.lisp --- the persistent watchlist and the alert engine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's Watch, Rules, evaluate() and Radar's watchlist half. One
;;;; JSON file holds every watch: oddsradar/watchlist.json under this home, or
;;;; the file data_file names. It is written atomically and under one lock, so
;;;; two turns -- or a turn and an alert check -- never clobber each other.
;;;; The engine is pure where it can be: EVALUATE takes a watch and a price
;;;; and answers the alerts that fired, so a test drives it with no network.

(in-package #:nodecode-oddsradar)

(defparameter +rearm-gap+ 0.01d0
  "One point of hysteresis before an above/below rule can fire again.")

(defvar *watchlist-lock* (bt2:make-lock :name "oddsradar watchlist")
  "One lock around every read-modify-write of the watchlist file.")

;;; --- the rules and the watch -------------------------------------------------------

(defstruct (watch (:constructor make-watch) (:copier nil))
  (id "" )
  (market-slug "")
  (event-slug "")
  (question "")
  (label "")
  (outcome "Yes")
  (move 5d0)
  (above nil)
  (below nil)
  (baseline 0d0)
  (baseline-at 0)
  (last-price 0d0)
  (last-checked 0)
  (created-at 0)
  (above-armed t)
  (below-armed t))

(defun fraction (value what)
  "VALUE, a percentage the operator typed (60) or a fraction (0.6), as a
fraction strictly between 0 and 1, or a refusal naming WHAT."
  (if (null value)
      nil
      (let ((number (float value 1d0)))
        (when (> number 1d0)
          (setf number (/ number 100d0)))
        (unless (< 0d0 number 1d0)
          (fail "~a must be between 0 and 100 (percent)" what))
        (/ (round number 1d-4) 10000d0))))

(defun build-rules (move above below)
  "The rules a watch keeps => (values MOVE ABOVE BELOW), or a refusal when
none is set or the pair is inverted."
  (let ((move (and move (float move 1d0)))
        (above (fraction above "above"))
        (below (fraction below "below")))
    (when (and move (not (< 0d0 move 100d0)))
      (fail "move must be between 0 and 100 points"))
    (when (and (null move) (null above) (null below))
      (fail "A watch needs at least one rule: move, above or below."))
    (when (and above below (>= below above))
      (fail "below must be lower than above"))
    (values move above below)))

(defun rules-describe (watch)
  "WATCH's rules as the one line a watchlist prints."
  (let ((parts '()))
    (when (watch-move watch)
      (push (format nil "±~a pts" (number-text (watch-move watch))) parts))
    (when (watch-above watch)
      (push (format nil "above ~a" (pct (watch-above watch))) parts))
    (when (watch-below watch)
      (push (format nil "below ~a" (pct (watch-below watch))) parts))
    (format nil "~{~a~^, ~}" (nreverse parts))))

(defun watch-title (watch)
  "WATCH's market, and the outcome when it is not Yes."
  (if (equal (watch-outcome watch) "Yes")
      (watch-question watch)
      (format nil "~a [~a]" (watch-question watch) (watch-outcome watch))))

(defun new-watch-id ()
  "A fresh six hex character watch id."
  (let ((digest (nlk:sha256-text (format nil "~a-~a-~a" (get-universal-time)
                                         (get-internal-real-time)
                                         (random most-positive-fixnum)))))
    (subseq digest 0 6)))

;;; --- the file --------------------------------------------------------------------

(defun watchlist-path ()
  "The watchlist file: the one data_file names, else oddsradar/watchlist.json
under this home."
  (let ((named (setting :data-file)))
    (if (and (stringp named) (plusp (length (nlk:trimmed named))))
        (nlk:expand-home named)
        (nlk:home "oddsradar/watchlist.json"))))

(defun watch-json (watch)
  "WATCH as the JSON object the file holds."
  (nlk:make-json-object
   "id" (watch-id watch)
   "market_slug" (watch-market-slug watch)
   "event_slug" (watch-event-slug watch)
   "question" (watch-question watch)
   "label" (watch-label watch)
   "outcome" (watch-outcome watch)
   "rules" (nlk:make-json-object "move" (watch-move watch)
                                  "above" (watch-above watch)
                                  "below" (watch-below watch))
   "baseline" (watch-baseline watch)
   "baseline_at" (watch-baseline-at watch)
   "last_price" (watch-last-price watch)
   "last_checked" (watch-last-checked watch)
   "created_at" (watch-created-at watch)
   "above_armed" (watch-above-armed watch)
   "below_armed" (watch-below-armed watch)))

(defun watch-from-json (object)
  "The WATCH OBJECT describes, as the file wrote it."
  (let ((rules (json-field object "rules")))
    (make-watch
     :id (jtext object "id" "")
     :market-slug (jtext object "market_slug" "")
     :event-slug (jtext object "event_slug" "")
     :question (jtext object "question" "")
     :label (jtext object "label" "")
     :outcome (jtext object "outcome" "Yes")
     :move (jnum rules "move")
     :above (jnum rules "above")
     :below (jnum rules "below")
     :baseline (or (jnum object "baseline") 0d0)
     :baseline-at (or (and (jnum object "baseline_at") (round (jnum object "baseline_at"))) 0)
     :last-price (or (jnum object "last_price") 0d0)
     :last-checked (or (and (jnum object "last_checked") (round (jnum object "last_checked"))) 0)
     :created-at (or (and (jnum object "created_at") (round (jnum object "created_at"))) 0)
     :above-armed (truthful-p (json-field object "above_armed"))
     :below-armed (truthful-p (json-field object "below_armed")))))

(defun load-watches ()
  "Every watch this home holds, oldest first; none when the file is absent."
  (let ((path (watchlist-path)))
    (if (probe-file path)
        (let ((json (handler-case (nlk:decode-json (uiop:read-file-string path))
                      (error () nil))))
          (unless (hash-table-p json)
            (fail "the watchlist file (~a) is not readable JSON: delete it to start over"
                  (namestring path)))
          (loop for object in (jlist (json-field json "watches"))
                when (hash-table-p object) collect (watch-from-json object)))
        '())))

(defun save-watches (watches)
  "WATCHES written to the watchlist file, atomically. => WATCHES."
  (let ((path (watchlist-path)))
    (ensure-directories-exist path)
    (nlk:write-file-atomically
     path
     (nlk:encode-json-object
      (nlk:make-json-object "version" 1
                             "watches" (coerce (mapcar #'watch-json watches) 'vector))))
    watches))

;;; --- adding, removing, reading ------------------------------------------------------

(defun add-watch (ref &key outcome (move 5d0) above below)
  "REF, a URL or slug, added to the watchlist with these rules => the WATCH.
A market already watched for that outcome has its rules replaced."
  (multiple-value-bind (move above below) (build-rules move above below)
    (let* ((market (resolve-market ref))
           (name (if outcome
                     (nth (market-outcome-index market outcome) (market-outcomes market))
                     (first (market-outcomes market)))))
      (when (market-closed market)
        (fail "~s is already closed." (market-question market)))
      (let ((price (live-price market name)))
        (unless price
          (fail "Could not read a price for that market right now."))
        (let ((now (get-universal-time)))
          (bt2:with-lock-held (*watchlist-lock*)
            (let* ((watches (load-watches))
                   (existing (find-if (lambda (one)
                                        (and (equal (watch-market-slug one) (market-slug market))
                                             (equal (watch-outcome one) name)))
                                      watches)))
              (when (and (null existing)
                         (>= (length watches) (setting :max-watches)))
                (fail "the watchlist holds ~d watches already, its limit: remove one with /oddsradar unwatch <id>, or raise max_watches"
                      (length watches)))
              (if existing
                  (progn
                    (setf (watch-move existing) move
                          (watch-above existing) above
                          (watch-below existing) below)
                    (save-watches watches)
                    existing)
                  (let ((watch (make-watch
                                :id (new-watch-id)
                                :market-slug (market-slug market)
                                :event-slug (market-event-slug market)
                                :question (market-question market)
                                :label (market-label market)
                                :outcome name
                                :move move :above above :below below
                                :baseline price :baseline-at now
                                :last-price price :last-checked now :created-at now
                                :above-armed (or (null above) (< price above))
                                :below-armed (or (null below) (> price below)))))
                    (save-watches (append watches (list watch)))
                    watch)))))))))

(defun remove-watch (key)
  "The watch KEY -- its id or its market slug -- removed => the WATCH, or
NIL when nothing matched."
  (let ((key (nlk:trimmed (or key ""))))
    (bt2:with-lock-held (*watchlist-lock*)
      (let ((watches (load-watches)))
        (let ((at (position-if (lambda (one) (member key (list (watch-id one)
                                                               (watch-market-slug one))
                                                   :test #'equal))
                               watches)))
          (when at
            (let ((gone (nth at watches)))
              (save-watches (append (subseq watches 0 at) (subseq watches (1+ at))))
              gone)))))))

;;; --- the engine --------------------------------------------------------------------

(defstruct (alert (:constructor make-alert) (:copier nil))
  (kind "move" :read-only t)
  (question "" :read-only t)
  (outcome "Yes" :read-only t)
  (old nil :read-only t)
  (new nil :read-only t)
  (url "" :read-only t)
  (since nil :read-only t)
  (detail "" :read-only t))

(defun alert-head (alert)
  "ALERT's first line: which rule fired, and how far."
  (cond ((equal (alert-kind alert) "move")
         (let ((delta (* 100d0 (- (or (alert-new alert) 0d0) (or (alert-old alert) 0d0)))))
           (format nil "~a ~a pts" (if (minusp delta) "▼" "▲") (signed delta))))
        ((equal (alert-kind alert) "above")
         (format nil "⬆ crossed above ~a" (alert-detail alert)))
        (t (format nil "⬇ crossed below ~a" (alert-detail alert)))))

(defun alert-who (alert)
  "ALERT's outcome, when it is not the usual Yes."
  (if (equal (alert-outcome alert) "Yes") "" (format nil " [~a]" (alert-outcome alert))))

(defun alert-since-text (alert now)
  "How long ALERT's move took, or an empty string when it is not a move."
  (if (alert-since alert)
      (format nil " in ~a" (ago (- now (alert-since alert))))
      ""))

(defun alert-text (alert &optional (now (get-universal-time)))
  "ALERT as one sentence and its link, the way the operator reads it."
  (if (equal (alert-kind alert) "resolved")
      (format nil "✔ RESOLVED · ~a → ~a~%   ~a"
              (alert-question alert) (alert-detail alert) (alert-url alert))
      (format nil "~a · ~a~a~%   ~a → ~a~a · ~a"
              (alert-head alert) (alert-question alert) (alert-who alert)
              (pct (alert-old alert)) (pct (alert-new alert))
              (alert-since-text alert now) (alert-url alert))))
(defun evaluate (watch market price now)
  "WATCH against a fresh PRICE for its market => (values ALERTS FINISHED-P).
Pure but for WATCH, which it brings up to date: a resolved market fires once
and is finished, a move fires once from the baseline it then moves."
  (cond
    ((market-closed market)
     (let ((winner (market-resolved-outcome market)))
       (values (list (make-alert :kind "resolved"
                                  :question (watch-question watch)
                                  :outcome (watch-outcome watch)
                                  :old (watch-last-price watch)
                                  :new price
                                  :url (market-url market)
                                  :detail (if winner
                                              (format nil "~a won" winner)
                                              "market closed (awaiting resolution)")))
               t)))
    ((null price) (values '() nil))
    (t
     (let ((alerts '()))
       (when (and (watch-move watch)
                  (>= (* 100d0 (abs (- price (watch-baseline watch))))
                      (- (watch-move watch) 1d-9)))
         (push (make-alert :kind "move"
                            :question (watch-question watch)
                            :outcome (watch-outcome watch)
                            :old (watch-baseline watch)
                            :new price
                            :url (market-url market)
                            :since (watch-baseline-at watch))
               alerts)
         (setf (watch-baseline watch) price
               (watch-baseline-at watch) now))
       (when (watch-above watch)
         (cond ((and (watch-above-armed watch) (>= price (watch-above watch)))
                (push (make-alert :kind "above"
                                   :question (watch-question watch)
                                   :outcome (watch-outcome watch)
                                   :old (watch-last-price watch)
                                   :new price
                                   :url (market-url market)
                                   :detail (pct (watch-above watch)))
                      alerts)
                (setf (watch-above-armed watch) nil))
               ((and (not (watch-above-armed watch))
                     (< price (- (watch-above watch) +rearm-gap+)))
                (setf (watch-above-armed watch) t))))
       (when (watch-below watch)
         (cond ((and (watch-below-armed watch) (<= price (watch-below watch)))
                (push (make-alert :kind "below"
                                   :question (watch-question watch)
                                   :outcome (watch-outcome watch)
                                   :old (watch-last-price watch)
                                   :new price
                                   :url (market-url market)
                                   :detail (pct (watch-below watch)))
                      alerts)
                (setf (watch-below-armed watch) nil))
               ((and (not (watch-below-armed watch))
                     (> price (+ (watch-below watch) +rearm-gap+)))
                (setf (watch-below-armed watch) t))))
       (setf (watch-last-price watch) price
             (watch-last-checked watch) now)
       (values (nreverse alerts) nil)))))

(defun check-watches (&key (commit t))
  "Every watch against its market now => (values ALERTS ERRORS). COMMIT NIL
peeks: the baselines move in memory only, so the alerts are not consumed."
  (bt2:with-lock-held (*watchlist-lock*)
    (let ((now (get-universal-time))
          (alerts '())
          (errors '())
          (keep '()))
      (dolist (watch (load-watches))
        (handler-case
            (let ((record (market-by-slug (watch-market-slug watch) (setting :timeout-seconds))))
              (if (null record)
                  (progn
                    (push (format nil "~a: market '~a' not found"
                                  (watch-id watch) (watch-market-slug watch))
                          errors)
                    (push watch keep))
                  (let* ((market (market-from-gamma record))
                         (price (live-price market (watch-outcome watch))))
                    (multiple-value-bind (fired finished) (evaluate watch market price now)
                      (setf alerts (append alerts fired))
                      (unless finished (push watch keep))))))
          (oddsradar-error (condition)
            (push (format nil "~a: ~a" (watch-id watch) condition) errors)
            (push watch keep))))
      (when commit
        (save-watches (nreverse keep)))
      (values alerts (nreverse errors)))))

(defun fmt-watches (watches &optional (now (get-universal-time)))
  "The watchlist as /oddsradar list prints it."
  (if (null watches)
      "Watchlist is empty. Add one with: watch <polymarket url or slug> [move=5] [above=60] [below=40]"
      (with-output-to-string (out)
        (loop for watch in watches
              do (format out "[~a] ~a~%   now ~a · baseline ~a (~a ago) · rules ~a~%"
                         (watch-id watch) (watch-title watch)
                         (pct (watch-last-price watch)) (pct (watch-baseline watch))
                         (ago (- now (watch-baseline-at watch)))
                         (rules-describe watch))))))

