;;;; decide.lisp --- one judgment into a tier, and the spend ledger beside it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Mirrors decide.py, ledger.py and pricing.py of the original: the demand
;;;; score, the kind floor, the confidence fallback, the budget policy, the
;;;; chain order (kind chain, the tier, its neighbours, the free pool) and the
;;;; first row that names a model. The catalogue, quota and cache-hold halves
;;;; of the original do not apply: a cell reads no provider catalogue, so
;;;; every configured row is eligible, as the catalog edition says.

(in-package #:nodecode-jev-routing)

;;; --- what Jev answered ------------------------------------------------------------

(defstruct (analysis (:constructor make-analysis))
  "The four answers, composed into numbers the tier rule reads."
  (kind "chat" :read-only t)
  (complexity 1d0)
  (capability 1d0)
  (deep 0d0)
  (confidence 0d0)
  probabilities
  (complexity-confidence 0d0)
  (capability-confidence 0d0)
  (latency-ms 0))

(defstruct (budget-spend (:constructor make-budget-spend))
  "How much of the operator's cap is gone: the pressure the tier rule reads."
  (pressure 0d0)
  (today 0d0)
  (month 0d0)
  daily
  monthly)

(defun clamp (value low high)
  "VALUE held between LOW and HIGH."
  (max low (min high value)))

(defun round-half-up (value)
  "VALUE rounded as the original's Math.round: halves away from zero."
  (if (minusp value) (- (floor (+ (- value) 0.5))) (floor (+ value 0.5))))

(defun tier-index (name)
  "NAME's place in +TIERS+; an unknown or missing tier is standard."
  (or (position (or name "standard") +tiers+ :test #'string-equal) 1))

(defun tier-name (index)
  "The tier at INDEX."
  (aref +tiers+ (clamp index 0 (1- (length +tiers+)))))

(defun route-of (router tier)
  "The chain ROUTER holds for TIER, as a list of targets."
  (gethash tier (router-routes router)))

(defun target-key (target)
  "TARGET as provider/model, the way a line names it."
  (format nil "~a/~a" (target-provider target) (target-model target)))

;;; --- the tier --------------------------------------------------------------------

(defun decide (analysis router spend)
  "ANALYSIS composed into a tier, the budget policy applied, and the first
configured row that serves it. => a plist, or NIL when no chain names a model."
  (let ((notes '())
        (deep (analysis-deep analysis))
        (demand (clamp (+ (* 0.55d0 (analysis-complexity analysis))
                           (* 0.45d0 (analysis-capability analysis)))
                        0d0 3d0)))
    (when (>= deep 0.65d0) (setf demand (clamp (+ demand 0.75d0) 0d0 3d0)))
    (when (<= deep 0.2d0) (setf demand (clamp (- demand 0.25d0) 0d0 3d0)))
    (let ((reaches-premium (>= (round-half-up demand) 3))
          (kind (analysis-kind analysis))
          (confidence (analysis-confidence analysis)))
      ;; A kind the operator protected can never route below its floor.
      (let ((floor (min (tier-index (gethash kind (router-kind-minimum-tier router))) 3)))
        (when (< demand floor)
          (push (format nil "~a floors at ~a" kind (tier-name floor)) notes)
          (setf demand (float floor 1d0))))
      (let* ((desired (clamp (round-half-up demand) 0 3))
             (index desired)
             (low-confidence nil))
        (when (and (plusp (router-confidence-threshold router))
                   (plusp confidence)
                   (< confidence (router-confidence-threshold router))
                   (> index 1))
          (push (format nil "low kind confidence ~,2f -> standard" confidence) notes)
          (setf index 1 low-confidence t))
        (let ((xpremium (and (plusp (length (route-of router "xpremium")))
                              reaches-premium
                              (plusp confidence)
                              (>= confidence (router-confidence-threshold router)))))
          (when xpremium (setf desired 4 index 4))
          (let* ((tier-count (if xpremium 5 4))
                 (pressure (budget-spend-pressure spend))
                 (downgraded nil))
            ;; The budget policy: over the hard ratio, only a premium demand keeps a tier.
            (cond ((and (>= pressure (router-hard-ratio router)) (plusp pressure))
                   (let ((forced (if (>= demand 2.5d0) 1 0)))
                     (when (< forced index)
                       (push (format nil "budget ~d% of cap -> capped at ~a"
                                      (round (* pressure 100d0)) (tier-name forced))
                             notes)
                       (setf index forced downgraded t))))
                  ((and (>= pressure (router-soft-ratio router)) (plusp pressure))
                   (when (plusp index)
                     (push (format nil "budget ~d% of cap -> one tier down"
                                    (round (* pressure 100d0)))
                           notes)
                     (decf index)
                     (setf downgraded t))))
            (let* ((free (when (router-free-enabled router)
                           (loop for target in (router-free-pool router)
                                 when (and (plusp (length (target-model target)))
                                           (plusp (length (target-provider target))))
                                   collect target)))
                   (kind-chain (when (plusp (length (gethash kind (router-kind-models router))))
                                 (stable-sort
                                  (remove-if-not
                                   (lambda (target) (<= (tier-index (target-min-tier target)) index))
                                   (copy-list (gethash kind (router-kind-models router))))
                                  (lambda (a b)
                                    (or (> (target-priority a) (target-priority b))
                                        (and (= (target-priority a) (target-priority b))
                                             (> (tier-index (target-min-tier a))
                                                (tier-index (target-min-tier b)))))))))
                   (ordered (append (when (member (router-free-policy router) '("prefer") :test #'string-equal)
                                      free)
                                    kind-chain
                                    (route-of router (tier-name index))
                                    (loop for offset from 1 below tier-count
                                          append (append (when (>= (- index offset) 0)
                                                           (route-of router (tier-name (- index offset))))
                                                         (when (< (+ index offset) tier-count)
                                                           (route-of router (tier-name (+ index offset))))))
                                    (unless (member (router-free-policy router) '("prefer") :test #'string-equal)
                                      free)))
                   (pick (find-if (lambda (target) (plusp (length (target-model target)))) ordered)))
              (unless pick
                (return-from decide nil))
              (when (member pick free) (push (format nil "free pool -> ~a" (target-model pick)) notes))
              ;; A row that only lives in a neighbouring chain is used as that tier.
              (let ((serving (unless (member pick kind-chain)
                               (loop for i below tier-count
                                     when (member pick (route-of router (tier-name i))) collect i))))
                (when (and serving (not (member index serving)))
                  (let ((effective (first (sort (copy-list serving)
                                                (lambda (a b)
                                                  (or (< (abs (- a index)) (abs (- b index)))
                                                      (and (= (abs (- a index)) (abs (- b index))) (< a b))))))))
                    (push (format nil "~a chain unavailable -> ~a" (tier-name index) (tier-name effective))
                          notes)
                    (setf downgraded (or downgraded (< effective index)))
                    (setf index effective))))
              (list :tier (tier-name index)
                    :desired-tier (tier-name desired)
                    :provider (target-provider pick)
                    :model (target-model pick)
                    :thinking (target-thinking pick)
                    :kind kind
                    :demand (float demand 1d0)
                    :confidence (float confidence 1d0)
                    :budget-pressure (float pressure 1d0)
                    :low-confidence low-confidence
                    :downgraded downgraded
                    :notes (nreverse notes)))))))))

;;; --- the spend ledger ---------------------------------------------------------------

(defun utc-parts ()
  "(values YEAR MONTH DAY) in UTC, the way the ledger keys a bucket."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour))
    (values year month day)))

(defun day-key ()
  "Today's bucket key, YYYY-MM-DD, in UTC."
  (multiple-value-bind (year month day) (utc-parts)
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun month-key ()
  "This month's bucket key, YYYY-MM, in UTC."
  (multiple-value-bind (year month) (utc-parts)
    (format nil "~4,'0d-~2,'0d" year month)))

(defun iso-now ()
  "The instant, as the ledger's updatedAt."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun round4 (value)
  "VALUE to four decimals, the ledger's own precision."
  (/ (round (* (float value 1d0) 10000d0)) 10000d0))

(defun state-path (path)
  "The ledger that sits beside the routing table PATH."
  (namestring (make-pathname :name (concatenate 'string (pathname-name path) "-state")
                              :type "json" :defaults path)))

(defun fresh-bucket ()
  (let ((bucket (make-hash-table :test 'equal)))
    (setf (gethash "total" bucket) 0.0d0
          (gethash "byModel" bucket) (make-hash-table :test 'equal))
    bucket))

(defun read-ledger (path)
  "The ledger beside PATH; a missing or unreadable one is empty, never fatal."
  (let* ((file (state-path path))
         (parsed (when (probe-file file)
                   (handler-case (nlk:decode-json (uiop:read-file-string file))
                     (error () nil)))))
    (unless (hash-table-p parsed)
      (setf parsed (make-hash-table :test 'equal)))
    (dolist (key '("days" "months"))
      (unless (hash-table-p (gethash key parsed))
        (setf (gethash key parsed) (make-hash-table :test 'equal))))
    (unless (hash-table-p (gethash "jev" parsed))
      (let ((counters (make-hash-table :test 'equal)))
        (setf (gethash "requests" counters) 0
              (gethash "inputTokens" counters) 0
              (gethash "outputTokens" counters) 0
              (gethash "jev" parsed) counters)))
    (unless (gethash "version" parsed) (setf (gethash "version" parsed) 1))
    parsed))

(defun save-ledger (path ledger)
  "Write LEDGER beside the routing table; a write that fails changes no route."
  (let* ((file (state-path path))
         (temporary (concatenate 'string file ".tmp")))
    (setf (gethash "updatedAt" ledger) (iso-now))
    (handler-case
        (progn (with-open-file (stream temporary :direction :output :if-exists :supersede
                                                 :if-does-not-exist :create)
                 (write-string (nlk:encode-json-object ledger) stream)
                 (terpri stream))
               (rename-file temporary file))
      (error () nil))
    ledger))

(defun bucket-total (ledger section key)
  "The total LEDGER records under SECTION (days, months) and KEY."
  (let ((bucket (gethash key (gethash section ledger))))
    (if (hash-table-p bucket) (or (jnum (gethash "total" bucket)) 0d0) 0d0)))

(defun spend-of (router)
  "ROUTER's ledger as a BUDGET-SPEND: today, this month, and the pressure of
the two caps (the larger share, zero when neither cap is set)."
  (let* ((ledger (read-ledger (router-path router)))
         (today (bucket-total ledger "days" (day-key)))
         (month (bucket-total ledger "months" (month-key)))
         (ratios (remove nil (list (when (and (router-daily router) (plusp (router-daily router)))
                                      (/ today (router-daily router)))
                                   (when (and (router-monthly router) (plusp (router-monthly router)))
                                      (/ month (router-monthly router)))))))
    (make-budget-spend :today today :month month
                       :pressure (if ratios (reduce #'max ratios) 0d0)
                       :daily (router-daily router) :monthly (router-monthly router))))

(defun add-cost (ledger section key provider model usd)
  "Add USD to one bucket of the ledger under SECTION and KEY."
  (let* ((buckets (gethash section ledger))
         (bucket (or (gethash key buckets)
                     (let ((fresh (fresh-bucket))) (setf (gethash key buckets) fresh) fresh)))
         (by-model (gethash "byModel" bucket)))
    (setf (gethash "total" bucket) (round4 (+ (or (jnum (gethash "total" bucket)) 0d0) usd)))
    (let ((name (format nil "~a/~a" (or provider "") (or model ""))))
      (setf (gethash name by-model) (round4 (+ (or (jnum (gethash name by-model)) 0d0) usd))))))

(defun record-spend (router provider model &key input output usd)
  "Record one provider response in the ledger beside ROUTER's table: a
reported USD total when given, else the operator's own rate card, and the
tokens either way. An unpriced model records tokens and no dollars."
  (let* ((ledger (read-ledger (router-path router)))
         (rates (pricing-of router provider model))
         (cost (or usd
                   (when rates
                     (+ (* (/ (or input 0) 1000000d0) (or (getf rates :input) 0d0))
                        (* (/ (or output 0) 1000000d0) (or (getf rates :output) 0d0))))))
         (counters (gethash "jev" ledger)))
    (when (and cost (plusp cost))
      (add-cost ledger "days" (day-key) provider model cost)
      (add-cost ledger "months" (month-key) provider model cost))
    (setf (gethash "requests" counters) (1+ (or (gethash "requests" counters) 0))
          (gethash "inputTokens" counters) (+ (or (gethash "inputTokens" counters) 0) (or input 0))
          (gethash "outputTokens" counters) (+ (or (gethash "outputTokens" counters) 0) (or output 0)))
    (save-ledger (router-path router) ledger)
    (values ledger cost)))

(defun usd-text (value)
  "VALUE as money, with the precision it needs."
  (cond ((>= value 1d0) (format nil "$~,2f" value))
        ((>= value 0.01d0) (format nil "$~,3f" value))
        (t (format nil "$~,4f" value))))
