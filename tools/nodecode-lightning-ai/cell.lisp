;;;; cell.lisp --- the Lightning AI request gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Lightning AI counts its API in a fixed wall-clock minute and allows about
;;;; fifteen requests in one (x-ratelimit-limit-requests: 15, no Retry-After,
;;;; and GET /models is not counted). This cell counts the requests Nodecode
;;;; sends to the endpoints the section names and holds the ones past the limit
;;;; until the next minute begins. It wraps WALK-PROVIDER-STREAM, the one
;;;; transport bracket every streaming lane shares, so one call here is one
;;;; POST, a retried one too. Every other host goes through untouched, and a
;;;; failure in the gate never costs a round.

(defpackage #:nodecode-lightning-ai
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting #:*lightning-ai*
           #:status-text #:run-slash #:gate-call))

(in-package #:nodecode-lightning-ai)

(nlk:define-peripheral lightning-ai :not-running t)

;;; --- the clock, and the two seams a test stubs ------------------------------

(defun wall-now ()
  "Seconds since the epoch, to the microsecond."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ seconds (/ microseconds 1000000))))

(defvar *clock* #'wall-now
  "What the gate reads the time from; a test binds a clock of its own.")

(defvar *sleeper* #'sleep
  "How the gate waits; a test binds a sleeper of its own, so nothing sleeps.")

(defparameter +settle-margin+ 0.7
  "Seconds waited past the boundary: the server clears its window on its own clock, a little after ours.")

;;; --- the window -------------------------------------------------------------

(defvar *lock* (sb-thread:make-mutex :name "nodecode-lightning-ai"))

(defvar *window* nil
  "The wall-clock window the counts below belong to, NIL before the first request.")

(defvar *spent* 0
  "Requests the gate has let into *WINDOW* already.")

(defvar *gated* 0
  "Every request gated since the last reset.")

(defvar *waited* 0
  "How many requests waited for the next window.")

(defvar *parked* 0
  "How many windows a 429 closed early.")

(defun window-start (now seconds)
  "The start of the wall-clock window NOW falls in, when a window is SECONDS long."
  (* seconds (floor now seconds)))

(defun nonempty (value)
  "VALUE trimmed when it is a non-empty string, else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun gated-hosts ()
  "The host names the section names, trimmed, empties dropped."
  (let ((value (setting :hosts)))
    (remove nil
            (mapcar #'nonempty
                    (cond ((stringp value) (list value))
                          ((vectorp value) (coerce value 'list))
                          (t value))))))

(defun gated-endpoint-p (endpoint)
  "Whether ENDPOINT is under one of the hosts the section names."
  (let ((endpoint (string-downcase (or endpoint ""))))
    (and (plusp (length endpoint))
         (loop for host in (gated-hosts)
               thereis (search (string-downcase host) endpoint)))))

(defun reset-state ()
  "Forget the window and every count."
  (sb-thread:with-mutex (*lock*)
    (setf *window* nil *spent* 0 *gated* 0 *waited* 0 *parked* 0)))

(defun acquire-slot ()
  "Claim one slot in the current window, sleeping to the next boundary when it
is full. Answers the seconds waited."
  (loop with waited = 0
        do (let ((wait nil))
             (sb-thread:with-mutex (*lock*)
               (let* ((now (funcall *clock*))
                      (seconds (setting :window-seconds))
                      (window (window-start now seconds)))
                 (unless (eql window *window*)
                   (setf *window* window *spent* 0))
                 (if (< *spent* (setting :max-per-minute))
                     (progn
                       (incf *spent*)
                       (incf *gated*)
                       (return-from acquire-slot waited))
                     (setf wait (max 0.05
                                     (min seconds
                                          (+ (- (+ window seconds) now) +settle-margin+)))))))
             (incf *waited*)
             (incf waited wait)
             (funcall *sleeper* wait))))

(defun park-window ()
  "The provider answered 429: the rest of this window is spent."
  (sb-thread:with-mutex (*lock*)
    (setf *window* (window-start (funcall *clock*) (setting :window-seconds))
          *spent* (setting :max-per-minute))
    (incf *parked*)))

(defun rate-limited-p (condition)
  "Whether CONDITION is the provider refusing a round over its rate."
  (let ((text (string-downcase (princ-to-string condition))))
    (or (search "429" text)
        (search "rate limit" text)
        (search "rate_limit" text))))

(defun gate-call (next &rest args)
  "Around WALK-PROVIDER-STREAM: wait for room in the window when the call goes
to a gated host, then make it. Every other host, and anything unreadable here,
goes straight through."
  (let ((gated (handler-case
                   (and *lightning-ai*
                        (gated-endpoint-p
                         (nodecode.evolved::effective-provider-config-endpoint
                          (getf (rest args) :config))))
                 (error () nil))))
    (unless gated
      (return-from gate-call (apply next args)))
    (acquire-slot)
    (handler-bind ((error (lambda (condition)
                            (when (rate-limited-p condition) (park-window)))))
      (apply next args))))

;;; --- what the operator reads -------------------------------------------------

(defun status-text ()
  "One report: the window, what the gate has done, and the hosts it holds."
  (let* ((seconds (setting :window-seconds))
         (now (funcall *clock*))
         (window (window-start now seconds)))
    (sb-thread:with-mutex (*lock*)
      (format nil "lightning-ai: ~d of ~d requests in this ~d s window, ~d s to the next; ~
~d gated, ~d waited, ~d parked~%hosts: ~{~a~^, ~}"
              (if (eql window *window*) *spent* 0)
              (setting :max-per-minute) seconds
              (round (max 0 (- (+ window seconds) now)))
              *gated* *waited* *parked* (gated-hosts)))))

(defun run-slash (args session-id)
  "The /lightning-ai command: this window's state, or a reset of the counts."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (words (if (plusp (length text))
                    (uiop:split-string text :separator " ")
                    '()))
         (verb (and words (string-downcase (first words)))))
    (cond
      ((null words) (status-text))
      ((member verb '("status" "show") :test #'string=) (status-text))
      ((string= verb "reset")
       (reset-state)
       (format nil "lightning-ai: counters cleared~%~a" (status-text)))
      ((string= verb "help")
       "lightning-ai: /lightning-ai [status | reset | help] - holds Lightning AI calls to their per-minute limit; the limit and the hosts are in /settings")
      (t (format nil "lightning-ai: unknown argument ~s (try /lightning-ai help)" verb)))))

(defun check-config ()
  "Refuse a section naming no host to gate, in the words that say what to set."
  (unless (gated-hosts)
    (error 'nlk:config-refusal
           :detail "lightning-ai: hosts is empty; name the endpoints to hold to the limit, e.g. [\"lightning.ai\"], in the lightning-ai section, then (restart-cells \"nodecode-lightning-ai\")")))

;;; --- the declaration ---------------------------------------------------------

(nle:define-cell lightning-ai
  (:section ("lightning-ai")
    (:guide "hosts is required: the endpoint hosts your Lightning AI key talks to, e.g. lightning.ai; max_per_minute is the provider's own limit, 15 a minute; window_seconds is the window it counts in, aligned to the wall clock")
    ("hosts" :list :required t
     :doc "endpoint hosts whose calls the gate holds to the limit, e.g. lightning.ai")
    ("max_per_minute" :integer :default 15 :min 1
     :doc "requests allowed in one window; Lightning AI answers 15")
    ("window_seconds" :integer :default 60 :min 1
     :doc "the window the provider counts in, aligned to the wall clock"))
  (:command "lightning-ai" 'run-slash
            :description "The Lightning AI request gate: this window's state")
  (:help :lightning-ai
   "lightning-ai: holds Lightning AI model calls to their per-minute limit - /lightning-ai [status | reset | help]"
   "Lightning AI serves its API in a fixed wall-clock minute and allows about fifteen requests in one, with no Retry-After. This cell counts the requests Nodecode sends to the hosts named in its section and makes the ones past max_per_minute wait for the next minute to begin, so a burst of rounds does not spend itself on 429s. It sits on the one transport bracket every streaming lane shares, so a retried round is counted too; a provider that is not Lightning AI is never touched, and a failure in the gate lets the round run. A 429 parks the rest of the window, since the server just said its counter was spent. Everything stays in memory: counts and seconds, never a prompt or an answer. Settings: hosts (required), max_per_minute, window_seconds.")
  (:hook 'nodecode.evolved::walk-provider-stream #'gate-call)
  (:start (lambda ()
            (check-config)
            (reset-state))))
