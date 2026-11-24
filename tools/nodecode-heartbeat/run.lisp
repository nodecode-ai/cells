;;;; run.lisp --- the probes, the wake, and the ticker's one pass.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The economy of the original, kept: a probe is decided in Lisp, so a
;;;; watch that has nothing to say costs no model turn. Only a due wake, or
;;;; a probe that crossed from what it last saw into what it watches for,
;;;; submits a prompt. That prompt goes into the session that armed the
;;;; watch -- there is no destination argument anywhere -- and it carries
;;;; the receipt: what was checked, what it answered, and when.
;;;;
;;;; Everything that leaves the process is behind two seams a test stubs:
;;;; SUBMIT-WAKE (the in-process ingress) and *HTTP* (the one network call).

(in-package #:nodecode-heartbeat)

(defparameter +wake-prompt+
  "No one typed this turn: it is a wake from the heartbeat cell, in the session that armed the watch.
Answer in a few lines: say what it saw, in your own words, and what you propose. If nothing here is
worth the operator's attention, say so in one line and stop. Do not re-arm, pause or remove the watch
to keep it alive: it keeps its own clock. Change it only when what it watches or how often it looks
should change."
  "What every wake turn is asked. Data, so a layer can reword it.")

;;; --- what a probe sees ---------------------------------------------------------------

(defun file-state (watch)
  "What the file WATCH watches looks like now: (:OK :VALUE :DETAIL)."
  (let ((path (watch-get watch :path)))
    (handler-case
        (if (probe-file path)
            (let* ((written (file-write-date path))
                   (size (with-open-file (in path :element-type '(unsigned-byte 8))
                           (file-length in))))
              (list :ok t
                    :value (format nil "~a:~a" written size)
                    :detail (format nil "~a bytes, written ~a" size (nlk:iso-time written))))
            (list :ok nil :value "missing" :detail "the file is gone"))
      (error (condition)
        (list :ok nil :value "unreadable" :detail (nlk:one-line (princ-to-string condition)))))))

(defun url-state (watch)
  "What the url WATCH answers now: any 2xx or 3xx is up. (:OK :VALUE :DETAIL)."
  (handler-case
      (multiple-value-bind (body status) (funcall *http* "GET" (watch-get watch :url)
                                                  :timeout (setting :check-timeout-seconds))
        (declare (ignore body))
        (let ((ok (and (integerp status) (<= 200 status 399))))
          (list :ok ok :value (format nil "~a" status) :detail (format nil "HTTP ~a" status))))
    (error (condition)
      (list :ok nil :value "unreachable"
            :detail (nlk:one-line (princ-to-string condition))))))

(defun command-state (watch)
  "What the command WATCH answers now: exit 0 is up, anything else is a break.
The command is an argv vector run without a shell, one more argument to a
program already on the machine, never a line of shell."
  (handler-case
      (multiple-value-bind (output error-output code)
          (uiop:run-program (coerce (watch-get watch :command #()) 'list)
                            :output :string :error-output :string
                            :ignore-error-status t)
        (let* ((exit (or code 0))
               (said (nlk:one-line (string-trim '(#\Space #\Newline #\Tab #\Return)
                                                (or (and (plusp (length output)) output)
                                                    error-output ""))
                                    :cap 200)))
          (list :ok (eql exit 0)
                :value (format nil "~a:~a" exit said)
                :detail (format nil "exit ~a~@[: ~a~]" exit (and (plusp (length said)) said)))))
    (error (condition)
      (list :ok nil :value "unrunnable" :detail (nlk:one-line (princ-to-string condition))))))

(defun probe-watch (watch)
  "Observe WATCH by its kind: a plist (:OK :VALUE :DETAIL)."
  (cond ((equal (watch-get watch :kind) "file") (file-state watch))
        ((equal (watch-get watch :kind) "url") (url-state watch))
        ((equal (watch-get watch :kind) "command") (command-state watch))
        (t (list :ok t :value "" :detail ""))))

(defun watch-fires-p (watch state)
  "Whether STATE, what probe WATCH just saw, is worth waking the session for.
The first look is a baseline except for a break, which speaks once."
  (let ((fire-on (or (watch-get watch :fire-on) "failure"))
        (last (watch-get watch :last-state))
        (last-value (watch-get watch :last-value))
        (ok (getf state :ok))
        (value (getf state :value)))
    (cond ((equal fire-on "change") (and last-value (not (equal value last-value))))
          ((equal fire-on "success") (and ok (equal last "fail")))
          (t (and (not ok) (not (equal last "fail")))))))

(defun due-p (watch now)
  "Whether WATCH's clock has come round."
  (let ((next (watch-get watch :next-at)))
    (and (numberp next) (<= next now))))

;;; --- the wake -------------------------------------------------------------------------

(defun submit-wake (session prompt command-id watch-id)
  "PROMPT into SESSION through the one in-process ingress. The cell's seam: a test stubs this name."
  (declare (ignore watch-id))
  (nle:submit session prompt :command-id command-id :source "heartbeat" :source-id watch-id))

(defun wake-text (watch &optional state)
  "The prompt a wake carries: the ask, then the watch's own receipt."
  (format nil "~a~%~%The watch \"~a\" (~a)~@[ what the operator wanted of it: ~a~]~@[~%It just saw: ~a~]~%Now: ~a."
          +wake-prompt+
          (or (watch-get watch :title) (watch-get watch :id))
          (watch-trigger-text watch)
          (let ((note (watch-get watch :note "")))
            (and (plusp (length note)) note))
          (and state (getf state :detail))
          (nlk:iso-time (get-universal-time))))

(defun announce (watch text &key (level :info))
  "Say TEXT on every attached shell under this watch's own key."
  (nle:notice text :level level :key (format nil "heartbeat:~a" (watch-get watch :id))))

(defun note-fire (watch)
  "Count the fire, and retire the watch when it is one-shot or spent. The caller holds the lock. => whether it retired."
  (let ((fires (incf (watch-get watch :fires 0)))
        (max (or (watch-get watch :max-fires 0) 0))
        (one-shot (equal (watch-get watch :kind) "after")))
    (when (or one-shot (and (plusp max) (>= fires max)))
      (setf *watches* (cl:remove watch *watches* :test #'eq))
      t)))

(defun send-wake (watch text now)
  "One wake into the session that armed WATCH. => T when it went out."
  (let ((id (watch-get watch :id)))
    (handler-case
        (progn
          (submit-wake (watch-get watch :session) text
                       (format nil "heartbeat-~a-~d" id now) id)
          (with-heartbeat-lock (note-fire watch))
          (announce watch (format nil "~a: woke its session" id))
          t)
      (error (condition)
        (announce watch (format nil "~a: could not wake its session: ~a"
                                id (nlk:one-line (princ-to-string condition)))
                  :level :warning)
        nil))))

;;; --- one pass ---------------------------------------------------------------------------

(defun tick-one (watch now)
  "Check WATCH if it still stands, advance its clock, record what it saw.
=> T when a wake went out."
  (let* ((kind (watch-get watch :kind "every"))
         (probe-p (member kind +probe-kinds+ :test #'equal))
         (state (and probe-p (probe-watch watch)))
         (fires (and probe-p (watch-fires-p watch state))))
    (when probe-p
      (with-heartbeat-lock
        (when (find-watch (watch-get watch :id))
          (setf (watch-get watch :last-value) (getf state :value)
                (watch-get watch :last-ok) (getf state :ok)
                (watch-get watch :last-state) (if (getf state :ok) "ok" "fail")
                (watch-get watch :next-at) (+ now (max 5 (watch-get watch :every 300)))))))
    (if probe-p
        (and fires (send-wake watch (wake-text watch state) now))
        (progn
          (with-heartbeat-lock
            (when (find-watch (watch-get watch :id))
              (setf (watch-get watch :next-at) (+ now (max 5 (watch-get watch :every 300))))))
          (send-wake watch (wake-text watch) now)))))

(defun tick (&optional (now (get-universal-time)))
  "One pass over the watches: every watch whose clock came round is checked,
and one that fires submits a wake. => how many wakes went out."
  (let ((sent 0)
        (standing (with-heartbeat-lock (copy-list *watches*))))
    (dolist (watch standing)
      (when (and (find-watch (watch-get watch :id))
                 (not (watch-get watch :paused))
                 (due-p watch now)
                 (tick-one watch now))
        (incf sent)))
    (when standing (with-heartbeat-lock (save-watches)))
    sent))
