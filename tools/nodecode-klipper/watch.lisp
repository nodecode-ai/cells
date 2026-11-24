;;;; watch.lisp --- one printer sample against the last, and the events between.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure: no network, no clock of its own, no file. CELL.LISP reads the
;;;; printer and keeps the state this compares, so the diff is testable on
;;;; its own. Ported from the original's watch.py.

(in-package #:nodecode-klipper)

(defparameter +state-version+ 1
  "The watch state file's version; a file of another version is not replaced.")

(defparameter +klippy-stopped+ '("shutdown" "error")
  "webhooks.state values that mean Klipper itself has stopped.")

(defparameter +klippy-message-max+ 300)

;;; --- reading decoded JSON ---------------------------------------------------------

(defun jget (object key)
  "KEY's member of the decoded JSON OBJECT, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun jnum (value)
  "VALUE when it is a JSON number, else NIL; a boolean is not one."
  (and (realp value) (float value)))

(defun jstr (value)
  (and (stringp value) value))

(defun json-of (&rest pairs)
  "A JSON object from (KEY VALUE) pairs, KEY a string; a VALUE of :ABSENT is
left out, so a member the printer did not report is missing rather than false."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          unless (eq value :absent) do (setf (gethash key object) value))
    object))

;;; --- one sample --------------------------------------------------------------------

(defun klippy-detail (message)
  "First line of webhooks.state_message, trimmed, capped."
  (let* ((text (or message ""))
         (newline (position #\Newline text))
         (first (string-trim '(#\Space #\Tab #\Return) (subseq text 0 newline)))
         (last (1- (length first))))
    (subseq (if (and (plusp (length first)) (char= (char first last) #\.))
                (subseq first 0 last)
                first)
            0 (min (length first) +klippy-message-max+))))

(defun sample-from-status (status)
  "STATUS (the printer objects Moonraker answered) as the plist the watch compares."
  (let ((stats (jget status "print_stats"))
        (virtual (jget status "virtual_sdcard"))
        (hooks (jget status "webhooks")))
    (list :filename (or (jstr (jget stats "filename")) "")
          :state (or (jstr (jget stats "state")) "")
          :file-position (jnum (jget virtual "file_position"))
          :progress (jnum (jget virtual "progress"))
          :klippy-state (or (jstr (jget hooks "state")) "")
          :klippy-message (or (jstr (jget hooks "state_message")) "")
          :message (or (jstr (jget stats "message")) ""))))

(defun fresh-state (now sample)
  (list :version +state-version+
        :filename (getf sample :filename)
        :state (getf sample :state)
        :klippy-state (getf sample :klippy-state)
        :klippy-stops 0
        :file-position (getf sample :file-position)
        :progress (getf sample :progress)
        :progress-changed-at now
        :stall-open nil
        :generation (if (string= (getf sample :state) "printing") 1 0)
        :notified '()))

(defun event (kind text generation)
  (list :kind kind :generation generation :text text))

(defun klippy-stop-event (sample previous-state stops)
  (let* ((kind (let ((value (getf sample :klippy-state)))
                 (if (plusp (length value)) value "shutdown")))
         (detail (let ((text (klippy-detail (getf sample :klippy-message))))
                   (if (plusp (length text)) text "no state_message")))
         (during (or (member previous-state '("printing" "paused") :test #'string=)
                     (member (getf sample :state) '("printing" "paused") :test #'string=)))
         (where (if during
                    (format nil "during print ~a"
                            (let ((name (getf sample :filename)))
                              (if (plusp (length name)) name "(no filename)")))
                    "while not printing"))
         (tail (if during
                   (format nil " print_stats.state now reads ~s; this is Klipper stopping, ~
not a normal pause or print error, and the print cannot be resumed from this plugin."
                           (if (plusp (length (getf sample :state))) (getf sample :state) "unknown"))
                   "")))
    (let ((result (event "klippy_shutdown"
                         (format nil "Klipper stopped (~a) ~a: ~a.~a Check the printer. ~
Klipper needs FIRMWARE_RESTART or RESTART before it can print again."
                                 kind where detail tail)
                         stops)))
      (append result (list :klippy-state kind)))))

(defun compare (previous sample now stall-minutes)
  "PREVIOUS state (NIL for none) against SAMPLE at NOW.
=> (values NEW-STATE EVENTS NOTE). A previous state is never dropped here."
  (let* ((stopped (member (getf sample :klippy-state) +klippy-stopped+ :test #'string=))
         (events '()))
    (flet ((once (kind text generation notified)
             (let ((key (format nil "~d:~a" generation kind)))
               (unless (member key notified :test #'string=)
                 (push key notified)
                 (push (event kind text generation) events))
               notified)))
      (if (null previous)
          (let ((state (fresh-state now sample)))
            (if stopped
                (progn (setf (getf state :klippy-stops) 1)
                       (values state (list (klippy-stop-event sample nil 1)) "New Klipper stop."))
                (values state '()
                        "No earlier sample is stored, so this check only records the current state.")))
          (let* ((stops (or (getf previous :klippy-stops) 0))
                 (was-stopped (member (getf previous :klippy-state) +klippy-stopped+ :test #'string=))
                 (generation (or (getf previous :generation) 0))
                 (notified (copy-list (or (getf previous :notified) '())))
                 (previous-state (or (getf previous :state) ""))
                 (entered (and (string= (getf sample :state) "printing")
                               (not (string= previous-state "printing"))))
                 (changed-at (or (getf previous :progress-changed-at) now))
                 (stall-open (not (null (getf previous :stall-open)))))
            (when (and stopped (not was-stopped))
              (incf stops)
              (push (klippy-stop-event sample previous-state stops) events))
            (when entered (incf generation))
            (unless (string= (getf sample :state) previous-state)
              (cond
                ((string= (getf sample :state) "complete")
                 (setf notified
                       (once "complete"
                             (format nil "Print finished: ~a."
                                     (let ((name (getf sample :filename)))
                                       (if (plusp (length name)) name "(no filename)")))
                             generation notified)))
                ((and (string= (getf sample :state) "error") (not stopped))
                 (setf notified
                       (once "error"
                             (format nil "Print error: ~a."
                                     (let ((text (getf sample :message)))
                                       (if (plusp (length text)) text "print_stats.state is error")))
                             generation notified)))
                ((and (string= (getf sample :state) "paused")
                      (string= previous-state "printing") (not stopped))
                 (setf notified
                       (once "paused"
                             (format nil "Print paused: ~a."
                                     (let ((name (getf sample :filename)))
                                       (if (plusp (length name)) name "(no filename)")))
                             generation notified)))
                ((string= (getf sample :state) "cancelled")
                 (setf notified
                       (once "cancelled"
                             (format nil "Print cancelled: ~a."
                                     (let ((name (getf sample :filename)))
                                       (if (plusp (length name)) name "(no filename)")))
                             generation notified)))))
            (let ((moved (or (not (equal (getf sample :filename) (getf previous :filename)))
                             (not (equal (getf sample :file-position) (getf previous :file-position)))
                             (not (equal (getf sample :progress) (getf previous :progress))))))
              (if (and (string= (getf sample :state) "printing")
                       (getf sample :file-position) (not stopped))
                  (cond ((or moved entered)
                         (setf changed-at now stall-open nil))
                        ((and (not stall-open) (>= (- now changed-at) (* 60 stall-minutes)))
                         (setf stall-open t)
                         (setf notified
                               (once "stalled"
                                     (format nil "No file progress for ~d minutes while printing ~a."
                                             stall-minutes
                                             (let ((name (getf sample :filename)))
                                               (if (plusp (length name)) name "(no filename)")))
                                     generation notified))))
                  (setf stall-open nil)))
            (let* ((state (list :version +state-version+
                                :filename (getf sample :filename)
                                :state (getf sample :state)
                                :klippy-state (getf sample :klippy-state)
                                :klippy-stops stops
                                :file-position (getf sample :file-position)
                                :progress (getf sample :progress)
                                :progress-changed-at changed-at
                                :stall-open stall-open
                                :generation generation
                                :notified (sort notified #'string<)))
                   (note (cond (events "New print event.")
                               (stopped "No new event. Klipper is still stopped; it was reported when it stopped.")
                               (t "No new print event."))))
              (values state (nreverse events) note)))))))
