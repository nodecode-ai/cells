;;;; brief.lisp --- the approval card, the opening line, and what a call says.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's brief card, its disclosure sentence and its cost estimate,
;;;; as data a person reads before they approve a call. Nothing here dials and
;;;; nothing here reaches the network: it is the text of the promise.

(in-package #:nodecode-phone-call)

(defparameter +disclosure+ "Hi, this is an AI assistant calling on behalf of ~a."
  "Every call opens with this sentence, in the original's words.")
(defparameter +recorded+ "This call may be recorded."
  "Appended to the opening line when the operator asked for a recording.")
(defparameter +goodbye+ "Thank you. Goodbye."
  "The last thing every call says.")
(defparameter +wont-share-always+ "card data, address, ID numbers, anything not listed above"
  "The original's line for what a call never volunteers.")
(defparameter +cost-per-minute-usd+ 0.014
  "Twilio's outbound price per started minute, the original's default; an estimate only.")

(defun user-name ()
  "Who the agent calls on behalf of, as the section names it."
  (or (nonempty (ignore-errors (setting :user-name))) "the operator"))

(defun opening-line (record)
  "The first sentence the callee hears: the AI disclosure, always first."
  (let ((line (format nil +disclosure+ (user-name))))
    (if record (concatenate 'string line " " +recorded+) line)))

(defun clamp-duration (value)
  "VALUE seconds inside the cell's floor and hard cap, the section's default when absent."
  (let ((default (or (ignore-errors (setting :max-duration-s)) +default-duration-s+)))
    (let ((n (cond ((integerp value) value)
                   ((and (stringp value) (every #'digit-char-p value))
                    (parse-integer value))
                   (t default))))
      (max +min-duration-s+ (min +max-duration-s+ n)))))

(defun estimate-cost-usd (duration-s)
  "A rough estimate, rounded up to whole started minutes: a promise, not a bill."
  (round (* (ceiling duration-s 60) +cost-per-minute-usd+) 0.01))

(defun call-granted (call)
  "The sensitive fields this call was granted."
  (getf call :granted))

(defun call-message (call)
  "What the callee is told after the disclosure: the operator's message, or
the goal, with anything not granted withheld."
  (let ((text (or (nonempty (getf call :message)) (getf call :goal) "")))
    (redact-text text (call-granted call))))

(defun format-card (call)
  "CALL as the card a person approves — the original's lines, and its hint
naming the command that dials."
  (let* ((granted (call-granted call))
         (raw (getf call :facts))
         (requested (getf call :sensitive-requested))
         (needs (remove-if (lambda (f) (member f granted :test #'equal)) requested)))
    (multiple-value-bind (safe withheld) (build-safe-facts raw granted)
      (with-output-to-string (out)
        (format out "Call brief ~a~%" (getf call :id))
        (format out "Who: ~a~%" (getf call :callee))
        (format out "Number: ~a (source: ~a)~%" (getf call :number)
                (or (nonempty (getf call :number-source)) "unspecified"))
        (format out "Goal: ~a~%" (redact-text (getf call :goal) granted))
        (format out "Opening line: \"~a\"~%" (opening-line (getf call :record)))
        (format out "Will say: \"~a\"~%" (call-message call))
        (format out "Will share: ~a~%"
                (if safe
                    (format nil "~{~a~^; ~}"
                            (mapcar (lambda (pair) (format nil "~a: ~a" (car pair)
                                                           (value-text (cdr pair))))
                                    safe))
                    "nothing beyond the goal"))
        (format out "Won't share: ~a~%"
                (format nil "~{~a~^; ~}" (cons +wont-share-always+ withheld)))
        (when needs
          (format out "Needs your consent to share: ~{~a~^, ~} (left out unless you allow it)~%"
                  needs))
        (format out "Limits: max ~d s, est. cost up to $~,2f, recording ~a~%"
                (getf call :max-duration-s) (estimate-cost-usd (getf call :max-duration-s))
                (if (getf call :record) "on" "off"))
        (when (getf call :override-quiet-hours)
          (format out "Quiet hours override: yes~%"))
        (format out "Nothing is dialed until the operator approves: /phone-call approve ~a~%"
                (getf call :id))))))

(defun brief-plist (call)
  "CALL's public brief, for a status answer: no raw facts unless granted."
  (multiple-value-list
   (build-safe-facts (getf call :facts) (call-granted call))))
