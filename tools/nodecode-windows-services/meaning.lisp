;;;; meaning.lisp --- what an exit code and an event id say, and the failure
;;;; policy decoded out of its blob.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Windows leaves the evidence of a dead service in three numbers and one
;;;; binary value: Win32_Service's ExitCode, the Service Control Manager's
;;;; event id, and the FailureActions blob nobody ever opened. This file is
;;;; the words for all three, and it is pure -- no child process, no clock,
;;;; nothing to stub -- so every rule here is proved without Windows.

(in-package #:nodecode-windows-services)

(defparameter +exit-meanings+
  '((0 . "success")
    (2 . "the binary could not be found")
    (3 . "the binary's path could not be found")
    (5 . "access was denied")
    (14 . "not enough storage to run it")
    (126 . "a DLL it needs could not be found")
    (193 . "the binary is not a valid Win32 program")
    (1053 . "it did not answer the start request in time")
    (1055 . "the account it runs as could not log on")
    (1056 . "an instance was already running")
    (1058 . "it is disabled and cannot be started")
    (1062 . "it has not been started")
    (1067 . "the process ended unexpectedly")
    (1072 . "it is marked for deletion")
    (1075 . "a service it depends on failed to start first")
    (1077 . "it has not tried to start since the last boot"))
  "Win32_Service's ExitCode values, in words. They are Win32 error numbers,
not HRESULTs.")

(defparameter +not-a-failure+ '(0 1077)
  "Exit codes that describe an untouched service rather than a failure: 1077
has simply not tried since the last boot.")

(defparameter +scm-events+
  '((7000 . "failed to start")
    (7001 . "could not connect to the service controller as its account")
    (7009 . "a start request timed out (30 s)")
    (7011 . "a control request timed out")
    (7023 . "ended with an error")
    (7024 . "ended with a service-specific error")
    (7031 . "ended unexpectedly")
    (7034 . "ended unexpectedly, more than once inside a minute")
    (7045 . "was installed on this machine"))
  "The Service Control Manager's own event ids in the System log, in words.")

(defparameter +action-names+
  '((0 . "nothing") (1 . "restart the service") (2 . "restart the computer")
    (3 . "run a command"))
  "The failure actions SERVICE_FAILURE_ACTIONS can carry.")

(defun exit-of (code)
  "CODE as (:code N :hex 0x0000 :meaning TEXT), or NIL when it is not a number."
  (when (and code (integerp code))
    (list :code code
          :hex (format nil "0x~8,'0X" (logand code #xFFFFFFFF))
          :meaning (or (cdr (assoc code +exit-meanings+)) "no known meaning"))))

(defun failed-p (row)
  "Whether ROW's exit code is a failure rather than an untouched service."
  (let ((code (getf row :exit-code)))
    (and (integerp code) (not (member code +not-a-failure+)))))

(defun sc-event-of (id)
  "Event ID in words; an id the cell does not translate says so."
  (or (and (integerp id) (cdr (assoc id +scm-events+)))
      "an event this cell does not translate"))

;;; --- the failure policy, out of its blob ------------------------------------

(defun u32 (bytes offset)
  "The little-endian DWORD at OFFSET of BYTES, a vector of integers, or NIL."
  (when (<= (+ offset 4) (length bytes))
    (+ (aref bytes offset)
       (ash (aref bytes (1+ offset)) 8)
       (ash (aref bytes (+ offset 2)) 16)
       (ash (aref bytes (+ offset 3)) 24))))

(defun decode-failure-actions (bytes)
  "BYTES, the FailureActions registry value as an array of numbers, decoded
as (:reset-seconds N :count N :command-p BOOL :actions ((NAME . SECONDS) ...))
-- or NIL when the service sets no failure action at all. The layout is the
32-bit SERVICE_FAILURE_ACTIONS ABI: the reset period, two string offsets,
the action count, the offset of the action array, then that many (type,
delay-in-milliseconds) pairs."
  (when (and bytes (vectorp bytes) (>= (length bytes) 20))
    (let ((reset (u32 bytes 0))
          (command (u32 bytes 8))
          (count (u32 bytes 12))
          (at (u32 bytes 16)))
      (when (and count at (<= (+ at (* 8 count)) (length bytes)))
        (list :reset-seconds reset
              :count count
              :command-p (and command (plusp command))
              :actions (loop for index below count
                             for type = (u32 bytes (+ at (* 8 index)))
                             for delay = (u32 bytes (+ at (* 8 index) 4))
                             collect (cons (or (cdr (assoc type +action-names+))
                                               (format nil "an action this cell does not know (~a)" type))
                                           (/ delay 1000))))))))

(defun seconds-text (seconds)
  "SECONDS as a person reads it: whole when it is whole."
  (if (integerp seconds) (princ-to-string seconds) (format nil "~,1F" seconds)))

(defun policy-text (policy)
  "POLICY, decoded, as the sentence a person reads."
  (unless policy
    (return-from policy-text
      "none is configured: Windows leaves this service dead when it fails"))
  (let* ((actions (getf policy :actions))
         (reset (getf policy :reset-seconds))
         (parts (if actions
                    (mapcar (lambda (action)
                              (format nil "~a after ~a s" (car action) (seconds-text (cdr action))))
                            actions)
                    (list "nothing"))))
    (when (and reset (plusp reset))
      (setf parts (append parts
                          (list (format nil "the count resets after ~a h"
                                        (seconds-text (/ reset 3600)))))))
    (when (getf policy :command-p)
      (setf parts (append parts (list "a command runs with it"))))
    (format nil "~{~a~^; ~}" parts)))
