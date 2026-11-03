;;;; cycle.lisp --- one observe cycle, and the wake source that runs it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cycle: the stop, the breakers and the idle gate, all in Lisp and before
;;;; any work, so a cycle that holds costs nothing; then the lock, the first
;;;; item the backlog ranks that the ledger still allows, its repro in a
;;;; throwaway home, and the outcome. A repro that fails as the item says is
;;;; a finding: the test is copied under selffix/findings/<fp>/, hashed, and
;;;; the hash goes in the ledger, so a later fix is checked against the test
;;;; as it was found, never against one the fixing session rewrote. Observe
;;;; changes no code, and makes no model call of its own.
;;;;
;;;; It runs as a source on the cron wake: when the wake is due, the source
;;;; runs one cycle and reports a new finding or a pause as news, which the
;;;; wake's turn reads. A wake that is off runs no cycle.

(in-package #:nodecode-selffix)

(defparameter *backlog-limit* 20 "Items read from the backlog for one cycle.")

(defparameter *output-cap* 4000 "Characters of repro output a finding keeps.")

(defvar *backlog* 'backlog "The backlog reader; a test binds a stub.")
(defvar *repro* 'repro "The repro; a test binds a stub.")
(defvar *busy* 'busy-reason "The idle gate's reader; a test binds a stub.")

(defun finding-dir (fp) (selffix-path (format nil "findings/~a/" fp)))
(defun scratch-home (fp) (selffix-path (format nil "scratch/~a/" fp)))

(defun file-sha256 (path)
  "PATH's SHA-256 as lowercase hex."
  (let ((digest (with-open-file (in path :element-type '(unsigned-byte 8))
                  (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
                    (read-sequence octets in)
                    (nlk:sha256-text octets)))))
    (subseq digest (1+ (position #\: digest)))))

(defun bare-hex (hash)
  (let ((text (string-downcase (princ-to-string (or hash "")))))
    (subseq text (if (uiop:string-prefix-p "sha256:" text) 7 0))))

(defun item-text (item)
  "What ITEM is called: its title, else its key."
  (princ-to-string (or (getf item :title) (getf item :key))))

(defun finding-line (fp item)
  (format nil "~a (~(~a~), seen ~d time~:p): ~a" fp (getf item :source) (or (getf item :count) 1)
          (nlk:one-line (item-text item) :cap 120)))

(defun write-finding (fp item result now)
  "Copy RESULT's test under FP's folder, check its hash, write finding.md;
answer (values FINDING-PATH TEST-COPY HASH)."
  (let* ((dir (finding-dir fp))
         (test (pathname (getf result :test)))
         (copy (merge-pathnames (file-namestring test) dir)))
    (ensure-directories-exist dir)
    (uiop:copy-file test copy)
    (let ((hash (file-sha256 copy)))
      (unless (equal hash (bare-hex (getf result :sha256)))
        (fail "the repro test changed between its run and its copy: ~a" (namestring test)))
      (let ((finding (merge-pathnames "finding.md" dir))
            (output (princ-to-string (or (getf result :output) ""))))
        (with-open-file (out finding :direction :output :if-exists :supersede)
          (format out "# selffix finding ~a~%~%" fp)
          (format out "- source: ~a~%- key: ~a~%- seen: ~d time~:p~@[, last ~a~]~%- found: ~a~%"
                  (getf item :source) (nlk:one-line (princ-to-string (getf item :key)) :cap 300)
                  (or (getf item :count) 1) (getf item :last) (iso now))
          (format out "- test: ~a, sha256 ~a~%" (file-namestring copy) hash)
          (format out "~%The test fails today. It is locked: a fix counts only when this file,~%~
                       unchanged (same sha256), passes.~%")
          (let ((evidence (getf item :evidence)))
            (when evidence
              (format out "~%## Evidence~%~%~{- ~a~%~}" (mapcar (lambda (e) (nlk:one-line (princ-to-string e) :cap 300))
                                                              (if (listp evidence) evidence (list evidence))))))
          (format out "~%## What the test printed~%~%```~%~a~%```~%"
                  (subseq output 0 (min *output-cap* (length output)))))
        (values finding copy hash)))))

(defun run-one (now)
  "With the lock held: one item through its repro => (STATUS DETAIL)."
  (let ((items (handler-case (funcall *backlog* :limit *backlog-limit*)
                 (error (condition) (record "failed" :detail (format nil "backlog: ~a" condition))
                   (return-from run-one (list :failed (format nil "the backlog could not be read: ~a" condition)))))))
    (multiple-value-bind (kept dropped) (eligible items (ledger-rows))
      (dolist (item dropped)
        (record "dropped" :fp (fingerprint item) :item item
                          :detail (format nil "~d cycles without a finding" *attempt-cap*)))
      (if (null kept)
          (list :nothing "nothing in the backlog to reproduce")
          (let* ((item (first kept))
                 (fp (fingerprint item)))
            (handler-case
                (let ((result (funcall *repro* item :home (scratch-home fp))))
                  (cond ((getf result :fails)
                         (multiple-value-bind (finding copy hash) (write-finding fp item result now)
                           (declare (ignore copy))
                           (record "finding" :fp fp :item item :detail (namestring finding) :sha256 hash :at now)
                           (list :finding (finding-line fp item))))
                        (t
                         (record "not-reproduced" :fp fp :item item :at now
                                                  :detail (nlk:one-line (princ-to-string (or (getf result :output) "")) :cap 200))
                         (list :not-reproduced (finding-line fp item)))))
              (error (condition)
                (record "failed" :fp fp :item item :at now :detail (princ-to-string condition))
                (list :failed (format nil "~a: ~a" fp condition)))))))))

(defun cycle (&key (now (get-universal-time)) (idle-seconds (* 60 (setting :idle-minutes))))
  "One observe cycle => (values STATUS DETAIL). STATUS is :STOPPED, :PAUSED,
:BUSY or :LOCKED when a gate held, else :NOTHING, :FINDING, :NOT-REPRODUCED
or :FAILED."
  (let ((rows (ledger-rows)))
    (nlk:when-let (stop (stopped rows))
      (return-from cycle (values :stopped (format nil "stopped at ~a~@[ by ~a~]" (row stop "at") (row stop "by")))))
    (nlk:when-let (reason (breaker rows))
      (return-from cycle (values :paused reason)))
    (nlk:when-let (busy (funcall *busy* now idle-seconds))
      (return-from cycle (values :busy busy)))
    (unless (take-lock now)
      (return-from cycle (values :locked "another cycle holds the lock")))
    (unwind-protect (values-list (run-one now))
      (drop-lock))))

(defun post-findings ()
  "Stand the findings count on the board under the cell's key, or clear it."
  (let ((count (length (findings (ledger-rows)))))
    (nle:notice (and (plusp count)
                     (format nil "selffix: ~d finding~:p, each with a failing test (/selffix findings)" count))
                :key "nodecode-selffix")))

(defun wake-source (since)
  "The cron wake's source: one cycle at the due instant. News buys the wake a
model turn, so only a finding and the failure that trips the breaker are news;
a single failure, a held pause and every gate answer nothing, and the wake
keeps its quiet backoff at $0. /selffix status shows the rest."
  (declare (ignore since))
  (multiple-value-bind (status detail) (cycle)
    (case status
      (:finding (post-findings) (list (format nil "selffix found ~a" detail)))
      (:failed (nlk:when-let (pause (breaker (ledger-rows)))
                 (list (format nil "selffix paused: ~a, the last ~a; (selffix:selffix :on) resumes it"
                               pause detail))))
      (t '()))))
