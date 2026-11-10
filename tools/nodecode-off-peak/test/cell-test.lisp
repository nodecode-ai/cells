;;;; cell-test.lisp --- the window math, the ledger, approval, the verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches the network or the operator's files: the clock is a
;;;; fixed instant passed to the pure functions, and every ledger is one
;;;; temporary file a test writes itself.

(in-package #:nodecode.test)

(define-test-slice "off-peak" "OFF-PEAK-CELL-" :start nodecode-off-peak:start-cell)

;;; --- fixtures ---------------------------------------------------------------------

(defun off-peak-ledger-path ()
  "The ledger every test writes and reads."
  (namestring (merge-pathnames "off-peak-ledger.md" (uiop:temporary-directory))))

(defun off-peak-at (year month date hour &optional (minute 0))
  "The universal time of an instant in UTC."
  (encode-universal-time 0 minute hour date month year 0))

(defun off-peak-write (text)
  "TEXT as the ledger every test reads; answers its path."
  (let ((path (off-peak-ledger-path)))
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-string text out))
    path))

(defparameter +ledger+
  (format nil "# Queue~%| ID | job | owner | evidence | status | window |~%| --- | --- | --- | --- | --- | --- |~%| K-001 | Rebuild the index | me | exit 0 | awaiting | weekdays |~%| K-002 | Re-run the corpus | me | report.md | onaylı | weekend |~%- id: K-003 | durum: koşuldu | evidence: 12 files~%- id: K-004 | status: atlandı | reason: no key~%")
  "A ledger of both shapes, one item per status.")

;;; --- lifecycle --------------------------------------------------------------------

(define-cell-lifecycle-tests "off-peak"
  (:config "queue_file" (off-peak-ledger-path))
  (:help :off-peak)
  (:command "off-peak")
  (:running (is (equal nodecode-off-peak::+primer+ (nle:help :off-peak)) "(help :off-peak) is the primer")
            (is (= 30 (nodecode-off-peak::setting :remind-minutes)) "remind_minutes defaults to thirty"))
  (:refused ("remind_minutes" 0))
  (:idle nodecode-off-peak:off-peak-error (nodecode-off-peak:status-text)))

;;; --- the window --------------------------------------------------------------------

(deftest off-peak-cell-window-is-peak-in-the-weekday-afternoon ()
  (let* ((wednesday-13 (off-peak-at 2026 11 4 13))
         (window (nodecode-off-peak:window wednesday-13)))
    (is (getf window :peak-p) "a Wednesday afternoon is peak")
    (is (equal "off-peak" (getf window :change-kind)) "and the next change is the off-peak start")
    (is (= (off-peak-at 2026 11 4 18) (getf window :change-at)) "at 18:00 UTC the same day")
    (is (= 18000 (getf window :seconds)) "five hours away")
    (is (equal "5h 00m" (getf window :countdown)) "counted down in hours and minutes"))
  (is (getf (nodecode-off-peak:window (off-peak-at 2026 11 2 12)) :peak-p) "Monday 12:00 UTC is already peak")
  (is (not (getf (nodecode-off-peak:window (off-peak-at 2026 11 2 18)) :peak-p)) "Monday 18:00 UTC is off-peak"))

(deftest off-peak-cell-window-is-off-peak-all-weekend ()
  (let ((window (nodecode-off-peak:window (off-peak-at 2026 11 7 9))))
    (is (not (getf window :peak-p)) "a Saturday is off-peak all day")
    (is (equal "peak" (getf window :change-kind)) "and the next change is the peak start")
    (is (= (off-peak-at 2026 11 9 12) (getf window :change-at)) "on Monday at 12:00 UTC")
    (is (equal "2d 3h" (getf window :countdown)) "two days and three hours away"))
  (is (equal "14m" (nodecode-off-peak::human-duration 840)) "a short countdown in minutes")
  (is (equal "2026-11-04T13:00:00Z" (nodecode-off-peak::iso-utc (off-peak-at 2026 11 4 13))) "an instant reads back as ISO 8601 in UTC"))

;;; --- the ledger --------------------------------------------------------------------

(deftest off-peak-cell-ledger-reads-both-shapes-and-both-word-sets ()
  (let* ((items (nodecode-off-peak:ledger-items (off-peak-write +ledger+)))
         (counts (nodecode-off-peak:queue-counts items)))
    (is (= 4 (length items)) "the header and the rule row are not items")
    (is (equal '("K-001" "K-002" "K-003" "K-004") (mapcar (lambda (item) (getf item :id)) items)) "the ids keep their own case, in the file's order")
    (is (equal '("awaiting" "approved" "ran" "skipped") (mapcar (lambda (item) (getf item :status)) items)) "onaylı, koşuldu and atlandı read as approved, ran and skipped")
    (is (= 4 (getf counts :total)) "four items counted")
    (is (= 1 (getf counts :approved)) "one approved")
    (is (= 1 (getf counts :awaiting)) "one awaiting")
    (is (null (nodecode-off-peak:ledger-items (merge-pathnames "off-peak-absent.md" (uiop:temporary-directory)))) "a ledger that is not there is no items, not an error")))

;;; --- approval ---------------------------------------------------------------------

(deftest off-peak-cell-approve-marks-only-the-named-item ()
  (let* ((path (off-peak-write +ledger+))
         (before (uiop:read-file-string path)))
    (with-cell-stop ((off-peak-start "queue_file" (off-peak-ledger-path)))
      (is (search "Approved K-001" (cell-entry "nodecode-off-peak" "off-peak" "approve K-001")) "the answer names the item it approved")
      (let* ((after (uiop:read-file-string path))
             (old (nodecode-off-peak::split-lines before))
             (new (nodecode-off-peak::split-lines after))
             (row (nodecode-off-peak::split-on #\| (nth 3 new))))
        (is (= (length old) (length new)) "not one line added or dropped")
        (is (equal "approved" (string-trim " " (nth 5 row))) "K-001 now says approved")
        (is (equal (nth 4 old) (nth 4 new)) "K-002's own line is untouched")
        (is (equal (nth 5 old) (nth 5 new)) "and so is K-003's")
        (is (equal (nth 6 old) (nth 6 new)) "and K-004's"))
      (is (search "Nothing changed" (cell-entry "nodecode-off-peak" "off-peak" "approve K-001")) "approving an approved item changes nothing")
      (is (search "Nothing changed" (cell-entry "nodecode-off-peak" "off-peak" "approve K-900")) "an id the ledger lacks changes nothing")
      (is (search "Approved K-004" (cell-entry "nodecode-off-peak" "off-peak" "approve k-004")) "a bullet item approves too, whatever the case typed"))))

;;; --- the verbs ---------------------------------------------------------------------

(deftest off-peak-cell-command-and-tool-answer-the-same-status ()
  (off-peak-write +ledger+)
  (with-cell-stop ((off-peak-start "queue_file" (off-peak-ledger-path) "remind_minutes" 15))
    (let ((report (cell-entry "nodecode-off-peak" "off-peak" "status")))
      (is (or (search "PEAK now" report) (search "OFF-PEAK now" report)) "the report names the window it is in")
      (is (search "4 items" report) "and the ledger's count")
      (is (search "/off-peak approve" report) "and how approval is given"))
    (let* ((answer (cell-json (cell-entry "nodecode-off-peak" "off-peak" "json")))
           (tool (cell-json (nodecode-off-peak::status-tool (nlk:make-json-object))))
           (queue (gethash "queue" answer)))
      (is (member (gethash "window" answer) '("peak" "off-peak") :test #'equal) "the JSON names the window")
      (is (= 4 (gethash "total" queue)) "and counts the ledger")
      (is (= 1 (gethash "approved" queue)) "approved")
      (is (= 15 (gethash "remind_minutes" answer)) "the reminder lead is the one set")
      (is (equal (gethash "window" answer) (gethash "window" tool)) "the tool answers the same status the command does")
      (is (equal (namestring (pathname (off-peak-ledger-path))) (gethash "queue_file" answer)) "and names the ledger it read"))
    (is (search "hash:" (cell-entry "nodecode-off-peak" "off-peak" "plan")) "the plan prints its hash")
    (is (search "Unknown argument" (cell-entry "nodecode-off-peak" "off-peak" "bogus")) "an unknown argument says so")))

(deftest off-peak-cell-missing-queue-file-refuses ()
  (with-cell-stop ((off-peak-start))
    (is (search "queue_file" (cell-entry "nodecode-off-peak" "off-peak" "status")) "the report names the setting that is missing")
    (is (signals-error nodecode-off-peak:off-peak-error (nodecode-off-peak::status-tool (nlk:make-json-object))) "the tool refuses with the cell's own condition")))

