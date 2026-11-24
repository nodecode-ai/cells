;;;; tools.lisp --- which device is unhappy, what it needs, what drives it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The two tools are the original's, by name and by answer: win_device_table
;;;; lists the tree with each device's problem code in words, win_device_detail
;;;; is one device with the driver behind it. Both answer JSON text, read-only,
;;;; and both answer a refusal as {"ok": false, ...} rather than as a signal,
;;;; because the model repairs a wrong question from the words, not from a stack.

(in-package #:nodecode-windows-devices)

;;; --- the shape of a call ----------------------------------------------------------

(nlk:define-contract table-query
  (:doc "Which device on this Windows machine is unhappy: every present PnP device
with its class, status, manufacturer, device id and problem code in words.")
  ("query" :string :default ""
   :doc "A case-insensitive substring of the device's name or its DeviceID, e.g. 'camera', 'usb', 'LENOVO'.")
  ("class" :string :default "" :slot device-class
   :doc "Only devices in this PNPClass, e.g. 'Net', 'HIDClass', 'Display'.")
  ("broken_only" :boolean
   :doc "Only devices that need attention (a disabled one does not; default false).")
  ("include_absent" :boolean
   :doc "Include historical devices that are not plugged in any more (default false: only present ones).")
  ("limit" :integer :default 100
   :doc "Rows to return, 1 to 300 (default 100)."))

(nlk:define-contract detail-query
  (:doc "One device in full: everything the listing shows, plus the driver behind it --
its name, version, date, provider, signer, INF file and whether it is signed -- or
no driver at all, which for problem code 28 is the same fact said twice.")
  ("name" :text :required t
   :doc "The device's name (a substring works) or its exact DeviceID; win_device_table lists them all."))

;;; --- words and rows ---------------------------------------------------------------

(defparameter +max-rows+ 300 "The most rows one listing answers, whatever it is asked for.")
(defparameter +name-chars+ 120 "How much of one localized name or manufacturer the answer carries.")

(defun clip (text)
  "TEXT with its whitespace runs collapsed, cut at +NAME-CHARS+ with an ellipsis."
  (when (stringp text)
    (let* ((flat (map 'string (lambda (c) (if (member c '(#\Newline #\Return #\Tab)) #\Space c)) text))
           (squeezed (with-output-to-string (out)
                       (let ((space nil))
                         (loop for c across flat
                               do (cond ((char= c #\Space)
                                         (unless space (write-char c out))
                                         (setf space t))
                                        (t (write-char c out) (setf space nil)))))))
           (text (nlk:trimmed squeezed)))
      (if (> (length text) +name-chars+)
          (concatenate 'string (subseq text 0 +name-chars+) "...")
          text))))

(defun limit-of (query)
  "QUERY's limit, held to 1..+MAX-ROWS+."
  (max 1 (min +max-rows+ (or (table-query-limit query) 100))))

(defun row-of (device)
  "DEVICE, one CIM row, as the listing shows it."
  (list :name (or (clip (getf device :name)) "(unnamed)")
        :class (getf device :class)
        :status (getf device :status)
        :code (getf device :code)
        :manufacturer (clip (getf device :manufacturer))
        :device-id (getf device :device-id)
        :present (getf device :present)))

(defun problem-object (code)
  "CODE as the answer's problem: its number and its meaning."
  (when (integerp code)
    (nlk:make-json-object "code" code "meaning" (code-meaning code))))

(defun row-object (row)
  "ROW as the JSON object the answer carries."
  (nlk:make-json-object
   "name" (getf row :name)
   "class" (getf row :class)
   "status" (getf row :status)
   "problem" (problem-object (getf row :code))
   "manufacturer" (getf row :manufacturer)
   "device_id" (getf row :device-id)))

(defun class-tally (rows)
  "((CLASS . COUNT) ...) over ROWS, the largest first, then by name."
  (let ((tally '()))
    (dolist (row rows (stable-sort (sort tally #'string< :key #'car) #'> :key #'cdr))
      (let* ((key (or (getf row :class) "(no class)"))
             (cell (assoc key tally :test #'string=)))
        (if cell (incf (cdr cell)) (push (cons key 1) tally))))))

(defun table-notes (&key listed candidates rows present unhappy limit classes)
  "The lines under a listing: how much of the tree was read, and what needs attention."
  (append
   (list (format nil "~d of ~d matching device(s) listed, ~d present, ~d historical"
                 listed candidates present (- rows present)))
   (when (< listed candidates)
     (list (format nil "stopped at limit=~d; raise it or narrow the query" limit)))
   (when unhappy
     (list (format nil "~d device(s) need attention: ~{~a~^, ~}"
                   (length unhappy)
                   (loop for row in (subseq unhappy 0 (min 5 (length unhappy)))
                         collect (format nil "~a (~a)" (getf row :name) (code-meaning (getf row :code)))))))
   (unless (plusp listed)
     (list "nothing matched; drop the query or the class filter"))
   (when classes
     (list (format nil "largest classes: ~{~a: ~d~^, ~}"
                   (loop for (name . count) in (subseq classes 0 (min 5 (length classes)))
                         collect name collect count))))))

(defun answer-of (thunk)
  "THUNK's answer object as the JSON text a model reads; a refusal as the same
shape with ok false, so one bad question never ends a turn."
  (handler-case (nlk:encode-json-object (funcall thunk))
    (cim-refusal (problem)
      (nlk:encode-json-object
       (nlk:make-json-object "ok" nil
                              "error" (cim-refusal-detail problem)
                              "code" (cim-refusal-code problem))))))

;;; --- win_device_table -------------------------------------------------------------

(defun table-report (query)
  "The answer object win_device_table gives: the rows that match and the notes that frame them."
  (let* ((all (mapcar #'row-of (devices)))
         (rows (if (table-query-include-absent query)
                   all
                   (remove-if-not (lambda (row) (getf row :present)) all)))
         (present (count-if (lambda (row) (getf row :present)) rows))
         (needle (string-downcase (or (table-query-query query) "")))
         (class (string-downcase (or (table-query-device-class query) "")))
         (limit (limit-of query))
         (candidates (remove-if-not
                      (lambda (row)
                        (and (or (zerop (length needle))
                                 (search needle (string-downcase (or (getf row :name) "")))
                                 (search needle (string-downcase (or (getf row :device-id) ""))))
                             (or (zerop (length class))
                                 (string-equal class (or (getf row :class) "")))
                             (or (not (table-query-broken-only query))
                                 (broken-p (getf row :code)))))
                      rows))
         (listed (subseq candidates 0 (min limit (length candidates))))
         (unhappy (remove-if-not (lambda (row) (broken-p (getf row :code))) rows)))
    (nlk:make-json-object
     "ok" t
     "devices" (coerce (mapcar #'row-object listed) 'vector)
     "notes" (coerce (table-notes :listed (length listed) :candidates (length candidates)
                                   :rows (length rows) :present present :unhappy unhappy
                                   :limit limit :classes (class-tally rows))
                      'vector))))

(defun win-device-table (query)
  "Every PnP device on this Windows machine, with the unhappy ones called out."
  (answer-of (lambda () (table-report query))))

;;; --- win_device_detail ------------------------------------------------------------

(defun find-device (requested present)
  "The device REQUESTED names among PRESENT: its DeviceID exactly, else the first
whose name or id carries the text."
  (let ((lower (string-downcase requested)))
    (or (find requested present :key (lambda (device) (getf device :device-id)) :test #'string-equal)
        (find-if (lambda (device)
                    (or (search lower (string-downcase (or (getf device :name) "")))
                        (search lower (string-downcase (or (getf device :device-id) "")))))
                  present))))

(defun near-names (requested present)
  "The names of devices whose first word is in REQUESTED's, so a miss suggests something."
  (let ((first-word (first (remove "" (nlk:lines requested) :test #'equal))))
    (when (and first-word (plusp (length first-word)))
      (loop for device in present
            for name = (getf device :name)
            when (and (stringp name) (search (string-downcase first-word) (string-downcase name)))
              collect name into names
            finally (return (mapcar #'clip (subseq names 0 (min 5 (length names)))))))))

(defun signed-line (driver)
  "Whether DRIVER is signed, in words."
  (if (getf driver :signed) "signed" "not signed"))

(defun detail-of (requested)
  "The device REQUESTED matches, the driver behind it, and the notes that read it.
 => (values DEVICE DRIVER NOTES), the device and driver plists; a refusal when
 nothing present matches."
  (let* ((present (remove-if-not (lambda (device) (getf device :present)) (devices)))
         (device (or (find-device requested present)
                     (refuse "not_found"
                             "no present device matches ~a~@[; near: ~{~a~^, ~}~]. win_device_table lists every present device."
                             requested (near-names requested present))))
         (notes '())
         (table (handler-case (drivers)
                  (cim-refusal ()
                    (push "the driver table could not be read; the driver section is not shown" notes)
                    '())))
         (driver (find (getf device :device-id) table
                       :key (lambda (entry) (getf entry :device-id)) :test #'string-equal)))
    (let ((code (getf device :code)))
      (cond ((eql code 28)
             (push "no driver is installed: Windows has the device but nothing to run it, and the empty driver section below is the same fact" notes))
            ((eql code 22)
             (push "this device is switched off on purpose; nothing is broken" notes))))
    (when (and driver (not (getf driver :signed)))
      (push "the driver is not signed" notes))
    (values device driver (nreverse notes))))

(defun driver-object (driver)
  "DRIVER as the answer's JSON object, or NIL when the device has none."
  (when driver
    (let ((date (getf driver :date)))
      (nlk:make-json-object
       "name" (or (clip (getf driver :name)) "(unnamed)")
       "class" (getf driver :class)
       "version" (getf driver :version)
       "date" (and (stringp date) (>= (length date) 10) (subseq date 0 10))
       "provider" (clip (getf driver :provider))
       "signer" (clip (getf driver :signer))
       "inf" (getf driver :inf)
       "signed" (getf driver :signed)))))

(defun detail-report (requested)
  "The answer object win_device_detail gives."
  (multiple-value-bind (device driver notes) (detail-of requested)
    (let ((row (row-of device)))
      (nlk:make-json-object
       "ok" t
       "name" (getf row :name)
       "class" (getf row :class)
       "status" (getf row :status)
       "problem" (problem-object (getf row :code))
       "manufacturer" (getf row :manufacturer)
       "device_id" (getf row :device-id)
       "driver" (driver-object driver)
       "notes" (coerce notes 'vector)))))

(defun win-device-detail (query)
  "One device in full: the driver behind it, or the absence of one."
  (answer-of (lambda () (detail-report (detail-query-name query)))))

;;; --- /windows-devices -------------------------------------------------------------

(defun summary-text (rows)
  "The command's text: what is here and what needs attention."
  (let* ((present (remove-if-not (lambda (row) (getf row :present)) rows))
         (unhappy (remove-if-not (lambda (row) (broken-p (getf row :code))) present))
         (tally (class-tally (mapcar #'row-of present))))
    (with-output-to-string (out)
      (format out "~d present device(s), ~d historical, ~d needing attention:~%"
              (length present) (- (length rows) (length present)) (length unhappy))
      (if unhappy
          (dolist (row (subseq unhappy 0 (min 12 (length unhappy))))
            (format out "  ~a (~a) [~a]~%"
                    (or (clip (getf row :name)) "(unnamed)")
                    (code-meaning (getf row :code)) (getf row :device-id)))
          (format out "  nothing: every present device reports working.~%"))
      (when tally
        (format out "largest classes: ~{~a: ~d~^, ~}~%"
                (loop for (name . count) in (subseq tally 0 (min 5 (length tally)))
                      collect name collect count)))
      (format out "~%Detail one with /windows-devices NAME, or ask for win_device_detail."))))

(defun summary-dialog (rows)
  "The panel /windows-devices shows typed alone: one device asked for, and each
unhappy one a row that opens it."
  (let* ((present (remove-if-not (lambda (row) (getf row :present)) rows))
         (unhappy (remove-if-not (lambda (row) (broken-p (getf row :code))) present)))
    (nle:list-dialog
     :title "Windows devices"
     :summary (format nil "~d present, ~d needing attention" (length present) (length unhappy))
     :rows (append
            (list (nle:ask-row "Detail one device" "Device name or id" "/windows-devices"
                               :placeholder "camera" :verb "show"))
            (loop for row in (subseq unhappy 0 (min 12 (length unhappy)))
                  collect (nle:run-row (format nil "~a -- ~a" (getf row :name) (code-meaning (getf row :code)))
                                       (format nil "/windows-devices ~a" (getf row :device-id))))))))

(defun detail-text (requested)
  "One device as lines: the command's answer for a name or an id."
  (handler-case
      (multiple-value-bind (device driver notes) (detail-of requested)
        (let ((row (row-of device)))
          (with-output-to-string (out)
            (format out "~a~%  class ~a, status ~a~@[, ~a~]"
                    (getf row :name) (getf row :class) (getf row :status)
                    (let ((problem (problem-object (getf row :code))))
                      (when problem (format nil "problem ~d: ~a" (getf row :code) (code-meaning (getf row :code))))))
            (format out "~%  manufacturer ~a~%  id ~a~%"
                    (or (getf row :manufacturer) "unknown") (getf row :device-id))
            (if driver
                (format out "  driver ~a ~a (~a, ~a), ~a, ~a~%"
                        (or (getf driver :name) "(unnamed)") (or (getf driver :version) "unknown version")
                        (let ((date (getf driver :date)))
                          (if (and (stringp date) (>= (length date) 10)) (subseq date 0 10) "no date"))
                        (or (getf driver :provider) "unknown provider")
                        (signed-line driver) (or (getf driver :inf) "no INF"))
                (format out "  driver: none reported~%"))
            (dolist (note notes) (format out "  note: ~a~%" note)))))
    (cim-refusal (problem) (format nil "ERROR: ~a" (cim-refusal-detail problem)))))

(defun run-command (args session-id)
  "The /windows-devices command: with no argument, what is here and what needs
attention, and a panel to open one; with a name or an id, that device in full."
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (if (plusp (length argument))
        (detail-text argument)
        (handler-case
            (let ((rows (devices)))
              (values (summary-text rows) (summary-dialog rows)))
          (cim-refusal (problem)
            (values (format nil "ERROR: ~a" (cim-refusal-detail problem)) nil))))))
