;;;; cell-test.lisp --- the device tools against a stubbed CIM.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here spawns a process: RUN-POWERSHELL is stubbed to answer a
;;;; fixture shaped exactly as the two scripts answer, so the parsing, the
;;;; filtering, the notes, the refusals and the command are all exercised
;;;; without a Windows machine and without PowerShell. The fixture's device ids
;;;; are abbreviated -- the separator Windows uses is data the cell carries
;;;; through, never parses.

(in-package #:nodecode.test)

(define-test-slice "windows-devices" "WINDOWS-DEVICES-CELL-"
  :start nodecode-windows-devices:start-cell)

;;; --- the fixture -------------------------------------------------------------

(defvar *scripts* '()
  "Every script a stubbed RUN-POWERSHELL was handed this test, newest first.")

(defun cim-object (&rest pairs)
  "One CIM row as the JSON object a script's ConvertTo-Json answer carries."
  (apply #'nlk:make-json-object pairs))

(defparameter +camera-id+ "USB-VID-1234-PID-5678")
(defparameter +net-id+ "PCI-VEN-8086-DEV-02F0")

(defun fixture-devices ()
  "The tree a Windows host with one broken device answers: a working adapter, a
camera with no driver, a button switched off on purpose, and a modem that is
not plugged in any more."
  (coerce
   (list (cim-object "name" "Intel(R) Wi-Fi 6 AX201" "class" "Net" "status" "OK" "code" 0
                      "manufacturer" "Intel" "device_id" +net-id+ "present" t)
         (cim-object "name" "USB Camera" "class" "Camera" "status" "Error" "code" 28
                      "manufacturer" "Contoso" "device_id" +camera-id+ "present" t)
         (cim-object "name" "HID Button" "class" "HIDClass" "status" "OK" "code" 22
                      "manufacturer" "Contoso" "device_id" "HID-0001" "present" t)
         (cim-object "name" "Old Modem" "class" "Modem" "status" "OK" "code" 0
                      "manufacturer" "Contoso" "device_id" "SER-0002" "present" nil))
   'vector))

(defun fixture-drivers ()
  "The driver table behind the same tree: a signed adapter driver, and a camera
driver that is not signed."
  (coerce
   (list (cim-object "device_id" +net-id+ "device_name" "Intel(R) Wi-Fi 6 AX201"
                      "class" "Net" "version" "22.150.1.1" "date" "2024-03-05T00:00:00.0000000"
                      "signer" "Microsoft Windows" "inf" "oem12.inf" "signed" t
                      "provider" "Intel")
         (cim-object "device_id" +camera-id+ "device_name" "USB Camera"
                      "class" "Camera" "version" "1.0.4.2" "date" "2019-11-02T00:00:00.0000000"
                      "signer" "" "inf" "oem44.inf" "signed" nil "provider" "Contoso"))
   'vector))

(defmacro with-cim ((answer) &body body)
  "BODY with RUN-POWERSHELL stubbed: ANSWER, a form with SCRIPT bound, is its text."
  `(let ((*scripts* '()))
     (with-stubbed-fdefinition (nodecode-windows-devices::run-powershell (script)
                                  (push script *scripts*)
                                  ,answer)
       ,@body)))

(defmacro with-windows ((&key (devices '(fixture-devices)) (drivers '(fixture-drivers))) &body body)
  "BODY on a Windows image whose CIM answers the fixtures, the cell started."
  `(let ((nodecode-windows-devices::*platform* :windows))
     (with-cim ((if (search "Win32_PnPSignedDriver" script)
                    (nlk:encode-json-object ,drivers)
                    (nlk:encode-json-object ,devices)))
       ,@body)))

(defmacro with-device-tools (&body body)
  "BODY with the cell started on its defaults and a Windows CIM behind it."
  `(with-windows ()
     (with-cell-stop ((windows-devices-start))
       ,@body)))

;;; --- reading an answer -------------------------------------------------------

(defun table-answer (&rest pairs)
  "WIN_DEVICE_TABLE's answer for the TABLE-QUERY the PAIRS make, decoded."
  (nlk:decode-json
   (nodecode-windows-devices:win-device-table
    (nodecode-windows-devices:decode-table-query (apply #'nlk:make-json-object pairs)))))

(defun detail-answer (&rest pairs)
  "WIN_DEVICE_DETAIL's answer for the DETAIL-QUERY the PAIRS make, decoded."
  (nlk:decode-json
   (nodecode-windows-devices:win-device-detail
    (nodecode-windows-devices:decode-detail-query (apply #'nlk:make-json-object pairs)))))

(defun rows-of (answer)
  (coerce (nlk:json-value answer :array "devices") 'list))

(defun note-text (answer)
  (format nil "~{~a~%~}" (coerce (nlk:json-value answer :array "notes") 'list)))

;;; --- the cell's own life -----------------------------------------------------

(define-cell-lifecycle-tests "windows-devices"
  (:help :windows-devices)
  (:command "windows-devices")
  (:running
   (is (= 120 (nodecode-windows-devices::setting :timeout)) "the default bound)")
   (is (equal "" (nodecode-windows-devices::setting :powershell)) "no executable named"))
  (:refused ("timeout" 1) ("timeout" "sixty")))

;;; --- win_device_table --------------------------------------------------------

(deftest windows-devices-cell-the-table-lists-what-is-here-and-what-needs-attention ()
  (with-device-tools
    (let* ((answer (table-answer))
           (rows (rows-of answer)))
      (is (eq t (nlk:json-value answer :boolean "ok")) "the answer says ok")
      (is (= 3 (length rows)) "the unplugged modem is left out")
      (is (search "3 of 3 matching" (note-text answer)) "and the note counts the tree")
      (is (search "USB Camera (no driver is installed for it)" (note-text answer)) "the unhappy device is named with its problem")
      (is (null (search "HID Button" (note-text answer))) "a disabled one is not an alarm")
      (is (search "largest classes: Camera: 1, HIDClass: 1, Net: 1" (note-text answer)) "and the largest classes are counted"))))

(deftest windows-devices-cell-the-table-filters-by-text-class-and-breakage ()
  (with-device-tools
    (is (= 1 (length (rows-of (table-answer "broken_only" t)))) "one device needs help")
    (let ((row (first (rows-of (table-answer "broken_only" t)))))
      (is (equal "USB Camera" (nlk:json-value row :string "name")))
      (is (= 28 (nlk:json-value (nlk:json-value row :object "problem") :integer "code")))
      (is (equal "no driver is installed for it"
                 (nlk:json-value (nlk:json-value row :object "problem") :string "meaning"))))
    (is (= 1 (length (rows-of (table-answer "class" "net")))) "a class filter is case-blind")
    (is (= 1 (length (rows-of (table-answer "query" "camera")))) "a name substring matches")
    (is (= 1 (length (rows-of (table-answer "query" "pci-ven")))) "and so does an id's")
    (is (= 4 (length (rows-of (table-answer "include_absent" t)))) "historical devices on request")
    (let ((one (table-answer "limit" 1)))
      (is (= 1 (length (rows-of one))) "one row when asked")
      (is (search "stopped at limit=1" (note-text one)) "and the note says why"))))

(deftest windows-devices-cell-an-answer-of-no-devices-says-so ()
  (with-windows (:devices (coerce '() 'vector))
    (with-cell-stop ((windows-devices-start))
      (let ((answer (table-answer)))
        (is (eq t (nlk:json-value answer :boolean "ok")) "an empty tree is not a failure")
        (is (null (rows-of answer)) "no rows")
        (is (search "nothing matched" (note-text answer)) "and the note says how to widen")))))

;;; --- win_device_detail -------------------------------------------------------

(deftest windows-devices-cell-the-detail-names-the-driver-or-its-absence ()
  (with-device-tools
    (let* ((answer (detail-answer "name" +camera-id+))
           (driver (nlk:json-value answer :object "driver")))
      (is (eq t (nlk:json-value answer :boolean "ok")) "ok")
      (is (equal "USB Camera" (nlk:json-value answer :string "name")) "the device")
      (is (= 28 (nlk:json-value (nlk:json-value answer :object "problem") :integer "code")))
      (is (equal "1.0.4.2" (nlk:json-value driver :string "version")) "the driver's version")
      (is (equal "2019-11-02" (nlk:json-value driver :string "date")) "the date, to the day")
      (is (null (nlk:json-value driver :boolean "signed")) "and that it is not signed")
      (let ((notes (note-text answer)))
        (is (search "no driver is installed" notes) "code 28 is spelled out")
        (is (search "the driver is not signed" notes) "and the signature is called out")))
    (let ((answer (detail-answer "name" "wi-fi")))
      (is (equal +net-id+ (nlk:json-value answer :string "device_id")) "a name substring matches")
      (is (eq t (nlk:json-value (nlk:json-value answer :object "driver") :boolean "signed")) "a signed driver reads signed"))
    (let ((answer (detail-answer "name" "HID Button")))
      (is (search "switched off on purpose" (note-text answer)) "code 22 is not an alarm")
      (is (null (nlk:json-value answer :object "driver")) "and has no driver row"))))

(deftest windows-devices-cell-a-device-nothing-matches-refuses-with-a-near-name ()
  (with-device-tools
    (let ((answer (detail-answer "name" "camera lamp")))
      (is (null (nlk:json-value answer :boolean "ok")) "the answer says it failed")
      (is (equal "not_found" (nlk:json-value answer :string "code")) "with a code")
      (is (search "no present device matches" (nlk:json-value answer :string "error")) "and the words a model repairs from"))
    (let ((answer (detail-answer "name" "camera")))
      (is (eq t (nlk:json-value answer :boolean "ok")) "the first word still finds it"))))

(deftest windows-devices-cell-a-call-without-its-device-is-refused ()
  (is (signals-error nlk:contract-refusal
        (nodecode-windows-devices:decode-detail-query (nlk:make-json-object))) "name is required")
  (is (signals-error nlk:contract-refusal
        (nodecode-windows-devices:decode-detail-query (nlk:make-json-object "name" ""))) "and an empty one is not a name")
  (with-device-tools
    (is (eq t (nlk:json-value (table-answer "limit" 5000) :boolean "ok")) "a limit past the ceiling is held, not refused")))

;;; --- a platform with no CIM ------------------------------------------------

(deftest windows-devices-cell-off-windows-the-tools-say-so-before-spawning ()
  (let ((nodecode-windows-devices::*platform* :linux)
        (spawned '()))
    (with-stubbed-fdefinition (nlk:run-bounded (argv &rest keys)
                               (push (cons argv keys) spawned)
                               (values "" "" 0))
      (with-cell-stop ((windows-devices-start))
        (let ((answer (table-answer)))
          (is (null (nlk:json-value answer :boolean "ok")) "the answer says it failed")
          (is (equal "windows_only" (nlk:json-value answer :string "code")) "with a code")
          (is (search "not Windows" (nlk:json-value answer :string "error")) "and why"))
        (is (null spawned) "nothing was spawned")))))

;;; --- /windows-devices -------------------------------------------------------

(deftest windows-devices-cell-the-command-reads-the-tree-and-opens-one ()
  (with-device-tools
    (let ((text (nodecode-windows-devices:run-command "" "s1")))
      (is (search "3 present device(s), 1 historical, 1 needing attention" text) "the counts")
      (is (search "USB Camera (no driver is installed for it)" text) "and what needs attention")
      (is (search "/windows-devices NAME" text) "and how to go on"))
    (let* ((dialog (nth-value 1 (nodecode-windows-devices:run-command "" "s1")))
           (rows (getf dialog :rows)))
      (is (= 2 (length rows)) "one row asks for a device, one opens the unhappy one")
      (is (equal "/windows-devices" (getf (getf (first rows) :ask) :line)) "the ask runs the command"))
    (let ((text (nodecode-windows-devices:run-command +camera-id+ "s1")))
      (is (search "problem 28: no driver is installed for it" text) "an argument details one")
      (is (search "driver USB Camera 1.0.4.2" text) "with its driver"))
    (is (search "ERROR:" (nodecode-windows-devices:run-command "nothing like this" "s1")) "and a miss is one line, not a signal")))

;;; --- the parser -------------------------------------------------------------

(deftest windows-devices-cell-one-object-and-a-byte-order-mark-read-as-a-list ()
  (let ((one (nodecode-windows-devices:cim-list
              (format nil "~c{\"name\": \"Solo\"}" (code-char #xFEFF)) "devices")))
    (is (= 1 (length one)) "one object is a list of one")
    (is (equal "Solo" (nlk:json-value (first one) :string "name")) "and the mark before it is dropped"))
  (is (null (nodecode-windows-devices:cim-list "  " "devices")) "no device at all is no rows")
  (is (signals-error nodecode-windows-devices:cim-refusal
        (nodecode-windows-devices:cim-list "not-json" "devices")) "and text that is not JSON is a refusal"))
