;;;; support.lisp --- the slice runner, a fixture machine, and the tool answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Startup tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a STARTUP-CELL-
;;;; name prefix; RUN-STARTUP-TESTS runs exactly that slice.
;;;;
;;;; The cell reads three kinds of source: a probe (the one seam the tools go
;;;; through), the registry query behind it, and folders on disk. A test binds
;;;; the first two to a fixture and plants the third under the scratch home, so
;;;; nothing here reads this machine's registry, its Startup folders or its
;;;; launchd jobs.

(in-package #:nodecode.test)

(define-test-slice "startup" "STARTUP-CELL-" :start nodecode-startup:start-cell)

(defun startup-entry (name command source scope &key state changed file note)
  "One autostart entry as PROBE answers them."
  (list :name name
        :command command
        :source source
        :scope scope
        :approved (and state (list :state state :changed changed))
        :file file
        :note note))

(defun startup-probe (entries &key orphans)
  "The probe seam, answering ENTRIES and ORPHANS."
  (lambda () (values entries orphans)))

(defmacro with-probe ((entries &key orphans) &body body)
  "BODY with the machine's reading replaced by ENTRIES and ORPHANS."
  `(let ((nodecode-startup::*probe* (startup-probe ,entries :orphans ,orphans)))
     ,@body))

(defun startup-entries ()
  "Four entries: one left alone, one with a record, one switched off, one in a
Startup folder."
  (list (startup-entry "OneDrive"
                        "\"C:/Users/someone/AppData/Local/Microsoft/OneDrive/OneDrive.exe\" /background"
                        "registry-hkcu-run" "user")
        (startup-entry "SecurityHealth" "C:/Windows/system32/SecurityHealthSystray.exe"
                       "registry-hklm-run" "machine"
                       :state "enabled" :changed "2026-01-02T03:04:05Z")
        (startup-entry "CC Switch" "C:/Program Files/CC Switch/cc-switch.exe"
                       "registry-hkcu-run" "user"
                       :state "disabled" :changed "2026-02-03T04:05:06Z")
        (startup-entry "spare.lnk" "C:/Users/someone/AppData/Roaming/Microsoft/Windows/Start Menu/Programs/Startup/spare.lnk"
                       "folder-user" "user")))

(defun startup-raw (name &rest pairs)
  "NAME called through the wire, as a provider sends it: the answer's text."
  (let ((call (nle::execute-tool-call
               (wire-tool-call "call-1" name (apply #'nlk:make-json-object pairs))
               (compiled-context '()))))
    (princ-to-string (nle::message-content call))))

(defun startup-answer (name &rest pairs)
  "NAME's answer, decoded: a JSON object."
  (nlk:decode-json (apply #'startup-raw name pairs)))

(defun startup-names (payload)
  "The entry names PAYLOAD lists, in order."
  (loop for row across (nlk:json-array payload "startup")
        collect (nlk:json-value row :string "name")))

(defun startup-row (payload name)
  "The row of NAME in PAYLOAD, or NIL."
  (loop for row across (nlk:json-array payload "startup")
        when (equal name (nlk:json-value row :string "name"))
          return row))

(defun startup-note (payload needle)
  "The first note of PAYLOAD naming NEEDLE, or NIL."
  (find-if (lambda (note) (search needle note))
           (coerce (nlk:json-array payload "notes") 'list)))

(defvar *fixture-count* 0
  "How many fixture folders this run has made, so two never share one.")

(defun startup-fixture-directory (&rest parts)
  "A fresh folder under the scratch home, the last of PARTS made and named."
  (let* ((root (nlk:home (format nil "startup-test/~d-~d/" (get-universal-time)
                                  (incf *fixture-count*))))
         (leaf (reduce (lambda (base part) (merge-pathnames part base))
                       parts :initial-value (uiop:ensure-directory-pathname root))))
    (ensure-directories-exist (merge-pathnames "keep" leaf))
    (namestring (uiop:ensure-directory-pathname leaf))))

(defun startup-plant (directory name text)
  "NAME in DIRECTORY holding TEXT; answers its namestring."
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname directory))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-string text out))
    (namestring path)))

(defun startup-on-disk (paths)
  "A *FILE-EXISTS-P* answering true only for PATHS."
  (lambda (path) (and (member path paths :test #'string=) t)))

(defun startup-approval-blob (state universal)
  "A StartupApproved record as reg query prints one: STATE's byte, three zero
bytes, then UNIVERSAL as a little-endian FILETIME, in hex."
  (let* ((filetime (* (+ (- universal 2208988800) 11644473600) 10000000))
         (octets (append (list state 0 0 0)
                         (loop for index from 0 below 8
                               collect (ldb (byte 8 (* 8 index)) filetime)))))
    (format nil "~{~2,'0x~}" octets)))

(defun startup-windows-folder (home)
  "The user Startup folder of the fixture home HOME, as a namestring."
  (namestring (merge-pathnames "AppData/Roaming/Microsoft/Windows/Start Menu/Programs/Startup/"
                                (uiop:ensure-directory-pathname home))))

(defun startup-windows-reg-query ()
  "A *REG-QUERY* answering two Run values and two approval records."
  (startup-reg-query
   (list (cons "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Run"
               (list "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Run"
                     "    OneDrive    REG_SZ    C:/OneDrive.exe /background"
                     "    CC Switch    REG_SZ    C:/Program Files/CC Switch/cc-switch.exe"))
         (cons "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\StartupApproved\\Run"
               (list "    CC Switch    REG_BINARY    060000000000000000000000"
                     "    gone.exe    REG_BINARY    010000000000000000000000")))))

(defun startup-reg-query (lines)
  "A *REG-QUERY* answering LINES for each key, and nothing for any other.
LINES is (KEY . OUTPUT-LINES) pairs."
  (lambda (key)
    (cdr (assoc key lines :test #'string-equal))))
