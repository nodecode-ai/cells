;;;; cell-test.lisp --- the windows-apps cell against a stubbed registry tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs Windows, a registry or reg.exe: every query goes through
;;;; WINDOWS-APPS::*REG-RUNNER*, a stub that records the command line it was
;;;; handed and answers what a Windows machine would have printed. What is
;;;; under test is the part this cell owns: the four keys it asks, the shape it
;;;; reads out of reg's own output, the rows and notes it makes of it, the
;;;; reading of an uninstall string, and the words a machine without the tool
;;;; gets.

(in-package #:nodecode.test)

(define-test-slice "windows-apps" "WINDOWS-APPS-CELL-"
  :start nodecode-windows-apps:start-cell)

(define-cell-lifecycle-tests "windows-apps"
  (:help :windows-apps)
  (:command "windows-apps")
  (:running
   (is (equal '("reg") (windows-apps::setting :reg)) "reg.exe on PATH is the default")
   (is (nle::find-tool "win_app_table") "the table tool is registered")
   (is (nle::find-tool "win_app_detail") "and the detail one"))
  (:stopped
   (is (null (nle::find-tool "win_app_table")) "and stop takes every one back"))
  (:refused ("timeout_seconds" 0))
  (:idle windows-apps:windows-apps-error (windows-apps:win-app-table)))

;;; --- what a Windows machine prints ------------------------------------------------

(defparameter +machine-64+
  "HKEY_LOCAL_MACHINE\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall

HKEY_LOCAL_MACHINE\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\{1A2B}
    DisplayName    REG_SZ    OBS Studio
    DisplayVersion    REG_SZ    31.1.2
    Publisher    REG_SZ    OBS Project
    EstimatedSize    REG_DWORD    0x2d800
    InstallDate    REG_SZ    20250304
    InstallLocation    REG_SZ    C:\\Program Files\\obs-studio
    UninstallString    REG_SZ    \"C:\\Program Files\\obs-studio\\uninstall.exe\"
    WindowsInstaller    REG_DWORD    0x0

HKEY_LOCAL_MACHINE\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\{2C3D}
    DisplayName    REG_SZ    Windows Update Helper
    DisplayVersion    REG_SZ    14.38.33130
    Publisher    REG_SZ    Microsoft Corporation
    EstimatedSize    REG_DWORD    0x4000
    SystemComponent    REG_DWORD    0x1
")

(defparameter +machine-32+
  "HKEY_LOCAL_MACHINE\\Software\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall

HKEY_LOCAL_MACHINE\\Software\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\{3E4F}
    DisplayName    REG_SZ    Python 3.13
    DisplayVersion    REG_SZ    3.13.1
    Publisher    REG_SZ    Python Software Foundation
    EstimatedSize    REG_DWORD    0x71c8
    InstallDate    REG_SZ    20250102
    UninstallString    REG_SZ    MsiExec.exe /X{3E4F}
    WindowsInstaller    REG_DWORD    0x1
")

(defparameter +user-64+
  "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall

HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\{4A5B}
    DisplayName    REG_SZ    Python 3.13
    DisplayVersion    REG_SZ    3.13.2
    Publisher    REG_SZ    Python Software Foundation
    InstallDate    REG_SZ    3/4/2025
    InstallLocation    REG_SZ    C:\\Users\\operator\\AppData\\Local\\Programs\\Python\\Python313
    UninstallString    REG_SZ    C:\\Users\\operator\\AppData\\Local\\Programs\\Python\\Python313\\uninstall.exe
    NoModify    REG_DWORD    0x1
    NoRepair    REG_DWORD    0x1
")

(defun default-answer (key)
  "What this made-up machine answers for the key named in the command line."
  (cond
    ((search "WOW6432Node" key)
     (if (search "HKCU" key)
         (values "" "ERROR: The system was unable to find the specified registry key or value." 1)
         (values +machine-32+ "" 0)))
    ((search "HKCU" key) (values +user-64+ "" 0))
    (t (values +machine-64+ "" 0))))

;;; --- the stub ----------------------------------------------------------------

(defvar *calls* nil
  "The command lines the stub was handed, newest first.")

(defmacro with-registry ((&key (answer ''default-answer)) &body body)
  "BODY with the cell started and every registry query answered by ANSWER, a
function of the key in the command line. *CALLS* records each command line."
  `(let ((*calls* nil)
         (windows-apps::*reg-runner*
           (lambda (argv)
             (push argv *calls*)
             (funcall ,answer (third argv)))))
     (with-cell-stop ((windows-apps-start))
       ,@body)))

(defun asked-keys ()
  (mapcar #'third (reverse *calls*)))

(defun tool-text (name arguments)
  "NAME called the way the model calls it, as the text it reads back."
  (nle::message-content (execute-wire-call "win-call" name arguments)))

;;; --- the sweep ------------------------------------------------------------

(deftest windows-apps-cell-the-four-keys-are-asked-in-order ()
  (with-registry ()
    (tool-text "win_app_table" "{}")
    (is (= 4 (length *calls*)) "four keys, four queries")
    (is (equal '("HKLM\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall"
                 "HKLM\\Software\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall"
                 "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall"
                 "HKCU\\Software\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall")
               (asked-keys)) "the machine's 64-bit and 32-bit views, then the user's")
    (is (equal '("reg" "query" "HKLM\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall" "/s")
               (car (last *calls*))) "each one asked of reg, with /s for the whole tree")))

(deftest windows-apps-cell-a-table-carries-every-field ()
  (with-registry ()
    (let ((text (tool-text "win_app_table" "{}")))
      (is (search "OBS Studio | 31.1.2 | OBS Project | 182.0 MB | 2025-03-04 | machine-64" text) "name, version, publisher, size in MB, install date and view")
      (is (search "3 named programs across 4 registry entries" text) "the notes count what was read")
      (is (search "1 hidden from Apps & Features" text) "and say what was folded away")
      (is (search "Windows Update Helper | 14.38.33130 | Microsoft Corporation | 16.0 MB | - | machine-64 | hidden from Apps & Features"
                  (tool-text "win_app_table" "{\"include_system_components\":true}")) "a SystemComponent entry is shown when asked for"))))

(deftest windows-apps-cell-a-dual-install-is-one-row ()
  (with-registry ()
    (let ((text (tool-text "win_app_table" "{}")))
      (is (= 1 (count "Python 3.13" (uiop:split-string text :separator '(#\Newline))
                      :key (lambda (line) (if (search "Python 3.13 |" line) "Python 3.13" ""))
                      :test #'string=)) "one row for the name, not two")
      (is (search "Python 3.13 | 3.13.1 | Python Software Foundation | 28.4 MB | 2025-01-02 | machine-32, user-64" text) "both views named on it, the localised date read")
      (is (search "1 program(s) registered in more than one view" text) "and a note says so"))))

(deftest windows-apps-cell-a-query-narrows-and-a-limit-stops ()
  (with-registry ()
    (let ((text (tool-text "win_app_table" "{\"query\":\"obs\"}")))
      (is (search "1 of 1 matching programs" text) "the query keeps the publisher match")
      (is (not (search "Python 3.13" text)) "and drops the rest"))
    (let ((text (tool-text "win_app_table" "{\"limit\":1}")))
      (is (search "stopped at limit=1" text) "a row cap says which one it stopped at"))
    (is (search "lte is not one of the views" (tool-text "win_app_table" "{\"view\":\"lte\"}")) "an unknown view filters nothing and says so")
    (is (search "machine-32: 1 entries" (tool-text "win_app_table" "{\"view\":\"machine-32\"}")) "a known view keeps its own entries")))

;;; --- one program in full --------------------------------------------------------

(deftest windows-apps-cell-detail-reads-the-uninstall-string ()
  (with-registry ()
    (let ((text (tool-text "win_app_detail" "{\"name\":\"obs studio\"}")))
      (is (search "uninstaller: setup executable - the publisher's own uninstaller program" text) "a quoted setup program is classified, not run")
      (is (search "uninstall string: \"C:\\Program Files\\obs-studio\\uninstall.exe\"" text) "and quoted back as the publisher wrote it")
      (is (search "install location: C:\\Program Files\\obs-studio (not on disk)" text) "the recorded folder is checked against disk"))
    (let ((text (tool-text "win_app_detail" "{\"name\":\"Python 3.13\"}")))
      (is (search "uninstaller: msiexec" text) "an MSI product code says removal goes through Windows Installer")
      (is (search "registered in 2 views (machine-32, user-64)" text) "both occurrences are shown")
      (is (search "the publisher locked modification and repair" text) "NoModify and NoRepair are read"))))

(deftest windows-apps-cell-detail-refuses-a-name-it-does-not-know ()
  (with-registry ()
    (let ((text (tool-text "win_app_detail" "{\"name\":\"obs studio pro\"}")))
      (is (search "no installed program named obs studio pro is registered" text))
      (is (search "did you mean OBS Studio" text) "and names what it has instead"))))

(deftest windows-apps-cell-a-machine-without-the-tool-says-so ()
  (with-registry (:answer (lambda (key) (declare (ignore key))
                            (values nil "reg could not be run: no such program. The installed-software registry is Windows only; set the windows-apps section's reg to the registry tool's argv, e.g. [\"reg\"]." 127)))
    (let ((text (tool-text "win_app_table" "{}")))
      (is (search "could not be read" text) "the answer says nothing was read")
      (is (search "Windows only" text) "and where the tool comes from")
      (is (search "ERROR" text) "as a failed call, not a table of nothing"))))

(deftest windows-apps-cell-the-command-prints-the-inventory ()
  (with-registry ()
    (let ((text (windows-apps::run-slash "python" nil)))
      (is (search "Python 3.13" text) "the words after the command are the query")
      (is (not (search "OBS Studio" text)) "and everything else stays out"))))
