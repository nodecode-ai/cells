;;;; cell-test.lisp --- the orcaslicer cell against a stubbed slicer and printer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No OrcaSlicer runs and no printer is dialled: RUN-SLICER is a function
;;;; that writes the G-code a real slice would, and NLK:HTTP answers by URL.
;;;; Each tool is called the way a model's call reaches it -- through the real
;;;; dispatch, contract and all -- so the schema a provider is shown and the
;;;; body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "orcaslicer" "ORCASLICER-CELL-")

(defun orcaslicer-config (&rest pairs)
  "A whole shared config holding the `orcaslicer' section PAIRS make, with a
slicer that exists so discovery never reaches this machine's own install."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "slicer" section (orcaslicer-fixture "bin/orca-slicer" ""))
    (nlk:json-object "orcaslicer" section)))

(defun orcaslicer-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-orcaslicer:start-cell (apply #'orcaslicer-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defparameter +help-text+
  (format nil "OrcaSlicer 2.3.0~%Usage: orca-slicer [options] [file]~%  --slice N~%  --outputdir DIR~%  --export-3mf NAME~%  --datadir DIR~%~%")
  "What a stock OrcaSlicer answers to --help.")

(defun fake-gcode ()
  "The G-code a stubbed slice writes: its comments are what orca_job reports."
  (format nil "; total layer number: 42~%; model printing time: 1h 12m; total estimated time: 1h 15m~%; filament used [mm] = 1234.5~%; nozzle_diameter = 0.4~%;LAYER_CHANGE~%;LAYER_CHANGE~%G1 X0 Y0~%"))

(defun orcaslicer-fixture (relative text)
  "RELATIVE under this test's own home, written with TEXT. => its namestring."
  (let ((path (merge-pathnames relative (nlk:home "fixtures/"))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string text out))
    (namestring path)))

(defun orcaslicer-presets ()
  "Three presets under this test's own home, and the root that holds them:
a machine, a process that names it as compatible, and a filament."
  (orcaslicer-fixture "profiles/machine/Voron 0.4 nozzle.json"
                      "{\"type\":\"machine\",\"name\":\"Voron 0.4 nozzle\",\"nozzle_diameter\":[\"0.4\"]}")
  (orcaslicer-fixture "profiles/process/0.20mm Standard.json"
                      "{\"type\":\"process\",\"name\":\"0.20mm Standard\",\"layer_height\":\"0.2\",\"compatible_printers\":[\"Voron 0.4 nozzle\"]}")
  (orcaslicer-fixture "profiles/filament/PLA Basic.json"
                      "{\"type\":\"filament\",\"name\":\"PLA Basic\",\"filament_type\":[\"PLA\"]}")
  (namestring (merge-pathnames "profiles/" (nlk:home "fixtures/"))))

(defun orcaslicer-model ()
  "A raw model to slice."
  (orcaslicer-fixture "model.stl" "solid test"))

(defmacro with-fake-slicer ((&key (status 0) (gcode '(fake-gcode))) &body body)
  "BODY with the slicer a function that writes GCODE into --outputdir and
answers STATUS, as a stock OrcaSlicer would."
  `(with-stubbed-fdefinition (nodecode-orcaslicer::run-slicer (argv &key timeout)
                              (declare (ignore timeout))
                              (let ((directory (and (find "--outputdir" argv :test #'string=)
                                                    (nth (1+ (position "--outputdir" argv :test #'string=))
                                                         argv))))
                                (when (and ,gcode directory)
                                  (let ((path (merge-pathnames "sliced.gcode"
                                                                (uiop:ensure-directory-pathname directory))))
                                    (ensure-directories-exist path)
                                    (with-open-file (out path :direction :output
                                                              :if-exists :supersede
                                                              :external-format :utf-8)
                                      (write-string ,gcode out)))))
                              (values "slice took 3s" ,status))
     ,@body))

(defmacro with-orca-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; ASKED collects (METHOD URL CONTENT), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected printer request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defmacro with-orca-token (&body body)
  "BODY with the printer key variable answering one fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "ORCA_TEST_KEY") "test-key"))
     ,@body))

(defun orca-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it, through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun orca-object (id tool arguments)
  "TOOL's answer, decoded; an ERROR answer is raised so the failure says what it was."
  (let ((text (orca-answer id tool arguments)))
    (if (search "ERROR:" text)
        (error "~a" text)
        (nlk:decode-json text))))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "orcaslicer"
  (:config)
  (:help :orcaslicer)
  (:command "orca")
  (:refused ("slice_timeout" 5) ("max_presets" 0))
  (:idle nodecode-orcaslicer:orcaslicer-error (nodecode-orcaslicer:search-presets nil)))

;;; --- orca_diagnose -------------------------------------------------------------------

(deftest orcaslicer-cell-says-where-the-slicer-is ()
  (with-stubbed-fdefinition (nodecode-orcaslicer::run-slicer (argv &key timeout)
                              (declare (ignore argv timeout))
                              (values +help-text+ 0))
  (let ((executable (orcaslicer-fixture "bin/orca-slicer" "")))
    (with-cell-stop ((orcaslicer-start "slicer" executable))
      (let ((object (orca-object "orca-diagnose-1" "orca_diagnose" "{}")))
        (is (equal executable (nlk:json-value object :string "executable")))
        (is (eq t (nlk:json-value object :boolean "cli_ready")) "every flag is in the help text")
        (is (= 0 (length (nlk:json-array object "missing_flags"))))
        (is (search "OrcaSlicer 2.3.0" (nlk:json-value object :string "version"))))))))

(deftest orcaslicer-cell-names-the-flags-an-old-build-lacks ()
  (with-stubbed-fdefinition (nodecode-orcaslicer::run-slicer (argv &key timeout)
                              (declare (ignore argv timeout))
                              (values "OrcaSlicer 1.6.0 Usage: --slice" 0))
    (let ((executable (orcaslicer-fixture "bin/old-orca-slicer" "")))
      (with-cell-stop ((orcaslicer-start "slicer" executable))
        (let ((object (orca-object "orca-diagnose-2" "orca_diagnose" "{}")))
          (is (null (nlk:json-value object :boolean "cli_ready")) "a build without the flags cannot slice")
          (let ((flags (nlk:json-array object "missing_flags")))
            (is (find "--datadir" flags :test #'string=) "and the answer names the one it lacks")))))))

(deftest orcaslicer-cell-refuses-without-a-slicer ()
  (with-cell-stop ((orcaslicer-start "slicer" "/nonexistent/orca-slicer"))
    (let ((answer (orca-answer "orca-diagnose-3" "orca_diagnose" "{}")))
      (is (search "is not a file" answer) "a configured path that does not exist is named back"))))

;;; --- orca_presets ---------------------------------------------------------------------

(deftest orcaslicer-cell-searches-the-installed-presets ()
  (let ((root (orcaslicer-presets)))
    (with-cell-stop ((orcaslicer-start "profiles" (vector root)))
      (let ((object (orca-object "orca-presets-1" "orca_presets" "{\"query\":\"voron\"}")))
        (is (= 1 (nlk:json-value object :integer "total")) "one preset carries the word")
        (let ((match (aref (nlk:json-array object "matches") 0)))
          (is (equal "Voron 0.4 nozzle" (nlk:json-value match :string "name")))
          (is (equal "machine" (nlk:json-value match :string "kind")))
          (is (uiop:file-exists-p (nlk:json-value match :string "path")) "the path is the preset's own"))))))

(deftest orcaslicer-cell-filters-presets-by-kind ()
  (let ((root (orcaslicer-presets)))
    (with-cell-stop ((orcaslicer-start "profiles" (vector root)))
      (let ((object (orca-object "orca-presets-2" "orca_presets"
                                  "{\"query\":\"\",\"kind\":\"filament\"}")))
        (is (= 1 (nlk:json-value object :integer "total")) "a filament search finds the one")
        (let ((answer (orca-answer "orca-presets-3" "orca_presets" "{\"kind\":\"nozzle\"}")))
          (is (search "kind must be one of" answer) "an unknown kind is refused by name"))))))

;;; --- orca_slice -------------------------------------------------------------------------

(defun orca-sliced-job (&optional (changes "{}"))
  "A job sliced from the fixture model with its three presets; its record."
  (let* ((root (orcaslicer-presets))
         (executable (orcaslicer-fixture "bin/slice-orca" ""))
         (machine (namestring (merge-pathnames "machine/Voron 0.4 nozzle.json"
                                                (uiop:ensure-directory-pathname root))))
         (process (namestring (merge-pathnames "process/0.20mm Standard.json"
                                                (uiop:ensure-directory-pathname root))))
         (filament (namestring (merge-pathnames "filament/PLA Basic.json"
                                                 (uiop:ensure-directory-pathname root)))))
    (orca-object
     "orca-slice-1" "orca_slice"
     (format nil "{\"source\":~s,\"machine\":~s,\"process\":~s,\"filaments\":[~s],\"changes\":~a}"
             (orcaslicer-model) machine process filament changes))))

(deftest orcaslicer-cell-slices-a-model-into-a-job (with-fake-slicer ())
  (with-cell-stop ((orcaslicer-start))
    (let ((record (orca-sliced-job)))
      (is (equal "sliced" (nlk:json-value record :string "state")))
      (is (equal "Voron 0.4 nozzle" (nlk:json-value record :string "printer_profile")))
      (let ((artifacts (nlk:json-array record "artifacts")))
        (is (= 1 (length artifacts)) "one artifact")
        (let ((artifact (aref artifacts 0)))
          (is (equal "sliced.gcode" (nlk:json-value artifact :string "name")))
          (is (<= 32 (length (nlk:json-value artifact :string "sha256"))) "the artifact is hashed")))
      (let ((report (nlk:json-value record :object "report")))
        (is (= 2 (nlk:json-value report :integer "layer_markers")) "the layer changes are counted")
        (is (equal "42" (nlk:json-value report :string "total layer number")))
        (is (equal "1h 15m" (nlk:json-value report :string "total estimated time")))))))

(deftest orcaslicer-cell-refuses-a-raw-model-without-presets (with-fake-slicer ())
  (with-cell-stop ((orcaslicer-start))
    (is (search "needs the exact machine, process and filament presets"
                (orca-answer "orca-slice-2" "orca_slice"
                             (format nil "{\"source\":~s}" (orcaslicer-model)))))))

(deftest orcaslicer-cell-refuses-a-setting-nobody-validated (with-fake-slicer ())
  (with-cell-stop ((orcaslicer-start))
    (let* ((arguments (format nil "{\"source\":~s,\"changes\":{\"sparse_infill_density_zzz\":1}}"
                              (orcaslicer-model)))
           (answer (orca-answer "orca-slice-3" "orca_slice" arguments)))
      (is (search "is not a setting this cell changes" answer) "a stray key is refused"))))

(deftest orcaslicer-cell-fails-a-job-that-made-no-gcode ()
  (with-fake-slicer (:status 3 :gcode nil)
    (with-cell-stop ((orcaslicer-start))
      (let ((record (orca-sliced-job)))
        (is (equal "failed" (nlk:json-value record :string "state")))
        (is (search "exited with status 3" (nlk:json-value record :string "error")))))))

;;; --- orca_job and orca_export -------------------------------------------------------

(deftest orcaslicer-cell-reads-a-job-and-export-its-artifact (with-fake-slicer ())
  (with-cell-stop ((orcaslicer-start))
    (let* ((record (orca-sliced-job))
           (id (nlk:json-value record :string "id"))
           (read-back (orca-object "orca-job-1" "orca_job" (format nil "{\"job_id\":~s}" id))))
      (is (equal id (nlk:json-value read-back :string "id")) "the job answers by id")
      (is (search "no job" (orca-answer "orca-job-2" "orca_job" "{\"job_id\":\"orca-job-missing\"}")) "an unknown job is refused")
      (let* ((target (namestring (merge-pathnames "exported.gcode" (nlk:home "fixtures/"))))
             (answer (orca-object "orca-export-1" "orca_export"
                                  (format nil "{\"job_id\":~s,\"destination\":~s}" id target))))
        (is (uiop:file-exists-p target) "the artifact is on disk")
        (let ((sliced (nlk:json-value (aref (nlk:json-array record "artifacts") 0) :string "sha256"))
              (second-answer (orca-answer "orca-export-2" "orca_export"
                                           (format nil "{\"job_id\":~s,\"destination\":~s}" id target))))
          (is (equal (nlk:json-value answer :string "sha256") sliced) "and it is the sliced one")
          (is (search "already exists" second-answer) "a second export never overwrites"))))))

;;; --- orca_printer_status and orca_upload ---------------------------------------------

(defparameter +moonraker-idle+
  (format nil "{\"result\":{\"status\":{\"print_stats\":{\"state\":\"standby\",\"filename\":\"\"},\"webhooks\":{\"state\":\"ready\"},\"virtual_sdcard\":{\"progress\":0.25},\"extruder\":{\"temperature\":200.0,\"target\":210.0},\"heater_bed\":{\"temperature\":60.0,\"target\":60.0}}}}")
  "What a ready Moonraker answers.")

(defparameter +moonraker-printing+
  (format nil "{\"result\":{\"status\":{\"print_stats\":{\"state\":\"printing\",\"filename\":\"other.gcode\"},\"webhooks\":{\"state\":\"ready\"},\"virtual_sdcard\":{\"progress\":0.5},\"extruder\":{\"temperature\":200.0,\"target\":210.0},\"heater_bed\":{\"temperature\":60.0,\"target\":60.0}}}}")
  "What a busy Moonraker answers.")

(defun orcaslicer-printer-section (&key (name "voron"))
  (vector (nlk:json-object "name" name "kind" "moonraker"
                            "url" "http://printer.test:7125"
                            "api_key_env" "ORCA_TEST_KEY")))

(deftest orcaslicer-cell-reads-a-printer-without-changing-it (with-orca-token)
  (with-cell-stop ((orcaslicer-start "printers" (orcaslicer-printer-section)))
    (with-orca-wire (asked ("/printer/objects/query" (values +moonraker-idle+ 200)))
      (let ((object (orca-object "orca-status-1" "orca_printer_status" "{\"name\":\"voron\"}")))
        (is (equal "standby" (nlk:json-value object :string "state")))
        (is (eq t (nlk:json-value object :boolean "ready_to_start")))
        (is (= 25 (nlk:json-value object :integer "progress_percent")))
        (is (= 200 (nlk:json-value object :number "temperatures" "nozzle" "actual")))
        (is (= 1 (length asked)) "one read-only GET, and nothing else")))))

(deftest orcaslicer-cell-refuses-a-printer-nobody-configured (with-orca-token)
  (with-cell-stop ((orcaslicer-start "printers" (orcaslicer-printer-section)))
    (is (search "no printer named"
                (orca-answer "orca-status-2" "orca_printer_status" "{\"name\":\"prusa\"}")))))

(deftest orcaslicer-cell-uploads-to-an-idle-printer-and-never-starts-it (with-orca-token)
  (with-fake-slicer ()
    (with-cell-stop ((orcaslicer-start "printers" (orcaslicer-printer-section)))
    (let* ((record (orca-sliced-job))
           (id (nlk:json-value record :string "id")))
      (with-orca-wire (asked ("/printer/objects/query" (values +moonraker-idle+ 200))
                        ("/server/files/upload" (values "{\"result\":{\"item\":{\"path\":\"gcodes/nodecode-test.gcode\"}}}" 201)))
        (let ((answer (orca-object "orca-upload-1" "orca_upload"
                                   (format nil "{\"name\":\"voron\",\"job_id\":~s}" id))))
          (is (= 2 (length asked)) "a status read, then the upload")
          (is (search "/server/files/upload" (second (second asked))) "the upload goes to Moonraker's file endpoint")
          (is (equal "gcodes/nodecode-test.gcode" (nlk:json-value answer :string "uploaded")))
          (is (null (nlk:json-value answer :boolean "print_started")) "an upload is not a print")))))))

(deftest orcaslicer-cell-uploads-nothing-to-a-busy-printer (with-orca-token)
  (with-fake-slicer ()
    (with-cell-stop ((orcaslicer-start "printers" (orcaslicer-printer-section)))
    (let* ((record (orca-sliced-job))
           (id (nlk:json-value record :string "id")))
      (with-orca-wire (asked ("/printer/objects/query" (values +moonraker-printing+ 200)))
        (let ((answer (orca-answer "orca-upload-2" "orca_upload"
                                    (format nil "{\"name\":\"voron\",\"job_id\":~s}" id))))
          (is (search "not idle" answer) "the refusal says why"))
        (is (= 1 (length asked)) "and nothing was uploaded"))))))
