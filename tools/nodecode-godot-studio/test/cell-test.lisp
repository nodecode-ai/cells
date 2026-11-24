;;;; cell-test.lisp --- the godot-studio cell against a stubbed engine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No Godot runs: RUN-GODOT is a function that records the argv it was
;;;; handed and answers what a real engine would, writing the file an export
;;;; names. Each tool is called the way a model's call reaches it -- through
;;;; the real dispatch, contract and all -- so the schema a provider is shown
;;;; and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "godot-studio" "GODOT-STUDIO-CELL-")

(defvar *godot-studio-argv* '()
  "The argv the stubbed engine was last called with.")

;;; --- fixtures --------------------------------------------------------------------

(defun godot-studio-fixture (relative text)
  "RELATIVE under this test's own home, written with TEXT. => its namestring."
  (let ((path (merge-pathnames relative (nlk:home "fixtures/"))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string text out))
    (namestring path)))

(defun godot-studio-directory (relative)
  "RELATIVE under this test's own home, made; answers its namestring."
  (let ((path (merge-pathnames relative (nlk:home "fixtures/"))))
    (ensure-directories-exist path)
    (namestring path)))

(defun godot-studio-command-line ()
  "A project's export_presets.cfg, as the editor writes it: two presets, each
followed by its own .options section, which names no preset."
  (format nil "[preset.0]~%~%name=~s~%platform=~s~%runnable=true~%export_path=~s~%~
~%[preset.0.options]~%~%custom_template/debug=~s~%~
~%[preset.1]~%~%name=~s~%platform=~s~%export_path=~s~%~
~%[preset.1.options]~%~%binary_format/architecture=~s~%"
          "Web" "Web" "build/hello.html" ""
          "macOS" "macOS" "build/hello.zip" "universal"))

(defun godot-studio-project (&key (artifact t))
  "A Godot project under this test's home: project.godot, two export presets,
and, unless ARTIFACT is NIL, the Web build an earlier export wrote. => the
project folder's namestring."
  (let ((root (merge-pathnames "game/" (nlk:home "fixtures/"))))
    (ensure-directories-exist root)
    (godot-studio-fixture "game/project.godot" "; Engine configuration file.~%")
    (godot-studio-fixture "game/export_presets.cfg" (godot-studio-command-line))
    (when artifact
      (godot-studio-fixture "game/build/hello.html" "<html></html>"))
    (namestring root)))

(defun godot-studio-binary ()
  "A file that stands in for the Godot executable, so discovery never reaches
this machine's own install."
  (godot-studio-fixture "bin/godot-studio" ""))

(defun godot-studio-config (&rest pairs)
  "A whole shared config holding the `godot-studio' section PAIRS make, with a binary
that exists."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "binary" section (godot-studio-binary))
    (nlk:json-object "godot-studio" section)))

(defun godot-studio-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-godot-studio:start-cell (apply #'godot-studio-config pairs)))

(defmacro with-fake-godot-studio ((&key (status 0) (stdout "Godot Engine v4.7.2.stable") (stderr "")) &body body)
  "BODY with RUN-GODOT recording its argv and answering STDOUT, STDERR and
STATUS; an export's output file is written, as a real export writes it."
  `(with-stubbed-fdefinition (nodecode-godot-studio::run-godot (argv &key timeout)
                              (declare (ignore timeout))
                              (setf *godot-studio-argv* argv)
                              (when (find "--export-release" argv :test #'string=)
                                (let ((path (uiop:parse-native-namestring (car (last argv)))))
                                  (ensure-directories-exist path)
                                  (with-open-file (out path :direction :output
                                                             :if-exists :supersede)
                                    (write-string "exported" out))))
                              (values ,stdout ,stderr ,status))
     (setf *godot-studio-argv* '())
     ,@body))

(defun godot-studio-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it, through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun godot-studio-object (id tool arguments)
  "TOOL's answer, decoded; an ERROR answer is raised so the failure says what it was."
  (let ((text (godot-studio-answer id tool arguments)))
    (if (uiop:string-prefix-p "ERROR:" text)
        (error "~a" text)
        (nlk:decode-json text))))

(defun godot-studio-words (object key)
  "OBJECT's array KEY as a list of strings."
  (coerce (nlk:json-array object key) 'list))

(defun godot-studio-arguments (&rest pairs)
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "godot-studio"
  (:config)
  (:help :godot-studio)
  (:command "godot-studio")
  (:refused ("check_timeout" 5) ("export_timeout" 1) ("max_output" 10))
  (:idle nodecode-godot-studio:godot-studio-error (nodecode-godot-studio:builds nil)))

;;; --- godot_headless_check -------------------------------------------------------

(deftest godot-studio-cell-checks-a-project-without-an-editor (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start))
    (let* ((project (godot-studio-project))
           (answer (godot-studio-object "godot-studio-check-1" "godot_headless_check"
                                  (godot-studio-arguments "project_path" project))))
      (is (eq t (nlk:json-value answer :boolean "ok")) "a clean run is ok")
      (is (= 0 (nlk:json-value answer :integer "returncode")))
      (is (equal (godot-studio-binary) (first *godot-studio-argv*)) "the configured engine ran")
      (is (find "--headless" *godot-studio-argv* :test #'string=) "headless, never an editor")
      (is (find "--quit" *godot-studio-argv* :test #'string=) "and it quits at once")
      (is (null (find "--quit-after" *godot-studio-argv* :test #'string=)) "no frames unless asked"))))

(deftest godot-studio-cell-checks-frames-when-the-runtime-is-the-question
    (with-fake-godot-studio (:status 1 :stdout "" :stderr "SCRIPT ERROR: main.gd:7 -- boom")
  (with-cell-stop ((godot-studio-start))
    (let* ((project (godot-studio-project))
           (answer (godot-studio-object "godot-studio-check-2" "godot_headless_check"
                                  (godot-studio-arguments "project_path" project
                                                   "quit_after_frames" 3))))
      (let ((frames (second (member "--quit-after" *godot-studio-argv* :test #'string=))))
        (is (equal "3" frames) "the frame count reaches the engine"))
      (is (null (nlk:json-value answer :boolean "ok")) "a failed run is not ok")
      (is (search "SCRIPT ERROR" (nlk:json-value answer :string "stderr")) "and its error is answered")))))

(deftest godot-studio-cell-refuses-a-folder-that-is-no-project (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start))
    (let ((answer (godot-studio-answer "godot-studio-check-3" "godot_headless_check"
                                 (godot-studio-arguments "project_path" (godot-studio-directory "empty/")))))
      (is (search "no project.godot" answer) "the answer says what is missing"))))

(deftest godot-studio-cell-names-a-binary-that-is-not-there (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start "binary" (namestring (merge-pathnames "bin/nothing"
                                                                      (nlk:home "fixtures/")))))
    (let ((answer (godot-studio-answer "godot-studio-check-4" "godot_headless_check"
                                 (godot-studio-arguments "project_path" (godot-studio-project)))))
      (is (search "is not a file" answer) "a configured path that does not exist is named back")
      (is (null *godot-studio-argv*) "and nothing ran"))))

;;; --- godot_export ----------------------------------------------------------------

(deftest godot-studio-cell-exports-the-presets-own-path (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start))
    (let* ((project (godot-studio-project))
           (answer (godot-studio-object "godot-studio-export-1" "godot_export"
                                  (godot-studio-arguments "project_path" project
                                                   "preset" "Web")))
           (artifact (nlk:json-value answer :object "artifact")))
      (is (eq t (nlk:json-value answer :boolean "ok")))
      (is (equal "release" (nlk:json-value answer :string "mode")))
      (let ((written (nlk:json-value answer :string "output_path")))
        (is (search "build/hello.html" written) "the build goes where the preset says"))
      (is (eq t (nlk:json-value artifact :boolean "exists")) "and the artifact is there")
      (is (< 0 (nlk:json-value artifact :integer "size_bytes")))
      (is (equal "Web" (nth 5 *godot-studio-argv*)) "the preset's name is what the engine was given"))))

(deftest godot-studio-cell-exports-a-debug-build-when-asked (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start))
    (let* ((project (godot-studio-project))
           (answer (godot-studio-object "godot-studio-export-2" "godot_export"
                                  (godot-studio-arguments "project_path" project
                                                   "preset" "macOS" "debug" t))))
      (is (equal "debug" (nlk:json-value answer :string "mode")))
      (is (find "--export-debug" *godot-studio-argv* :test #'string=) "the debug export flag"))))

(deftest godot-studio-cell-refuses-an-output-outside-the-project (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start))
    (let ((answer (godot-studio-answer "godot-studio-export-3" "godot_export"
                                 (godot-studio-arguments "project_path" (godot-studio-project)
                                                  "preset" "Web"
                                                  "output_path" "~/elsewhere/build.html"))))
      (is (search "must be inside the project directory" answer) "an escape is refused")
      (is (null *godot-studio-argv*) "and nothing ran"))))

(deftest godot-studio-cell-refuses-to-overwrite-what-it-did-not-build (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start))
    (let* ((project (godot-studio-project))
           (notes (godot-studio-fixture "game/build/notes.txt" "keep me")))
      (declare (ignore notes))
      (let ((answer (godot-studio-answer "godot-studio-export-4" "godot_export"
                                   (godot-studio-arguments "project_path" project
                                                    "preset" "Web"
                                                    "output_path" "build/notes.txt"))))
        (let ((kept (uiop:read-file-string
                     (merge-pathnames "game/build/notes.txt" (nlk:home "fixtures/")))))
          (is (search "not a known export artifact" answer) "a file no preset writes is left alone")
          (is (equal "keep me" kept) "and its bytes are untouched"))))))

(deftest godot-studio-cell-says-when-the-export-templates-are-missing
    (with-fake-godot-studio (:status 1 :stdout "" :stderr "Export template not found in export_templates/4.7.2.stable/")
  (with-cell-stop ((godot-studio-start))
    (let ((answer (godot-studio-object "godot-studio-export-5" "godot_export"
                                 (godot-studio-arguments "project_path" (godot-studio-project)
                                                  "preset" "Web"))))
      (let ((hint (nlk:json-value answer :string "hint")))
        (is (search "Manage Export Templates" hint) "the fix is named, not engine noise"))))))

(deftest godot-studio-cell-refuses-a-preset-the-project-does-not-carry (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start))
    (let ((answer (godot-studio-answer "godot-studio-export-6" "godot_export"
                                 (godot-studio-arguments "project_path" (godot-studio-project)
                                                  "preset" "Linux"))))
      (is (search "has no export_path" answer) "a preset nobody configured is refused"))))

;;; --- godot_builds ---------------------------------------------------------------

(deftest godot-studio-cell-reads-the-builds-a-project-has ()
  (with-cell-stop ((godot-studio-start))
    (let* ((project (godot-studio-project))
           (answer (godot-studio-object "godot-studio-builds-1" "godot_builds"
                                  (godot-studio-arguments "project_path" project)))
           (builds (godot-studio-words answer "builds")))
      (is (eq t (nlk:json-value answer :boolean "presets_file")) "the presets file was read")
      (is (= 2 (length builds)) "one line per preset, not per .options section")
      (let ((web (aref (nlk:json-array answer "builds") 0))
            (mac (aref (nlk:json-array answer "builds") 1)))
        (is (equal "Web" (nlk:json-value web :string "preset")))
        (is (eq t (nlk:json-value web :boolean "exists")) "the written build is there")
        (is (= 13 (nlk:json-value web :integer "size_bytes")) "with its size")
        (is (< 0 (nlk:json-value web :integer "modified")) "and its modification time")
        (is (equal "macOS" (nlk:json-value mac :string "preset")))
        (is (null (nlk:json-value mac :boolean "exists")) "a build never made is absent")
        (is (search "build/hello.zip" (nlk:json-value mac :string "artifact_path")))))))

(deftest godot-studio-cell-reads-a-project-with-no-presets ()
  (with-cell-stop ((godot-studio-start))
    (let* ((project (godot-studio-project))
           (answer (godot-studio-object "godot-studio-builds-2" "godot_builds"
                                  (godot-studio-arguments "project_path" project))))
      (declare (ignore project))
      (delete-file (merge-pathnames "game/export_presets.cfg" (nlk:home "fixtures/")))
      (let ((empty (godot-studio-object "godot-studio-builds-3" "godot_builds"
                                  (godot-studio-arguments "project_path"
                                                    (nlk:home "fixtures/game/")))))
        (declare (ignore answer))
        (is (= 0 (length (godot-studio-words empty "builds"))) "no presets is an empty list")
        (is (null (nlk:json-value empty :boolean "presets_file")) "and no presets file")))))

;;; --- godot_setup and /godot-studio -----------------------------------------------------

(deftest godot-studio-cell-reports-the-engine-and-the-configured-projects (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start "projects" (vector (godot-studio-project))))
    (let* ((answer (godot-studio-object "godot-studio-setup-1" "godot_setup" "{}"))
           (projects (godot-studio-words answer "projects")))
      (is (equal (godot-studio-binary) (nlk:json-value answer :string "binary")))
      (is (search "Godot Engine" (nlk:json-value answer :string "version")))
      (is (= 1 (length projects)) "one line per configured project")
      (let ((project (aref (nlk:json-array answer "projects") 0)))
        (is (eq t (nlk:json-value project :boolean "exists")))
        (is (= 2 (nlk:json-value project :integer "presets"))))
      (let ((next (nlk:json-value answer :string "next_step")))
        (is (search "export templates" next) "and what an export needs is named")))))

(deftest godot-studio-cell-slash-command-says-what-it-can-see (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start "projects" (vector (godot-studio-project))))
    (let* ((entry (nle::find-registered-command "godot-studio"))
           (text (funcall (nle::slash-command-headless entry) "" "s1")))
      (is (search "godot-studio: " text) "the engine line")
      (is (search (godot-studio-binary) text) "names the executable")
      (is (search "2 presets, 1 built" text) "and each project's builds"))))

(deftest godot-studio-cell-slash-command-names-a-project-it-was-given (with-fake-godot-studio ())
  (with-cell-stop ((godot-studio-start))
    (let* ((entry (nle::find-registered-command "godot-studio"))
           (text (funcall (nle::slash-command-headless entry) (godot-studio-project) "s1")))
      (is (search "2 presets, 1 built" text) "an argument is read as the project"))))
