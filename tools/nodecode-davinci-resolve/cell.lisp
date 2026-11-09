;;;; cell.lisp --- DaVinci Resolve, live and by interchange.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One declaration: the `davinci_resolve' section, seven tools, /resolve and
;;;; (help :resolve). Two halves ride it. With DaVinci Resolve Studio open and
;;;; scripting enabled, the verbs reach the running app through its own
;;;; scripting module: what project is open, the media pool, markers, a render.
;;;; With free Resolve nothing can be driven, so the same cell writes files
;;;; Resolve imports: an FCPXML timeline and a marker CSV manifest.

(in-package #:nodecode-davinci-resolve)

;;; --- what a call carries ------------------------------------------------------

(nlk:define-contract scan-folder
  (:doc "Walk a folder the way a media pool would: the footage, music and stills under it, recursively, by kind, so a timeline can be built from what is really there.")
  ("folder" :text :required t :doc "the folder to walk")
  ("extensions" :array :of :string
                 :doc "file-name types to keep, without the dot; empty keeps every video, audio and still type Resolve opens"))

(nlk:define-contract import-job
  (:doc "Bring media files into the open project's media pool. Without confirm the call only reports what it would do, so nothing changes until the operator says yes.")
  ("paths" :array :of :string :required t :doc "the files to import")
  ("bin" :string :default "" :doc "a bin to import into, created when missing; empty imports into the current bin")
  ("confirm" :boolean :default nil :doc "true imports; false only reports the plan"))

(nlk:define-contract fcpxml-job
  (:doc "Write an FCPXML 1.10 timeline that Resolve imports through File > Import > Timeline: one clip per media path, in the order given, with markers. Needs no Resolve at all, so it works with the free edition.")
  ("name" :text :required t :doc "the timeline's name, which Resolve shows after the import")
  ("media_paths" :array :of :string :required t :doc "the files, in timeline order")
  ("markers" :array :of marker :doc "markers, by timeline frame, placed on the clip whose span holds them")
  ("frame_rate" :integer :default 0 :doc "the timeline frame rate; 0 takes the section's")
  ("clip_seconds" :number :default 5.0 :doc "how long each clip is on the timeline")
  ("output_path" :string :default "" :doc "the file to write; empty writes into export_dir"))

(nlk:define-contract marker-csv-job
  (:doc "Write the marker manifest as CSV: one row a marker, its frame, its seconds, its name, colour, note and duration. For review notes and edit beats, and for workflows that read markers back into free Resolve.")
  ("name" :text :default "Resolve Markers" :doc "the manifest's name, which names the file")
  ("markers" :array :of marker :required t :doc "the markers, in any order")
  ("frame_rate" :integer :default 0 :doc "the frame rate seconds are counted at; 0 takes the section's")
  ("output_path" :string :default "" :doc "the file to write; empty writes into export_dir"))

(nlk:define-contract marker-job
  (:doc "Put a marker on the current timeline of the open project, where the operator will see it in Resolve. Without confirm the call only reports what it would do.")
  ("frame" :integer :required t :doc "the timeline frame the marker sits on")
  ("name" :text :default "Marker" :doc "the marker's title")
  ("color" :string :default "Blue" :options +marker-colors+ :doc "one of Resolve's marker colours")
  ("note" :string :default "" :doc "the note the operator reads in the marker list")
  ("duration" :integer :default 1 :doc "frames the marker spans; 1 is a point marker")
  ("confirm" :boolean :default nil :doc "true places it in Resolve; false only reports the plan"))

(nlk:define-contract render-job
  (:doc "Queue a render of the current timeline in the open project, and start it. Without confirm the call only reports the plan, so no render starts unasked.")
  ("target_dir" :text :required t :doc "the folder the rendered file lands in")
  ("custom_name" :text :default "resolve-render" :doc "the rendered file's name, without extension")
  ("render_format" :string :default "mov" :doc "the container, as Resolve names it: mov, mp4, mxf")
  ("render_codec" :string :default "H264" :doc "the codec, as Resolve names it: H264, ProRes422HQ, DNxHR_HQ")
  ("preset_name" :string :default "" :doc "a render preset to load first; empty keeps Resolve's current one")
  ("confirm" :boolean :default nil :doc "true queues and starts the render; false only reports the plan"))

(defparameter +status-schema+
  (list :description "What DaVinci Resolve has open right now: the app and its version, the project, every timeline in it, the current timeline and its frame rate, and whether a render is running. Says why when nothing answers."
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object)))
  "The one tool with nothing to ask: a schema with no members.")

;;; --- texts the model reads ----------------------------------------------------

(defun answer-string (answer key &optional default)
  "ANSWER's member KEY as a string, or DEFAULT when absent."
  (or (nlk:json-value answer :string key) default))

(defun answer-list (answer key)
  "ANSWER's member KEY as a list, or NIL."
  (let ((value (nlk:json-value answer :array key)))
    (and value (coerce value 'list))))

(defun join-names (names &optional (limit 12))
  "NAMES, the first LIMIT of them, comma-separated, with a count of the rest."
  (let ((shown (subseq names 0 (min limit (length names)))))
    (format nil "~{~a~^, ~}~a" shown
            (if (> (length names) limit)
                (format nil ", and ~d more" (- (length names) limit))
                ""))))

(defun human-bytes (bytes)
  "BYTES as the size a person reads: 1.2 GB."
  (if (null bytes) "?"
      (loop with units = #("B" "KB" "MB" "GB" "TB")
            for value = (coerce bytes 'double-float) then (/ value 1024)
            for unit across units
            when (or (< value 1024) (string= unit "TB"))
              return (if (string= unit "B") (format nil "~dB" (round value))
                         (format nil "~,1f ~a" value unit)))))

(defun status ()
  "What DaVinci Resolve answers now, as the few lines an operator reads."
  (check-running)
  (let ((answer (call-resolve "status")))
    (with-output-to-string (out)
      (format out "~a~@[ ~a~]~@[ (page ~a)~]~%"
              (answer-string answer "product" "DaVinci Resolve")
              (answer-string answer "version")
              (let ((page (answer-string answer "page"))) (and page (plusp (length page)) page)))
      (let ((project (answer-string answer "project")) (timelines (answer-list answer "timelines")))
        (if project
            (format out "project: ~a (~d timeline~:p)~%" project (length timelines))
            (format out "project: none open; open one in Resolve, or import a timeline this cell writes~%")))
      (let ((timeline (nlk:json-value answer :object "timeline")))
        (when timeline
          (format out "current timeline: ~a at ~a fps, frames ~a to ~a~%"
                  (answer-string timeline "name")
                  (answer-string timeline "frame_rate")
                  (nlk:json-value timeline :integer "start")
                  (nlk:json-value timeline :integer "end"))))
      (when (nlk:json-value answer :boolean "rendering")
        (format out "a render is running~%")))))

(defun scan-media (folder &key extensions)
  "The media under FOLDER, recursively, as (:PATH :KIND :BYTES) plists."
  (check-running)
  (let ((types (and extensions (coerce extensions 'list))))
    (media-files folder
                  :extensions (and types
                                   (mapcar (lambda (type)
                                             (string-downcase (string-left-trim "." (string type))))
                                           types)))))

(defun media-report (folder extensions)
  "What FOLDER holds: how many of each kind, then the first forty files."
  (let* ((files (scan-media folder :extensions extensions))
         (types (and extensions (coerce extensions 'list))))
    (with-output-to-string (out)
      (format out "~d media file~:p under ~a~a~%"
              (length files) folder
              (if types (format nil " (only ~{~a~^, ~})" types) ""))
      (loop for kind in '("video" "audio" "image")
            for count = (count kind files :key (lambda (file) (getf file :kind)) :test #'string=)
            when (plusp count)
              do (format out "  ~d ~a~:p~%" count kind))
      (loop for file in files
            for shown from 1
            when (<= shown 40)
              do (format out "  ~a  ~a  ~a~%"
                         (getf file :kind)
                         (human-bytes (getf file :bytes))
                         (getf file :path)))
      (when (> (length files) 40)
        (format out "  ... and ~d more~%" (- (length files) 40))))))

;;; --- the seven tools -----------------------------------------------------------

(defun tool-status (arguments)
  "Check DaVinci Resolve: the app, the project, the timeline, a running render."
  (declare (ignore arguments))
  (status))

(defun tool-scan-media (job)
  "Scan a folder for footage, music and stills."
  (media-report (scan-folder-folder job) (scan-folder-extensions job)))

(defun tool-import-media (job)
  "Import media into the open project's media pool, on confirm."
  (let* ((paths (coerce (import-job-paths job) 'list))
         (bin (import-job-bin job))
         (where (if (and bin (plusp (length bin))) (format nil " into bin ~s" bin) "")))
    (if (not (import-job-confirm job))
        (format nil "Would import ~d file~:p~a:~%~{  ~a~%~}~%Nothing has changed in Resolve: ask again with confirm true to import."
                (length paths) where paths)
        (let ((answer (call-resolve "import"
                                    (nlk:json-object "paths" (coerce paths 'vector)
                                                     "bin" (or bin "")))))
          (let ((missing (answer-list answer "missing")))
            (format nil "Imported ~d item~:p~a~@[; not found: ~a~].~%"
                    (or (nlk:json-value answer :integer "imported") 0)
                    where
                    (and missing (join-names missing))))))))

(defun tool-write-fcpxml (job)
  "Write an FCPXML timeline free Resolve can import."
  (let* ((rate (fcpxml-job-frame-rate job))
         (path (write-fcpxml
                :name (fcpxml-job-name job)
                :media-paths (coerce (fcpxml-job-media-paths job) 'list)
                :markers (fcpxml-job-markers job)
                :frame-rate (and (plusp rate) rate)
                :clip-seconds (fcpxml-job-clip-seconds job)
                :output-path (fcpxml-job-output-path job))))
    (format nil "Wrote ~a~%Import it in Resolve: File > Import > Timeline > Import AAF, EDL, XML...~%"
            path)))

(defun tool-write-marker-csv (job)
  "Write the marker manifest as CSV."
  (let* ((rate (marker-csv-job-frame-rate job))
         (path (write-marker-csv :name (marker-csv-job-name job)
                                 :markers (marker-csv-job-markers job)
                                 :frame-rate (and (plusp rate) rate)
                                 :output-path (marker-csv-job-output-path job))))
    (format nil "Wrote ~a~%One row a marker: frame, seconds, name, colour, note, duration.~%"
            path)))

(defun tool-add-marker (job)
  "Put a marker on the current timeline, on confirm."
  (if (not (marker-job-confirm job))
      (format nil "Would put a ~a marker ~s at frame ~d, ~d frame~:p long~@[, with the note ~s~], on the current timeline. Nothing has changed: ask again with confirm true."
              (marker-job-color job) (marker-job-name job) (marker-job-frame job)
              (marker-job-duration job)
              (let ((note (marker-job-note job))) (and (plusp (length note)) note)))
      (let ((answer (call-resolve "marker"
                                  (nlk:json-object
                                   "frame" (marker-job-frame job)
                                   "name" (marker-job-name job)
                                   "color" (marker-job-color job)
                                   "note" (marker-job-note job)
                                   "duration" (marker-job-duration job)))))
        (format nil "Placed the ~a marker ~s at frame ~d on the current timeline.~%"
                (answer-string answer "color" "Blue")
                (answer-string answer "name" "Marker")
                (or (nlk:json-value answer :integer "frame") (marker-job-frame job))))))

(defun tool-render (job)
  "Queue and start a render of the current timeline, on confirm."
  (let ((target (render-job-target-dir job))
        (preset (render-job-preset-name job)))
    (if (not (render-job-confirm job))
        (format nil "Would render the current timeline into ~a as ~a.~a (~a/~a)~@[, with the preset ~s~]. Nothing has started: ask again with confirm true."
                target (render-job-custom-name job)
                "" (render-job-render-format job) (render-job-render-codec job)
                (and preset (plusp (length preset)) preset))
        (let ((answer (call-resolve "render"
                                    (nlk:json-object
                                     "target_dir" target
                                     "custom_name" (render-job-custom-name job)
                                     "render_format" (render-job-render-format job)
                                     "render_codec" (render-job-render-codec job)
                                     "preset_name" (or preset "")
                                     "start" t))))
          (format nil "Render job ~a ~:[is queued~;started~]: ~a/~a into ~a.~%"
                  (answer-string answer "job_id" "?")
                  (nlk:json-value answer :boolean "started")
                  (render-job-render-format job) (render-job-render-codec job) target)))))

;;; --- /resolve ----------------------------------------------------------------

(defparameter +usage+
  "/resolve | /resolve scan FOLDER"
  "What the command reads.")

(defun resolve-dialog ()
  "The rows /resolve answers with, so the next step is one key away."
  (nle:list-dialog
   :title "DaVinci Resolve"
   :summary "Live control needs Resolve Studio open with scripting enabled; the interchange files import into free Resolve."
   :rows (list (nle:ask-row "Scan a folder" "Folder" "/resolve"
                             :placeholder "a folder of footage" :verb "scan"))))

(defun run-slash (args session-id)
  "One /resolve invocation: the status, or a folder scanned."
  (declare (ignore session-id))
  (nlk:with-handlers ((resolve-error (condition) (format nil "resolve: ~a" condition)))
    (let* ((words (nlk:split-words args))
           (head (first words)))
      (cond ((null head) (values (status) (resolve-dialog)))
            ((member head '("status" "check") :test #'string-equal)
             (values (status) (resolve-dialog)))
            ((string-equal head "scan")
             (let ((folder (format nil "~{~a~^ ~}" (rest words))))
               (if (zerop (length folder))
                   (values +usage+ (resolve-dialog))
                   (media-report folder nil))))
            (t (format nil "resolve: unknown subcommand ~a; usage ~a" head +usage+))))))

;;; --- the manual ----------------------------------------------------------------

(defparameter +manual+
  "resolve: DaVinci Resolve, live and by interchange.

(resolve:status) - what Resolve has open: the app, the project, every timeline,
  the current one and its frame rate, a running render.
(resolve:import-media path... :bin name :confirm t) - import files into the
  media pool; without :confirm it only says what it would import.
(resolve:add-marker frame :name title :color Blue :note text :duration 1
                    :confirm t) - put a marker on the current timeline.
(resolve:render target-dir :custom-name name :render-format mov
                :render-codec H264 :confirm t) - queue and start a render.

These four reach a running Resolve Studio through its Python scripting
module; the section's `python' names the interpreter that can import
DaVinciResolveScript, and `script_api' its Scripting folder.

These three need no Resolve, and work with the free edition:

(resolve:scan-media folder) - the footage, music and stills under a folder.
(resolve:write-fcpxml name paths :markers marks) - an FCPXML 1.10 timeline
  Resolve imports through File > Import > Timeline.
(resolve:write-marker-csv name markers) - a marker manifest, one row a marker.

Mutations never happen unasked: without confirm a verb reports the plan and
leaves Resolve alone. /resolve prints the status."
  "(help :resolve)'s text.")

;;; --- the one declaration -----------------------------------------------------------

(nle:define-cell davinci-resolve
  (:section ("davinci-resolve")
    (:guide "macOS and DaVinci Resolve; Resolve Studio open with scripting enabled drives the live verbs, and the interchange verbs need nothing; python is the interpreter that can import DaVinciResolveScript; export_dir is where FCPXML and CSV files land; frame_rate is what exports and markers count at")
    ("python" :string :default "python3"
              :doc "the interpreter that can import DaVinciResolveScript; python3 from Homebrew or python.org, whichever sees Resolve's module")
    ("script_api" :string :default ""
                  :doc "Resolve's Scripting folder, holding Modules/DaVinciResolveScript.py; empty reads RESOLVE_SCRIPT_API, else the standard macOS location")
    ("export_dir" :path :default ""
                  :doc "where FCPXML timelines and marker CSV files are written; empty means Documents/Nodecode Resolve Exports")
    ("frame_rate" :integer :default 24 :min 1
                  :doc "the frame rate exports and marker seconds are counted at"))
  ;; A frame rate outside 1-240 is not a rate, and the section says so at
  ;; start rather than in an FCPXML Resolve will refuse to import.
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((rate (getf values :frame-rate)))
                 (unless (and (integerp rate) (<= 1 rate 240))
                   (nlk:config-error "davinci-resolve.frame_rate is ~s; a frame rate is a whole number from 1 to 240" rate))
                 values)))
  (:tool "resolve_status" +status-schema+ 'tool-status)
  (:tool "resolve_scan_media_folder" (nlk:contract 'scan-folder) 'tool-scan-media)
  (:tool "resolve_import_media" (nlk:contract 'import-job) 'tool-import-media)
  (:tool "resolve_add_timeline_marker" (nlk:contract 'marker-job) 'tool-add-marker)
  (:tool "resolve_render_timeline" (nlk:contract 'render-job) 'tool-render)
  (:tool "resolve_generate_fcpxml_timeline" (nlk:contract 'fcpxml-job) 'tool-write-fcpxml)
  (:tool "resolve_generate_marker_csv" (nlk:contract 'marker-csv-job) 'tool-write-marker-csv)
  (:command "resolve" 'run-slash
            :description "What DaVinci Resolve has open, or scan a folder of footage"
            :argument-hint "[scan FOLDER]")
  (:help :resolve "resolve: DaVinci Resolve live (project, media, markers, render) and by interchange (FCPXML, marker CSV) - (resolve:status)" +manual+))
