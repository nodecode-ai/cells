;;;; tools.lisp --- the five tools the model calls, and the /postkitty overview.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every body answers a JSON string and never signals: a refusal the model can
;;;; repair is an {"error": ...} object, the way the original's handlers
;;;; answered. The tools decide whether a file passes; the model only reports
;;;; what the tool said.

(in-package #:nodecode-postkitty)

;;;; --- reading the call, writing the answer -----------------------------------

(defun obj (&rest pairs)
  "A JSON object of the PAIRS that have a value: a NIL value is an absent member."
  (apply #'nlk:make-json-object
         (loop for (key value) on pairs by #'cddr
               when value append (list key value))))

(defun json-array (list)
  "LIST as the JSON array of its members."
  (coerce (or list '()) 'vector))

(defun answer (value)
  (nlk:encode-json-object value))

(defun refused (text &optional hint)
  (obj "error" text "hint" (or hint "Check the arguments; run postkitty_scan first for a new job.")))

(defmacro answering (&body body)
  "BODY's value as JSON; a condition becomes the object the model repairs from."
  `(handler-case (answer (progn ,@body))
     (error (condition) (answer (refused (princ-to-string condition))))))

(defun arg-string (args key)
  (or (nlk:json-value args :string key) ""))

(defun arg-list (args key)
  "KEY's array of strings, as a list."
  (let ((value (nlk:json-value args :array key)))
    (and value (coerce value 'list))))

(defun arg-object (args key)
  (nlk:json-value args :object key))

(defun arg-boolean (args key &optional (default t))
  (if (and (hash-table-p args) (nth-value 1 (gethash key args)))
      (let ((value (gethash key args)))
        (and value (not (eq value :false))))
      default))

(defun known-name (name)
  "NAME checked against the table, and answered as its canonical platform."
  (or (platform-of name)
      (error "unknown platform or target ~s; one of ~{~a~^, ~}"
             name (append (platform-names) (mapcar (lambda (target) (getf target :id)) (all-targets))))))

(defun know-wanted (names)
  "The platform names NAMES asks for, each checked against the table."
  (mapcar (lambda (name) (progn (known-name name) (canonical-name name))) names))

;;;; --- scan ------------------------------------------------------------------

(defun source-for (infos target)
  "The input TARGET is made from: a video for a video target, a photo for a photo one."
  (let ((kind (getf target :kind)))
    (cond ((string= kind "image_or_video") (or (find "video" infos :key (lambda (info) (getf info :kind))
                                                         :test #'string=)
                                                (find "image" infos :key (lambda (info) (getf info :kind))
                                                      :test #'string=)))
          (t (find kind infos :key (lambda (info) (getf info :kind)) :test #'string=)))))

(defun plan-targets (infos wanted)
  "One plan per target the inputs can make."
  (loop for target in (targets-for (remove-duplicates (mapcar (lambda (info) (getf info :kind)) infos)
                                                      :test #'string=)
                                   wanted)
        for info = (source-for infos target)
        when info
          collect (list :id (getf target :id) :platform (getf target :platform) :input (getf info :id)
                        :size (format nil "~dx~d" (getf target :width) (getf target :height))
                        :layout "crop")))

(defun target-fit-json (report)
  (obj "limit" (getf report :limit)
       "unit" (getf report :unit)
       "counted" (getf report :counted)
       "ok" (if (getf report :ok) t)
       "overflow" (getf report :overflow)
       "hashtags" (getf report :hashtags)
       "hashtags_max" (getf report :hashtags-max)
       "mentions" (getf report :mentions)
       "mentions_max" (getf report :mentions-max)
       "bytes" (getf report :bytes)
       "bytes_max" (getf report :bytes-max)
       "problems" (and (getf report :problems) (json-array (mapcar #'string (getf report :problems))))
       "advisory" (and (getf report :advisory) (json-array (mapcar #'string (getf report :advisory))))
       "thread" (and (getf report :thread) (json-array (getf report :thread)))))

(defun fit-json (text wanted)
  "Per platform, what TEXT counts against the platform's limit."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (name (or wanted (platform-names)))
      (when (platform-spec name)
        (setf (gethash name table) (target-fit-json (fit-report name text)))))
    table))

(defun drop-targets (id manifest)
  "Forget every rendered target of a job whose inputs changed, and take their files out."
  (dolist (entry (getf manifest :targets))
    (dolist (key '(:file :cover :failed_file))
      (let ((name (getf entry key)))
        (when name
          (ignore-errors (delete-file (merge-pathnames name (job-root id))))))))
  (setf (getf manifest :targets) nil)
  manifest)

(defun input-json (info)
  (obj "id" (getf info :id) "name" (getf info :name) "path" (getf info :path)
       "kind" (getf info :kind) "format" (getf info :format) "bytes" (getf info :bytes)
       "width" (getf info :width) "height" (getf info :height)
       "duration" (getf info :duration) "fps" (getf info :fps)
       "notes" (and (getf info :notes) (json-array (getf info :notes)))))

(defun text-json (text)
  (let ((stats (text-stats text)))
    (obj "chars" (getf stats :chars) "x_weighted" (getf stats :x-weighted)
         "graphemes" (getf stats :graphemes) "utf16" (getf stats :utf16)
         "utf8_bytes" (getf stats :utf8-bytes) "hashtags" (getf stats :hashtags)
         "mentions" (getf stats :mentions) "urls" (getf stats :urls))))

(defun tool-scan (args)
  "Open a job, probe the files, and plan one framing per target that fits them."
  (let* ((paths (mapcar (lambda (path) (uiop:parse-native-namestring path)) (arg-list args "inputs")))
         (text (arg-string args "text"))
         (wanted (know-wanted (or (arg-list args "platforms") *default-platforms*)))
         (asked (arg-string args "job")))
    (when (and (null paths) (zerop (length (nlk:trimmed text))))
      (error "give at least one input file or a draft text"))
    (let* ((infos (loop for path in paths for index from 1
                        collect (probe-input path (format nil "in~d" index))))
           (id (create-job (and (plusp (length asked)) asked)))
           (manifest (load-manifest id))
           (same (equal (mapcar (lambda (info) (list (getf info :path) (getf info :bytes))) infos)
                        (mapcar (lambda (entry) (list (getf entry :path) (getf entry :bytes)))
                                (getf manifest :inputs))))
           (notes (loop for info in infos nconc (getf info :notes)))
           (plans (plan-targets infos wanted)))
      (unless same (setf manifest (drop-targets id manifest)))
      (setf (getf manifest :inputs) infos
            (getf manifest :text) text
            (getf manifest :platforms) wanted)
      (save-manifest id manifest)
      (save-scan id (list :targets plans :inputs infos))
      (obj "job" id
           "job_dir" (uiop:native-namestring (job-root id))
           "inputs" (json-array (mapcar #'input-json infos))
           "plan" (json-array (loop for plan in plans
                                collect (obj "id" (getf plan :id) "input" (getf plan :input)
                                             "size" (getf plan :size) "layout" (getf plan :layout))))
           "fit" (and (plusp (length (nlk:trimmed text))) (fit-json text wanted))
           "text" (and (plusp (length (nlk:trimmed text))) (text-json text))
           "notes" (and notes (json-array notes))))))

;;;; --- copy ------------------------------------------------------------------

(defun write-text (path text)
  "TEXT written to PATH, its folders made."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text out))
  path)

(defun draft-text (value)
  "A draft as TEXT, with PARTS when the caller sent the parts of a thread."
  (cond ((stringp value) (values value nil))
        ((vectorp value) (let ((parts (coerce value 'list)))
                           (values (format nil "~{~a~^~%~%~}" parts) parts)))
        (t (error "a draft must be a string, or a list of strings for a thread"))))

(defun copy-report (id name value split)
  "NAME's draft fitted and written into its own platform folder. => (values REPORT FILE)."
  (let ((platform (canonical-name name)))
    (unless (platform-spec platform) (error "unknown platform ~s" name))
    (multiple-value-bind (text parts) (draft-text value)
      (if parts
          (let* ((unit (getf (platform-rules platform) :unit))
                 (limit (getf (platform-rules platform) :limit))
                 (counted (loop for part in parts maximize (count-text part unit)))
                 (file (write-text (merge-pathnames "thread.txt" (ready-directory id platform))
                                   (format nil "~{~a~^~%~%~}" parts))))
            (values (obj "platform" platform "unit" unit "limit" limit "counted" counted
                         "ok" (if (<= counted limit) t) "parts" (length parts))
                    file))
          (let* ((report (fit-report platform text :split split))
                 (thread (getf report :thread))
                 (file (write-text (merge-pathnames (if thread "thread.txt" "post.txt")
                                                    (ready-directory id platform))
                                   (if thread (format nil "~{~a~^~%~%~}" thread) text))))
            (values (target-fit-json report) file))))))

(defun advice-text (advice)
  "ADVICE as the markdown the job keeps."
  (with-output-to-string (out)
    (let ((topic (nlk:json-value advice :string "topic")))
      (when topic (format out "# ~a~%~%" topic)))
    (dolist (pair '( ("audience" . "Audience") ("tone" . "Tone") ("key_message" . "Key message")))
      (let ((value (nlk:json-value advice :string (car pair))))
        (when value (format out "**~a:** ~a~%" (cdr pair) value))))
    (let ((ranking (nlk:json-array advice "ranking")))
      (when (plusp (length ranking))
        (format out "~%## Where to post~%")
        (loop for entry across ranking for index from 1 do
          (format out "~d. ~a - ~a~%" index (or (nlk:json-value entry :string "platform") "?")
                  (or (nlk:json-value entry :string "reason") "")))))
    (let ((tips (nlk:json-array advice "tips")))
      (when (plusp (length tips))
        (format out "~%## Tips~%")
        (loop for tip across tips do (format out "- ~a~%" tip))))))

(defun tool-copy (args)
  "Store the advice and the drafts, counted with each platform's own rule."
  (let* ((id (open-job (arg-string args "job")))
         (manifest (load-manifest id))
         (split (arg-boolean args "split_threads" t))
         (advice (arg-object args "advice"))
         (drafts (arg-object args "drafts"))
         (alt (arg-object args "alt_text"))
         (reports (make-hash-table :test 'equal))
         (kept '())
         (files '()))
    (when advice
      (push (write-text (job-file id "ready/advice.md") (advice-text advice)) files))
    (when drafts
      (maphash (lambda (name value)
                 (multiple-value-bind (report file) (copy-report id name value split)
                   (setf (gethash (canonical-name name) reports) report)
                   (push (list :platform (canonical-name name) :text (draft-text value)
                               :ok (equal t (nlk:json-value report :boolean "ok")))
                         kept)
                   (push file files)))
               drafts))
    (when alt
      (maphash (lambda (key value)
                 (let ((target (target-spec key)))
                   (push (if target
                             (write-text (merge-pathnames "alt.txt" (ready-directory id (getf target :platform)))
                                         (format nil "~a: ~a~%" (getf target :id) value))
                             (write-text (job-file id (format nil "ready/alt-~a.txt" (file-namestring (princ-to-string key))))
                                         (princ-to-string value)))
                         files)))
               alt))
    (setf (getf manifest :copy) (nreverse kept))
    (save-manifest id manifest)
    (obj "job" id
         "job_dir" (uiop:native-namestring (job-root id))
         "drafts" reports
         "files" (json-array (mapcar (lambda (file) (relative-name id file)) (nreverse files))))))

;;;; --- make and redo ----------------------------------------------------------

(defun override-list (plan overrides)
  "PLAN's stored override, with this call's members over it."
  (let ((stored (getf plan :override))
        (fresh (and (hash-table-p overrides) (gethash (getf plan :id) overrides))))
    (append (and (hash-table-p fresh) (list fresh))
            (and (hash-table-p stored) (list stored)))))

(defun override-value (sets key)
  "KEY's value in the first override object of SETS that carries it."
  (loop for object in sets
        when (hash-table-p object)
          do (multiple-value-bind (value present) (gethash key object)
               (when present (return value)))))

(defun build-target (id manifest plan overrides)
  "Render one planned target, check it, and answer its row."
  (let* ((target (target-spec (getf plan :id)))
         (info (find (getf plan :input) (getf manifest :inputs)
                     :key (lambda (entry) (getf entry :id)) :test #'string=))
         (sets (override-list plan overrides))
         (layout (or (override-value sets "layout") (getf plan :layout) "crop"))
         (trim (coerce (or (override-value sets "trim") #()) 'list))
         (cover-at (override-value sets "cover_at"))
         (platform (getf target :platform))
         (extension (if (string= (getf target :kind) "image") "jpg" "mp4"))
         (output (merge-pathnames (format nil "~a.~a" (getf target :id) extension)
                                  (ready-directory id platform))))
    (unless info (error "target ~a has no input in this job" (getf target :id)))
    (ensure-directories-exist output)
    (multiple-value-bind (ok message)
        (if (string= (getf info :kind) "image")
            (render-image info target output layout)
            (render-video info target output trim cover-at))
      (if (null ok)
          (if (nlk::executable-on-path "ffmpeg")
              (list :id (getf target :id) :status "failed" :layout layout
                    :size (format nil "~dx~d" (getf target :width) (getf target :height))
                    :problems (list (format nil "the renderer could not make this file: ~a" (first-line message))))
              (list :id (getf target :id) :status "skipped" :layout layout
                    :size (format nil "~dx~d" (getf target :width) (getf target :height))
                    :note "ffmpeg is not on PATH, so nothing was rendered"))
          (let ((probed (probe-input output (getf target :id))))
            (multiple-value-bind (rows pass) (check-file probed target)
              (let ((problems (loop for row in rows when (string= (getf row :status) "fail")
                                    collect (getf row :detail)))
                    (advisory (loop for row in rows when (string= (getf row :status) "advisory")
                                    collect (getf row :detail)))
                    (cover (merge-pathnames "cover.jpg" output)))
                (if pass
                    (list :id (getf target :id) :status "ready" :file (relative-name id output)
                          :size (format nil "~dx~d" (or (getf probed :width) 0) (or (getf probed :height) 0))
                          :bytes (getf probed :bytes) :layout layout :advisory advisory
                          :cover (and (probe-file cover) (relative-name id cover)))
                    (let ((failed (job-file id (format nil "failed/~a.~a" (getf target :id) extension))))
                      (ensure-directories-exist failed)
                      (ignore-errors (rename-file output failed))
                      (list :id (getf target :id) :status "failed" :failed_file (relative-name id failed)
                            :problems problems :layout layout))))))))))

(defun rows-json (entries)
  (json-array (loop for entry in entries
               collect (obj "target" (getf entry :id) "status" (getf entry :status)
                            "file" (getf entry :file) "failed_file" (getf entry :failed_file)
                            "size" (getf entry :size) "layout" (getf entry :layout)
                            "problems" (and (getf entry :problems) (json-array (getf entry :problems)))
                            "advisory" (and (getf entry :advisory) (json-array (getf entry :advisory)))
                            "note" (getf entry :note)))))

(defun text-files (id)
  (let ((directory (job-file id "ready/")))
    (and (probe-file directory)
         (loop for file in (uiop:directory-files directory "*.txt")
               collect (relative-name id file)))))

(defun finish (id entries)
  "The manifest, the page, and the answer MAKE and REDO share."
  (let* ((manifest (load-manifest id))
         (old (getf manifest :targets))
         (merged (append (remove-if (lambda (entry) (find (getf entry :id) entries
                                                          :key (lambda (row) (getf row :id))
                                                          :test #'string=))
                                    old)
                         entries))
         (ready (find-if (lambda (entry) (string= (getf entry :status) "ready")) entries)))
    (setf (getf manifest :targets) merged)
    (save-manifest id manifest)
    (write-board id manifest)
    (obj "job" id
         "job_dir" (uiop:native-namestring (job-root id))
         "targets" (rows-json entries)
         "files" (and (null entries) (json-array (text-files id)))
         "board" (relative-name id (job-file id "board.html"))
         "media_line" (and ready (format nil "MEDIA:~a" (namestring (job-file id (getf ready :file)))))
         "note" (and (null entries)
                      "This job is text only, so there is nothing to render; the copy files are already in ready/."))))

(defun selected-plans (plans wanted)
  "The plans WANTED names, by target id or platform; every plan when it names none."
  (if (null wanted)
      plans
      (let ((wanted (know-wanted wanted)))
        (remove-if-not (lambda (plan)
                         (or (member (getf plan :id) wanted :test #'string=)
                             (member (getf plan :platform) wanted :test #'string=)))
                       plans))))

(defun tool-make (args)
  "Render the planned targets, check each file and put only passing ones in ready/."
  (let* ((id (open-job (arg-string args "job")))
         (manifest (load-manifest id))
         (scan (load-scan id))
         (all (getf scan :targets))
         (plans (selected-plans all (arg-list args "targets")))
         (overrides (arg-object args "overrides")))
    (cond ((null all) (finish id nil))
          ((null plans) (error "no planned target matches that; planned: ~{~a~^, ~}"
                               (mapcar (lambda (plan) (getf plan :id)) all)))
          (t (finish id (loop for plan in plans
                              collect (build-target id manifest plan overrides)))))))

(defun override-json (args)
  "REDO's own members as the one override object MAKE takes per target."
  (let ((layout (arg-string args "layout"))
        (focus (arg-string args "focus"))
        (cover (nlk:json-value args :number "cover_at"))
        (trim (nlk:json-value args :array "trim")))
    (nlk:make-json-object "layout" (and (plusp (length layout)) layout)
                          "focus" (and (plusp (length focus)) focus)
                          "cover_at" cover
                          "trim" trim)))

(defun tool-redo (args)
  "Render one target again with another layout, focus, trim or cover frame."
  (let* ((id (open-job (arg-string args "job")))
         (target (arg-string args "target"))
         (scan (load-scan id))
         (plan (find (canonical-name target) (getf scan :targets)
                     :key (lambda (entry) (getf entry :id)) :test #'string=)))
    (when (zerop (length (nlk:trimmed target)))
      (error "argument 'target' is required; give the target id, for example tiktok"))
    (unless plan (error "target ~a is not planned in this job" target))
    (let* ((manifest (load-manifest id))
           (overrides (nlk:make-json-object (getf plan :id) (override-json args)))
           (entry (build-target id manifest plan overrides)))
      (finish id (list entry)))))

;;;; --- check ------------------------------------------------------------------

(defun check-json (rows pass target path)
  (obj "target" target "file" path "pass" (if pass t)
       "checks" (json-array (loop for row in rows
                             collect (obj "id" (getf row :id) "status" (getf row :status)
                                          "measured" (princ-to-string (or (getf row :measured) ""))
                                          "limit" (princ-to-string (or (getf row :limit) ""))
                                          "detail" (getf row :detail))))))

(defun tool-check (args)
  "Hold a file the user brings from elsewhere against one target, without rendering."
  (let ((path (arg-string args "path"))
        (target (arg-string args "target")))
    (when (zerop (length (nlk:trimmed path))) (error "give the file's path"))
    (when (zerop (length (nlk:trimmed target))) (error "give the target id, for example x_video"))
    (let* ((spec (or (target-spec target) (error "unknown target ~s" target)))
           (info (probe-input (uiop:parse-native-namestring path) "check")))
      (multiple-value-bind (rows pass) (check-file info spec)
        (check-json rows pass (getf spec :id) (getf info :path))))))

;;;; --- the operator's overview -------------------------------------------------

(defun overview (&optional filter-text)
  "What the cell makes, and every target with its size and main limits."
  (let ((wanted (loop for word in (ppcre:split "[ ,]+" (string-downcase (or filter-text "")))
                       when (plusp (length word)) collect (canonical-name word)))
        (lines '()))
    (dolist (entry *platforms*)
      (let ((name (first entry))
            (label (second entry)))
        (when (or (null wanted) (member name wanted :test #'string=))
          (let ((rules (platform-rules name)))
            (push (format nil "~a: text ~d ~a~@[ (up to ~d hashtags)~]~@[ (up to ~d mentions)~]"
                          label (getf rules :limit) (getf rules :unit)
                          (getf rules :hashtags) (getf rules :mentions))
                  lines))
          (dolist (target (all-targets))
            (when (string= (getf target :platform) name)
              (push (format nil "  ~a: ~dx~d ~a~@[ (~,0f-~,0fs)~]~@[ (up to ~,0f bytes)~]"
                            (getf target :id) (getf target :width) (getf target :height)
                            (getf target :kind)
                            (limit-of target :duration-min) (limit-of target :duration-max)
                            (limit-of target :bytes-max))
                    lines))))))
    (if lines
        (format nil "~{~a~^~%~}" (nreverse lines))
        (format nil "no platform matches ~s; one of ~{~a~^, ~}" filter-text (platform-names)))))

(defun usage-text ()
  "The /postkitty answer with no argument: what it does, then every target."
  (format nil "postkitty: one post in, every platform out.~%~%\
Give it a draft, photos or a landscape video: it fixes the wording and writes a version for each\n\
platform, crops the photos and reframes the video, and checks every file against the platform's\n\
own limits. It never posts or uploads; the files in ready/ are the user's to upload.~%~%~a"
          (overview)))
