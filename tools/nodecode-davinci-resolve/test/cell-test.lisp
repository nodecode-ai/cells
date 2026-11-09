;;;; cell-test.lisp --- the resolve cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs DaVinci Resolve, a socket or the network: the live verbs
;;;; answer through a stub bound as *BRIDGE*, and the interchange verbs write
;;;; into a scratch folder and are read back.

(in-package #:nodecode.test)

(define-test-slice "davinci-resolve" "DAVINCI-RESOLVE-CELL-" :start nodecode-davinci-resolve:start-cell)

(define-cell-lifecycle-tests "davinci-resolve"
  (:help :resolve)
  (:command "resolve")
  (:refused ("frame_rate" 0) ("frame_rate" 999) ("frame_rate" "fast"))
  (:idle nodecode-davinci-resolve:resolve-error
         (nodecode-davinci-resolve:status)
         (nodecode-davinci-resolve:scan-media ".")))

;;; --- fixtures ----------------------------------------------------------------

(defun resolve-cell-dir (name)
  "A fresh directory for one test, emptied first."
  (let ((dir (temp-path name)))
    (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))
    (ensure-directories-exist dir)
    dir))

(defun resolve-cell-touch (dir name)
  "Create NAME in DIR, holding a byte, and answer its namestring."
  (namestring (write-temp-file (merge-pathnames name dir) "x")))

(defun resolve-cell-occurrences (needle haystack)
  "How many times NEEDLE appears in HAYSTACK."
  (loop with start = 0
        for at = (search needle haystack :start2 start)
        while at
        do (setf start (+ at (length needle)))
        count 1))

(defun resolve-cell-record (contract-name object)
  "OBJECT as the record CONTRACT-NAME materializes."
  (nlk:contract-decode (nlk:contract contract-name) object))

(defparameter +resolve-cell-status+
  (nlk:json-object
   "ok" t
   "product" "DaVinci Resolve Studio"
   "version" "20.1"
   "page" "Edit"
   "project" "A Film"
   "timelines" (vector "Cut 1" "Cut 2")
   "timeline" (nlk:json-object "name" "Cut 1" "frame_rate" "24" "start" 0 "end" 1200)
   "rendering" nil)
  "What the bridge answers about a project with two timelines.")

;;; --- the live half, answered through a stub --------------------------------------

(deftest davinci-resolve-cell-status-says-what-is-open ()
  (let ((nodecode-davinci-resolve::*bridge* (lambda (op params)
                                               (declare (ignore op params))
                                               +resolve-cell-status+)))
    (with-cell-stop ((davinci-resolve-start))
      (let ((text (cell-entry "nodecode-davinci-resolve" "resolve" "")))
        (is (search "DaVinci Resolve Studio 20.1" text))
        (is (search "project: A Film (2 timelines)" text))
        (is (search "current timeline: Cut 1 at 24 fps, frames 0 to 1200" text))
        (is (not (search "a render is running" text))))
      (is (search "project: A Film" (nodecode-davinci-resolve:status)) "the same verbs answer in eval"))))

(deftest davinci-resolve-cell-a-mutation-reports-its-plan-until-confirmed ()
  (let ((nodecode-davinci-resolve::*bridge*
          (lambda (op params)
            (declare (ignore params))
            (cond ((string= op "import")
                   (nlk:json-object "ok" t "imported" 2 "names" (vector "a" "b")
                                    "missing" (vector "/nowhere/c.mov")))
                  ((string= op "marker")
                   (nlk:json-object "ok" t "added" t "frame" 12 "name" "Beat" "color" "Red"))
                  (t (nlk:json-object "ok" t "job_id" "j-1" "started" t))))))
    (with-cell-stop ((davinci-resolve-start))
      (let ((plan (nodecode-davinci-resolve::tool-import-media
                   (resolve-cell-record 'nodecode-davinci-resolve::import-job
                                        (nlk:json-object "paths" (vector "/nowhere/a.mov"))))))
        (is (search "Would import 1 file" plan))
        (is (search "confirm true" plan) "the model is told how to go ahead"))
      (let ((done (nodecode-davinci-resolve::tool-import-media
                   (resolve-cell-record 'nodecode-davinci-resolve::import-job
                                        (nlk:json-object "paths" (vector "/nowhere/a.mov")
                                                         "confirm" t)))))
        (is (search "Imported 2 items" done))
        (is (search "not found: /nowhere/c.mov" done)))
      (let ((unasked (nodecode-davinci-resolve::tool-add-marker
                      (resolve-cell-record 'nodecode-davinci-resolve::marker-job
                                           (nlk:json-object "frame" 12 "name" "Beat"
                                                            "color" "Red")))))
        (is (search "Would put a Red marker" unasked))
        (is (search "Nothing has changed" unasked)))
      (let ((asked (nodecode-davinci-resolve::tool-add-marker
                    (resolve-cell-record 'nodecode-davinci-resolve::marker-job
                                         (nlk:json-object "frame" 12 "name" "Beat"
                                                          "color" "Red" "confirm" t)))))
        (is (search "Placed the Red marker" asked)))
      (let ((render-plan (nodecode-davinci-resolve::tool-render
                          (resolve-cell-record 'nodecode-davinci-resolve::render-job
                                               (nlk:json-object "target_dir" "/exports"
                                                                "custom_name" "Cut 1")))))
        (is (search "Would render the current timeline into /exports as Cut 1" render-plan)))
      (let ((render-done (nodecode-davinci-resolve::tool-render
                          (resolve-cell-record 'nodecode-davinci-resolve::render-job
                                               (nlk:json-object "target_dir" "/exports"
                                                                "custom_name" "Cut 1"
                                                                "confirm" t)))))
        (is (search "Render job j-1 started" render-done))))))

(deftest davinci-resolve-cell-says-why-when-the-bridge-cannot-run ()
  (with-cell-stop ((davinci-resolve-start "python" "nodecode-no-such-interpreter"))
    (let ((text (refusal-text nodecode-davinci-resolve:resolve-error
                               (nodecode-davinci-resolve:status))))
      (is (search "nodecode-no-such-interpreter" text) text)
      (is (search "davinci-resolve.python" text) "the refusal says what to set"))))

;;; --- the interchange half: files, no Resolve ---------------------------------------

(deftest davinci-resolve-cell-writes-an-fcpxml-timeline ()
  (let* ((dir (resolve-cell-dir "fcpxml"))
         (first (resolve-cell-touch dir "a.mov"))
         (second (resolve-cell-touch dir "b.wav")))
    (with-cell-stop ((davinci-resolve-start "export_dir" (namestring dir)))
      (let ((said (nodecode-davinci-resolve::tool-write-fcpxml
                   (resolve-cell-record 'nodecode-davinci-resolve::fcpxml-job
                                        (nlk:json-object
                                         "name" "Rough Cut 1"
                                         "media_paths" (vector first second)
                                         "markers" (vector (nlk:json-object
                                                            "frame" 30 "name" "Beat"
                                                            "note" "the turn")))))))
        (is (search "Rough_Cut_1.fcpxml" said))
        (is (search "File > Import > Timeline" said))
        (let ((file (uiop:read-file-string (merge-pathnames "Rough_Cut_1.fcpxml" dir))))
          (is (search "<!DOCTYPE fcpxml>" file) "the doctype Resolve expects")
          (is (search "<fcpxml version=\"1.10\">" file))
          (is (= 2 (resolve-cell-occurrences "<asset id=" file)) "one asset a file")
          (is (= 2 (resolve-cell-occurrences "<asset-clip" file)) "one clip a file")
          (is (= 1 (resolve-cell-occurrences "<marker" file)))
          (is (search "<marker start=\"30/24s\" value=\"Beat\" note=\"the turn\"/>" file))
          (is (search "file://" file) "assets carry a file URI")
          (is (search "duration=\"240/24s\"" file) "the sequence is as long as its clips"))))))

(deftest davinci-resolve-cell-writes-the-marker-csv ()
  (let ((dir (resolve-cell-dir "markers")))
    (with-cell-stop ((davinci-resolve-start "export_dir" (namestring dir)))
      (let ((said (nodecode-davinci-resolve::tool-write-marker-csv
                   (resolve-cell-record 'nodecode-davinci-resolve::marker-csv-job
                                        (nlk:json-object
                                         "name" "Review Notes"
                                         "markers" (vector
                                                    (nlk:json-object "frame" 0 "name" "Marker")
                                                    (nlk:json-object "frame" 12 "name" "Beat"
                                                                     "note" "a, b"
                                                                     "color" "Red"
                                                                     "duration" 2)))))))
        (is (search "Review_Notes.markers.csv" said))
        (let* ((path (merge-pathnames "Review_Notes.markers.csv" dir))
               (lines (uiop:read-file-lines path)))
          (is (equal "frame,timecode_seconds,name,color,note,duration" (first lines)))
          (is (equal "0,0.000,Marker,Blue,,1" (second lines)))
          (is (equal "12,0.500,Beat,Red,\"a, b\",2" (third lines)) "a note with a comma is one quoted field")
          (is (= 3 (length lines))))))))

(deftest davinci-resolve-cell-scans-a-media-folder ()
  (let ((dir (resolve-cell-dir "scan")))
    (resolve-cell-touch dir "a.mov")
    (resolve-cell-touch dir "b.wav")
    (resolve-cell-touch dir "notes.txt")
    (with-cell-stop ((davinci-resolve-start))
      (let ((text (nodecode-davinci-resolve::tool-scan-media
                   (resolve-cell-record 'nodecode-davinci-resolve::scan-folder
                                        (nlk:json-object "folder" (namestring dir))))))
        (is (search "2 media files" text))
        (is (search "1 video" text))
        (is (search "1 audio" text))
        (is (not (search "notes.txt" text)) "a text file is not media"))
      (let ((only-video (nodecode-davinci-resolve::tool-scan-media
                          (resolve-cell-record 'nodecode-davinci-resolve::scan-folder
                                               (nlk:json-object "folder" (namestring dir)
                                                                "extensions" (vector "mov"))))))
        (is (search "1 media file" only-video))
        (is (not (search "b.wav" only-video)))))))

;;; --- the boundary ---------------------------------------------------------------

(deftest davinci-resolve-cell-a-call-missing-a-required-member-is-refused ()
  (let ((text (refusal-text nlk:contract-refusal
                             (resolve-cell-record 'nodecode-davinci-resolve::marker-job
                                                  (nlk:json-object "name" "Beat")))))
    (is (search "frame" text) text))
  (let ((text (refusal-text nlk:contract-refusal
                             (resolve-cell-record 'nodecode-davinci-resolve::import-job
                                                  (nlk:json-object "bin" "B-roll")))))
    (is (search "paths" text) text)))
