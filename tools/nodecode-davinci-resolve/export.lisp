;;;; export.lisp --- what works without Resolve running: scan, FCPXML, marker CSV.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Free DaVinci Resolve cannot be driven through its scripting API, and the
;;;; original plugin answers that by writing interchange files the operator
;;;; imports through Resolve's own File > Import dialog. These are those
;;;; writers, in Lisp, with no interpreter and no Resolve on the machine.

(in-package #:nodecode-davinci-resolve)

;;; --- the marker, as the one declaration ------------------------------------

(defparameter +marker-colors+
  '("Blue" "Cyan" "Green" "Yellow" "Red" "Pink" "Purple" "Fuchsia" "Rose"
    "Lavender" "Sky" "Mint" "Lemon" "Sand" "Cocoa")
  "The colours Resolve names a timeline marker by, as its own menus list them.")

(nlk:define-contract marker
  (:doc "One marker for a timeline or a marker manifest: where it sits, what it says, how long it holds.")
  ("frame" :integer :required t
           :doc "the timeline frame the marker sits on, from the timeline's first frame")
  ("name" :text :default "Marker" :doc "the marker's title, what Resolve lists")
  ("color" :string :default "Blue" :options +marker-colors+
           :doc "Blue, Cyan, Green, Yellow, Red, Pink, Purple or another of Resolve's marker colours")
  ("note" :string :default "" :doc "the marker's note, what the operator reads in the marker list")
  ("duration" :integer :default 1 :doc "how many frames it spans; 1 is a point marker"))

(defun marker-fields (markers)
  "MARKERS as (:FRAME :NAME :COLOR :NOTE :DURATION) plists: a MARKER record,
 a plist, or a JSON object read leniently, so both the model and a Lisp
 caller reach the same writers."
  (loop for item across (coerce markers 'vector)
        collect (cond ((typep item 'marker)
                       (list :frame (marker-frame item) :name (marker-name item)
                             :color (marker-color item) :note (marker-note item)
                             :duration (marker-duration item)))
                      ((hash-table-p item)
                       (list :frame (or (nlk:json-value item :integer "frame") 0)
                             :name (or (nlk:json-value item :string "name") "Marker")
                             :color (or (nlk:json-value item :string "color") "Blue")
                             :note (or (nlk:json-value item :string "note") "")
                             :duration (or (nlk:json-value item :integer "duration") 1)))
                      (t (list :frame (or (getf item :frame) 0)
                               :name (or (getf item :name) "Marker")
                               :color (or (getf item :color) "Blue")
                               :note (or (getf item :note) "")
                               :duration (or (getf item :duration) 1))))))

;;; --- media ----------------------------------------------------------------

(defparameter +video-extensions+
  '("3g2" "3gp" "avi" "braw" "cin" "crm" "dv" "flv" "m2ts" "m4v" "mkv"
    "mov" "mp4" "mpeg" "mpg" "mxf" "r3d" "ts" "vob" "webm")
  "The video containers the original plugin scans for, as file-name types.")

(defparameter +audio-extensions+
  '("aac" "aif" "aiff" "bwf" "flac" "m4a" "mp3" "ogg" "wav" "wma")
  "The audio containers it scans for.")

(defparameter +image-extensions+
  '("ari" "arw" "bmp" "cr2" "cr3" "dng" "dpx" "exr" "gif" "heic" "jpeg"
    "jpg" "nef" "png" "psd" "raf" "tif" "tiff")
  "The still formats it scans for.")

(defun media-kind (path)
  "PATH's kind: \"video\", \"audio\", \"image\" or NIL for anything else."
  (let ((type (string-downcase (or (pathname-type path) ""))))
    (cond ((member type +video-extensions+ :test #'string=) "video")
          ((member type +audio-extensions+ :test #'string=) "audio")
          ((member type +image-extensions+ :test #'string=) "image"))))

(defun media-files (folder &key extensions)
  "Every media file under FOLDER, recursively, as (:PATH :KIND :BYTES), by
 name. EXTENSIONS, when given, keeps only those file-name types."
  (let* ((dir (uiop:ensure-directory-pathname (expand-file-name folder)))
         (pattern (concatenate 'string (string-right-trim "/" (namestring dir)) "/**/*.*"))
         (found (sort (remove-if-not #'uiop:file-exists-p (directory pattern))
                      #'string< :key #'namestring)))
    (loop for file in found
          for kind = (media-kind file)
          when (and kind
                    (or (null extensions)
                        (member (string-downcase (or (pathname-type file) ""))
                                extensions :test #'string=)))
            collect (list :path (uiop:native-namestring file)
                          :kind kind :bytes (file-bytes file)))))

(defun file-bytes (file)
  "FILE's size in bytes, or NIL when it cannot be read."
  (ignore-errors
   (with-open-file (stream file :element-type '(unsigned-byte 8))
     (file-length stream))))

;;; --- where an export lands -------------------------------------------------

(defun expand-file-name (text)
  "TEXT with a leading ~ or ~/ read against this user's home directory."
  (let ((text (if (pathnamep text) (uiop:native-namestring text) (string text))))
    (if (and (plusp (length text)) (char= #\~ (char text 0)))
        (uiop:native-namestring
         (merge-pathnames (if (> (length text) 2) (subseq text 2) "")
                          (uiop:ensure-directory-pathname (user-homedir-pathname))))
        text)))

(defun export-directory ()
  "The directory exports are written to: the section's export_dir, else a
 Documents folder of this cell's own."
  (let ((text (expand-file-name (or (setting :export-dir) ""))))
    (if (plusp (length text))
        (uiop:ensure-directory-pathname text)
        (uiop:ensure-directory-pathname (nlk:home "Documents/Nodecode Resolve Exports")))))

(defun safe-stem (text)
  "TEXT fit for a file name: letters and digits kept, the rest one underscore."
  (let ((stem (string-trim "_"
                            (map 'string (lambda (ch)
                                           (if (or (alphanumericp ch) (member ch '(#\- #\_))) ch #\_))
                                 (string text)))))
    (if (plusp (length stem)) stem "resolve")))

(defun export-path (name suffix output-path)
  "Where an export named NAME with SUFFIX lands: OUTPUT-PATH when given, else
 EXPORT-DIRECTORY under a safe stem. The parent exists afterwards."
  (let ((path (if (and output-path (plusp (length (string output-path))))
                  (expand-file-name output-path)
                  (namestring (merge-pathnames (format nil "~a~a" (safe-stem name) suffix)
                                               (export-directory))))))
    (ensure-directories-exist path)
    path))

;;; --- FCPXML ----------------------------------------------------------------

(defun xml-escape (text)
  "TEXT with the five XML sigils replaced, so a title cannot open an element."
  (with-output-to-string (out)
    (loop for ch across (string text)
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&apos;" out))
               (t (write-char ch out))))))

(defun percent-encode (text)
  "TEXT with the characters a file URI may not carry spelled %XX."
  (with-output-to-string (out)
    (loop for ch across text
          do (case ch
               (#\Space (write-string "%20" out))
               (#\# (write-string "%23" out))
               (#\? (write-string "%3F" out))
               (#\% (write-string "%25" out))
               (t (write-char ch out))))))

(defun file-uri (path)
  "PATH as an absolute file:// URI, the form an FCPXML asset's src takes."
  (format nil "file://~a"
          (percent-encode
           (namestring (uiop:ensure-absolute-pathname
                        (uiop:parse-native-namestring path) (uiop:getcwd))))))

(defun fcpx-time (frames frame-rate)
  "FRAMES as the rational time FCPXML counts in: 120/24s."
  (format nil "~d/~ds" (round frames) (round frame-rate)))

(defun write-fcpxml (&key name media-paths (markers nil) (output-path nil)
                          (frame-rate nil) (clip-seconds 5.0) (width 1920) (height 1080))
  "Write an FCPXML 1.10 timeline NAME of MEDIA-PATHS, one clip each of
 CLIP-SECONDS, with MARKERS placed on the clip whose span holds them.
 => the path written."
  (let* ((rate (or frame-rate (setting :frame-rate)))
         (paths (mapcar #'expand-file-name media-paths))
         (clip-frames (max 1 (round (* clip-seconds rate))))
         (destination (export-path name ".fcpxml" output-path))
         (marks (marker-fields markers)))
    (unless paths
      (resolve-error "an FCPXML timeline needs at least one media path"))
    (with-open-file (out destination :direction :output :if-exists :supersede
                                       :if-does-not-exist :create :external-format :utf-8)
      (macrolet ((line (control &rest args)
                   `(format out "~a~%" (format nil ,control ,@args))))
        (line "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
        (line "<!DOCTYPE fcpxml>")
        (line "<fcpxml version=\"1.10\">")
        (line "  <resources>")
        (line "    <format id=\"r1\" name=\"FFVideoFormat~dp~d\" frameDuration=\"~a\" width=\"~d\" height=\"~d\" colorSpace=\"1-1-1 (Rec. 709)\"/>"
              height rate (fcpx-time 1 rate) width height)
        (loop for path in paths
              for index from 2
              do (line "    <asset id=\"r~d\" name=\"~a\" src=\"~a\" start=\"0s\" duration=\"~a\" hasVideo=\"1\" hasAudio=\"1\" audioSources=\"1\" audioChannels=\"2\" audioRate=\"48000\"/>"
                       index
                       (xml-escape (pathname-name (uiop:parse-native-namestring path)))
                       (xml-escape (file-uri path))
                       (fcpx-time clip-frames rate)))
        (line "  </resources>")
        (line "  <library>")
        (line "    <event name=\"Nodecode Interchange\">")
        (line "      <project name=\"~a\">" (xml-escape name))
        (line "        <sequence format=\"r1\" duration=\"~a\" tcStart=\"0s\" tcFormat=\"NDF\">"
              (fcpx-time (* clip-frames (length paths)) rate))
        (line "          <spine>")
        (loop for path in paths
              for index from 2
              for offset = (* (- index 2) clip-frames)
              do (progn
                   (line "            <asset-clip name=\"~a\" ref=\"r~d\" offset=\"~a\" start=\"0s\" duration=\"~a\">"
                         (xml-escape (pathname-name (uiop:parse-native-namestring path)))
                         index (fcpx-time offset rate) (fcpx-time clip-frames rate))
                   (loop for mark in marks
                         for frame = (getf mark :frame)
                         when (<= offset frame (+ offset (1- clip-frames)))
                           do (line "              <marker start=\"~a\" value=\"~a\" note=\"~a\"/>"
                                    (fcpx-time (- frame offset) rate)
                                    (xml-escape (getf mark :name))
                                    (xml-escape (getf mark :note))))
                   (line "            </asset-clip>")))
        (line "          </spine>")
        (line "        </sequence>")
        (line "      </project>")
        (line "    </event>")
        (line "  </library>")
        (line "</fcpxml>")))
    destination))

;;; --- marker CSV -------------------------------------------------------------

(defparameter +marker-csv-header+
  '("frame" "timecode_seconds" "name" "color" "note" "duration")
  "The manifest's columns, in the order the original plugin wrote them.")

(defun csv-field (text)
  "TEXT as one CSV field: quoted when it carries a comma, a quote or a newline."
  (let ((text (string text)))
    (if (find-if (lambda (ch) (member ch '(#\, #\" #\Newline #\Return))) text)
        (format nil "\"~a\"" (with-output-to-string (out)
                                  (loop for ch across text
                                        do (if (char= ch #\") (write-string "\"\"" out) (write-char ch out)))))
        text)))

(defun write-marker-csv (&key name (markers nil) (output-path nil) (frame-rate nil))
  "Write the marker manifest NAME as CSV, one row a marker, with its frame,
 its seconds at FRAME-RATE, its colour, note and duration. => the path written."
  (let ((rate (or frame-rate (setting :frame-rate)))
        (destination (export-path name ".markers.csv" output-path)))
    (with-open-file (out destination :direction :output :if-exists :supersede
                                       :if-does-not-exist :create :external-format :utf-8)
      (format out "~{~a~^,~}~%" +marker-csv-header+)
      (loop for mark in (marker-fields markers)
            for frame = (max 0 (getf mark :frame))
            do (format out "~{~a~^,~}~%"
                       (mapcar #'csv-field
                               (list (princ-to-string frame)
                                     (format nil "~,3f" (/ frame rate))
                                     (getf mark :name)
                                     (getf mark :color)
                                     (getf mark :note)
                                     (princ-to-string (getf mark :duration)))))))
    destination))
