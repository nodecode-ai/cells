;;;; corpus.lisp --- the corpus, its chunkers and its two search lanes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original keeps its corpus in SQLite and its vectors in an ONNX
;;;; embedding tower. This rewrite keeps the same shape of thing -- rows of
;;;; (uri, kind, span, snippet) -- in one readable file, and ranks with two
;;;; lanes fused the way the original fuses them: a keyword lane (BM25 over
;;;; words) and a fuzzy lane (cosine over hashed character 4-grams, so a
;;;; misspelling or a different ending still matches). No model is loaded and
;;;; nothing leaves the machine.

(in-package #:nodecode-mediarag)

(defparameter +space+ '(#\Space #\Tab #\Newline #\Return #\Page))

(defparameter +text-extensions+ '("md" "markdown" "txt" "text" "org" "rst" "csv"
                                  "html" "htm" "lisp" "asd" "json" "yaml" "yml" "log")
  "Formats read as text. The original's pdf and office lanes needed pandoc and
pdftotext; a document is indexed here when its text is already its bytes.")

(defparameter +subtitle-extensions+ '("srt" "vtt"))
(defparameter +image-extensions+ '("png" "jpg" "jpeg" "webp" "gif"))
(defparameter +audio-extensions+ '("mp3" "m4a" "ogg" "flac" "aac" "opus"))
(defparameter +audio-window+ 11.2
  "Seconds per audio row, as the original's 179200 samples at 16 kHz.")
(defparameter +subtitle-chunk+ 600)

(defun index-kinds-p (extension)
  "Whether some lane here reads EXTENSION."
  (or (member extension +text-extensions+ :test #'string=)
      (member extension +subtitle-extensions+ :test #'string=)
      (member extension +image-extensions+ :test #'string=)
      (member extension +audio-extensions+ :test #'string=)
      (string= extension "wav")))

;;; --- the corpus file ---------------------------------------------------------

(defun corpus-root ()
  "The directory the corpus lives in, as a pathname ending in a slash."
  (uiop:ensure-directory-pathname (uiop:parse-native-namestring (setting :corpus-root))))

(defun store-path ()
  "The one file the corpus is: rows of plists, readable and greppable."
  (merge-pathnames "corpus.sexp" (corpus-root)))

(defun read-corpus ()
  "The corpus's rows in the order they were indexed, or NIL when there is none yet."
  (let ((path (store-path)))
    (if (probe-file path)
        (with-open-file (in path :direction :input :external-format :utf-8)
          (with-standard-io-syntax
            (let ((*read-eval* nil))
              (read in nil))))
        nil)))

(defun write-corpus (rows)
  "Write ROWS as the corpus: a temporary file renamed over the old one."
  (let ((path (store-path)))
    (ensure-directories-exist path)
    (let ((tmp (merge-pathnames "corpus.sexp.tmp" (corpus-root))))
      (with-open-file (out tmp :direction :output :if-exists :supersede :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*print-readably* t) (*print-pretty* t) (*print-length* nil) (*print-level* nil))
            (prin1 rows out)
            (terpri out))))
      (rename-file tmp path))
    rows))

(defun replace-uri (rows uri new)
  "ROWS without URI's own, NEW appended."
  (append (remove-if (lambda (row) (equal uri (getf row :uri))) rows) new))

;;; --- reading and chunking ----------------------------------------------------

(defun replace-all (text from to)
  "TEXT with every FROM replaced by TO."
  (with-output-to-string (out)
    (loop with start = 0 with size = (length text) with span = (length from)
          for at = (search from text :start2 start)
          do (cond (at (write-string text out :start start :end at)
                       (write-string to out)
                       (setf start (+ at span)))
                   (t (write-string text out :start start :end size) (return))))))

(defun strip-tags (text)
  "TEXT with HTML tags dropped: a document's text is what a reader would see."
  (let ((plain (with-output-to-string (out)
                 (loop with in-tag = nil
                       for char across text
                       do (cond ((char= char #\<) (setf in-tag t))
                                ((char= char #\>) (setf in-tag nil) (write-char #\Space out))
                                ((not in-tag) (write-char char out)))))))
    (dolist (pair '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                    ("&quot;" . "\"") ("&#39;" . "'") ("&nbsp;" . " ")))
      (setf plain (replace-all plain (car pair) (cdr pair))))
    plain))

(defun whitespace-p (char)
  (and char (member char +space+)))

(defun chunk-text (text limit)
  "TEXT as (START END CHUNK) rows of at most LIMIT characters, cut on whitespace,
so every span is a real offset into the text."
  (let ((size (length text)) (start 0) (rows '()))
    (loop while (< start size)
          do (let* ((hard (min size (+ start limit)))
                    (space (and (< hard size) (position-if #'whitespace-p text :start start :end hard :from-end t)))
                    (cut (if (and space (> space start)) space hard))
                    (chunk (string-trim +space+ (subseq text start cut))))
               (unless (zerop (length chunk))
                 (push (list (coerce start 'single-float) (coerce cut 'single-float) chunk) rows))
               (setf start (or (position-if-not #'whitespace-p text :start cut) size))))
    (nreverse rows)))

(defun clean-line (line)
  (string-trim +space+ line))

(defun cue-time (text)
  "Seconds from a cue timestamp like 00:01:23,400, or NIL."
  (let ((parts (uiop:split-string (substitute #\. #\, (or text "")) :separator ":")))
    (when (<= 2 (length parts) 3)
      (let ((numbers (mapcar (lambda (part) (ignore-errors (read-from-string (clean-line part)))) parts)))
        (when (every #'realp numbers)
          (let* ((in-order (if (= 3 (length numbers)) numbers (cons 0 numbers)))
                 (hours (first in-order)) (minutes (second in-order)) (seconds (third in-order)))
            (when (and (< hours 10000) (< minutes 60) (< seconds 60))
              (+ (* 3600 hours) (* 60 minutes) seconds))))))))

(defun subtitle-chunks (text &optional (limit +subtitle-chunk+))
  "TEXT as (START END CHUNK) cues grouped to LIMIT characters, with real spans."
  (let ((rows '()) (buffer "") (start nil) (finish 0.0))
    (flet ((flush ()
             (when (plusp (length buffer))
               (push (list (or start 0.0) finish buffer) rows)
               (setf buffer "" start nil))))
      (dolist (block (uiop:split-string (replace-all text (string #\Return) "")
                                         :separator (format nil "~%~%")))
        (let ((times nil) (lines '()))
          (dolist (line (uiop:split-string block :separator (string #\Newline)))
            (let ((arrow (search "-->" line)))
              (cond (arrow (let ((from (cue-time (subseq line 0 arrow)))
                                 (to (cue-time (subseq line (+ arrow 3)))))
                             (when (and from to) (setf times (list from to)))))
                    ((and times (plusp (length (clean-line line))))
                     (push (clean-line line) lines)))))
          (when (and times lines)
            (let ((content (format nil "~{~a~^ ~}" (nreverse lines))))
              (when (and (plusp (length buffer)) (> (+ (length buffer) (length content) 1) limit))
                (flush))
              (setf buffer (if (zerop (length buffer)) content (format nil "~a~%~a" buffer content)))
              (setf start (or start (first times)))
              (setf finish (second times)))))))
    (flush)
    (nreverse rows)))

;;; --- media --------------------------------------------------------------------

(defun read-u32 (in)
  "One little-endian 32-bit word from IN."
  (logior (read-byte in) (ash (read-byte in) 8) (ash (read-byte in) 16) (ash (read-byte in) 24)))

(defun read-chunk-id (in)
  "Four bytes of IN as a string, or NIL at end of file."
  (let ((id (make-string 4)))
    (dotimes (index 4)
      (let ((byte (read-byte in nil nil)))
        (unless byte (return-from read-chunk-id nil))
        (setf (char id index) (code-char byte))))
    id))

(defun wav-scan (in)
  "Seconds of the WAVE stream IN, from its fmt and data chunks."
  (let ((byte-rate nil) (found nil))
    (loop for id = (read-chunk-id in)
          while id
          do (let ((size (read-u32 in)))
               (cond ((string= id "fmt ")
                      (read-u32 in)                         ; format and channels
                      (read-u32 in)                         ; sample rate
                      (setf byte-rate (read-u32 in))
                      (file-position in (+ (file-position in) (max 0 (- size 12)))))
                     ((string= id "data")
                      (setf found size)
                      (return))
                     (t (file-position in (+ (file-position in) size))))))
    (when (and found byte-rate (plusp byte-rate))
      (/ found byte-rate))))

(defun wav-duration (path)
  "Seconds of a RIFF/WAVE file from its own header, or NIL when it is not one."
  (handler-case
      (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
        (let ((header (make-array 12 :element-type '(unsigned-byte 8))))
          (read-sequence header in)
          (when (equalp (subseq header 8 12)
                        (map '(vector (unsigned-byte 8)) #'char-code "WAVE"))
            (wav-scan in))))
    (error () nil)))

(defun label-row (uri kind name)
  (list (list :uri uri :kind kind :start 0.0 :end 0.0 :snippet name)))

(defun audio-rows (uri path extension)
  "One row per window of an audio file: WAV from its header, everything else by name."
  (let* ((name (file-namestring path))
         (duration (and (string= extension "wav") (wav-duration path))))
    (if (and duration (>= duration +audio-window+))
        (loop for start from 0.0 below duration by +audio-window+
              for end = (min duration (+ start +audio-window+))
              collect (list :uri uri :kind "audio" :start (coerce start 'single-float)
                            :end (coerce end 'single-float)
                            :snippet (format nil "~a [~,1f-~,1fs]" name start end)))
        (list (list :uri uri :kind "audio" :start 0.0
                    :end (coerce (or duration 0.0) 'single-float) :snippet name)))))

;;; --- one file into rows -------------------------------------------------------

(defun path-extension (path)
  (string-downcase (or (pathname-type path) "")))

(defun text-rows (uri path extension)
  (let* ((raw (uiop:read-file-string path))
         (text (if (member extension '("html" "htm") :test #'string=) (strip-tags raw) raw))
         (limit (setting :max-chunk-chars)))
    (loop for (start end chunk) in (chunk-text text limit)
          collect (list :uri uri :kind "text" :start start :end end :snippet chunk))))

(defun index-file (path)
  "PATH's rows, or NIL when no lane here reads that format."
  (let* ((true-path (truename path))
         (uri (namestring true-path))
         (extension (path-extension true-path)))
    (cond
      ((member extension +subtitle-extensions+ :test #'string=)
       (loop for (start end chunk) in (subtitle-chunks (uiop:read-file-string true-path))
             collect (list :uri uri :kind "text" :start (coerce start 'single-float)
                           :end (coerce end 'single-float) :snippet chunk)))
      ((member extension +text-extensions+ :test #'string=) (text-rows uri true-path extension))
      ((member extension +image-extensions+ :test #'string=)
       (label-row uri "image" (file-namestring true-path)))
      ((or (member extension +audio-extensions+ :test #'string=) (string= extension "wav"))
       (audio-rows uri true-path extension))
      (t nil))))

(defun list-files (directory)
  "Every regular file under DIRECTORY, depth first, dot directories skipped."
  (labels ((walk (dir)
             (append (uiop:directory-files dir)
                     (loop for sub in (uiop:subdirectories dir)
                           for name = (first (last (pathname-directory sub)))
                           unless (and (stringp name) (plusp (length name)) (char= #\. (char name 0)))
                             append (walk sub)))))
    (walk (uiop:ensure-directory-pathname directory))))

;;; --- adding -------------------------------------------------------------------

(defun expand-path (text)
  "TEXT as a pathname, a leading tilde expanded."
  (let ((text (if (and (plusp (length text)) (char= #\~ (char text 0)))
                  (namestring (merge-pathnames (subseq text (min 2 (length text)))
                                                (user-homedir-pathname)))
                  text)))
    (uiop:parse-native-namestring text)))

(defun index-paths (paths &optional (existing (read-corpus)))
  "Index PATHS into EXISTING; answers (values ROWS ADDED SKIPPED)."
  (let ((rows existing) (added 0) (skipped 0))
    (dolist (path paths)
      (let ((new (index-file path)))
        (cond (new (setf rows (replace-uri rows (namestring (truename path)) new)) (incf added))
              (t (incf skipped)))))
    (write-corpus rows)
    (values rows added skipped)))

(defun add-note (text)
  "TEXT as a note row, searchable at once."
  (let* ((text (string-trim +space+ (or text "")))
         (uri (format nil "note://~a-~a" (get-universal-time) (mod (sxhash text) 100000))))
    (when (zerop (length text)) (error "a note needs some text"))
    (let ((rows (loop for (start end chunk) in (chunk-text text (setting :max-chunk-chars))
                      collect (list :uri uri :kind "text" :start start :end end :snippet chunk))))
      (write-corpus (append (read-corpus) rows))
      (format nil "mediarag: noted ~d row~:p under ~a" (length rows) uri))))

(defun ingest-dir (path)
  "Index every readable file under PATH."
  (let ((directory (uiop:ensure-directory-pathname (expand-path path))))
    (unless (probe-file directory) (error "no such directory: ~a" path))
    (let ((files (remove-if-not (lambda (file) (index-kinds-p (path-extension file)))
                                (list-files directory))))
      (multiple-value-bind (rows added skipped) (index-paths files)
        (format nil "mediarag: indexed ~d of ~d files (~d skipped) under ~a; the corpus now holds ~d rows over ~d sources"
                added (+ added skipped) skipped (namestring directory) (length rows)
                (length (remove-duplicates (mapcar (lambda (row) (getf row :uri)) rows) :test #'equal)))))))

;;; --- one URL in ---------------------------------------------------------------

(defun http-call (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; a test stubs this name."
  (apply #'nlk:http method url arguments))

(defun short-hash (text)
  (format nil "~8,'0X" (mod (sxhash text) (expt 16 8))))

(defun safe-name (url)
  "URL's last path segment as a file name, or bin."
  (let* ((tail (car (last (uiop:split-string url :separator "/"))))
         (clean (string-downcase (map 'string (lambda (char)
                                                (if (or (alphanumericp char)
                                                        (member char '(#\. #\- #\_)))
                                                    char #\_))
                                      tail))))
    (cond ((zerop (length clean)) "bin")
          ((find #\. clean) clean)
          (t (concatenate 'string clean ".bin")))))

(defun fetch-url (url)
  "URL's body written into the corpus cache; answers its pathname."
  (multiple-value-bind (body status) (http-call :get url :timeout 120)
    (unless (and (integerp status) (<= 200 status 299))
      (error "~a answered ~a" url status))
    (let ((path (merge-pathnames (format nil "cache/~a-~a" (short-hash url) (safe-name url))
                                 (corpus-root))))
      (ensure-directories-exist path)
      (if (stringp body)
          (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
            (write-string body out))
          (with-open-file (out path :direction :output :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
            (write-sequence body out)))
      path)))

(defun add-source (source)
  "A local file, a local directory, or an http(s) URL."
  (let* ((source (string-trim +space+ (or source "")))
         (remote (or (uiop:string-prefix-p "http://" source)
                     (uiop:string-prefix-p "https://" source))))
    (cond
      (remote
       (let ((path (fetch-url source)))
         (multiple-value-bind (rows added skipped) (index-paths (list path))
           (declare (ignore skipped))
           (if (plusp added)
               (format nil "mediarag: fetched ~a, indexed ~d rows; the corpus holds ~d rows"
                       source added (length rows))
               (format nil "mediarag: fetched ~a, but neither its bytes nor its extension is a format this cell reads"
                       source)))))
      ((null (probe-file (expand-path source)))
       (format nil "mediarag: no such file or directory: ~a" source))
      ((uiop:directory-pathname-p (probe-file (expand-path source))) (ingest-dir source))
      (t (index-paths (list (expand-path source)))
         (format nil "mediarag: indexed ~a" source)))))

;;; --- searching ----------------------------------------------------------------

(defun terms (text)
  "TEXT's words, downcased, two letters and up."
  (let ((words '()) (start nil) (size (length text)))
    (loop for index from 0 to size
          for char = (and (< index size) (char text index))
          for alnum = (and char (alphanumericp char))
          do (cond ((and alnum (null start)) (setf start index))
                   ((and (null alnum) start)
                    (let ((word (string-downcase (subseq text start index))))
                      (when (<= 2 (length word)) (push word words)))
                    (setf start nil))))
    (nreverse words)))

(defun grams (text)
  "TEXT's character 4-grams, downcased."
  (let* ((clean (string-downcase (string-trim +space+ (or text ""))))
         (size (length clean)))
    (cond ((zerop size) '())
          ((< size 4) (list clean))
          (t (loop for index from 0 to (- size 4) collect (subseq clean index (+ index 4)))))))

(defun vectorise (grams &aux (vector (make-array 256 :initial-element 0.0)))
  (dolist (gram grams vector)
    (incf (aref vector (mod (sxhash gram) 256)) 1.0)))

(defun cosine (left right)
  (let ((dot 0.0) (left-norm 0.0) (right-norm 0.0))
    (dotimes (index (length left))
      (incf dot (* (aref left index) (aref right index)))
      (incf left-norm (expt (aref left index) 2))
      (incf right-norm (expt (aref right index) 2)))
    (if (or (zerop left-norm) (zerop right-norm)) 0.0
        (/ dot (sqrt (* left-norm right-norm))))))

(defun keyword-lane (entries query-terms)
  "BM25 over ENTRIES, one (SCORE . ROW) per row a query term occurs in."
  (let* ((count (max 1 (length entries)))
         (lengths (mapcar (lambda (entry) (length (car entry))) entries))
         (average (/ (max 1 (reduce #'+ lengths)) count))
         (frequencies (loop for term in query-terms
                            collect (count-if (lambda (entry) (member term (car entry) :test #'string=))
                                              entries))))
    (loop for (tokens . row) in entries
          for length = (length tokens)
          for score = (loop for term in query-terms
                            for frequency = (count term tokens :test #'string=)
                            for document-frequency in frequencies
                            when (plusp frequency)
                              sum (* (/ (* frequency 2.2)
                                        (+ frequency (* 1.2 (- 1 (* 0.75 (/ length (max 1 average)))))))
                                     (log (/ (+ 0.5 count (- document-frequency) 0.5)
                                             (+ 0.5 document-frequency)))))
          when (plusp score) collect (cons score row))))

(defun fuzzy-lane (entries query)
  "Cosine over hashed character 4-grams: the lane that survives a typo or an ending."
  (let ((vector (vectorise (grams query))))
    (loop for (tokens . row) in entries
          for score = (cosine vector (vectorise (grams (getf row :snippet))))
          when (plusp score) collect (cons score row))))

(defun row-key (row)
  (format nil "~a@~a" (getf row :uri) (getf row :start)))

(defun fuse (lanes limit)
  "Reciprocal rank fusion of LANES, as the original fuses its two: 1/(60+rank)."
  (let ((scores (make-hash-table :test #'equal)) (order '()) (rows (make-hash-table :test #'equal)))
    (dolist (lane lanes)
      (loop for (nil . row) in (subseq lane 0 (min (length lane) 50))
            for rank from 1
            for key = (row-key row)
            do (unless (gethash key rows) (push key order) (setf (gethash key rows) row))
               (incf (gethash key scores 0.0) (/ 1.0 (+ 60 rank)))))
    (let ((ranked (sort order (lambda (left right)
                                 (let ((left-score (gethash left scores))
                                       (right-score (gethash right scores)))
                                   (if (= left-score right-score)
                                       (string< left right)
                                       (> left-score right-score)))))))
      (loop for key in ranked
            repeat limit
            collect (cons (gethash key rows) (gethash key scores))))))

(defun one-line (text cap)
  "TEXT on one line, at most CAP characters."
  (let* ((flat (map 'string (lambda (char) (if (member char +space+) #\Space char)) (or text "")))
         (flat (string-trim +space+ flat)))
    (if (> (length flat) cap)
        (concatenate 'string (subseq flat 0 (max 0 (- cap 1))) "...")
        flat)))

(defun span-text (row)
  (let ((start (getf row :start)) (end (getf row :end)))
    (if (and (numberp start) (numberp end) (plusp end))
        (format nil "[~,1f-~,1fs]" (coerce start 'single-float) (coerce end 'single-float))
        "")))

(defun search-text (query &key k kind)
  "Search the corpus; answers the hits as text the model reads."
  (let* ((query (string-trim +space+ (or query "")))
         (all (read-corpus))
         (rows (if kind (remove-if-not (lambda (row) (string= kind (getf row :kind))) all) all))
         (limit (or k (setting :default-results)))
         (query-terms (terms query)))
    (when (zerop (length query)) (return-from search-text "mediarag: a search needs a query"))
    (when (null rows)
      (return-from search-text
        (if all
            (format nil "mediarag: no rows of kind ~a -- the corpus holds ~d rows, none of them ~a"
                    kind (length all) kind)
            (format nil "mediarag: the corpus is empty -- mediarag_ingest_dir a folder first (~a)"
                    (namestring (corpus-root))))))
    (let* ((entries (mapcar (lambda (row) (cons (terms (getf row :snippet)) row)) rows))
           (hits (fuse (list (keyword-lane entries query-terms) (fuzzy-lane entries query)) limit)))
      (if (null hits)
          (format nil "mediarag: nothing in ~d rows matches ~s" (length rows) query)
          (with-output-to-string (out)
            (format out "mediarag: ~d of ~d rows for ~s" (length hits) (length rows) query)
            (loop for (row . score) in hits
                  for index from 1
                  do (format out "~% ~d. [~a] ~a ~a~%    ~a" index (getf row :kind) (getf row :uri)
                             (span-text row) (one-line (getf row :snippet) 240))))))))

;;; --- what the corpus holds ----------------------------------------------------

(defun status-text ()
  (let* ((rows (read-corpus))
         (uris (remove-duplicates (mapcar (lambda (row) (getf row :uri)) rows) :test #'equal)))
    (format nil "mediarag: ~d rows over ~d sources~%root: ~a~%by kind: ~{~a~^, ~}"
            (length rows) (length uris) (namestring (corpus-root))
            (loop for kind in '("text" "image" "audio")
                  collect (format nil "~a ~d" kind
                                  (count kind rows :key (lambda (row) (getf row :kind)) :test #'string=))))))
