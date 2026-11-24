;;;; export.lisp --- shipping a web build: a PWA bundle, a zip, one file, and
;;;; a Capacitor project.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every target here writes files and nothing else, which is the half of the
;;;; plugin's forge_export a cell can own offline: publishing (butler push to
;;;; itch) and the native builds are one process and one toolchain further
;;;; out.

(in-package #:nodecode-gameforge)

;;; --- bytes out of the way -----------------------------------------------------

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The alphabet a data URI is written in.")

(defun base64 (octets)
  "OCTETS as base64 text, padded: what a data URI carries."
  (let ((out (make-string-output-stream))
        (length (length octets)))
    (loop for index from 0 below length by 3
          do (let* ((a (aref octets index))
                    (b (if (< (+ index 1) length) (aref octets (+ index 1)) 0))
                    (c (if (< (+ index 2) length) (aref octets (+ index 2)) 0))
                    (triple (logior (ash a 16) (ash b 8) c)))
               (write-char (char +base64-alphabet+ (ldb (byte 6 18) triple)) out)
               (write-char (char +base64-alphabet+ (ldb (byte 6 12) triple)) out)
               (write-char (if (< (+ index 1) length)
                               (char +base64-alphabet+ (ldb (byte 6 6) triple))
                               #\=)
                           out)
               (write-char (if (< (+ index 2) length)
                               (char +base64-alphabet+ (ldb (byte 6 0) triple))
                               #\=)
                           out)))
    (get-output-stream-string out)))

(defparameter +mime-types+
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("wav" . "audio/wav") ("mp3" . "audio/mpeg")
    ("ogg" . "audio/ogg") ("json" . "application/json") ("js" . "text/javascript")
    ("css" . "text/css") ("html" . "text/html") ("ico" . "image/x-icon")
    ("svg" . "image/svg+xml") ("webmanifest" . "application/manifest+json"))
  "The types a game build is made of.")

(defun mime-type (path)
  "The type a file of PATH's extension is served as."
  (or (cdr (assoc (string-downcase (or (pathname-type (pathname path)) ""))
                   +mime-types+ :test #'string=))
      "application/octet-stream"))

(defun data-uri (path)
  "PATH inlined as text: how a single-file export carries its assets."
  (format nil "data:~a;base64,~a" (mime-type path) (base64 (file-octets path))))

;;; --- files and folders ---------------------------------------------------------

(defun directory-files (directory)
  "Every regular file under DIRECTORY, recursively, as namestrings, sorted."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (if (probe-file directory)
        (sort (mapcar #'namestring
                      (remove-if #'uiop:directory-pathname-p
                                 (uiop:directory* (merge-pathnames "**/*.*" directory))))
              #'string<)
        '())))

(defun relative-name (path directory)
  "PATH's name inside DIRECTORY, with slashes: a zip entry's name."
  (enough-namestring path (uiop:ensure-directory-pathname directory)))

(defun copy-file (from to)
  (ensure-directories-exist to)
  (with-open-file (in from :direction :input :element-type '(unsigned-byte 8))
    (with-open-file (out to :direction :output :if-exists :supersede
                             :if-does-not-exist :create :element-type '(unsigned-byte 8))
      (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
        (loop for count = (read-sequence buffer in)
              while (plusp count)
              do (write-sequence buffer out :end count))))))

(defun copy-folder (from to)
  "FROM's files into TO, keeping their names; the namestrings written."
  (let ((written '()))
    (dolist (file (directory-files from))
      (let ((target (merge-pathnames (relative-name file from)
                                     (uiop:ensure-directory-pathname to))))
        (copy-file file target)
        (push (namestring target) written)))
    (nreverse written)))

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
    (write-string text out))
  (namestring path))

(defun read-text (path)
  (with-open-file (in path :direction :input)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))

(defun slug (text)
  "TEXT as a file stem: lowercase, a dash for every run of anything else."
  (let ((out (make-string-output-stream))
        (pending nil))
    (loop for char across (string-downcase (or text "game"))
          do (if (or (alphanumericp char) (char= char #\-))
                 (progn (when (and pending)
                          (write-char #\- out))
                        (setf pending nil)
                        (write-char char out))
                 (setf pending t)))
    (let ((text (string-trim "-" (get-output-stream-string out))))
      (if (plusp (length text)) text "game"))))

;;; --- inlining ----------------------------------------------------------------

(defun json-string (text)
  "TEXT as a JSON string: quotes, backslashes and control characters escaped."
  (let ((out (make-string-output-stream)))
    (write-char #\" out)
    (loop for char across (or text "")
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (t (if (< (char-code char) 32)
                      (format out "\\u~4,'0x" (char-code char))
                      (write-char char out)))))
    (write-char #\" out)
    (get-output-stream-string out)))

(defun local-reference-p (reference)
  "T when REFERENCE names a file beside the page rather than elsewhere."
  (and (plusp (length reference))
       (not (member (char reference 0) '(#\# #\/)))
       (not (search "http" reference :test #'char-equal :end2 (min 4 (length reference))))
       (not (search "data:" reference :test #'char-equal :end2 (min 5 (length reference))))
       (not (search "mailto:" reference :test #'char-equal :end2 (min 7 (length reference))))
       (not (search "//" reference :test #'char= :end2 (min 2 (length reference))))))

(defun inline-between (text prefix terminator directory)
  "TEXT with every PREFIX...TERMINATOR reference that names a local file
replaced by its data URI; every other reference is left as it was."
  (let ((out (make-string-output-stream))
        (start 0)
        (prefix-length (length prefix)))
    (loop for at = (search prefix text :start2 start)
          do (if (null at)
                 (progn (write-string text out :start start)
                        (return (get-output-stream-string out)))
                 (let ((from (+ at prefix-length))
                       (end (position terminator text :start (+ at prefix-length))))
                   (write-string text out :start start :end from)
                   (if (null end)
                       (progn (write-string text out :start from)
                              (return (get-output-stream-string out)))
                       (let* ((reference (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                      (subseq text from end)))
                              (path (and (local-reference-p reference)
                                         (probe-file (merge-pathnames
                                                      reference
                                                      (uiop:ensure-directory-pathname directory))))))
                         (write-string (if path (data-uri path) reference) out)
                         (setf start end))))))))

(defun inline-page (html directory)
  "HTML with every local src, href and CSS url inlined: one file that
carries the whole build."
  (let ((text html))
    (setf text (inline-between text "src=\"" #\" directory))
    (setf text (inline-between text "src='" #\' directory))
    (setf text (inline-between text "href=\"" #\" directory))
    (setf text (inline-between text "href='" #\' directory))
    (setf text (inline-between text "url(" #\) directory))
    text))

;;; --- the PWA bundle -------------------------------------------------------------

(defparameter +pwa-head+
  (format nil "<link rel=\"manifest\" href=\"manifest.webmanifest\">~%<link rel=\"apple-touch-icon\" href=\"icons/icon-192.png\">~%<meta name=\"apple-mobile-web-app-capable\" content=\"yes\">~%")
  "What a page needs to be installable, put before its </head>.")

(defparameter +sw-register+
  (format nil "<script>if ('serviceWorker' in navigator) addEventListener('load', () => navigator.serviceWorker.register('sw.js').catch(() => {}));</script>~%")
  "The service worker's registration, put before the page's </body>: with it
the bundle plays with the network off.")

(defun insert-before (text marker insertion)
  "TEXT with INSERTION placed before the first MARKER, or at its end."
  (let ((at (search marker text)))
    (if at
        (concatenate 'string (subseq text 0 at) insertion (subseq text at))
        (concatenate 'string text insertion))))

(defun icon-canvas (size)
  "An icon of SIZE pixels: a gem on the dark the web page uses, drawn eight
pixels across and scaled up, so it stays pixel art."
  (let* ((unit 8)
         (small (make-canvas unit unit))
         (background '(26 28 44 255))
         (body '(65 166 246 255))
         (light '(115 239 247 255))
         (dark '(51 60 87 255)))
    (dotimes (y unit) (dotimes (x unit) (set-pixel small x y background)))
    (loop for y from 1 below 7
          for half = (- 3 (abs (- y 3)))
          do (loop for x from (- 3 half) to (+ 3 half)
                   do (set-pixel small x y (cond ((< y 3) light)
                                                 ((= y 3) (if (<= x 4) light body))
                                                 ((= y 5) dark)
                                                 (t body)))))
    (canvas-scaled small (max 1 (truncate size unit)))))

(defun manifest-text (title)
  "The PWA manifest: installable, landscape, plays offline."
  (format nil
          (concatenate 'string
                       "{~%  \"name\": ~a,~%  \"short_name\": ~a,~%  \"start_url\": \".\",~%"
                       "  \"display\": \"fullscreen\",~%  \"orientation\": \"landscape\",~%"
                       "  \"background_color\": \"#1a1c2c\",~%  \"theme_color\": \"#1a1c2c\",~%"
                       "  \"icons\": [~%"
                       "    {\"src\": \"icons/icon-192.png\", \"sizes\": \"192x192\", \"type\": \"image/png\", \"purpose\": \"any maskable\"},~%"
                       "    {\"src\": \"icons/icon-512.png\", \"sizes\": \"512x512\", \"type\": \"image/png\", \"purpose\": \"any maskable\"}~%"
                       "  ]~%}~%")
          (json-string title)
          (json-string (subseq title 0 (min 12 (length title))))))

(defun service-worker-text (digest names)
  "The service worker: cache the build once, then answer from the cache."
  (format nil
          (concatenate 'string
                       "const CACHE = 'gameforge-~a';~%"
                       "const FILES = ['.'~{, '~a'~}];~%"
                       "self.addEventListener('install', e => e.waitUntil(caches.open(CACHE).then(c => c.addAll(FILES)).then(() => self.skipWaiting())));~%"
                       "self.addEventListener('activate', e => e.waitUntil(caches.keys().then(ks => Promise.all(ks.filter(k => k !== CACHE).map(k => caches.delete(k)))))));~%"
                       "self.addEventListener('fetch', e => e.respondWith(caches.match(e.request).then(r => r || fetch(e.request))));~%")
          digest names))

(defun bundle-digest (names)
  "A short digest of the build's file names: the worker's cache version."
  (let ((octets (make-octets (length names))))
    (dolist (name names) (push-ascii octets name))
    (format nil "~8,'0x" (crc32 octets))))

(defun pwa-bundle (web-dir out-dir title)
  "WEB-DIR's build copied to OUT-DIR with a manifest, two icons, a service
worker and the page patched to install it; the files written, sorted."
  (unless (probe-file (merge-pathnames "index.html" web-dir))
    (fail "~a has no index.html; scaffold the web build first (forge_engine engine=web), or pass source"
          (namestring (uiop:ensure-directory-pathname web-dir))))
  (when (probe-file (uiop:ensure-directory-pathname out-dir))
    (uiop:delete-directory-tree (uiop:ensure-directory-pathname out-dir) :validate t))
  (let ((files (copy-folder web-dir out-dir)))
    (dolist (size '(192 512))
      (push (write-png (merge-pathnames (format nil "icons/icon-~d.png" size) out-dir)
                       (icon-canvas size))
            files))
    (let* ((names (mapcar (lambda (path) (relative-name path out-dir))
                          (remove-if-not #'stringp (copy-list files))))
           (page (merge-pathnames "index.html" out-dir))
           (html (read-text page)))
      (push (write-text (merge-pathnames "manifest.webmanifest" out-dir)
                        (manifest-text title))
            files)
      (push (write-text (merge-pathnames "sw.js" out-dir)
                        (service-worker-text (bundle-digest names)
                                             (sort (cons "index.html" names) #'string<)))
            files)
      (unless (search "rel=\"manifest\"" html)
        (setf html (insert-before html "</head>" +pwa-head+)))
      (unless (search "serviceWorker" html)
        (setf html (insert-before html "</body>" +sw-register+)))
      (write-text page html))
    (sort (mapcar #'namestring files) #'string<)))

;;; --- what forge_export builds ------------------------------------------------------

(defun web-source (root source)
  "The web build to export: SOURCE inside the project, or <project>/web."
  (let ((directory (if (and source (plusp (length source)))
                       (merge-pathnames source root)
                       (merge-pathnames "web/" root))))
    (unless (uiop:subpathp directory root)
      (fail "source ~a is outside the project ~a; export only what this project built"
            source (namestring root)))
    (uiop:ensure-directory-pathname directory)))

(defun export-web (root title source)
  "PWA bundle and the zip itch.io wants; the bundle's files and the zip."
  (let* ((web (web-source root source))
         (dist (asset-directory root "export"))
         (out (merge-pathnames "web/" dist))
         (files (pwa-bundle web out title))
         (zip (merge-pathnames (format nil "~a-web.zip" (slug title)) dist)))
    (write-zip zip (mapcar (lambda (path) (cons (relative-name path out) (file-octets path))) files))
    (values files zip)))

(defun export-single (root title source)
  "The build as one self-contained .html; the file written."
  (let* ((web (web-source root source))
         (page (merge-pathnames "index.html" web))
         (dest (merge-pathnames (format nil "~a.html" (slug title))
                                (asset-directory root "export"))))
    (unless (probe-file page)
      (fail "~a has no index.html; scaffold the web build first (forge_engine engine=web), or pass source"
            (namestring web)))
    (write-text dest (inline-page (read-text page) web))))

(defun export-capacitor (root title source app-id)
  "A Capacitor project wrapping the bundle; the files written and what to run next."
  (multiple-value-bind (files zip) (export-web root title source)
    (declare (ignore zip))
    (let* ((dist (asset-directory root "export"))
           (mobile (merge-pathnames "mobile/" dist))
           (www (merge-pathnames "www/" mobile)))
      (when (probe-file www) (uiop:delete-directory-tree (uiop:ensure-directory-pathname www) :validate t))
      (let ((copied (copy-folder (merge-pathnames "web/" dist) www)))
        (push (write-text (merge-pathnames "package.json" mobile)
                          (format nil
                                  (concatenate 'string
                                               "{~% \"name\": \"~a\",~% \"version\": \"1.0.0\",~% \"private\": true,~%"
                                               " \"scripts\": {~%  \"android\": \"cap add android && cap sync android && cd android && ./gradlew assembleDebug\",~%"
                                               "  \"ios\": \"cap add ios && cap sync ios\"~% },~%"
                                               " \"dependencies\": {\"@capacitor/core\": \">=7,<8\", \"@capacitor/android\": \">=7,<8\", \"@capacitor/ios\": \">=7,<8\"},~%"
                                               " \"devDependencies\": {\"@capacitor/cli\": \">=7,<8\"}~%}~%")
                                  (slug title)))
              files)
        (push (write-text (merge-pathnames "capacitor.config.json" mobile)
                          (format nil
                                  (concatenate 'string
                                               "{~%  \"appId\": ~a,~%  \"appName\": ~a,~%  \"webDir\": \"www\",~%"
                                               "  \"android\": {\"backgroundColor\": \"#1a1c2c\"},~%"
                                               "  \"ios\": {\"backgroundColor\": \"#1a1c2c\"}~%}~%")
                                  (json-string app-id) (json-string title)))
              files)
        (values (sort (append (mapcar #'namestring files) copied) #'string<) mobile)))))
