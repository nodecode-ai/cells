;;;; archive.lisp --- what an archive says about itself, read in memory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's forensics, kept whole: path traversal, symlinks, encrypted
;;;; entries, decompression bombs, double extensions and documents that are
;;;; really executables, nested up to three archives deep -- all from the
;;;; container's own headers, in memory. Nothing is extracted to disk and
;;;; nothing is executed. Executable content inside an archive is reported as
;;;; INFO, never as a warning: source tarballs, npm packages and installers are
;;;; full of scripts, and `this zip contains a .py' is not a security signal.

(in-package #:nodecode-vt-check)

(defparameter +eicar-md5+ "44d88612fea8a8f36de82e1278abb02f"
  "The EICAR test file's MD5, which VirusTotal knows: a key that answers it works.")
(defparameter +max-archive-depth+ 3 "How many archives deep a nested one is read.")
(defparameter +nested-read-limit+ (* 32 1024 1024) "The biggest entry read to look inside it.")
(defparameter +max-archive-bytes+ (* 512 1024 1024) "A bigger archive is not read inline.")
(defparameter +max-entries+ 4000 "Entries inspected before the rest are counted only.")
(defparameter +bomb-ratio+ 100 "Content to container above this is a decompression bomb.")
(defparameter +bomb-bytes+ (* 10 1024 1024) "...when it expands past this.")

(defparameter +code-extensions+ '("py" "sh" "pl" "rb" "php" "js" "jar" "war")
  "Code inside an archive: normal in source and installer archives.")
(defparameter +binary-extensions+ '("exe" "dll" "scr" "com" "pif" "msi" "msp" "bat" "cmd"
                                    "ps1" "psm1" "vbs" "vbe" "jse" "wsf" "hta" "lnk" "apk" "dex"
                                    "deb" "rpm" "appimage" "run" "elf" "so" "dylib" "iso" "img"
                                    "bin" "ocx")
  "Binaries inside an archive: normal in installers.")
(defparameter +doc-extensions+ '("pdf" "jpg" "jpeg" "png" "gif" "webp" "svg" "doc" "docx"
                                "xls" "xlsx" "ppt" "pptx" "txt" "csv" "rtf" "mp3" "mp4" "mkv"
                                "avi" "json" "html" "htm" "xml" "odt")
  "Extensions that promise a document, so an executable behind one is a lie.")
(defparameter +zip-extensions+ '("zip" "jar" "apk" "docx" "xlsx" "pptx" "odt" "epub" "whl"
                                 "xpi" "crx")
  "What a ZIP container is usually named.")
(defparameter +tar-extensions+ '("tar" "tgz" "tbz2" "txz") "What a tar archive is usually named.")
(defparameter +thick-extensions+ '("7z" "rar" "cab")
  "Formats that need external tools, so their contents stay unknown.")

;;; --- octets --------------------------------------------------------------------------

(defun u16 (octets at)
  "The little-endian 16-bit word at AT."
  (+ (aref octets at) (ash (aref octets (+ at 1)) 8)))

(defun u32 (octets at)
  "The little-endian 32-bit word at AT."
  (+ (aref octets at)
     (ash (aref octets (+ at 1)) 8)
     (ash (aref octets (+ at 2)) 16)
     (ash (aref octets (+ at 3)) 24)))

(defun octet-slice (octets start end)
  "OCTETS between START and END, both clamped to it."
  (let ((start (max 0 (min start (length octets))))
        (end (max 0 (min end (length octets)))))
    (and (< start end) (subseq octets start end))))

(defun decode-name (octets)
  "OCTETS as text: UTF-8 when they are, else Latin-1."
  (or (ignore-errors (sb-ext:octets-to-string octets :external-format :utf-8))
      (map 'string #'code-char octets)))

(defun nul-string (octets start length)
  "LENGTH octets of OCTETS from START, up to the first NUL."
  (let* ((end (min (length octets) (+ start length)))
         (stop (or (position 0 octets :start start :end end) end)))
    (decode-name (subseq octets start stop))))

(defun parse-octal (octets start length)
  "A tar header's `0000644' field, as a number, or 0."
  (let ((value 0) (digits 0))
    (loop for at from start below (min (length octets) (+ start length))
          for char = (aref octets at)
          while (and (<= 48 char) (<= char 55))
          do (setf value (+ (* value 8) (- char 48)))
             (incf digits))
    (if (plusp digits) value 0)))

(defun inflate (octets format &key limit)
  "OCTETS decompressed as FORMAT -- CHIPZ:DEFLATE or CHIPZ:GZIP -- at most
LIMIT octets; NIL when the stream cannot be read."
  (handler-case
      (let* ((input (flexi-streams:make-in-memory-input-stream octets))
             (stream (chipz:make-decompressing-stream format input))
             (cap (max 1 (or limit +nested-read-limit+)))
             (out (make-array (min cap 65536) :element-type '(unsigned-byte 8)
                                             :adjustable t :fill-pointer 0))
             (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
        (unwind-protect
             (block read
               (loop for count = (read-sequence buffer stream)
                     while (plusp count)
                     do (loop for index below count
                              do (when (>= (fill-pointer out) cap)
                                   (return-from read (coerce out '(simple-array (unsigned-byte 8) (*)))))
                                 (vector-push-extend (aref buffer index) out))
                     finally (return-from read (coerce out '(simple-array (unsigned-byte 8) (*))))))
          (ignore-errors (close stream))
          (ignore-errors (close input))))
    (error () nil)))

;;; --- names ------------------------------------------------------------------------------

(defun path-parts (name)
  "NAME split on both separators."
  (uiop:split-string name :separator '(#\/ #\\)))

(defun extension (name)
  "NAME's extension, lower-cased, without the dot; the empty string when none."
  (let* ((base (car (last (path-parts name))))
         (dot (position #\. base :from-end t)))
    (if (and dot (< dot (1- (length base))))
        (string-downcase (subseq base (1+ dot)))
        "")))

(defun double-extension-p (base)
  "BASE is named like a document and really carries an executable extension:
`invoice.pdf.exe', the plugin's double-extension alert."
  (let* ((parts (uiop:split-string base :separator "."))
         (count (length parts)))
    (and (>= count 3)
         (member (string-downcase (nth (- count 2) parts)) +doc-extensions+ :test #'string=)
         (or (member (string-downcase (nth (1- count) parts)) +code-extensions+ :test #'string=)
             (member (string-downcase (nth (1- count) parts)) +binary-extensions+ :test #'string=))
         t)))

(defun finding (level control &rest arguments)
  "One finding: how loud it is, and what it means for the person extracting it."
  (list :level level :why (apply #'format nil control arguments)))

(defun decompression-bomb (label total container)
  "The alert a container that unpacks far bigger than it is makes."
  (finding "alert" "~a: decompression bomb -- ~a from ~a (~,0f:1); extracting it can fill the disk"
           label (human-bytes total) (human-bytes container) (/ (float total) (max container 1))))

;;; --- ZIP -----------------------------------------------------------------------------------

(defun find-end-of-central-directory (octets)
  "The offset of the ZIP end-of-central-directory record, or NIL."
  (let ((length (length octets))
        (low (max 0 (- (length octets) (+ 65557 22)))))
    (loop for at from (- length 22) downto low
          when (and (>= at 0) (= (u32 octets at) #x06054b50))
            do (return at))))

(defun zip-entries (octets)
  "OCTETS' central directory as a list of plists, or NIL when it is unreadable."
  (let ((eocd (find-end-of-central-directory octets)))
    (when eocd
      (let ((count (u16 octets (+ eocd 10)))
            (at (u32 octets (+ eocd 16)))
            (entries '()))
        (loop repeat (min count +max-entries+)
              while (and (<= (+ at 46) (length octets)) (= (u32 octets at) #x02014b50))
              do (let* ((flags (u16 octets (+ at 8)))
                        (method (u16 octets (+ at 10)))
                        (csize (u32 octets (+ at 20)))
                        (usize (u32 octets (+ at 24)))
                        (name-length (u16 octets (+ at 28)))
                        (extra-length (u16 octets (+ at 30)))
                        (comment-length (u16 octets (+ at 32)))
                        (external (u32 octets (+ at 38)))
                        (local (u32 octets (+ at 42)))
                        (name (decode-name (or (octet-slice octets (+ at 46) (+ at 46 name-length))
                                               (make-array 0 :element-type '(unsigned-byte 8))))))
                   (push (list :name name :method method :flags flags :csize csize :usize usize
                               :mode (ash external -16) :offset local)
                         entries)
                   (setf at (+ at 46 name-length extra-length comment-length)))
              finally (return (nreverse entries)))))))

(defun zip-data-start (octets entry)
  "Where ENTRY's content begins in OCTETS, or NIL."
  (let ((at (getf entry :offset)))
    (when (and at (<= (+ at 30) (length octets)) (= (u32 octets at) #x04034b50))
      (+ at 30 (u16 octets (+ at 26)) (u16 octets (+ at 28))))))

(defun zip-entry-head (octets entry limit)
  "The first LIMIT octets of ENTRY's content -- stored as they are, deflated
through chipz -- or NIL."
  (let ((start (zip-data-start octets entry))
        (method (getf entry :method))
        (csize (getf entry :csize)))
    (when (and start (<= start (length octets)))
      (let ((raw (octet-slice octets start (min (length octets) (+ start (max csize 1))))))
        (cond ((and (= method 0) raw) (octet-slice raw 0 limit))
              ((and (= method 8) raw) (inflate raw 'chipz:deflate :limit limit))
              (t nil))))))

(defun zip-findings (octets label depth)
  "Everything OCTETS' own headers say, plus what a nested ZIP one deep says."
  (let ((entries (zip-entries octets)))
    (if (null entries)
        (list (finding "warn" "~a: corrupt ZIP container" label))
        (let ((findings '())
              (total 0))
          (dolist (entry entries)
            (let* ((name (getf entry :name))
                   (parts (path-parts name))
                   (base (car (last parts)))
                   (ext (extension name))
                   (usize (or (getf entry :usize) 0))
                   (csize (or (getf entry :csize) 0)))
              (incf total usize)
              (when (or (uiop:string-prefix-p "/" name)
                        (and (>= (length name) 2) (alpha-char-p (char name 0)) (char= (char name 1) #\:))
                        (member ".." parts :test #'string=))
                (push (finding "alert" "~a: path traversal entry '~a' -- extracting it writes outside ~
the folder you extract into" label name)
                      findings))
              (when (= (logand (getf entry :mode) #o170000) #o120000)
                (push (finding "warn" "~a: symlink entry '~a' -- can point outside the extraction ~
folder" label name)
                      findings))
              (when (logtest 1 (getf entry :flags))
                (push (finding "warn" "~a: encrypted entry '~a' -- scanners cannot read its contents"
                               label name)
                      findings))
              (when (and (plusp (length base)) (not (uiop:string-suffix-p name "/")))
                (cond ((double-extension-p base)
                       (push (finding "alert" "~a: double extension '~a' -- really a '~a' file dressed ~
as a document" label name ext)
                             findings))
                      ((member ext +code-extensions+ :test #'string=)
                       (push (finding "info" "~a: contains code file '~a' (~a) -- normal in source and ~
installer archives" label name (human-bytes usize))
                             findings))
                      ((member ext +binary-extensions+ :test #'string=)
                       (push (finding "info" "~a: contains binary '~a' (~a) -- normal in installers"
                                      label name (human-bytes usize))
                             findings)))
                (when (and (> usize (* 10 1024 1024)) (> (/ (float usize) (max csize 1)) 200))
                  (push (finding "alert" "~a: extreme compression on '~a' (~,0f:1) -- expands to ~a on ~
extraction" label name (/ (float usize) (max csize 1)) (human-bytes usize))
                        findings))
                (when (and (member ext +doc-extensions+ :test #'string=)
                           (<= usize +nested-read-limit+))
                  (let ((head (zip-entry-head octets entry 8)))
                    (when (and head
                               (or (starts-with head 0 "MZ")
                                   (starts-with head 0 (vector 127 69 76 70))))
                      (push (finding "alert" "~a: '~a' is named like a document but its content is an ~
executable (~a)" label name (if (starts-with head 0 "MZ") "PE" "ELF"))
                            findings))))
                (when (and (< depth +max-archive-depth+)
                           (<= usize +nested-read-limit+)
                           (member ext +zip-extensions+ :test #'string=))
                  (let ((nested (zip-entry-head octets entry (max usize 1))))
                    (if nested
                        (setf findings
                              (append (zip-findings nested (format nil "~a!~a" label base) (1+ depth))
                                      findings))
                        (push (finding "info" "~a: nested '~a' unreadable" label name) findings)))))))
          (when (and (plusp (length octets))
                     (> (/ (float total) (length octets)) +bomb-ratio+)
                     (> total +bomb-bytes+))
            (push (decompression-bomb label total (length octets)) findings))
          (nreverse findings)))))

;;; --- tar -----------------------------------------------------------------------------------

(defun tar-findings (octets label depth)
  "Everything a tar archive's own headers say."
  (declare (ignore depth))
  (let ((findings '())
        (at 0)
        (count 0)
        (total 0))
    (loop while (and (<= (+ at 512) (length octets)) (< count +max-entries+))
          do (let ((name (nul-string octets at 100)))
               (if (zerop (length name))
                   (loop-finish)
                   (let* ((size (parse-octal octets (+ at 124) 12))
                          (type (code-char (min 127 (aref octets (+ at 156)))))
                          (link (nul-string octets (+ at 157) 100))
                          (parts (path-parts name))
                          (ext (extension name)))
                     (incf count)
                     (when (or (uiop:string-prefix-p "/" name) (member ".." parts :test #'string=))
                       (push (finding "alert" "~a: path traversal entry '~a' -- extracting it writes ~
outside the folder you extract into" label name)
                             findings))
                     (when (member type '(#\2 #\1) :test #'char=)
                       (push (finding "warn" "~a: link entry '~a' -> '~a' -- can point outside the ~
extraction folder" label name link)
                             findings))
                     (when (char= type #\0)
                       (incf total size)
                       (cond ((double-extension-p (car (last parts)))
                              (push (finding "alert" "~a: double extension '~a' -- really a '~a' file ~
dressed as a document" label name ext)
                                    findings))
                             ((member ext +code-extensions+ :test #'string=)
                              (push (finding "info" "~a: contains code file '~a' (~a) -- normal in ~
source archives" label name (human-bytes size))
                                    findings))
                             ((member ext +binary-extensions+ :test #'string=)
                              (push (finding "info" "~a: contains binary '~a' (~a) -- normal in ~
installers" label name (human-bytes size))
                                    findings))))
                     (incf at (+ 512 (* 512 (ceiling size 512))))))))
    (when (and (plusp (length octets))
               (> (/ (float total) (length octets)) +bomb-ratio+)
               (> total +bomb-bytes+))
      (push (decompression-bomb label total (length octets)) findings))
    (nreverse findings)))

;;; --- the one entry point ----------------------------------------------------------------

(defun archive-kind (head name)
  "What the container is, by its magic bytes first and its name second."
  (let ((ext (extension name))
        (lower (string-downcase name)))
    (cond ((or (starts-with head 0 (vector 80 75 3 4)) (starts-with head 0 (vector 80 75 5 6))
               (member ext +zip-extensions+ :test #'string=))
           "zip")
          ((or (starts-with head 0 (vector 31 139))
               (member ext +tar-extensions+ :test #'string=)
               (uiop:string-suffix-p lower ".tar.gz")
               (uiop:string-suffix-p lower ".tar.bz2")
               (uiop:string-suffix-p lower ".tar.xz"))
           "tar")
          ((or (starts-with head 0 (vector 55 122 188 175)) (starts-with head 0 "Rar!")
               (member ext +thick-extensions+ :test #'string=))
           "thick")
          ((starts-with head 257 "ustar") "tar")
          (t nil))))

(defun read-whole-file (path limit)
  "PATH's octets, or NIL when it is bigger than LIMIT or unreadable."
  (handler-case
      (with-open-file (stream path :element-type '(unsigned-byte 8))
        (let ((size (file-length stream)))
          (when (and size (<= size limit))
            (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
              (read-sequence buffer stream)
              buffer))))
    (error () nil)))

(defun archive-findings (path head)
  "PATH's findings, from its headers, in memory only. A file that is no
archive has none."
  (let ((kind (archive-kind head (file-namestring path)))
        (label (file-namestring path)))
    (cond ((null kind) '())
          ((string= kind "thick")
           (list (finding "warn" "~a: 7z/rar/cab cannot be inspected without external tools -- treat ~
the contents as unknown" label)))
          (t (let ((raw (read-whole-file path +max-archive-bytes+)))
               (cond ((null raw)
                      (list (finding "warn" "~a: archive too large to inspect inline, or unreadable"
                                     label)))
                     ((starts-with raw 0 (vector 31 139))
                      (let ((inflated (inflate raw 'chipz:gzip :limit +nested-read-limit+)))
                        (cond ((null inflated)
                               (list (finding "warn" "~a: gzip stream could not be decompressed" label)))
                              ((starts-with inflated 257 "ustar") (tar-findings inflated label 0))
                              ((starts-with inflated 0 (vector 80 75 3 4))
                               (zip-findings inflated label 0))
                              (t (list (finding "warn" "~a: compressed stream that is neither a tar ~
nor a ZIP archive -- not inspected" label))))))
                     ((string= kind "zip") (zip-findings raw label 0))
                     (t (tar-findings raw label 0))))))))
