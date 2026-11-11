;;;; query.lisp --- the catalogue, and any one prompt quoted by line.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The catalogue answers offline: DATA.LISP carries every entry, with measured
;;;; sizes and digests. Only READING a prompt reaches the network, and only once
;;;; per file: it is fetched from the author's own repository, cached under
;;;; ~/.cache/nodecode-jailbreaks/, and the cache is read after that. The
;;;; collection has no licence file, so nothing here redistributes it.

(in-package #:nodecode-jailbreaks)

;;; --- the collection ----------------------------------------------------------------

(defun entries ()
  "Every prompt in the collection, in the catalogue's order."
  +entries+)

(defun entry-key (entry) (getf entry :key))
(defun entry-vendor (entry) (getf entry :vendor))
(defun entry-target (entry) (getf entry :target))
(defun entry-kind (entry) (getf entry :kind))
(defun entry-says (entry) (getf entry :says))
(defun entry-path (entry) (getf entry :path))
(defun entry-bytes (entry) (getf entry :bytes))
(defun entry-lines (entry) (getf entry :lines))
(defun entry-sha (entry) (getf entry :sha256))
(defun entry-group (entry) (getf entry :group))

(defun vendors ()
  "The folders the collection is filed in, once each, in order of appearance."
  (remove-duplicates (mapcar #'entry-vendor (entries)) :test #'string-equal))

(defun targets ()
  "Every target the collection names, once each, in order of appearance."
  (remove-duplicates (mapcar #'entry-target (entries)) :test #'string-equal))

;;; --- what kind of file it is --------------------------------------------------------

(defun kinds () "Every kind the catalogue uses, in the order DATA.LISP declares them." (mapcar #'car +kinds+))

(defun kind-key (kind)
  "KIND as the keyword the data uses; a string names the same kind."
  (etypecase kind
    (keyword kind)
    (string (intern (string-upcase kind) :keyword))))

(defun kind-name (kind)
  "A kind as a person reads it."
  (string-downcase (string (kind-key kind))))

(defun kind-sentence (kind)
  "What a kind means, in one line."
  (cdr (assoc (kind-key kind) +kinds+)))

(defun kind-counts ()
  "How many entries of each kind, kinds with none left out."
  (loop for kind in (kinds)
        for n = (count kind (entries) :key #'entry-kind)
        when (plusp n) collect (cons kind n)))

;;; --- the files that are the same file ---------------------------------------------

(defun groups () "The files that are byte-identical to each other." +groups+)
(defun group-name (group) (getf group :name))
(defun group-paths (group) (coerce (getf group :paths) 'list))
(defun group-says (group) (getf group :says))

(defun group-of (path)
  "The name of the group PATH belongs to, or NIL when it stands alone."
  (loop for group in (groups)
        when (member path (group-paths group) :test #'string=)
          return (group-name group)))

;;; --- matching ---------------------------------------------------------------------

(defun contains-p (needle haystack)
  "Whether NEEDLE appears in HAYSTACK, ignoring case; NIL when either is empty."
  (and (stringp haystack) (plusp (length haystack))
       (stringp needle) (plusp (length needle))
       (not (null (search (string-downcase needle) (string-downcase haystack))))))

(defun entry-mentions-p (entry text)
  "Whether TEXT appears in anything the entry says."
  (or (contains-p text (entry-key entry))
      (contains-p text (entry-vendor entry))
      (contains-p text (entry-target entry))
      (contains-p text (kind-name (entry-kind entry)))
      (contains-p text (entry-says entry))
      (contains-p text (entry-path entry))))

(defun entries-matching (&key vendor kind text)
  "The entries that satisfy every given filter: a folder, a kind (persona,
workspace-policy, content-policy, system-prompt, modelfile, withdrawn, empty),
or a word matched against everything an entry says."
  (remove-if-not
   (lambda (entry)
     (and (or (null vendor) (zerop (length vendor))
              (string-equal vendor (entry-vendor entry)))
          (or (null kind) (zerop (length kind))
              (eq (kind-key kind) (entry-kind entry)))
          (or (null text) (zerop (length text)) (entry-mentions-p entry text))))
   (entries)))

(defun entry-by-key (key)
  "The entry KEY most nearly names: its own key, else one starting with it, else
one whose key, target or path contains it. A string that names none is NIL."
  (let ((wanted (string-downcase (or key ""))))
    (when (plusp (length wanted))
      (or (find wanted (entries) :test #'string= :key (lambda (e) (string-downcase (entry-key e))))
          (find-if (lambda (e) (and (cl:search wanted (string-downcase (entry-key e)))
                                    (= 0 (cl:search wanted (string-downcase (entry-key e))))))
                   (entries))
          (find-if (lambda (e) (contains-p wanted (entry-key e))) (entries))
          (find-if (lambda (e) (contains-p wanted (entry-target e))) (entries))
          (find-if (lambda (e) (contains-p wanted (entry-path e))) (entries))))))

(defun facts () "The questions the collection is worth opening for." +facts+)
(defun fact-question (fact) (getf fact :question))
(defun fact-answer (fact) (getf fact :answer))

(defun facts-matching (text)
  "The questions and answers that mention TEXT."
  (if (or (null text) (zerop (length text)))
      (facts)
      (remove-if-not (lambda (fact)
                       (or (contains-p text (fact-question fact))
                           (contains-p text (fact-answer fact))))
                     (facts))))

;;; --- what a person reads ----------------------------------------------------------

(defun key-width ()
  "The column the entry keys are padded to."
  (1+ (reduce #'max (entries) :key (lambda (e) (length (entry-key e))))))

(defun kind-width ()
  "The column the kinds are padded to."
  (1+ (reduce #'max (kinds) :key (lambda (k) (length (kind-name k))))))

(defun entry-row (entry)
  "One prompt as the three lines a listing gives: what it is, where it lives, what it does."
  (format nil "  ~va ~va ~a~%      ~a - ~:d bytes, ~:d lines~%      ~a"
          (key-width) (entry-key entry)
          (kind-width) (kind-name (entry-kind entry))
          (entry-target entry)
          (entry-path entry) (entry-bytes entry) (entry-lines entry)
          (entry-says entry)))

(defun entries-text (entries &key (heading nil))
  "ENTRIES as the lines of one listing, HEADING above them, the count with it."
  (if (null entries)
      "no prompt in the collection matches that."
      (format nil "~@[~a~%~]~{~a~^~%~}"
              (and heading (format nil "~a (~d)" heading (length entries)))
              (mapcar #'entry-row entries))))

(defun group-line (group)
  "One group of identical files as the line a person reads."
  (format nil "  ~a - ~a~%      ~{~a~^~%      ~}"
          (group-name group) (group-says group) (group-paths group)))

(defun groups-text ()
  "The files that are the same file, which a digest finds and a reader does not."
  (if (null (groups))
      "every file in the collection is a file of its own."
      (format nil "~{~a~^~%~}" (mapcar #'group-line (groups)))))

(defun summary-text ()
  "How many prompts, of what kinds, over how many targets, filed how."
  (format nil "~d prompt files in ~d folders, ~:d bytes, across ~d target labels.~%~%  ~{~a~^~%~}~%~%the same file more than once:~%~a"
          (length (entries)) (length (vendors))
          (reduce #'+ (entries) :key #'entry-bytes)
          (length (targets))
          (loop for (kind . n) in (kind-counts)
                collect (format nil "~v@a ~:d  ~a" 18 (kind-name kind) n (kind-sentence kind)))
          (groups-text)))

(defun source-line ()
  "The provenance line every answer ends with."
  (format nil "Source: ~a at ~a, read ~a. No licence file: the prompts are fetched from the author's own repo, and every answer names its file."
          (getf +archive+ :name) (subseq (getf +archive+ :head) 0 8) +captured+))

(defun source-block ()
  "The provenance, whole: the collection, its head, its size, and where a prompt comes from."
  (format nil "collection: ~a~%url:        ~a~%head:       ~a (~a)~%read:       ~a~%stars:      ~:d at the time of reading~%licence:    none - the collection carries no licence file~%~%files:      ~d, ~:d bytes~%folders:    ~{~a~^, ~}~%targets:    ~d~%~%a prompt is fetched from ~a/raw/<head or main>/<path> once and cached under~%  ~a~%~%Every word of a prompt is its author's. This cell indexes them, quotes short passages~%with their file and line, and redistributes none of them."
          (getf +archive+ :name) (getf +archive+ :url)
          (getf +archive+ :head) (getf +archive+ :head-date) +captured+
          (getf +archive+ :stars)
          (length (entries)) (reduce #'+ (entries) :key #'entry-bytes)
          (vendors) (length (targets))
          (getf +archive+ :url) (namestring (cache-directory))))

;;; --- the prompts themselves --------------------------------------------------------

(defun cache-directory ()
  "The directory the fetched prompts live in."
  (merge-pathnames "nodecode-jailbreaks/"
                   (uiop:ensure-directory-pathname
                    (or (ignore-errors (uiop:xdg-cache-home)) "~/.cache/"))))

(defun cache-path (entry)
  "Where the local copy of ENTRY's file lives."
  (merge-pathnames (format nil "~a.md" (entry-key entry)) (cache-directory)))

(defun url-escape (path)
  "PATH with the characters a URL will not carry spelled out."
  (with-output-to-string (out)
    (loop for ch across path
          do (cond ((member ch '(#\Space) :test #'char=) (write-string "%20" out))
                   ((char= ch #\#) (write-string "%23" out))
                   ((char= ch #\?) (write-string "%3F" out))
                   (t (write-char ch out))))))

(defun raw-url (entry)
  "The URL ENTRY's file is fetched from."
  (format nil "~a/raw/main/~a" (getf +archive+ :url) (url-escape (entry-path entry))))

(defun fetch-file (&key entry force)
  "The path of a local copy of ENTRY (an entry, or its key), downloaded once
unless FORCE. Signals an error when there is no copy and the download fails."
  (let* ((record (etypecase entry
                    (list entry)
                    (string (or (entry-by-key entry)
                                (error "no prompt in the collection is named ~s" entry)))))
         (path (cache-path record)))
    (when (or force (not (probe-file path)))
      (ensure-directories-exist path)
      (let ((status (nth-value 2 (uiop:run-program
                                  (list "curl" "-fsSL" "--max-time" "120" "-o" (namestring path)
                                        (raw-url record))
                                  :ignore-error-status t))))
        (unless (and (zerop status) (probe-file path))
          (ignore-errors (delete-file path))
          (error "could not fetch ~a (curl exited ~a)" (raw-url record) status))))
    path))

(defun split-lines (text)
  "TEXT as a list of its lines."
  (with-input-from-string (in text)
    (loop for line = (read-line in nil nil) while line collect line)))

(defun cache-lines (entry)
  "ENTRY's own lines, fetched once."
  (split-lines (uiop:read-file-string (fetch-file :entry entry))))

(defun read-lines (from to &key path)
  "The file at PATH, lines FROM..TO as a string, each numbered."
  (let* ((lines (split-lines (uiop:read-file-string path)))
         (lo (max 1 (min from to)))
         (hi (min (length lines) (max from to))))
    (if (> lo hi)
        (format nil "the file has ~d lines; there is no line ~d" (length lines) from)
        (format nil "~{~a~^~%~}"
                (loop for n from lo to hi
                      collect (format nil "~5d  ~a" n (nth (1- n) lines)))))))

(defun line-range (text)
  "TEXT parsed as a line or a range (117, or 116-122); NIL when it is neither."
  (when (and (stringp text) (plusp (length text)))
    (let* ((dash (position #\- text))
           (a (handler-case (parse-integer (if dash (subseq text 0 dash) text) :junk-allowed t)
                (error () nil)))
           (b (and dash (handler-case (parse-integer (subseq text (1+ dash)) :junk-allowed t)
                          (error () nil)))))
      (if (and a (or (null dash) b))
          (cons a (or b a))
          nil))))

(defun numbered (hits)
  "HITS as the numbered lines a person reads."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (hit) (format nil "~5d  ~a" (car hit) (cdr hit))) hits)))

(defun opening-text (entry &key (lines 8) path)
  "ENTRY's own opening: its first LINES lines that carry something, numbered.
The first look at a prompt without reading all of it. A prompt that is not cached
and cannot be fetched is reported, not signalled: the rest of the answer stands."
  (let* ((file (or path
                   (handler-case (fetch-file :entry entry)
                     (error (condition)
                       (return-from opening-text
                         (format nil "~a~%  (the opening is not cached and the collection did not answer: ~a)~%~a"
                                 (entry-key entry) condition (source-line)))))))
         (all (split-lines (uiop:read-file-string file)))
         (hits (loop for line in all
                     for n from 1
                     when (plusp (length (string-trim '(#\Space #\Tab #\Return) line)))
                       collect (cons n line) into kept
                     when (= (length kept) lines) return kept
                     finally (return kept))))
    (format nil "~a~%~a~%  (the opening ~d lines of ~:d; jailbreaks_quote reads the rest)~%~a"
            (entry-key entry) (numbered hits) (length hits) (length all) (source-line))))

(defun quote-text (&key text lines-text entry path)
  "One prompt's own words: the LINES-TEXT range when given, else every line that
carries TEXT, numbered, at most twenty. ENTRY names the entry (its key) or is an
entry; PATH reads a local copy instead of fetching."
  (let* ((record (etypecase entry
                    (null (error "name a prompt: jailbreaks_list shows the keys"))
                    (list entry)
                    (string (or (entry-by-key entry)
                                (error "no prompt in the collection is named ~s" entry)))))
         (file (or path (fetch-file :entry record)))
         (range (line-range lines-text)))
    (cond
      (range
       (format nil "~a, lines ~a-~a~%~a~%~a"
               (entry-key record) (car range) (cdr range)
               (read-lines (car range) (cdr range) :path file) (source-line)))
      ((and text (plusp (length text)))
       (let* ((all (split-lines (uiop:read-file-string file)))
              (hits (loop for line in all
                          for n from 1
                          when (contains-p text line) collect (cons n line))))
         (if (null hits)
             (format nil "no line of ~a carries ~s. That file is ~d lines, ~:d bytes."
                     (entry-key record) text (length all) (entry-bytes record))
             (format nil "~a, every line carrying ~s~%~a~%~a~@[~%~a carrying ~s~]"
                     (entry-key record) text
                     (numbered (subseq hits 0 (min 20 (length hits))))
                     (source-line)
                     (when (> (length hits) 20)
                       (format nil "the first 20 of ~d lines of ~a" (length hits) (entry-key record)))
                     (when (> (length hits) 20) text)))))
      (t (format nil "give a word to look for, or a line range as 12 or 1-40. ~a is ~d lines.~%~a"
                 (entry-key record) (entry-lines record) (source-line))))))

(defun grep-text (text &key entry)
  "Every line of every prompt (or of one, when ENTRY is given) that carries TEXT.
Fetches each file once, and says how many it searched."
  (let* ((records (if entry
                      (list (etypecase entry
                              (list entry)
                              (string (or (entry-by-key entry)
                                          (error "no prompt in the collection is named ~s" entry)))))
                      (entries)))
         (hits '())
         (searched 0)
         (failed '()))
    (dolist (record records)
      (handler-case
          (let ((all (cache-lines record)))
            (incf searched)
            (loop for line in all
                  for n from 1
                  when (contains-p text line)
                    do (push (list (entry-key record) n line) hits)))
        (error (e) (push (entry-key record) failed))))
    (setf hits (nreverse hits))
    (format nil "~a~%~@[(no line of ~d prompts carries ~s)~%~]~@[~a~%~]~a~@[~%could not fetch: ~{~a~^, ~}~]"
            (if hits
                (format nil "~{~a~^~%~}"
                        (loop for (key n line) in (subseq hits 0 (min 40 (length hits)))
                              collect (format nil "  ~va ~5d  ~a" 20 key n line)))
                (format nil "no line of ~d prompts carries ~s" searched text))
            (and hits searched) text
            (when (> (length hits) 40)
              (format nil "(the first 40 of ~d lines, across ~d prompts)" (length hits) searched))
            (source-line) failed)))
