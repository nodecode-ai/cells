;;;; query.lisp --- the archive, the hierarchy, and any document quoted by line.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The hierarchy and the archive index answer offline: DATA.LISP carries them.
;;;; Only QUOTE-TEXT reaches the network, and only once per document -- it puts
;;;; that document under the cache and reads the cache after that.

(in-package #:nodecode-model-tiers)

;;; --- the archive ------------------------------------------------------------------

(defun files ()
  "Every document the cell can quote, in the archive's order."
  +files+)

(defun file-key (file) (getf file :key))
(defun file-path (file) (getf file :path))
(defun file-vendor (file) (getf file :vendor))
(defun file-document (file) (getf file :document))
(defun file-bytes (file) (getf file :bytes))
(defun file-lines (file) (getf file :lines))
(defun file-sha (file) (getf file :sha256))
(defun file-says (file) (getf file :says))

(defun vendors ()
  "The vendors whose documents the archive holds, once each, in order of appearance."
  (remove-duplicates (mapcar #'file-vendor (files)) :test #'string-equal))

(defun file-by-key (key)
  "The document KEY most nearly names: its own key, else one starting with it,
else one whose key or document contains it; a string that names none is NIL."
  (let ((wanted (string-downcase (or key ""))))
    (when (plusp (length wanted))
      (or (find wanted (files) :test #'string= :key (lambda (f) (string-downcase (file-key f))))
          (find-if (lambda (f) (contains-p wanted (file-key f))) (files))
          (find-if (lambda (f) (contains-p wanted (file-document f))) (files))))))

(defun files-matching (&key vendor text)
  "The documents of one vendor, or those whose key, document or summary mentions TEXT."
  (remove-if-not
   (lambda (file)
     (and (or (null vendor) (zerop (length vendor))
              (string-equal vendor (file-vendor file)))
          (or (null text) (zerop (length text))
              (contains-p text (file-key file))
              (contains-p text (file-document file))
              (contains-p text (file-says file))
              (contains-p text (file-vendor file)))))
   (files)))

;;; --- the hierarchy ----------------------------------------------------------------

(defun models ()
  "Every model the archive names, in the archive's order."
  +models+)

(defun model-name (model) (getf model :name))
(defun model-tier (model) (getf model :tier))
(defun model-availability (model) (getf model :availability))
(defun model-note (model) (getf model :note))
(defun model-quotes (model) (getf model :quotes))
(defun model-since (model) (getf model :since))
(defun model-api (model) (getf model :api))

(defun model-tier-name (model)
  "The model's tier, as the archive spells it."
  (tier-name (model-tier model)))

(defun tiers ()
  "The tiers the archive names, from the top of the ladder down."
  +tier-order+)

(defun tier-key (tier)
  "TIER as the keyword the data uses; a string names the same tier."
  (etypecase tier
    (keyword tier)
    (string (intern (string-upcase tier) :keyword))))

(defparameter +tier-names+
  '((:mythos . "mythos") (:opus . "opus") (:sonnet . "sonnet") (:haiku . "haiku"))
  "A tier as a person reads it.")

(defun tier-name (tier)
  "A tier as a person reads it."
  (let ((key (tier-key tier)))
    (or (cdr (assoc key +tier-names+)) (string-downcase (string key)))))

(defun tier-sentence (tier)
  (cdr (assoc (tier-key tier) +tiers+)))

(defun tier-models (tier)
  "Every model on one tier, the archive's order kept."
  (models-matching :tier (tier-name tier)))

(defun event-when (event) (getf event :when))
(defun event-what (event) (getf event :what))
(defun event-file (event) (getf event :file))
(defun event-line (event) (getf event :line))

(defun events () "The dates the archive gives, in order." +events+)

(defun facts () "The questions the archive is worth opening for." +facts+)
(defun fact-question (fact) (getf fact :question))
(defun fact-answer (fact) (getf fact :answer))
(defun fact-file (fact) (getf fact :file))
(defun fact-line (fact) (getf fact :line))

;;; --- matching --------------------------------------------------------------------

(defun contains-p (needle haystack)
  "Whether NEEDLE appears in HAYSTACK, ignoring case; NIL when either is empty."
  (and (stringp haystack)
       (plusp (length haystack))
       (stringp needle)
       (plusp (length needle))
       (not (null (search (string-downcase needle) (string-downcase haystack))))))

(defun quote-text-of (quote) (getf quote :text))

(defun model-mentions-p (model text)
  "Whether TEXT appears in anything the entry says."
  (or (contains-p text (model-name model))
      (contains-p text (tier-name (model-tier model)))
      (contains-p text (model-availability model))
      (contains-p text (model-note model))
      (some (lambda (quote) (contains-p text (quote-text-of quote))) (model-quotes model))))

(defun models-matching (&key tier text)
  "The models that satisfy every given filter. TIER is a tier name as the archive
spells it (mythos, opus); TEXT is matched against the name, the tier, the
availability, the note and every quote. With no filter, every model."
  (remove-if-not
   (lambda (model)
     (and (or (null tier) (zerop (length tier))
              (string-equal tier (model-tier-name model)))
          (or (null text) (zerop (length text)) (model-mentions-p model text))))
   (models)))

(defun model-by-name (name)
  "The one model NAME most nearly names: the same name, a name starting with it,
or a name containing it. A tier that has exactly one model names it too."
  (let ((wanted (string-downcase (or name ""))))
    (when (plusp (length wanted))
      (or (find wanted (models) :test #'string= :key (lambda (m) (string-downcase (model-name m))))
          (find-if (lambda (m) (contains-p wanted (model-name m))) (models))
          (let ((by-tier (models-matching :tier wanted)))
            (and (= 1 (length by-tier)) (first by-tier)))))))

(defun facts-matching (text)
  "The questions and answers that mention TEXT."
  (if (or (null text) (zerop (length text)))
      (facts)
      (remove-if-not (lambda (fact)
                       (or (contains-p text (fact-question fact))
                           (contains-p text (fact-answer fact))))
                     (facts))))

;;; --- what a person reads ----------------------------------------------------------

(defun model-quote-lines (model)
  "The entry's quotes as the lines a person reads, each with its document and line."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (quote)
                    (format nil "  ~a line ~a: ~a"
                            (getf quote :file) (getf quote :line) (quote-text-of quote)))
                  (model-quotes model))))

(defun name-width ()
  "The column the model names are padded to, wide enough for the longest."
  (1+ (reduce #'max (models) :key (lambda (m) (length (model-name m))))))

(defun model-row (model)
  "One model as the line a listing gives."
  (format nil "  ~va ~va ~a~@[  [~a]~]"
          (name-width) (model-name model)
          8 (model-tier-name model)
          (model-availability model)
          (model-api model)))

(defun model-block (model)
  "One model as the block a person reads: where it sits, whether they can have it, and its own words."
  (format nil "~a~%  tier: ~a - ~a~%  available: ~a~@[~%  since: ~a~]~@[~%  api: ~a~]~%  note: ~a~%  the archive says:~%~a"
          (model-name model)
          (model-tier-name model)
          (or (tier-sentence (model-tier model)) "a tier the archive does not describe")
          (model-availability model)
          (model-since model)
          (model-api model)
          (or (model-note model) "none")
          (model-quote-lines model)))

(defun model-list-text (models &key (heading nil))
  "MODELS as the lines of one listing, HEADING above them."
  (if (null models)
      "no model in the archive matches that."
      (format nil "~@[~a~%~]~{~a~^~%~}"
              (and heading (format nil "~a (~d)" heading (length models)))
              (mapcar #'model-row models))))

(defun hierarchy-text ()
  "The ladder itself: tier by tier, top down, the models the archive puts on each."
  (format nil "~{~a~^~%~%~}"
          (loop for tier in (tiers)
                for on-tier = (tier-models tier)
                collect (format nil "~a - ~a~%~@[~{~a~^~%~}~]"
                                (string-upcase (tier-name tier))
                                (or (tier-sentence tier) "a tier the archive does not describe")
                                (when on-tier
                                  (mapcar (lambda (m)
                                            (format nil "  ~a: ~a" (model-name m) (model-availability m)))
                                          on-tier))))))

(defun timeline-text ()
  "The dates the archive gives, in order."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (event)
                    (format nil "  ~va ~a~@[  (~a line ~a)~]"
                            (+ 2 (reduce #'max (events) :key (lambda (e) (length (event-when e)))))
                            (event-when event) (event-what event)
                            (event-file event) (event-line event)))
                  (events))))

(defun file-key-width ()
  "The column the document keys are padded to."
  (1+ (reduce #'max (files) :key (lambda (f) (length (file-key f))))))

(defun file-row (file)
  "One document as the line an index gives."
  (format nil "  ~va ~va ~a~%      ~a~%      ~:d bytes, ~:d lines, sha256 ~a"
          (file-key-width) (file-key file)
          10 (file-vendor file)
          (file-document file)
          (file-says file)
          (file-bytes file) (file-lines file) (subseq (file-sha file) 0 16)))

(defun files-text (files &key (heading nil))
  "FILES as the index a person reads, HEADING above them."
  (if (null files)
      "no document in the archive matches that."
      (format nil "~@[~a~%~]~{~a~^~%~}"
              (and heading (format nil "~a (~d)" heading (length files)))
              (mapcar #'file-row files))))

(defun summary-text ()
  "How many models, on which tiers, over how many documents, from where."
  (format nil "~d models on ~d tiers, quoted from ~d documents (~:d bytes) of ~a.~%~%~a"
          (length (models)) (length (tiers)) (length (files))
          (reduce #'+ (files) :key #'file-bytes) (getf +archive+ :name)
          (hierarchy-text)))

(defun source-line ()
  "The provenance line every answer ends with."
  (format nil "Source: ~a at ~a, read ~a. The hierarchy is quoted from ~a."
          (getf +archive+ :name) (subseq (getf +archive+ :head) 0 9) +captured+ +primary+))

(defun source-block ()
  "The provenance, whole: the archive, its head, and the primary document's digest."
  (let ((file (file-by-key +primary+)))
    (format nil "archive: ~a~%head:    ~a (~a)~%read:    ~a~%~%documents: ~d, ~:d bytes in all~%~%primary: ~a~%  path:   ~a~%  bytes:  ~:d~%  lines:  ~:d~%  sha256: ~a~%~%~a"
            (getf +archive+ :name) (getf +archive+ :head) (getf +archive+ :head-date) +captured+
            (length (files)) (reduce #'+ (files) :key #'file-bytes)
            (file-key file) (file-path file) (file-bytes file) (file-lines file) (file-sha file)
            "Every word above is quoted from that archive, not from a vendor page. Where a vendor and the archive disagree, this cell gives the archive's reading of it.")))

;;; --- the documents themselves -------------------------------------------------------

(defun cache-directory ()
  "The directory the fetched documents live in."
  (merge-pathnames "nodecode-model-tiers/"
                   (uiop:ensure-directory-pathname
                    (or (ignore-errors (uiop:xdg-cache-home)) "~/.cache/"))))

(defun cache-path (file)
  "Where the local copy of FILE lives."
  (merge-pathnames (format nil "~a~@[.~a~]" (file-key file)
                           (let ((dot (position #\. (file-path file) :from-end t)))
                             (and dot (subseq (file-path file) (1+ dot)))))
                   (cache-directory)))

(defun raw-url (file)
  "The URL FILE is fetched from."
  (format nil "https://raw.githubusercontent.com/elder-plinius/CL4R1T4S/main/~a"
          (substitute #\% #\Space (file-path file))))

(defun fetch-file (&key file force)
  "The path of a local copy of FILE (a document, or its key), downloaded once unless
FORCE. Signals an error when there is no copy and the download fails."
  (let* ((document (etypecase file
                      (list file)
                      (string (or (file-by-key file)
                                  (error "no document in the archive is named ~s" file)))))
         (path (cache-path document)))
    (when (or force (not (probe-file path)))
      (ensure-directories-exist path)
      (let ((status (nth-value 2 (uiop:run-program
                                  (list "curl" "-fsSL" "--max-time" "180" "-o" (namestring path)
                                        (raw-url document))
                                  :ignore-error-status t))))
        (unless (and (zerop status) (probe-file path))
          (ignore-errors (delete-file path))
          (error "could not fetch ~a (curl exited ~a)" (raw-url document) status))))
    path))

(defun split-lines (text)
  "TEXT as a list of its lines."
  (with-input-from-string (in text)
    (loop for line = (read-line in nil nil) while line collect line)))

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

(defun quote-text (&key text lines-text file path)
  "One document's own words: the LINES-TEXT range when given, else every line that
carries TEXT, numbered, at most twenty. FILE names the document (its key, by
default the primary one); PATH reads a local copy instead of fetching. The one
verb here that reaches the network."
  (let* ((document (if file (or (file-by-key file)
                                (error "no document in the archive is named ~s" file))
                       (file-by-key +primary+)))
         (path (or path (fetch-file :file document)))
         (range (line-range lines-text)))
    (cond
      (range
       (format nil "~a, lines ~a-~a~%~a~%~a"
               (file-key document) (car range) (cdr range)
               (read-lines (car range) (cdr range) :path path) (source-line)))
      ((and text (plusp (length text)))
       (let* ((all (split-lines (uiop:read-file-string path)))
              (hits (loop for line in all
                          for n from 1
                          when (contains-p text line) collect (cons n line))))
         (if (null hits)
             (format nil "no line of ~a carries ~s. That document is ~:d lines, ~:d bytes."
                     (file-key document) text (length all) (file-bytes document))
             (format nil "~a~%~a~%~a~%~a"
                     (numbered (subseq hits 0 (min 20 (length hits))))
                     (if (> (length hits) 20)
                         (format nil "(the first 20 of ~d lines of ~a carrying ~s)"
                                 (length hits) (file-key document) text)
                         "")
                     (source-line) (file-key document)))))
      (t (format nil "give a word to look for, or a line range as 117 or 116-122. ~a is ~:d lines.~%~a"
                 (file-key document) (file-lines document) (source-line))))))
