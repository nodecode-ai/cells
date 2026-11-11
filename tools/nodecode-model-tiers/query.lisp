;;;; query.lisp --- the hierarchy read four ways, and the file quoted.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The hierarchy answers offline: DATA.LISP carries it. Only QUOTE-TEXT goes
;;;; to the network, and only once -- it puts the file under the cache and reads
;;;; it from there after that.

(in-package #:nodecode-model-tiers)

;;; --- reading one entry -----------------------------------------------------------

(defun models ()
  "Every model the file names, in the file's order."
  +models+)

(defun model-name (model) (getf model :name))
(defun model-tier (model) (getf model :tier))
(defun model-availability (model) (getf model :availability))
(defun model-note (model) (getf model :note))
(defun model-quotes (model) (getf model :quotes))
(defun model-since (model) (getf model :since))
(defun model-api (model) (getf model :api))

(defun model-tier-name (model)
  "The model's tier, as the file spells it."
  (tier-name (model-tier model)))

(defun tiers ()
  "The tiers the file names, from the top of the ladder down."
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
  "Every model on one tier, the file's order kept."
  (models-matching :tier (tier-name tier)))

(defun event-when (event) (getf event :when))
(defun event-what (event) (getf event :what))
(defun event-line (event) (getf event :line))

(defun events () "The dates the file gives, in order." +events+)

(defun facts () "The questions this file is worth opening for." +facts+)
(defun fact-question (fact) (getf fact :question))
(defun fact-answer (fact) (getf fact :answer))
(defun fact-lines (fact) (getf fact :lines))

;;; --- matching --------------------------------------------------------------------

(defun contains-p (needle haystack)
  "Whether NEEDLE appears in HAYSTACK, ignoring case; NIL when either is empty."
  (and (stringp haystack)
       (plusp (length haystack))
       (stringp needle)
       (plusp (length needle))
       (not (null (search (string-downcase needle) (string-downcase haystack))))))

(defun model-mentions-p (model text)
  "Whether TEXT appears in anything the entry says."
  (or (contains-p text (model-name model))
      (contains-p text (tier-name (model-tier model)))
      (contains-p text (model-availability model))
      (contains-p text (model-note model))
      (some (lambda (quote) (contains-p text (cdr quote))) (model-quotes model))))

(defun models-matching (&key tier text)
  "The models that satisfy every given filter. TIER is a tier name as the file
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

(defun quote-line-number (quote) (car quote))
(defun quote-sentence (quote) (cdr quote))

(defun model-quote-lines (model)
  "The entry's quotes as the lines a person reads, each with the file's line number."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (quote)
                    (format nil "  line ~a: ~a" (quote-line-number quote) (quote-sentence quote)))
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
  (format nil "~a~%  tier: ~a - ~a~%  available: ~a~@[~%  since: ~a~]~@[~%  api: ~a~]~%  note: ~a~%  the file says:~%~a"
          (model-name model)
          (model-tier-name model)
          (or (tier-sentence (model-tier model)) "a tier the file does not describe")
          (model-availability model)
          (model-since model)
          (model-api model)
          (or (model-note model) "none")
          (model-quote-lines model)))

(defun model-list-text (models &key (heading nil))
  "MODELS as the lines of one listing, HEADING above them."
  (if (null models)
      "no model in the file matches that."
      (format nil "~@[~a (~d)~%~]~{~a~^~%~}"
              heading (length models) (mapcar #'model-row models))))

(defun hierarchy-text ()
  "The ladder itself: tier by tier, top down, the models the file puts on each."
  (format nil "~{~a~^~%~%~}"
          (loop for tier in (tiers)
                for on-tier = (tier-models tier)
                collect (format nil "~a - ~a~%~@[~{~a~^~%~}~]"
                                (string-upcase (tier-name tier))
                                (or (tier-sentence tier) "a tier the file does not describe")
                                (when on-tier
                                  (mapcar (lambda (m)
                                            (format nil "  ~a: ~a" (model-name m) (model-availability m)))
                                          on-tier))))))

(defun timeline-text ()
  "The dates the file gives, in order."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (event)
                    (format nil "  ~va ~a~@[  (line ~a)~]"
                            (+ 2 (reduce #'max (events) :key (lambda (e) (length (event-when e)))))
                            (event-when event)
                            (event-what event) (event-line event)))
                  (events))))

(defun summary-text ()
  "How many models, on which tiers, from where."
  (format nil "~d models on ~d tiers, from one file of ~:d bytes (~:d lines).~%~%~a"
          (length (models)) (length (tiers))
          (getf +file+ :bytes) (getf +file+ :lines)
          (hierarchy-text)))

(defun source-line ()
  "The provenance line every answer ends with."
  (format nil "Source: ~a (~:d bytes, sha256 ~a). Read ~a."
          +source+ (getf +file+ :bytes)
          (subseq (getf +file+ :sha256) 0 16)
          +captured+))

(defun source-block ()
  "The provenance, whole: the file, its digest, its commit, and where to read it."
  (format nil "file:    ~a~%bytes:   ~:d~%lines:   ~:d~%sha256:  ~a~%commit:  ~a (~a)~%in:      elder-plinius/CL4R1T4S~%read it: ~a~%read on: ~a~%~%~a"
          (getf +file+ :path) (getf +file+ :bytes) (getf +file+ :lines)
          (getf +file+ :sha256) (getf +file+ :commit) (getf +file+ :committed)
          (getf +file+ :url) +captured+
          "Every word above is quoted from that file, not from a vendor page. Where a vendor and the file disagree, this cell gives the file's reading of itself."))

;;; --- the file itself, for a quote the data does not carry ---------------------------

(defun cache-directory ()
  "The directory a fetched copy of the file lives in."
  (merge-pathnames "nodecode-model-tiers/"
                   (uiop:ensure-directory-pathname
                    (or (ignore-errors (uiop:xdg-cache-home)) "~/.cache/"))))

(defun file-path ()
  "Where a fetched copy of the file lives."
  (merge-pathnames "CLAUDE-CODE-FABLE-5.2.txt" (cache-directory)))

(defun fetch-file (&key force)
  "The path of a local copy of the file, downloading it once unless FORCE. Signals
an error when there is no copy and the download fails."
  (let ((path (file-path)))
    (when (or force (not (probe-file path)))
      (ensure-directories-exist path)
      (let ((status (nth-value 2 (uiop:run-program
                                  (list "curl" "-fsSL" "--max-time" "120" "-o" (namestring path)
                                        (getf +file+ :raw))
                                  :ignore-error-status t))))
        (unless (and (zerop status) (probe-file path))
          (ignore-errors (delete-file path))
          (error "could not fetch ~a (curl exited ~a)" (getf +file+ :raw) status))))
    path))

(defun split-lines (text)
  "TEXT as a list of its lines."
  (with-input-from-string (in text)
    (loop for line = (read-line in nil nil) while line collect line)))

(defun read-lines (from to &key path)
  "The file's lines FROM..TO as a string, each numbered. PATH defaults to a fetched copy."
  (let* ((lines (split-lines (uiop:read-file-string (or path (fetch-file)))))
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

(defun quote-text (&key text lines lines-text path)
  "The file's own words: the LINES-TEXT range when given, else every line that
carries TEXT, numbered, at most 20. The one verb here that reaches the network."
  (let* ((path (or path (fetch-file)))
         (range (line-range (or lines-text lines))))
    (cond
      (range
       (format nil "~a~%~a" (read-lines (car range) (cdr range) :path path) (source-line)))
      ((and text (plusp (length text)))
       (let* ((all (split-lines (uiop:read-file-string path)))
              (hits (loop for line in all
                          for n from 1
                          when (contains-p text line) collect (cons n line))))
         (if (null hits)
             (format nil "no line of the file carries ~s. The file is ~:d lines, ~:d bytes."
                     text (length all) (getf +file+ :bytes))
             (format nil "~a~%~a~%~a"
                     (numbered (subseq hits 0 (min 20 (length hits))))
                     (if (> (length hits) 20)
                         (format nil "(the first 20 of ~d lines carrying ~s)" (length hits) text)
                         "")
                     (source-line)))))
      (t (format nil "give a word to look for, or a line range as 117 or 116-122. The file is ~:d lines.~%~a"
                 (getf +file+ :lines) (source-line))))))
