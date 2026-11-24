;;;; docs.lisp --- searching WOOF's own documentation, offline.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original plugin ships WOOF's documentation as a chunked pack and
;;;; searches it with BM25 (its docs.py, standard library only). The pack is
;;;; here as docs/index.json beside this file, carried from the original, and
;;;; this is the same search: the same tokeniser, the same constants, the
;;;; same two-title weighting, the same idf and length normalisation. Nothing
;;;; is downloaded and no service is asked.

(in-package #:nodecode-woof)

(defparameter +stop-words+
  '("a" "an" "and" "are" "as" "at" "be" "by" "for" "from" "has" "have" "how"
    "i" "in" "is" "it" "its" "of" "on" "or" "that" "the" "this" "to" "was"
    "what" "when" "where" "which" "with" "you" "your" "can" "do" "does" "not"
    "no")
  "The words the original's search drops before it scores.")

(defparameter +word-ends+ '(#\. #\- #\+)
  "What the original trims off each word's end.")

(defun word-token-p (character)
  "Whether CHARACTER may stand inside a word."
  (or (alphanumericp character)
      (member character '(#\_ #\. #\- #\+) :test #'char=)))

(defun word-starts-p (character)
  "Whether CHARACTER may open a word: a letter or a digit, never punctuation."
  (alphanumericp character))

(defun split-run (run)
  "RUN's parts, split where the original splits a hyphen or underscore. => a list."
  (loop with start = 0
        for index from 0 below (length run)
        when (member (char run index) '(#\_ #\-) :test #'char=)
          append (let ((part (subseq run start index)))
                   (prog1 (unless (< (length part) 2) (list part))
                     (setf start (1+ index))))
        finally (return (let ((part (subseq run start)))
                          (if (< (length part) 2) nil (list part))))))

(defun words (text)
  "TEXT as the words the search scores: lowercased, stop words dropped, each
hyphenated or underscored word also split into its parts."
  (let ((text (string-downcase (or text "")))
        (found '()))
    (loop with index = 0
          while (< index (length text))
          do (let ((character (char text index)))
               (cond ((not (word-starts-p character)) (incf index))
                     (t (let ((start index))
                          (loop while (and (< index (length text))
                                           (word-token-p (char text index)))
                                do (incf index))
                          (let* ((run (string-right-trim +word-ends+
                                                          (subseq text start index)))
                                 (plain (and (< 1 (length run))
                                             (not (member run +stop-words+
                                                          :test #'string=))
                                             run)))
                            (when plain (push plain found))
                            (when (find-if (lambda (c) (member c '(#\_ #\-) :test #'char=)) run)
                              (dolist (part (split-run run))
                                (unless (member part +stop-words+ :test #'string=)
                                  (push part found))))))))))
    (nreverse found)))

(defun pack-file (name)
  "One file of the documentation pack this cell carries."
  (asdf:system-relative-pathname "nodecode-woof" (format nil "docs/~a" name)))

(defvar *pack* nil
  "The pack, read once: the chunks, each one's word counts, and the corpus's
document frequencies and average length. NIL until the first search.")

(defun read-pack ()
  "The documentation pack as (values CHUNKS DOCUMENT-FREQUENCIES AVERAGE-LENGTH).
Each chunk is a plist of :doc, :section, :text and :counts."
  (let* ((path (probe-file (pack-file "index.json")))
         (document (and path (read-json path)))
         (entries (and document (nlk:json-value document :array "chunks"))))
    (if (null entries)
        (values #() (make-hash-table :test #'equal) 1)
        (let ((counts (make-hash-table :test #'equal))
              (frequencies (make-hash-table :test #'equal))
              (total 0)
              (chunks '()))
          (loop for entry across entries
                do (let* ((title (or (nlk:json-value entry :string "title") ""))
                          (text (or (nlk:json-value entry :string "text") ""))
                          ;; the original counts the title twice, as its docs.py does
                          (tally (make-hash-table :test #'equal)))
                     (dolist (word (words (concatenate 'string title " " title " " text)))
                       (incf (gethash word tally 0))
                       (incf total))
                     (loop for word being the hash-keys of tally
                           do (incf (gethash word frequencies 0)))
                     (push (list :doc (or (nlk:json-value entry :string "doc") "")
                                 :section title
                                 :text text
                                 :counts tally)
                           chunks)))
          (setf counts frequencies)
          (values (coerce (nreverse chunks) 'vector) counts
                  (max 1 (/ total (max 1 (length chunks)))))))))

(defun pack ()
  "The pack, read on the first search and kept."
  (or *pack* (setf *pack* (multiple-value-list (read-pack)))))

(defun score-chunk (chunk query frequencies average total)
  "CHUNK's BM25 score against QUERY, as the original's search computes it."
  (let* ((tally (getf chunk :counts))
         (length (max 1 (loop for count being the hash-values of tally sum count)))
         (score 0.0d0))
    (dolist (word query)
      (let ((count (gethash word tally 0)))
        (when (plusp count)
          (let* ((appears (max 1 (gethash word frequencies 1)))
                 (idf (log (1+ (/ (- total appears -0.5) (+ appears 0.5)))))
                 (k1 1.4d0)
                 (b 0.75d0))
            (incf score (* idf (/ (* count (1+ k1))
                                  (+ count (* k1 (- 1 b (* b (/ length average))))))))))))
    score))

(defun round-two (number)
  "NUMBER to two decimal places, the way the original answers a score."
  (/ (fround (* (coerce number 'double-float) 100)) 100))

(defun doc-hits (query limit)
  "The LIMIT best passages for QUERY. => a list of plists."
  (multiple-value-bind (chunks frequencies average) (apply #'values (pack))
    (let* ((asked (words query))
           (total (max 1 (length chunks))))
      (when (and asked (plusp (length chunks)))
        (let ((scored '()))
          (loop for chunk across chunks
                for score = (score-chunk chunk asked frequencies average total)
                when (plusp score) do (push (cons score chunk) scored))
          (let ((best (sort scored #'> :key #'car)))
            (loop for (score . chunk) in (subseq best 0 (min limit (length best)))
                  collect (list :doc (getf chunk :doc)
                                :section (getf chunk :section)
                                :score (round-two score)
                                :text (getf chunk :text)))))))))

(defun pack-manifest ()
  "The pack's own manifest: which WOOF release the passages came from."
  (let ((path (probe-file (pack-file "MANIFEST.json"))))
    (and path (read-json path))))

(defun search-docs (call)
  "woof_docs: the passages of WOOF's documentation that answer a question."
  (let* ((query (docs-read-query call))
         (limit (max 1 (min 10 (docs-read-max-results call))))
         (hits (doc-hits query limit))
         (manifest (pack-manifest)))
    (nlk:encode-json-object
     (nlk:make-json-object
      "ok" (not (null hits))
      "query" query
      "results" (coerce (loop for hit in hits collect (nlk:make-json-object
                                                       "doc" (getf hit :doc)
                                                       "section" (getf hit :section)
                                                       "score" (getf hit :score)
                                                       "text" (getf hit :text)))
                        'vector)
      "pack" (nlk:make-json-object
              "docs" (or (and manifest (nlk:json-value manifest :string "docs")) 0)
              "chunks" (or (and manifest (nlk:json-value manifest :integer "chunks")) 0)
              "woof_version" (or (and manifest (nlk:json-value manifest :string "woof_version")) ""))
      "note" (if hits
                 "quoted from WOOF's own documentation; the section names the page"
                 "nothing in the pack matched those words; try the subject's own name, e.g. 'physics profile', 'restart' or 'render'")))))
