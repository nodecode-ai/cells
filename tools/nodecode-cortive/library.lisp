;;;; library.lisp --- reading Cortive's library as the original read it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Cortive keeps its captures in one SQLite file: a `captures' table and an
;;;; FTS5 index over it, `captures_fts'. The original plugin opened that file
;;;; read-only (mode=ro, query_only) and ranked with
;;;;
;;;;   score = 1.00 * relevance (bm25, scaled against the best candidate)
;;;;         + 0.25 * recency   (90-day half-life)
;;;;         + 0.15 * tag match + 0.10 * title match
;;;;
;;;; and a capture matching more of the query's words always ranked above one
;;;; matching fewer. This file is that, in Lisp: the same search terms, the
;;;; same FTS5 expression, the same weights, the same tie-breaks, the same
;;;; read-only handle. Meaning search through the running Cortive app is the
;;;; one part left out (see NOTICE).

(in-package #:nodecode-cortive)

;;; --- a failure a person can act on ------------------------------------------

(define-condition cortive-error (error)
  ((detail :initarg :detail :reader cortive-error-detail))
  (:report (lambda (condition stream)
             (format stream "~a" (cortive-error-detail condition)))))

(defun cortive-fail (control &rest arguments)
  "Signal CORTIVE-ERROR with CONTROL formatted."
  (error 'cortive-error :detail (apply #'format nil control arguments)))

;;; --- what a query is made of -------------------------------------------------

(defparameter *stopwords*
  '("a" "about" "after" "again" "all" "also" "am" "an" "and" "any" "are" "as" "at"
    "be" "been" "before" "being" "but" "by" "can" "could" "did" "do" "does" "doing"
    "done" "for" "from" "had" "has" "have" "having" "he" "her" "here" "hers" "him"
    "his" "how" "i" "if" "in" "into" "is" "it" "its" "just" "me" "more" "most"
    "my" "no" "nor" "not" "of" "off" "on" "once" "only" "or" "other" "our" "ours"
    "out" "over" "own" "please" "same" "she" "should" "so" "some" "such" "than"
    "that" "the" "their" "them" "then" "there" "these" "they" "this" "those"
    "through" "to" "too" "under" "until" "up" "very" "was" "we" "were" "what"
    "when" "where" "which" "while" "who" "whom" "why" "will" "with" "would" "you"
    "your" "yours" "yourself" "remember" "recall" "find" "show" "tell" "know"
    "saved" "save" "captured" "earlier" "ago" "last" "thing" "things" "something"
    "stuff" "anything")
  "Words that carry no search signal in a chat message, as the original kept them.")

(defparameter *max-query-terms* 12)

(defparameter *token-pattern* (ppcre:create-scanner "[A-Za-z0-9_]+")
  "A word, as the original's word regex read it.")

(defun light-stem (word)
  "Trim common English endings so a prefix match finds other forms of the word,
conservatively: a trimmed stem still has to be specific."
  (let ((n (length word)))
    (cond ((or (< n 5) (plusp (count-if-not #'alpha-char-p word))) word)
          ((and (>= n 6) (ppcre:scan "ies$" word)) (subseq word 0 (- n 3)))
          ((ppcre:scan "(sses|shes|ches|xes|zes)$" word) (subseq word 0 (- n 2)))
          ((and (ppcre:scan "s$" word) (not (ppcre:scan "(ss|us|is)$" word)))
           (subseq word 0 (- n 1)))
          ((and (>= n 7) (ppcre:scan "ing$" word)) (subseq word 0 (- n 3)))
          ((and (>= n 6) (ppcre:scan "ed$" word) (not (ppcre:scan "eed$" word)))
           (subseq word 0 (- n 2)))
          (t word))))

(defun query-terms (text)
  "The searchable words of TEXT: lowercased, de-duplicated, stopwords dropped,
stemmed and capped. Everything but word characters is discarded, so FTS5 syntax
cannot get through."
  (let ((seen '()))
    (when (stringp text)
      (dolist (token (ppcre:all-matches-as-strings *token-pattern* (string-downcase text)))
        (let ((token (string-trim "_" token)))
          (when (and (plusp (length token))
                     (not (member token *stopwords* :test #'string=))
                     (or (> (length token) 1) (digit-char-p (char token 0))))
            (let ((stem (light-stem token)))
              (unless (member stem seen :test #'string=)
                (push stem seen)
                (when (>= (length seen) *max-query-terms*)
                  (return))))))))
    (nreverse seen)))

(defun fts-term (term)
  "TERM quoted, so FTS5 keywords are plain words; prefix match from three letters on."
  (if (>= (length term) 3)
      (format nil "\"~a\"*" term)
      (format nil "\"~a\"" term)))

(defun fts-match (terms)
  "An FTS5 MATCH expression over the text columns: every term, or NIL for none."
  (when terms
    (format nil "{title url domain tags content note} : (~{~a~^ OR ~})"
            (mapcar #'fts-term terms))))

;;; --- time ------------------------------------------------------------------

(defun parse-iso-time (text)
  "Universal time for an ISO-8601 timestamp the library stores, or NIL."
  (when (and (stringp text) (>= (length text) 19))
    (handler-case
        (encode-universal-time (parse-integer text :start 17 :end 19)
                               (parse-integer text :start 14 :end 16)
                               (parse-integer text :start 11 :end 13)
                               (parse-integer text :start 8 :end 10)
                               (parse-integer text :start 5 :end 7)
                               (parse-integer text :start 0 :end 4)
                               0)
      (error () nil))))

(defun recency (timestamp now)
  "Exponential decay with a 90-day half-life, 0 for an unreadable stamp."
  (let ((then (parse-iso-time timestamp)))
    (if (null then)
        0.0
        (exp (* (- (log 2.0)) (/ (max 0.0 (- now then)) (* 86400.0 90.0)))))))

(defun stamp-text (universal)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun date-bound (value &key end)
  "A date the user named - 2026-05-15, 2026-05 or 2026 - as the UTC moment its
period starts, or with END the moment the day after it starts: text that
compares correctly against the library's UTC timestamps."
  (let ((text (and (stringp value) (string-trim " " value))))
    (when (or (null text) (zerop (length text)))
      (return-from date-bound nil))
    (handler-case
        (let* ((year (parse-integer text :end 4))
               (month (if (> (length text) 5) (parse-integer text :start 5 :end 7) 1))
               (day (if (> (length text) 8) (parse-integer text :start 8 :end 10) 1))
               (start (encode-universal-time 0 0 0 day month year 0)))
          (stamp-text
           (if (not end)
               start
               (cond ((> (length text) 8) (+ start 86400))
                     ((> (length text) 5)
                      (encode-universal-time 0 0 0 1
                                             (if (= month 12) 1 (1+ month))
                                             (if (= month 12) (1+ year) year) 0))
                     (t (encode-universal-time 0 0 0 1 1 (1+ year) 0))))))
      (error () (cortive-fail "Dates look like 2026-05-15, 2026-05 or 2026 (got ~s)." value)))))

;;; --- the handle ---------------------------------------------------------------

(defun library (path)
  "PATH opened read-only: SQLite itself refuses every write on this handle."
  (unless (and (stringp path) (plusp (length path)) (probe-file path))
    (cortive-fail "Cortive's library is not at ~a. Open the Cortive app once so it creates its library, or set <library> in the cortive section to the file it keeps."
          path))
  (let ((db (sqlite:connect (namestring (probe-file path)) :busy-timeout 5000)))
    (sqlite:execute-non-query db "pragma query_only = on")
    db))

(defmacro with-library ((var path) &body body)
  `(let ((,var (library ,path)))
     (unwind-protect (progn ,@body)
       (ignore-errors (sqlite:disconnect ,var)))))

(defun schema-version (db)
  (or (sqlite:execute-single db "pragma user_version") 0))

;;; --- filters ------------------------------------------------------------------

(defparameter *source-aliases*
  '(("screenshot" . "screenshot_capture") ("screenshots" . "screenshot_capture")
    ("photo" . "_import") ("photos" . "_import") ("picture" . "_import")
    ("image" . "_import") ("images" . "_import") ("audio" . "_import")
    ("voice" . "_import") ("recording" . "_import") ("document" . "_import")
    ("documents" . "_import") ("pdf" . "pdf_import") ("web" . "browser_extension")
    ("browser" . "browser_extension") ("page" . "browser_extension")
    ("article" . "browser_extension") ("clipboard" . "clipboard_capture")
    ("copied" . "clipboard_capture") ("folder" . "watched_folder"))
  "Everyday words for kinds of capture, to the source names Cortive records.")

(defun resolve-sources (db wanted)
  "What a caller passed - screenshot_capture, screenshots, pdf, photos - as the
source values present in this library. An unknown word resolves to nothing."
  (let* ((present (mapcar #'first
                          (sqlite:execute-to-list
                           db "select distinct source from captures where source is not null")))
         (key (string-downcase (string-trim " " wanted)))
         (tail (or (cdr (assoc key *source-aliases* :test #'string=))
                   (cdr (assoc (light-stem key) *source-aliases* :test #'string=)))))
    (cond ((member key present :test #'string=) (list key))
          (tail (remove-if-not (lambda (source)
                                  (ppcre:scan (ppcre:quote-meta-chars tail) source))
                                present))
          (t (let ((stem (light-stem (substitute #\_ #\Space key))))
               (remove-if-not (lambda (source)
                                (or (ppcre:scan (format nil "^~a" (ppcre:quote-meta-chars stem)) source)
                                    (string= stem (first (ppcre:split "_" source)))))
                              present))))))

(defun filters (db source since-days from to)
  "The WHERE fragments and their parameters a caller's filters make."
  (let ((fragments '()) (params '()))
    (labels ((add (fragment &rest arguments)
               (push fragment fragments)
               (dolist (argument arguments) (push argument params))))
      (let ((start (date-bound from)) (finish (date-bound to :end t)))
        (when start (push (format nil "c.timestamp >= '~a'" start) fragments))
        (when finish (push (format nil "c.timestamp < '~a'" finish) fragments)))
      (when (and since-days (plusp since-days))
        (push (format nil "c.timestamp >= '~a'" (stamp-text (- (get-universal-time) (* 86400 since-days)))) fragments))
      (when (and source (plusp (length source)))
        (let ((sources (resolve-sources db source)))
          (if sources
              (progn (push (format nil "c.source in (~{\"~a\"~^, ~})" sources) fragments)
                     )
              (push "0" fragments)))))
    (values fragments (reverse params))))

;;; --- one row, read ---------------------------------------------------------

(defun tags-of (row index)
  (let ((raw (nth index row)))
    (if (and (stringp raw) (plusp (length raw)))
        (ppcre:all-matches-as-strings "\"[^\"]*\"" raw)
        '())))

(defun clip (text limit)
  "TEXT on one line, cut to LIMIT characters at a word boundary."
  (let* ((flat (ppcre:regex-replace-all "\\s+" (or text "") " "))
         (flat (string-trim " " flat)))
    (if (<= (length flat) limit)
        flat
        (concatenate 'string
                     (string-right-trim " ,.;:-" (subseq flat 0 (max 1 (1- limit))))
                     "…"))))

;;; --- search -------------------------------------------------------------------

(defparameter +max-limit+ 20)

(defun term-in (term text)
  "Is TERM the start of a word in TEXT, as the original's word hit test asked?"
  (and (stringp text) (plusp (length term))
       (some (lambda (word)
               (and (>= (length word) (length term))
                    (string= term word :end2 (length term))))
             (ppcre:all-matches-as-strings "[A-Za-z0-9_]+" (string-downcase text)))))

(defun word-hits (terms text)
  (if (and terms text (plusp (length terms)))
      (/ (float (count-if (lambda (term) (term-in term text)) terms)) (length terms))
      0.0))

(defun rank-rows (rows terms limit now)
  "ROWS (one per FTS match) scored as the original scored them: more of the
query's words first, then the score, ties newest first and then by id."
  (let* ((best (let ((top 0.0))
                 (dolist (row rows top) (setf top (max top (- (getf row :rank)))))))
         (scored
           (mapcar (lambda (row)
                     (let* ((haystack (format nil "~a ~a ~a" (getf row :title)
                                               (getf row :tags) (getf row :head)))
                            (matched (count-if (lambda (term) (term-in term haystack)) terms))
                            (relevance (if (plusp best)
                                           (/ (max 0.0 (- (getf row :rank))) best)
                                           0.0))
                            (score (+ relevance
                                      (* 0.25 (recency (getf row :timestamp) now))
                                      (* 0.15 (word-hits terms (format nil "~{~a~^ ~}" (getf row :tags))))
                                      (* 0.10 (word-hits terms (getf row :title))))))
                       (list* :matched matched :score score row)))
                   rows)))
    (setf scored (stable-sort scored #'string< :key (lambda (row) (getf row :id))))
    (setf scored (stable-sort scored #'> :key (lambda (row) (or (parse-iso-time (getf row :timestamp)) 0))))
    (setf scored (stable-sort scored (lambda (left right)
                                        (let ((left-matched (getf left :matched))
                                              (right-matched (getf right :matched)))
                                          (if (/= left-matched right-matched)
                                              (> left-matched right-matched)
                                              (> (getf left :score) (getf right :score)))))))
    (let ((seen '()) (out '()))
      (dolist (row scored)
        (let ((hash (getf row :hash)))
          (unless (and hash (member hash seen :test #'equal))
            (when hash (push hash seen))
            (push row out))))
      (subseq (nreverse out) 0 (min limit (length out))))))

(defun browse (db where params limit)
  "The newest captures a filter allows: the answer to a question with no words to search for."
  (mapcar (lambda (row)
            (list :id (nth 0 row) :title (nth 1 row) :url (nth 2 row) :source (nth 3 row)
                  :timestamp (nth 4 row) :tags (tags-of row 5) :score nil
                  :excerpt (clip (nth 6 row) 400)))
          (apply #'sqlite:execute-to-list db
                 (format nil "select c.id, c.title, c.url, c.source, c.timestamp, c.tags, ~
                                 substr(c.content, 1, 600) from captures c ~
                              where coalesce(c.is_encrypted, 0) = 0~a ~
                              order by c.timestamp desc, c.id limit ?" where)
                 (append params (list (max 1 (min limit +max-limit+)))))))

(defun search-captures (path query &key (limit 5) source (since-days 0) from-date to-date)
  "Captures matching QUERY, best first, as plists; with no searchable word, the
newest the filters allow."
  (let ((limit (max 1 (min limit +max-limit+)))
        (terms (query-terms query)))
    (with-library (db path)
      (multiple-value-bind (fragments params)
          (filters db source since-days from-date to-date)
        (let ((where (if fragments (format nil " and ~{~a~^ and ~}" fragments) "")))
          (if (null terms)
              (browse db where params limit)
              (rank-rows
               (mapcar (lambda (row)
                         (list :id (nth 0 row) :title (nth 1 row) :url (nth 2 row)
                               :source (nth 3 row) :timestamp (nth 4 row) :tags (tags-of row 5)
                               :hash (nth 6 row) :rank (nth 7 row) :excerpt (nth 8 row)
                               :head (nth 9 row)))
                       (apply #'sqlite:execute-to-list db
                              (format nil "select c.id, c.title, c.url, c.source, c.timestamp, c.tags, ~
                                              c.content_hash, ~
                                              bm25(captures_fts, 0.0, 3.0, 0.5, 0.5, 1.5, 0.0, 1.0, 1.0) as rank, ~
                                              snippet(captures_fts, 6, '', '', '…', 32) as excerpt, ~
                                              substr(c.content, 1, 600) as head ~
                                       from captures_fts join captures c on c.rowid = captures_fts.rowid ~
                                       where captures_fts match ? and coalesce(c.is_encrypted, 0) = 0~a ~
                                       order by rank limit ?" where)
                              (append (list (fts-match terms)) params (list (* 4 limit)))))
               terms limit (get-universal-time))))))))

;;; --- one capture ---------------------------------------------------------------

(defparameter +get-content-chars+ 8000)

(defparameter *id-pattern* (ppcre:create-scanner "^[A-Za-z0-9_-]{1,128}$"))

(defun get-capture (path id &key (offset 0))
  "One capture in full, by id, from OFFSET characters on."
  (unless (and (stringp id) (ppcre:scan *id-pattern* id))
    (cortive-fail "id must be a capture id from cortive_search."))
  (with-library (db path)
    (let* ((columns (mapcar #'second (sqlite:execute-to-list db "pragma table_info(captures)")))
           (note (if (member "note" columns :test #'string=) "note" "null as note"))
           (rows (sqlite:execute-to-list
                  db (format nil "select id, title, content, url, source, tags, timestamp, ~a ~
                                  from captures where id = ? and coalesce(is_encrypted, 0) = 0" note)
                  id)))
      (unless rows
        (cortive-fail "No readable capture with id ~a." id))
      (let* ((row (first rows))
             (content (or (nth 2 row) ""))
             (offset (max 0 (min offset (length content)))))
        (list :id (nth 0 row) :title (nth 1 row) :url (nth 3 row) :source (nth 4 row)
              :tags (tags-of row 5) :timestamp (nth 6 row) :note (nth 7 row)
              :content (subseq content offset (min (length content) (+ offset +get-content-chars+)))
              :offset offset :chars (length content)
              :truncated (> (length content) (+ offset +get-content-chars+))
              :next (when (> (length content) (+ offset +get-content-chars+))
                      (+ offset +get-content-chars+)))))))

;;; --- the whole library ---------------------------------------------------------

(defparameter +example-titles+ 4)
(defparameter +recent-captures+ 5)
(defparameter +collection-list-max+ 100)

(defun collection-captures (db collection)
  (mapcar (lambda (row)
            (list :id (nth 0 row) :title (nth 1 row) :source (nth 2 row) :timestamp (nth 3 row)))
          (sqlite:execute-to-list
           db "select c.id, c.title, c.source, c.timestamp from capture_collections cc
                 join captures c on c.id = cc.capture_id
                where cc.collection_id = ? and coalesce(c.is_encrypted, 0) = 0
                order by cc.score desc, c.timestamp desc limit ?"
           collection (1+ +collection-list-max+))))

(defun library-report (path &key collection)
  "Totals, where captures came from, the newest, and every collection."
  (with-library (db path)
    (let* ((tables (mapcar #'first
                            (sqlite:execute-to-list
                             db "select name from sqlite_master where type = 'table'")))
           (grouped (and (member "collections" tables :test #'string=)
                         (member "capture_collections" tables :test #'string=))))
      (when collection
        (unless grouped
          (cortive-fail "This Cortive library has no collections yet."))
        (let* ((by-id (sqlite:execute-to-list
                       db "select id, name from collections where id = ?" collection))
               (by-name (sqlite:execute-to-list
                         db "select id, name from collections where name = ? collate nocase order by member_count desc limit 1"
                         collection))
               (found (or (first by-id) (first by-name))))
          (unless found
            (cortive-fail "No collection named ~s. Call cortive_collections without arguments to see the collection names."
                  collection))
          (return-from library-report
            (list :collection (second found)
                  :captures (collection-captures db (first found))))))
      (list :total (or (sqlite:execute-single
                        db "select count(*) from captures where coalesce(is_encrypted, 0) = 0") 0)
            :first (caar (sqlite:execute-to-list
                          db "select min(timestamp) from captures where coalesce(is_encrypted, 0) = 0"))
            :latest (caar (sqlite:execute-to-list
                           db "select max(timestamp) from captures where coalesce(is_encrypted, 0) = 0"))
            :by-source (mapcar (lambda (row) (cons (or (first row) "unknown") (second row)))
                               (sqlite:execute-to-list
                                db "select source, count(*) from captures
                                    where coalesce(is_encrypted, 0) = 0
                                    group by source order by 2 desc, 1"))
            :newest (mapcar (lambda (row)
                              (list :id (nth 0 row) :title (nth 1 row)
                                    :source (nth 2 row) :timestamp (nth 3 row)))
                            (sqlite:execute-to-list
                             db "select id, title, source, timestamp from captures c
                                 where coalesce(c.is_encrypted, 0) = 0
                                 order by c.timestamp desc, c.id limit ?"
                             +recent-captures+))
            :collections
            (when grouped
              (mapcar (lambda (row)
                        (list :name (first row) :id (second row) :count (third row)
                              :examples (ppcre:split
                                         (string (code-char 31))
                                         (or (fourth row) ""))))
                      (sqlite:execute-to-list
                       db "select g.name, cc.collection_id, count(*) as n,
                                  group_concat(substr(c.title, 1, 80), char(31))
                             from capture_collections cc
                             join collections g on g.id = cc.collection_id
                             join captures c on c.id = cc.capture_id
                            where coalesce(c.is_encrypted, 0) = 0 and cc.collection_id <> ''
                            group by cc.collection_id order by n desc, g.name limit 40")))))))
