;;;; cell.lisp --- the five tools, the /knowledge command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin served one idea through five tools: ask about local
;;;; operational knowledge, get the whole artifact back, walk the edges beside
;;;; it, and keep a local record of what helped. Its index was over the
;;;; operator's own tree; so is this one, over the roots the section names.
;;;;
;;;; Nothing here reaches the network. The scan is a walk, the record is one
;;;; tab-separated file under the home, and every answer is a string.

(in-package #:nodecode-local-knowledge)

;;; --- the manual -------------------------------------------------------------

(defparameter *primer*
  "Local knowledge is this machine's own tree, indexed as whole artifacts: a question routes to
THE file to read next, never to a passage of it. The five tools:
  knowledge_search   ask in your own words; an optional artifact_type (skill, script, runbook,
                     doc, config) and limit; rebuild: true walks the roots again now.
  knowledge_get      one artifact by the id a search answered: title, path, size, headings,
                     the start of its text, and the artifacts beside it.
  knowledge_neighbors  the edges out of one artifact: siblings in its directory, a skill's own
                     support documents, documents that name it.
  knowledge_feedback  record that a hit was useful, missing, noisy, wrong_artifact, stale or
                     other, with an optional note.
  knowledge_usage_report  what was asked, what found nothing, and what was said about the hits.
How to work: search with the words the file itself would carry (its subject and operation, not a
conversation), open the top hit with knowledge_get, then read that file before acting on it. A
hit is a routing hint, not proof the file is current. When two hits look alike, the type and
the path settle it. Record what helped; nothing leaves this machine.
The operator searches and rebuilds with /knowledge WORDS and /knowledge rebuild."
  "What (help :local-knowledge) answers while the cell runs.")

;;; --- the corpus in hand ------------------------------------------------------

(defvar *corpus* nil
  "The scan this cell runs on, or NIL before the first question and after a stop.")

(defvar *corpus-key* nil
  "The settings the scan was made under, so a changed section rescans.")

(defun excluded-names ()
  "The directory names the walk skips: the built-in list and the section's own."
  (remove-duplicates (append *excluded-directories* (setting :exclude-dir-names))
                     :test #'string=))

(defun corpus (&key force)
  "The scan of the configured roots, rebuilt when it is stale, unset, or FORCE."
  (let* ((roots (setting :roots))
         (key (list roots (setting :max-depth) (setting :max-files) (excluded-names))))
    (unless roots
      (fail "no roots are configured: set local-knowledge.roots to the directories to index"))
    (when (or force
              (null *corpus*)
              (not (equal key *corpus-key*))
              (<= (setting :max-index-age-seconds)
                  (- (get-universal-time) (corpus-built-at *corpus*))))
      (setf *corpus* (build-corpus roots
                                    :max-depth (setting :max-depth)
                                    :max-files (setting :max-files)
                                    :excluded (excluded-names)))
      (setf *corpus-key* key))
    *corpus*))

;;; --- lines an answer is made of ----------------------------------------------

(defun size-text (bytes)
  "BYTES as a reader thinks of them: 812 B, 4.2 KB, 1.3 MB."
  (cond ((null bytes) "size unknown")
        ((< bytes 1024) (format nil "~d B" bytes))
        ((< bytes 1048576) (format nil "~,1f KB" (/ bytes 1024.0)))
        (t (format nil "~,1f MB" (/ bytes 1048576.0)))))

(defun date-text (universal)
  "UNIVERSAL as YYYY-MM-DD, or a word when the file does not say."
  (if universal
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time universal)
        (declare (ignore second minute hour))
        (format nil "~4,'0d-~2,'0d-~2,'0d" year month day))
      "unknown"))

(defun paths-text (corpus)
  (format nil "~{~a~^, ~}" (corpus-roots corpus)))

(defun index-summary (corpus)
  "One paragraph: what is indexed, where from, and how fresh the walk is."
  (format nil "local knowledge: ~d artifact~:p under ~d root~:p (~a)~@[; ~d skill~:p~]~@[; ~d script~:p~]~@[; ~d doc~:p~]~@[; ~d runbook~:p~]~@[; ~d config~:p~]; walked ~a~@[; not found: ~{~a~^, ~}~]"
          (length (corpus-artifacts corpus))
          (length (corpus-roots corpus))
          (paths-text corpus)
          (let ((n (count "skill" (corpus-artifacts corpus) :key #'artifact-type :test #'string=)))
            (and (plusp n) n))
          (let ((n (count "script" (corpus-artifacts corpus) :key #'artifact-type :test #'string=)))
            (and (plusp n) n))
          (let ((n (count "doc" (corpus-artifacts corpus) :key #'artifact-type :test #'string=)))
            (and (plusp n) n))
          (let ((n (count "runbook" (corpus-artifacts corpus) :key #'artifact-type :test #'string=)))
            (and (plusp n) n))
          (let ((n (count "config" (corpus-artifacts corpus) :key #'artifact-type :test #'string=)))
            (and (plusp n) n))
          (date-text (corpus-built-at corpus))
          (corpus-missing corpus)))

(defun hit-line (index artifact score query)
  "One hit as the model reads it: what to open, where it is, why it answered."
  (format nil "~d. ~a  ~a  [~a, ~a, ~a]  ~a~@[  matched: ~{~a~^ ~}~]~@[  score ~d~]"
          index
          (artifact-id artifact)
          (nlk:clip (artifact-title artifact) 90)
          (artifact-type artifact)
          (size-text (artifact-size artifact))
          (date-text (artifact-modified artifact))
          (artifact-relative artifact)
          (matched-words artifact (words-of (string-downcase query)))
          (and (> score 12) score)))

;;; --- the search --------------------------------------------------------------

(defun search-text (query &key type limit rebuild)
  "The artifacts QUERY routes to, best first, as the answer the model reads."
  (let ((query (nlk:trimmed (or query ""))))
    (unless (plusp (length query))
      (fail "a search needs a question: give knowledge_search the words the file itself would carry"))
    (let* ((scan (corpus :force rebuild))
           (wanted (and type (plusp (length type)) type))
           (limit (max 1 (min 50 (or limit (setting :max-hits)))))
           (hits (search-corpus scan query :type wanted :limit limit)))
      (record-usage "search" query "" "" (princ-to-string (length hits)))
      (with-output-to-string (out)
        (cond ((null hits)
               (format out "no artifact matches ~s~@[ among ~a~]; ~d indexed~@[ under ~{~a~^, ~}~]~@[; missing: ~{~a~^, ~}~]~%Try fewer, plainer words (the subject and the operation), or rebuild: true when the file is new.~%"
                       query wanted (length (corpus-artifacts scan))
                       (corpus-roots scan) (corpus-missing scan)))
              (t
               (format out "~d of ~d artifacts match ~s~@[ among ~a~]:~%"
                       (length hits) (length (corpus-artifacts scan)) query wanted)
               (loop for (score . artifact) in hits
                     for index from 1
                     do (format out "~a~%" (hit-line index artifact score query)))
               (format out "a hit is a routing hint: open the file before acting~@[; missing: ~{~a~^, ~}~]~%"
                       (corpus-missing scan))))))))

;;; --- one artifact ------------------------------------------------------------

(defun artifact-or-fail (scan id)
  (let ((artifact (gethash (nlk:trimmed (or id "")) (corpus-by-id scan))))
    (or artifact
        (fail "~s is no artifact in the index: run knowledge_search and use the id it answers"
              (or id "")))))

(defun fetch-text (id &key (with-neighbors t))
  "One artifact whole enough to decide whether to open it, with its edges."
  (let* ((scan (corpus))
         (artifact (artifact-or-fail scan id))
         (text (read-text (artifact-path artifact) :limit 200000)))
    (with-output-to-string (out)
      (format out "~a  ~a~%" (artifact-id artifact) (artifact-title artifact))
      (format out "type: ~a   size: ~a   modified: ~a~%"
              (artifact-type artifact) (size-text (artifact-size artifact))
              (date-text (artifact-modified artifact)))
      (format out "path: ~a~%" (artifact-path artifact))
      (when (artifact-headings artifact)
        (format out "headings: ~{~a~^ / ~}~%" (artifact-headings artifact)))
      (format out "~@[neighbors: ~{~a~^, ~}~%~]"
              (when with-neighbors
                (loop for (id . edge) in (neighbors-of scan artifact)
                      collect (format nil "~a (~a)" id (car edge)))))
      (when text
        (let ((prose (prose-text text)))
          (when (plusp (length prose))
            (format out "~%excerpt:~%  ~a~%" (nlk:clip prose (setting :max-excerpt-chars))))))
      (format out "~%read it before acting on it~@[; missing: ~{~a~^, ~}~]~%"
              (corpus-missing scan)))))

(defun neighbors-text (id &key limit)
  "The edges out of one artifact, each named by the relation that made it."
  (let* ((scan (corpus))
         (artifact (artifact-or-fail scan id))
         (edges (neighbors-of scan artifact :limit (max 1 (min 30 (or limit 12))))))
    (with-output-to-string (out)
      (format out "~d artifact~:p beside ~a:~%" (length edges) (artifact-id artifact))
      (if (null edges)
          (format out "none: this artifact stands alone in the index~%")
          (loop for (id . edge) in edges
                do (format out "- ~a  ~a  [~a, ~a]  ~a~%"
                           id (nlk:clip (artifact-title (cdr edge)) 90)
                           (car edge) (artifact-type (cdr edge))
                           (artifact-relative (cdr edge))))))))

;;; --- the local record ----------------------------------------------------------

(defparameter +ratings+ '("useful" "not_useful" "missing" "noisy" "wrong_artifact" "stale" "other")
  "The ratings the original took, unchanged; the report counts them.")

(defun usage-path ()
  "Where this cell keeps what was asked and what was said about it."
  (nlk:home "knowledge/usage.tsv"))

(defun clean-field (value)
  "One field as one line can hold it: no tab, no newline, bounded."
  (substitute #\Space #\Tab (nlk:clip (nlk:one-line (princ-to-string (or value ""))) 200)))

(defun record-usage (kind &rest fields)
  "Append one line: the time, KIND, then FIELDS. Never fails a caller."
  (let ((path (usage-path)))
    (ignore-errors
     (ensure-directories-exist path)
     (with-open-file (out path :direction :output :if-exists :append :if-does-not-exist :create
                                :external-format :utf-8)
       (dolist (value (mapcar #'clean-field (list* (nlk:iso-now) kind fields)))
         (write-string value out)
         (write-char #\Tab out))
       (terpri out))
     (trim-usage path))
    path))

(defun trim-usage (path)
  "Keep the newest two thousand lines once the file passes 400 KB."
  (let ((size (ignore-errors
               (with-open-file (in path :element-type '(unsigned-byte 8))
                 (file-length in)))))
    (when (and size (> size 400000))
      (let ((kept (last (ignore-errors (nlk:lines (uiop:read-file-string path))) 2000)))
        (ignore-errors
         (nlk:write-file-atomically path (format nil "~{~a~%~}" kept)))))))

(defun usage-rows ()
  "Every kept line as (ISO KIND QUERY ARTIFACT RATING HITS)."
  (let ((path (usage-path)))
    (if (ignore-errors (probe-file path))
        (loop for line in (or (ignore-errors (nlk:lines (uiop:read-file-string path))) '())
              for fields = (uiop:split-string line :separator '(#\Tab))
              when (>= (length fields) 6)
                collect fields)
        '())))

(defun feedback-text (rating &key artifact-id query note)
  "Record what one hit did for the question, so the next one can be better."
  (let ((rating (string-downcase (nlk:trimmed (or rating "")))))
    (unless (member rating +ratings+ :test #'string=)
      (fail "~s is no rating: one of ~{~a~^, ~}" rating +ratings+))
    (record-usage "feedback" (or query "") (or artifact-id "") rating (or note ""))
    (format nil "recorded ~a~@[ for ~a~]~@[ on ~s~]; ~d note~:p kept on this machine~%"
            rating artifact-id query
            (count "feedback" (usage-rows) :key #'second :test #'string=))))

(defun report-text ()
  "What was asked, what found nothing, and what was said about the hits."
  (let* ((rows (usage-rows))
         (searches (remove-if-not (lambda (row) (string= (second row) "search")) rows))
         (notes (remove-if-not (lambda (row) (string= (second row) "feedback")) rows))
         (empty (remove-if-not (lambda (row) (string= "0" (sixth row))) searches))
         (asked (make-hash-table :test #'equal))
         (missed (make-hash-table :test #'equal))
         (ratings (make-hash-table :test #'equal)))
    (dolist (row searches)
      (when (plusp (length (third row))) (incf (gethash (third row) asked 0)))
      (when (string= "0" (sixth row)) (incf (gethash (third row) missed 0))))
    (dolist (row notes) (incf (gethash (fifth row) ratings 0)))
    (labels ((top (table limit)
               (loop for key being the hash-keys of table using (hash-value count)
                     collect (cons key count) into pairs
                     finally (return (subseq (sort pairs #'> :key #'cdr)
                                             0 (min limit (length pairs))))))
             (render (pairs)
               (if pairs
                   (format nil "~{~a (~d)~^, ~}"
                           (loop for (key . count) in pairs append (list key count)))
                   "none")))
      (with-output-to-string (out)
        (format out "local knowledge: ~d question~:p recorded, ~d with no hit, ~d feedback note~:p~%"
                (length searches) (length empty) (length notes))
        (format out "most asked: ~a~%" (render (top asked 5)))
        (format out "found nothing: ~a~%" (render (top missed 5)))
        (format out "feedback: ~a~%"
                (if (plusp (hash-table-count ratings))
                    (format nil "~{~a ~d~^, ~}"
                            (loop for rating in +ratings+ when (gethash rating ratings)
                                    append (list rating (gethash rating ratings))))
                    "none yet"))
        (let ((trouble (remove-if-not
                        (lambda (row) (member (fifth row) '("not_useful" "missing" "noisy"
                                                          "wrong_artifact" "stale")
                                              :test #'string=))
                        notes)))
          (when trouble
            (format out "trouble:~%")
            (loop for row in (last trouble (min 8 (length trouble)))
                  do (format out "  [~a] ~a~@[ ~a~]~@[ - ~s~]~%"
                             (subseq (first row) 0 (min 10 (length (first row))))
                             (fifth row) (third row)
                             (let ((note (sixth row)))
                               (and (plusp (length note)) note))))))
        (format out "Across the whole history, not this session.~%")))))

;;; --- the tools ----------------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Find the local skill, script, runbook, document or config that a question should read first. Answers whole artifacts as routing hints, best first: the id to open, where it is, and why it answered. Search in the words the file itself would carry -- its subject and its operation -- not the conversation.")
  ("query" :text :required t :doc "what the artifact is about, in its own words")
  ("artifact_type" :string :options '("skill" "script" "runbook" "doc" "config")
   :doc "keep only this kind of artifact")
  ("limit" :integer :default 8
   :doc "how many hits to answer, from 1 to 50")
  ("rebuild" :boolean
   :doc "walk the configured roots again before answering, when a file was just added"))

(nlk:define-contract fetch-call
  (:doc "One artifact by the id a search answered: its title, path, size, date, headings, the start of its text, and the artifacts beside it. Read the file itself before acting on it.")
  ("artifact_id" :text :required t :doc "the id a search answered, such as runbook:ops/backup"))

(nlk:define-contract neighbors-call
  (:doc "The artifacts beside one artifact: siblings in its directory, a skill's own support documents, and documents that name it.")
  ("artifact_id" :text :required t :doc "the id a search answered")
  ("limit" :integer :default 12 :doc "how many edges to answer, from 1 to 30"))

(nlk:define-contract feedback-call
  (:doc "Record what one hit did for the question, so the operator can improve the index. Kept on this machine, in a file beside the index.")
  ("rating" :text :required t :options '("useful" "not_useful" "missing" "noisy" "wrong_artifact" "stale" "other")
   :doc "what the hit was: useful, missing (nothing was found), noisy, wrong_artifact, stale, other")
  ("artifact_id" :string :doc "the id the hit answered, when there was one")
  ("query" :string :doc "the question it answered, in the words it was asked")
  ("note" :string :doc "one sentence on what was wrong or right, at most 200 characters"))

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object))))

(defun run-search (call)
  "knowledge_search: the artifacts a question routes to."
  (search-text (search-call-query call)
                :type (search-call-artifact-type call)
                :limit (search-call-limit call)
                :rebuild (search-call-rebuild call)))

(defun run-fetch (call)
  "knowledge_get: one artifact, whole enough to decide."
  (fetch-text (fetch-call-artifact-id call)))

(defun run-neighbors (call)
  "knowledge_neighbors: the edges out of one artifact."
  (neighbors-text (neighbors-call-artifact-id call) :limit (neighbors-call-limit call)))

(defun run-feedback (call)
  "knowledge_feedback: one rating, kept locally."
  (feedback-text (feedback-call-rating call)
                 :artifact-id (feedback-call-artifact-id call)
                 :query (feedback-call-query call)
                 :note (feedback-call-note call)))

(defun run-report (arguments)
  "knowledge_usage_report: what was asked and what was said about it."
  (declare (ignore arguments))
  (report-text))

;;; --- the operator's command ---------------------------------------------------

(defun panel ()
  "The dialog bare /knowledge opens."
  (nle:list-dialog
   :title "Local knowledge" :summary "find the file to read first"
   :rows (list (nle:ask-row "Search" "Words" "/knowledge"
                             :placeholder "backup runbook" :verb "search")
               (nle:run-row "Walk the roots again" "/knowledge rebuild"
                            :detail "a file was just added"))))

(defun run-command (args session-id)
  "/knowledge WORDS searches; /knowledge rebuild walks the roots again;
/knowledge alone says what is indexed and opens the panel."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (character) (member character '(#\Space #\Tab))) text))
         (head (string-downcase (subseq text 0 space)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((string= text "")
           (values (index-summary (corpus)) (panel)))
          ((string= head "rebuild")
           (values (format nil "rebuilt: ~a" (index-summary (corpus :force t))) nil))
          ((string= head "search")
           (values (search-text rest) nil))
          (t (values (search-text text) nil)))))

;;; --- the declaration ----------------------------------------------------------

(nle:define-cell local-knowledge
  (:section ("local-knowledge")
    (:guide "roots are the directories to index, one path each, absolute or starting with ~; a skill is a SKILL.md, a runbook is prose whose name or directory says runbook, scripts and configs are known by their extension; max_index_age_seconds bounds how stale a walk may be before a question walks again")
    ("roots" :list :required t
     :doc "the local directories to index, one path each")
    ("max_depth" :integer :default 6 :min 1
     :doc "how deep below each root the walk goes")
    ("max_files" :integer :default 4000 :min 1
     :doc "how many artifacts one walk keeps; the rest wait for a narrower root")
    ("max_index_age_seconds" :integer :default 300 :min 0
     :doc "how old a walk may be before a question walks again; 0 walks on every question")
    ("exclude_dir_names" :list
     :doc "directory names the walk never enters, on top of the built-in list")
    ("max_excerpt_chars" :integer :default 800 :min 200
     :doc "how much of an artifact's text knowledge_get quotes")
    ("max_hits" :integer :default 8 :min 1
     :doc "the most hits a search answers when the call names no limit"))
  (:help :local-knowledge
         "local-knowledge: route a question to the local skill, script or runbook to read first - (help :local-knowledge)"
         *primer*)
  (:tool "knowledge_search" (nlk:contract 'search-call) #'run-search)
  (:tool "knowledge_get" (nlk:contract 'fetch-call) #'run-fetch)
  (:tool "knowledge_neighbors" (nlk:contract 'neighbors-call) #'run-neighbors)
  (:tool "knowledge_feedback" (nlk:contract 'feedback-call) #'run-feedback)
  (:tool "knowledge_usage_report"
         (no-arguments "What was asked of this index, what found nothing, and what the hits were said to be. Kept on this machine.")
         #'run-report)
  (:command "knowledge" 'run-command
            :description "Local knowledge: find the skill, script or runbook to read first"
            :argument-hint "WORDS"
            :catches 'local-knowledge-error
            :session nil)
  (:start (lambda ()
            (setf *corpus* nil *corpus-key* nil)
            (nle:on-stop (lambda () (setf *corpus* nil *corpus-key* nil))))))
