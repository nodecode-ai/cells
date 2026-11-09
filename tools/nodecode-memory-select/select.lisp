;;;; select.lisp --- the memories that answer the query, inside the budget.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's selector.py, tokens.py and security/redaction.py, in Lisp:
;;;; a candidate read off a decoded object, the lexical relevance that
;;;; compares it with the query, a freshness half-life, the caller's own
;;;; importance, a duplicate test against what is already kept, and a token
;;;; budget that stops the list rather than cutting one memory in half.
;;;; Nothing here touches the network or a file.

(in-package #:nodecode-memory-select)

;;; --- one candidate ------------------------------------------------------------------

(defstruct (item (:constructor make-item (&key id text created-at importance tags scope
                                               expires-at similarity)))
  "One candidate memory: what the caller retrieved, before any selection."
  (id "" :type string)
  (text "" :type string)
  (created-at 0 :type integer)
  (importance 0.5 :type real)
  (tags '() :type list)
  (scope "default" :type string)
  (expires-at nil)
  (similarity nil))

(defstruct (selected-memory (:constructor make-selected-memory (id text score tokens reasons)))
  "One memory kept, with its score, its cost and the reasons it was kept."
  (id "" :type string)
  (text "" :type string)
  (score 0d0 :type real)
  (tokens 0 :type integer)
  (reasons '() :type list))

(defstruct (dropped-memory (:constructor make-dropped-memory (id reason)))
  "One memory left behind, and the one reason it was."
  (id "" :type string)
  (reason "" :type string))

(defstruct (selection (:constructor make-selection (selected dropped total-tokens)))
  "What one call answers: kept, dropped, and the tokens the kept ones cost."
  (selected '() :type list)
  (dropped '() :type list)
  (total-tokens 0 :type integer))

;;; --- reading a candidate --------------------------------------------------------------

(defun field (object key)
  "Two values from OBJECT under KEY (a snake_case string): the value, and
whether it was there. OBJECT is a decoded JSON object, or a plist from eval
whose keys are keywords (:created-at)."
  (cond ((hash-table-p object) (gethash key object))
        ((listp object)
         (let ((name (intern (string-upcase (substitute #\- #\_ key)) :keyword)))
           (values (getf object name) (not (null (member name object :test #'eq))))))
        (t (values nil nil))))

(defun field-text (object key)
  "OBJECT's KEY when it is a non-empty string, else NIL."
  (let ((value (field object key)))
    (and (stringp value) (plusp (length value)) value)))

(defun field-number (object key)
  "OBJECT's KEY when it is a real, else NIL."
  (let ((value (field object key)))
    (and (realp value) (float value 1d0))))

(defun all-digits (text start end)
  "Whether TEXT carries a digit at every position from START below END."
  (and (<= end (length text))
       (loop for index from start below end always (digit-char-p (char text index)))))

(defun parse-instant (text)
  "The universal time TEXT (YYYY-MM-DDTHH:MM:SS with an optional fraction and
zone) names, or NIL when it does not parse. A time with no zone reads as UTC."
  (when (and (stringp text) (>= (length text) 19)
             (char= (char text 4) #\-) (char= (char text 7) #\-))
    (let* ((rest (subseq text 19))
           (zone (cond ((and (plusp (length rest)) (char= (char rest 0) #\Z)) 0)
                       ((and (>= (length rest) 6) (find (char rest 0) "+-"))
                        (* (if (char= (char rest 0) #\+) 1 -1)
                           (+ (* 3600 (parse-integer rest :start 1 :end 3))
                              (* 60 (parse-integer rest :start 4 :end 6)))))
                       (t 0))))
      (when (loop for (start end) in '((0 4) (5 7) (8 10) (11 13) (14 16) (17 19))
                  always (all-digits text start end))
        (ignore-errors
          (- (encode-universal-time
              (parse-integer text :start 17 :end 19) (parse-integer text :start 14 :end 16)
              (parse-integer text :start 11 :end 13) (parse-integer text :start 8 :end 10)
              (parse-integer text :start 5 :end 7) (parse-integer text :start 0 :end 4) 0)
             zone))))))

(defun field-time (object key)
  "OBJECT's KEY as a universal time, or NIL: an ISO-8601 string, or a number
of seconds as eval may pass one."
  (let ((value (field object key)))
    (cond ((null value) nil)
          ((integerp value) value)
          ((stringp value) (parse-instant value))
          (t nil))))

(defun field-tags (object key)
  "OBJECT's KEY as a list of strings: an array, a list, or one string."
  (let ((value (field object key)))
    (cond ((stringp value) (list value))
          ((vectorp value) (remove-if-not #'stringp (coerce value 'list)))
          ((listp value) (remove-if-not #'stringp value))
          (t '()))))

(defun item-from-object (object index)
  "OBJECT (a decoded JSON memory, or a plist from eval) as an ITEM; a refusal
names the position INDEX it had in the list."
  (unless (or (hash-table-p object) (listp object))
    (fail "memory ~d is not an object: give each memory an id and a text" (1+ index)))
  (let* ((id (field-text object "id"))
         (text (or (field-text object "text") (field-text object "content")))
         (importance (or (field-number object "importance") 0.5d0))
         (similarity (field-number object "similarity"))
         (scope (or (field-text object "scope") (field-text object "namespace") "default")))
    (unless id
      (fail "memory ~d has no id: give each memory a stable, unique id" (1+ index)))
    (unless text
      (fail "memory \"~a\" has no text: give it text, or content" id))
    (unless (<= 0 importance 1)
      (fail "memory \"~a\" has importance ~a: importance is 0 to 1" id importance))
    (when (and similarity (not (<= 0 similarity 1)))
      (fail "memory \"~a\" has similarity ~a: similarity is 0 to 1" id similarity))
    (make-item :id id
               :text text
               :created-at (or (field-time object "created_at") (get-universal-time))
               :importance importance
               :tags (field-tags object "tags")
               :scope scope
               :expires-at (field-time object "expires_at")
               :similarity similarity)))

;;; --- tokens -------------------------------------------------------------------------

(defparameter +word-chars+
  "abcdefghijklmnopqrstuvwxyz0123456789àâäéèêëíìîïòóôöùúûüçñ"
  "What a token is made of, after STRING-DOWNCASE: the original's one regex class.")

(defparameter +stopwords+
  '("au" "aux" "avec" "ce" "ces" "dans" "de" "des" "du" "elle" "en" "et" "eux" "il" "je"
    "la" "le" "les" "leur" "lui" "ma" "mais" "me" "meme" "mes" "moi" "mon" "ne" "nos"
    "notre" "nous" "on" "ou" "par" "pas" "pour" "qu" "que" "qui" "sa" "se" "ses" "son"
    "sur" "ta" "te" "tes" "toi" "ton" "tu" "un" "une" "vos" "votre" "vous" "c" "d" "j"
    "l" "m" "n" "s" "t" "y" "the" "a" "an" "and" "or" "of" "to" "in" "for" "with"
    "without" "is" "are" "was" "were" "be" "been" "this" "that")
  "Words a query and a memory share without saying anything: the original's list.")

(defun tokenize (text)
  "TEXT's tokens: lowercased, word characters only, one-letter words and
stopwords dropped. Answers a list of unique strings, in the order they appear."
  (let ((text (string-downcase (or text "")))
        (tokens '())
        (start nil))
    (labels ((cut (end)
               (when start
                 (let ((word (subseq text start end)))
                   (when (and (or (> (length word) 1) (every #'digit-char-p word))
                              (not (member word +stopwords+ :test #'string=)))
                     (pushnew word tokens :test #'string=)))
                 (setf start nil))))
      (loop for index from 0 below (length text)
            for character = (char text index)
            do (if (find character +word-chars+)
                   (unless start (setf start index))
                   (cut index)))
      (cut (length text))
      (nreverse tokens))))

(defun item-tokens (item)
  "ITEM's tokens: its text's, and its tags'."
  (let ((tokens (tokenize (item-text item))))
    (dolist (tag (item-tags item) tokens)
      (setf tokens (union tokens (tokenize tag) :test #'string=)))))

(defun estimate-tokens (text)
  "About four characters to the token: the original's deterministic estimate."
  (if (zerop (length text))
      0
      (max 1 (ceiling (length text) 4))))

(defun jaccard (left right)
  "How much two token lists share: the intersection over the union, 0 to 1."
  (let ((shared (length (intersection left right :test #'string=)))
        (all (length (union left right :test #'string=))))
    (if (zerop all) 0d0 (float (/ shared all) 1d0))))

(defparameter +contradictions+
  '(("mysql" "postgresql" "postgres")
    ("sqlite" "postgresql" "postgres" "mysql"))
  "Each list names one answer to a question the others answer differently: two
kept memories naming two of them are worth a warning.")

(defun contradiction-with (item selected)
  "The id of a kept memory whose text ITEM contradicts, or NIL."
  (let ((left (tokenize (item-text item))))
    (loop for kept in selected
          for right = (tokenize (selected-memory-text kept))
          when (loop for names in +contradictions+
                     thereis (let ((here (member (first names) left :test #'string=))
                                   (there (intersection right (rest names) :test #'string=)))
                               (or (and here there)
                                   (and (member (first names) right :test #'string=)
                                        (intersection left (rest names) :test #'string=)))))
            return (selected-memory-id kept))))

;;; --- redaction -----------------------------------------------------------------------

(defparameter +redactions+
  (list (cons (cl-ppcre:create-scanner
               "-----BEGIN [A-Z0-9 ]*PRIVATE KEY-----.*?-----END [A-Z0-9 ]*PRIVATE KEY-----"
               :single-line-mode t)
              "[REDACTED_PRIVATE_KEY]")
        (cons (cl-ppcre:create-scanner "sk-ant-[A-Za-z0-9_-]+") "[REDACTED_API_KEY]")
        (cons (cl-ppcre:create-scanner "sk-[A-Za-z0-9]{20,}") "[REDACTED_API_KEY]")
        (cons (cl-ppcre:create-scanner "jev_[A-Za-z0-9_-]{8,}") "[REDACTED_API_KEY]")
        (cons (cl-ppcre:create-scanner "Bearer\\s+[A-Za-z0-9._\\-+=/]+") "[REDACTED_BEARER]")
        (cons (cl-ppcre:create-scanner "(?i)(password|passwd|secret)\\s*[:=]\\s*\\S+")
              "\\1=[REDACTED_PASSWORD]"))
  "The original's secret shapes and what each becomes: a memory that carries a
key answers with the shape of it, never the key.")

(defun redact-text (text)
  "TEXT with every secret it shows replaced by a stable marker."
  (reduce (lambda (text pattern)
            (cl-ppcre:regex-replace-all (car pattern) text (cdr pattern)))
          +redactions+ :initial-value text))

;;; --- the selection -------------------------------------------------------------------

(defun round-score (score)
  "SCORE to six places, the way the original rounded before comparing two."
  (/ (round (* (coerce score 'double-float) 1000000d0)) 1000000d0))

(defun score-of (relevance recency item)
  "The original's local score: relevance and freshness weighted above importance."
  (+ (* 0.5d0 relevance) (* 0.3d0 recency) (* 0.2d0 (item-importance item))))

(defun recency-of (item now half-life)
  "ITEM's freshness: a half-life of HALF-LIFE days, 1 today and 0.5 that old."
  (let ((age (/ (float (max 0 (- now (item-created-at item))) 1d0) 86400d0)))
    (expt 0.5d0 (/ age half-life))))

(defun apply-budget (candidates dropped budget limit dedupe)
  "Walk CANDIDATES best first, keeping what fits BUDGET tokens and LIMIT
memories, dropping DUPLICATE-of-kept and the rest with the reason."
  (let ((selected '())
        (tokens-by-memory '())
        (total 0))
    (dolist (candidate candidates)
      (destructuring-bind (score relevance recency item) candidate
        (let ((tokens (estimate-tokens (item-text item)))
              (text-tokens (tokenize (item-text item))))
          (cond
            ((>= (length selected) limit)
             (push (make-dropped-memory (item-id item) "max_items") dropped))
            ((> (+ total tokens) budget)
             (push (make-dropped-memory (item-id item) "budget") dropped))
            (t
             (let ((duplicate (and (plusp dedupe)
                                   (loop for kept in selected
                                         for kept-tokens in tokens-by-memory
                                         when (>= (jaccard text-tokens kept-tokens) dedupe)
                                           return (selected-memory-id kept)))))
               (if duplicate
                   (push (make-dropped-memory (item-id item)
                                               (format nil "duplicate_of:~a" duplicate))
                         dropped)
                   (let ((conflict (contradiction-with item selected)))
                     (push (make-selected-memory
                            (item-id item) (item-text item) (round-score score) tokens
                            (append (list (format nil "relevance=~,2f" relevance)
                                          (format nil "recency=~,2f" recency)
                                          (format nil "importance=~,2f" (item-importance item)))
                                    (when (item-similarity item)
                                      (list (format nil "similarity=~,2f"
                                                    (item-similarity item))))
                                    (when conflict
                                      (list (format nil "possible_conflict:~a" conflict)))))
                           selected)
                     (push text-tokens tokens-by-memory)
                     (setf total (+ total tokens))))))))))
    (make-selection (nreverse selected) (nreverse dropped) total)))

(defun select (query memories &key scope max-memories max-tokens now)
  "Keep the MEMORIES that answer QUERY inside the budget, best first.
MEMORIES is a list or vector of objects (decoded JSON, or plists from eval),
one per candidate. => a SELECTION: what was kept with its score and reasons,
what was dropped with the reason, and the tokens the kept ones cost.
MAX-MEMORIES and MAX-TOKENS answer over the section's max_results and
max_tokens; SCOPE defaults to \"default\"; NOW is a universal time, for a
reproducible answer. Refuses, naming the member, when one is not a memory
or when the budget is not at least one."
  (let* ((settings (running-settings))
         (budget (or max-tokens (getf settings :max-tokens)))
         (limit (or max-memories (getf settings :max-results)))
         (freshness (getf settings :recency-days))
         (dedupe (getf settings :dedupe))
         (scope (or scope "default"))
         (now (or now (get-universal-time))))
    (unless (and (integerp budget) (>= budget 1))
      (fail "max_tokens is ~s: the budget is at least one token" budget))
    (unless (and (integerp limit) (>= limit 1))
      (fail "max_memories is ~s: at least one memory" limit))
    (let* ((items (loop for object in (coerce memories 'list)
                        for index from 0
                        for item = (item-from-object object index)
                        do (when (getf settings :redact)
                             (setf (item-text item) (redact-text (item-text item))))
                        collect item))
           (query-tokens (tokenize query))
           (seen '())
           (candidates '())
           (dropped '()))
      (dolist (item items)
        (let ((id (item-id item)))
          (cond
            ((member id seen :test #'string=)
             (push (make-dropped-memory id "duplicate_input") dropped))
            (t
             (push id seen)
             (cond
               ((not (string= (item-scope item) scope))
                (push (make-dropped-memory id "out_of_scope") dropped))
               ((and (item-expires-at item) (<= (item-expires-at item) now))
                (push (make-dropped-memory id "expired") dropped))
               (t
                (let* ((tokens (item-tokens item))
                       (relevance (if query-tokens
                                      (float (/ (length (intersection query-tokens tokens
                                                                       :test #'string=))
                                                (length query-tokens))
                                             1d0)
                                      0d0))
                       (similarity (or (item-similarity item) 0d0)))
                  (if (and query-tokens (< (max relevance similarity) (getf settings :min-relevance)))
                      (push (make-dropped-memory id "low_relevance") dropped)
                      (push (list (score-of relevance (recency-of item now freshness) item)
                                  relevance (recency-of item now freshness) item)
                            candidates)))))))))
      (let ((ordered (sort candidates
                            (lambda (left right)
                              (let ((left-score (round-score (first left)))
                                    (right-score (round-score (first right))))
                                (cond ((/= left-score right-score) (> left-score right-score))
                                      ((/= (item-created-at (fourth left))
                                           (item-created-at (fourth right)))
                                       (> (item-created-at (fourth left))
                                          (item-created-at (fourth right))))
                                      (t (string< (item-id (fourth left))
                                                  (item-id (fourth right))))))))))
        (apply-budget ordered dropped budget limit dedupe)))))

;;; --- the tool's answer ----------------------------------------------------------------

(defun to-json (selection)
  "SELECTION as the JSON text the memory_select tool answers: the kept
memories with their scores and reasons, the tokens they cost, and every
memory left behind with the one reason it was."
  (nlk:encode-json-object
   (nlk:make-json-object
    "selected" (coerce (loop for memory in (selection-selected selection)
                              collect (nlk:make-json-object
                                       "id" (selected-memory-id memory)
                                       "content" (selected-memory-text memory)
                                       "score" (selected-memory-score memory)
                                       "tokens" (selected-memory-tokens memory)
                                       "reasons" (coerce (selected-memory-reasons memory) 'vector)))
                        'vector)
    "total_tokens" (selection-total-tokens selection)
    "dropped" (coerce (loop for memory in (selection-dropped selection)
                             collect (nlk:make-json-object
                                      "id" (dropped-memory-id memory)
                                      "reason" (dropped-memory-reason memory)))
                       'vector))))
