;;;; store.lisp --- the verbatim store: one ledger of turns, ranked by BM25.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two files under the cell's folder: turns.jsonl, one JSON object a turn
;;;; (session_id, session_turn, speaker, text, at, seq), and progress.jsonl,
;;;; a session's log position and nothing else, appended when it moves. Both
;;;; are append-only; forgetting one session rewrites the first and moves
;;;; that session's position past what it forgot.
;;;;
;;;; The ranking is the sparse half of the original Rust core: BM25 over
;;;; content tokens (K1 1.2, B 0.75) with an exact-phrase bonus, newest
;;;; first on a tie. Its dense embedder and its two-view graph/hierarchy
;;;; fusion are not ported; NOTICE says what that costs.
;;;;
;;;; Nothing here touches the network, and nothing writes outside the store's
;;;; own folder.

(in-package #:nodecode-zeromem)

(defparameter +kinds+ '("turn.input_committed" "turn.assistant_message_completed")
  "The two wire kinds a stored turn is made of: what was asked, and what was
said. Tool traffic is not memory here, so it is not read.")

(defparameter +page+ 50
  "How many log rows one read of a session asks for.")

(defparameter +k1+ 1.2d0 "BM25's term-frequency saturation, as the core had it.")
(defparameter +b+ 0.75d0 "BM25's length normalization, as the core had it.")

(defparameter +stopwords+
  '("a" "an" "the" "and" "or" "but" "if" "then" "else" "when" "while" "of" "at"
    "by" "for" "with" "about" "against" "between" "into" "through" "during"
    "before" "after" "above" "below" "to" "from" "up" "down" "in" "out" "on"
    "off" "over" "under" "again" "further" "once" "here" "there" "where" "why"
    "how" "all" "any" "both" "each" "few" "more" "most" "other" "some" "such"
    "no" "nor" "not" "only" "own" "same" "so" "than" "too" "very" "can" "will"
    "just" "should" "now" "i" "me" "my" "we" "our" "you" "your" "it" "its"
    "is" "are" "was" "were" "be" "been" "being" "do" "does" "did" "have" "has"
    "had" "this" "that" "these" "those")
  "The core's English stopwords, the length of the list it filters queries to
content tokens by.")

(defstruct (store (:constructor make-store (directory)) (:copier nil))
  "One store: DIRECTORY holds its two ledgers, TURNS are its turns oldest
first, PROGRESS is session id -> (:after POSITION)."
  directory
  (turns '())
  (progress (make-hash-table :test #'equal)))

(defvar *store* nil
  "The running store, or NIL while the cell is not started.")

(defun running-store ()
  "The store, or a refusal the model can read."
  (or *store*
      (fail "the zeromem cell is not running: check (cells) and the zeromem section of ~
             ~/.nodecode/config.jsonc, then (restart-cells \"nodecode-zeromem\")")))

;;; --- a turn on the wire and in memory ---------------------------------------

(defun turn-out (turn)
  "TURN as the JSON object its line carries."
  (list :session_id (getf turn :session-id)
        :session_turn (getf turn :session-turn)
        :speaker (getf turn :speaker)
        :text (getf turn :text)
        :at (getf turn :at)
        :seq (getf turn :seq)))

(defun turn-in (line)
  "A decoded line as a turn, or NIL for a line that is not one."
  (when (and (getf line :session_id) (getf line :speaker) (getf line :text))
    (list :session-id (getf line :session_id)
          :session-turn (getf line :session_turn)
          :speaker (getf line :speaker)
          :text (getf line :text)
          :at (getf line :at)
          :seq (getf line :seq))))

(defun store-file (store name)
  "The path of NAME in STORE's folder."
  (merge-pathnames name (store-directory store)))

(defun turn-json (turn)
  "TURN as one compact JSON object, the bytes its line is made of."
  (nlk:encode-json-object (nlk:plist-json (turn-out turn))))

;;; --- opening, keeping, forgetting -------------------------------------------

(defun open-store (directory)
  "Open the store in DIRECTORY -- made when absent -- read both ledgers back,
and answer it. A torn line is skipped, never a refusal."
  (let ((store (make-store (uiop:ensure-directory-pathname directory))))
    (ensure-directories-exist (store-file store "turns.jsonl"))
    (setf (store-turns store)
          (loop for line in (nlk:read-json-lines (store-file store "turns.jsonl"))
                for turn = (turn-in (nlk:json-plist line))
                when turn collect turn))
    (loop for line in (nlk:read-json-lines (store-file store "progress.jsonl"))
          for entry = (nlk:json-plist line)
          for session = (getf entry :session_id)
          when session
            do (setf (gethash session (store-progress store))
                     (list :after (or (getf entry :after) 0))))
    (setf *store* store)))

(defun close-store ()
  "Let go of the store: the memory first, so a stop is complete even when the
files could not be read at all."
  (setf *store* nil))

(defun store-session-cursor (store session)
  "The log position STORE read SESSION up to, 0 the first time."
  (or (getf (gethash session (store-progress store)) :after) 0))

(defun note-progress (store session after)
  "Record, in memory and on its line, that SESSION is read up to AFTER."
  (setf (gethash session (store-progress store)) (list :after after))
  (nlk:append-json-line (store-file store "progress.jsonl")
                         (nlk:plist-json (list :session_id session :after after)))
  after)

(defun keep-turns (store turns)
  "Append TURNS to STORE's ledger and its memory, oldest first. => TURNS."
  (dolist (turn turns)
    (nlk:append-json-line (store-file store "turns.jsonl") (nlk:plist-json (turn-out turn)))
    (setf (store-turns store) (nconc (store-turns store) (list turn))))
  turns)

(defun rewrite-turns (store)
  "Write STORE's ledger afresh from its memory: what forgetting does."
  (nlk:write-file-atomically
   (store-file store "turns.jsonl")
   (with-output-to-string (out)
     (dolist (turn (store-turns store))
       (write-line (turn-json turn) out)))))

(defun forget-turns (store session)
  "Drop every turn of SESSION from STORE, and keep the drop: its position
moves to the newest line it had, so its old words are never read in again.
=> how many turns went."
  (let* ((turns (store-turns store))
         (kept (remove session turns :key (lambda (turn) (getf turn :session-id))
                                 :test #'equal))
         (gone (- (length turns) (length kept))))
    (when (plusp gone)
      (setf (store-turns store) kept)
      (rewrite-turns store)
      (let ((newest (loop for turn in turns
                          when (equal session (getf turn :session-id))
                            maximize (or (getf turn :seq) 0))))
        (note-progress store session (or newest 0))))
    gone))

;;; --- what a session said since the store last read it -----------------------

(defun row-turn (session row)
  "One RECALL-LOG row of SESSION as a turn, with its provenance."
  (list :session-id session
        :session-turn (getf row :turn)
        :speaker (getf row :role)
        :text (getf row :text)
        :at (getf row :at)
        :seq (getf row :seq)))

(defun session-rows (session after)
  "Every conversational row of SESSION newer than log position AFTER, oldest
first: pages read backwards, so a long gap loses nothing in the middle."
  (let ((pages '()) (before nil) (done nil))
    (loop until done
          do (let* ((answer (nlk:recall-log :session-id session :kinds +kinds+
                                            :after after :before before
                                            :limit +page+ :clip 1000000))
                    (rows (if (and answer (getf (first answer) :matched))
                              (rest answer)
                              answer)))
               (cond ((null rows) (setf done t))
                     (t (push rows pages)
                        (if (< (length rows) +page+)
                            (setf done t)
                            (setf before (getf (first rows) :seq)))))))
    (loop for row in (loop for page in (nreverse pages) append page)
          for text = (getf row :text)
          when (and text (plusp (length text)))
            collect (row-turn session row))))

(defun ingest-session (store session &key dry-run)
  "Keep what SESSION said since STORE last read it, unless DRY-RUN. => the
turns that would be, or were, kept, oldest first."
  (let* ((cursor (store-session-cursor store session))
         (rows (session-rows session cursor)))
    (unless dry-run
      (keep-turns store rows)
      (when rows
        (note-progress store session
                        (loop for row in rows maximize (or (getf row :seq) 0)))))
    rows))

;;; --- ranking -----------------------------------------------------------------

(defun tokenize (text)
  "TEXT's words: split on anything but a letter, a digit or an apostrophe,
apostrophes trimmed, lowercased -- the core's own tokenizer."
  (let ((out '()) (start nil))
    (flet ((cut (end)
             (when start
               (let ((word (string-downcase (string-trim "'" (subseq text start end)))))
                 (unless (zerop (length word)) (push word out)))
               (setf start nil))))
      (loop for index from 0 below (length text)
            for character = (char text index)
            do (if (or (alphanumericp character) (char= character #\'))
                   (unless start (setf start index))
                   (cut index)))
      (cut (length text)))
    (nreverse out)))

(defun content-tokens (text)
  "TEXT's tokens less the stopwords and the one-letter words, as queries and
documents are compared."
  (remove-if (lambda (token)
               (or (<= (length token) 1) (member token +stopwords+ :test #'string=)))
             (tokenize text)))

(defun bm25 (documents query-tokens)
  "An alist (index . score) over DOCUMENTS -- lists of tokens -- for
QUERY-TOKENS, highest first. The core's BM25, K1 and B as its constants are."
  (let* ((count (length documents))
         (lengths (mapcar #'length documents))
         (total (reduce #'+ lengths :initial-value 0))
         (average (if (plusp count) (/ (float total 1d0) count) 0d0))
         (scores (make-hash-table)))
    (when (plusp count)
      (dolist (token (remove-duplicates query-tokens :test #'string=))
        (let ((frequency (count-if (lambda (document) (member token document :test #'string=))
                                   documents)))
          (when (plusp frequency)
            (let ((idf (log (+ 1d0 (/ (- count frequency 0.5d0) (+ frequency 0.5d0))))))
              (loop for document in documents
                    for index from 0
                    for tf = (count token document :test #'string=)
                    when (plusp tf)
                      do (incf (gethash index scores 0d0)
                               (* idf
                                  (/ (* tf (+ +k1+ 1d0))
                                     (+ tf (* +k1+
                                              (- 1d0 +b+
                                                 (* +b+ (/ (float (nth index lengths) 1d0)
                                                           average))))))))))))))
    (sort (loop for index being the hash-keys of scores using (hash-value score)
                collect (cons index score))
          #'> :key #'cdr)))

(defun query-phrase (query)
  "QUERY flattened and lowercased, the phrase the exact-match bonus looks for."
  (string-downcase (nlk:one-line (or query ""))))

(defun recall-turns (store query &key (top-k 5) exclude)
  "The TOP-K turns of STORE that answer QUERY, best first, and never one of
EXCLUDE's: its own turns are already in its window. Score is BM25 plus a
half point for QUERY appearing verbatim in the turn."
  (let ((query-tokens (content-tokens query))
        (turns (store-turns store)))
    (when (and query-tokens turns)
      (let* ((documents (mapcar (lambda (turn) (content-tokens (getf turn :text))) turns))
             (phrase (and (> (length query-tokens) 1) (query-phrase query)))
             (scored (loop for (index . score) in (bm25 documents query-tokens)
                           for turn = (nth index turns)
                           unless (and exclude (equal exclude (getf turn :session-id)))
                             collect (list* :score (if (and phrase
                                                             (search phrase
                                                                     (string-downcase
                                                                      (getf turn :text))))
                                                        (+ score 0.5d0)
                                                        score)
                                            turn))))
        (subseq (sort scored
                       (lambda (left right)
                         (let ((left-score (getf left :score))
                               (right-score (getf right :score)))
                           (if (/= left-score right-score)
                               (> left-score right-score)
                               (> (or (getf left :seq) 0) (or (getf right :seq) 0))))))
                0 (min top-k (length scored)))))))

;;; --- what a reader is shown -------------------------------------------------

(defun turn-date (turn)
  "The day TURN was said, or ? when its stamp carries none."
  (let ((at (getf turn :at)))
    (if (and at (> (length at) 10)) (subseq at 0 10) "?")))

(defun evidence-line (turn chars)
  "One evidence line, the shape the original's formatter answers: the
provenance in brackets, then the words, clipped."
  (format nil "- [~a #~a ~a ~a] ~a"
          (getf turn :session-id)
          (or (getf turn :session-turn) "?")
          (or (getf turn :speaker) "?")
          (turn-date turn)
          (nlk:clip (getf turn :text) chars :ellipsis "…")))

(defun recall-block (evidence chars)
  "EVIDENCE as the reminder a turn is told, verbatim and with provenance."
  (format nil "Recalled from past sessions (verbatim, with provenance):~%~{~a~^~%~}"
          (mapcar (lambda (turn) (evidence-line turn chars)) evidence)))

(defun evidence-json (turn)
  "TURN as the JSON object a tool answer carries."
  (list :session_id (getf turn :session-id)
        :session_turn (getf turn :session-turn)
        :speaker (getf turn :speaker)
        :at (getf turn :at)
        :score (format nil "~,3f" (getf turn :score))
        :text (getf turn :text)))

(defun json-answer (plist)
  "PLIST as a JSON object whose keys are spelled as their names, downcased:
SESSION_ID on the wire is `session_id'."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash (string-downcase (string key)) object)
                   (if (and (consp value) (keywordp (first value)))
                       (json-answer value)
                       value)))
    object))

(defun json-text (plist)
  "PLIST as the JSON text a tool or a verb answers."
  (nlk:encode-json-object (json-answer plist)))

(defun store-stats (store)
  "The counters zeromem_stats answers: turns, sessions, and the two ends."
  (let ((turns (store-turns store)))
    (list :turns (length turns)
          :sessions (length (remove-duplicates
                             (mapcar (lambda (turn) (getf turn :session-id)) turns)
                             :test #'equal))
          :oldest (if turns (turn-date (first turns)) "")
          :newest (if turns (turn-date (car (last turns))) ""))))
