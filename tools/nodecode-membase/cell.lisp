;;;; cell.lisp --- the section, the three tools, /membase, the round.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Membase is a hosted memory API: one developer key is one person's
;;;; account, and that account's Memories hold what its memory agent learned.
;;;; This cell is the client half of that, against the same API the original
;;;; Hermes plugin talks to through the unibaseio-membase SDK.
;;;;
;;;; Three tools, the same three the plugin gives a model:
;;;;   membase_search   the passages the Memories in reach hold on a question
;;;;   membase_save     save one note into the key's write target
;;;;   membase_profile  the user's standing facts, newest first
;;;;
;;;; Two automatic halves, one switch each:
;;;;   auto_recall    at every model request, the turn's own input is searched
;;;;                  and the passages ride that request, each naming the
;;;;                  Memory it came from; time-boxed and fail-open, exactly
;;;;                  as the plugin's prefetch is: a slow or refused search
;;;;                  leaves the turn without memory, never stops it.
;;;;   capture_turns  the operator's own words from each completed turn, saved
;;;;                  as one note (off by default). The assistant's half is
;;;;                  never sent, so an answer is never learned as a fact.
;;;;
;;;; Nothing here runs while the cell is off, and a section without a key
;;;; answers every verb with one sentence naming what to set.

(in-package #:nodecode-membase)

;;; --- the words -------------------------------------------------------------------

(defparameter +preamble+
  "## Membase: the user's long-term memory (each passage names the Memory it came from; use a passage only where it bears on the user's message)"
  "The first line of the block auto-recall injects.")

(defparameter *recall-reuse-seconds* 600
  "How long one conversation's identical input reuses the answer it already
injected, so a turn's later rounds and a prompt re-sent do not dial again.")

(defparameter *passage-chars* 1200
  "How much of one passage reaches the model.")

(defparameter *capture-chars* 8000
  "How much of a turn's own words a capture saves.")

(defparameter *profile-fact-chars* 500
  "How much of one profile fact reaches the model.")

(defvar *seen* (make-hash-table :test #'equal)
  "Session id -> (INPUT . UNIVERSAL-TIME) it last recalled, while the cell runs.")

(defvar *captured* (make-hash-table :test #'equal)
  "Turn id -> T once a capture wrote it, so a turn is saved once.")

(defvar *recalled* 0
  "How many passages this cell has injected since it started, for /membase.")

(defvar *recall-refused* nil
  "T once a turn's recall was answered 403, which stops automatic recall for
the session instead of retrying every turn; explicit calls still report it.")

;;; --- the manual -------------------------------------------------------------------

(defparameter +primer+
  "Membase is the user's hosted long-term memory: their Memories hold what a
memory agent learned from what they handed it, and every passage names the
Memory it came from.

(membase:recall \"query\")               the passages the Memories in reach hold on a query.
(membase:remember \"text\" &key title)    save one note into the key's write target; the
    Memory learns it on its next run, so it is searchable after that.
(membase:profile)                        the user's standing facts, newest first (needs the
    key's profile tick).
The model has the same three as tools: membase_search, membase_save and
membase_profile. Save only what the user asked you to remember, in their own
words.

Auto-recall is on: at each model request the turn's own input is searched and
the passages ride that request under `## Membase'. Capture of every turn is
off unless capture_turns is on. /membase shows the endpoint and the switches;
/membase search QUERY, /membase save TEXT and /membase profile ask by hand. A
refusal is ERROR: MEMBASE-ERROR and says what to set."
  "What (help :membase) answers while the cell runs.")

;;; --- what a search came back with ---------------------------------------------

(defun passages (value &key (limit (setting :recall-limit)) (max-chars (setting :recall-max-chars)))
  "VALUE's passages as (MEMORY . TEXT) pairs, in the API's order, at most
LIMIT of them and MAX-CHARS characters of rendering."
  (let ((pairs '()) (used 0))
    (loop for item across (items-of value)
          for content = (nlk:json-value item :string "content")
          when (and content (plusp (length (nlk:trimmed content))))
            do (let* ((body (clip content *passage-chars*))
                      (memory (passage-memory item))
                      (width (+ 4 (length memory) (length body))))
                 (when (and pairs (> (+ used width) max-chars)) (return))
                 (push (cons memory body) pairs)
                 (incf used width))
          when (>= (length pairs) (max 1 limit)) do (return))
    (nreverse pairs)))

(defun passage-text (pairs)
  "PAIRS as one line each: '- [Memory] passage'."
  (with-output-to-string (out)
    (dolist (pair pairs)
      (format out "- [~a] ~a~%" (car pair) (cdr pair)))))

(defun fast-mode-p ()
  "Whether the section asks for one round of retrieval and no model call."
  (let ((mode (setting :recall-mode)))
    (and (stringp mode) (string-equal mode "fast"))))

(defun recall-block-limit ()
  "Passages one turn's recall carries: more in fast mode, where this model,
not Membase's rerank, judges which of them bear on the turn."
  (let ((limit (setting :recall-limit)))
    (if (fast-mode-p) (max limit 10) limit)))

(defun recall-block-chars ()
  "The ceiling on one turn's block, raised in fast mode as the plugin does."
  (let ((chars (setting :recall-max-chars)))
    (if (fast-mode-p) (max chars 8000) chars)))

(defun recall-block (value)
  "The block a turn's recall injects for VALUE, or NIL when there is nothing
to say. A Memory that could not answer is named, so the model does not take
silence for 'nothing is remembered'."
  (let ((pairs (passages value :limit (recall-block-limit) :max-chars (recall-block-chars)))
        (missing (unavailable-memories value)))
    (when (or pairs missing)
      (with-output-to-string (out)
        (format out "~a~%" +preamble+)
        (if pairs
            (write-string (passage-text pairs) out)
            (format out "No passage matched this turn.~%"))
        (when missing
          (format out "Not searched this turn, so absence of a passage proves nothing for: ~{~a~^; ~}.~%"
                  missing))))))

(defun facts (value)
  "VALUE's profile facts as texts, newest first."
  (loop for item across (items-of value)
        for content = (nlk:json-value item :string "content")
        when (and content (plusp (length (nlk:trimmed content))))
          collect (clip content *profile-fact-chars*)))

;;; --- the three verbs ------------------------------------------------------------

(defun guarded (body)
  "Run BODY, answering its string; a refusal or a dial that failed becomes
the answer ERROR: ..., so the model reads what went wrong and the transcript
draws the call failed."
  (handler-case (funcall body)
    (membase-error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: MEMBASE-ERROR: ~a" condition)))
    (error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: MEMBASE-ERROR: ~a" condition)))))

(defun recall (query &key limit)
  "The passages the user's Membase Memories hold on QUERY, one line each =>
a string. LIMIT is how many passages in all (1-50)."
  (guarded
   (lambda ()
     (let ((text (nlk:trimmed (or query ""))))
       (when (zerop (length text)) (fail "a Membase search needs a query"))
       (multiple-value-bind (value status) (search-response text :limit limit)
         (let ((value (checked "search" value status))
               (pairs nil) (missing nil))
           (setf *last-error* nil
                 pairs (passages value :limit (or limit (setting :recall-limit))
                                        :max-chars 50000)
                 missing (unavailable-memories value))
           (cond ((and (null pairs) missing)
                  (format nil "No passage came back. ~{~a~^; ~} could not be searched, so nothing is ~
                               known about the rest of the Memories.~%" missing))
                 ((null pairs) "No passage matched.")
                 (t (with-output-to-string (out)
                      (write-string (passage-text pairs) out)
                      (when missing
                        (format out "Not searched, so absence of a passage proves nothing for: ~{~a~^; ~}.~%"
                                missing)))))))))))

(defun save-answer (value)
  "One sentence for a save's receipt VALUE."
  (let ((id (nlk:json-value value :string "id"))
        (status (or (nlk:json-value value :string "status") "pending"))
        (memories (learning-memories value)))
    (format nil "Saved~@[ ~a~]~@[ into ~{~a~^, ~}~] (status ~a): ~a"
            id memories status
            (if (equal status "succeeded")
                "it is searchable now."
                "the Memory learns it on its next run, so it is searchable after that."))))

(defun remember (content &key (title ""))
  "Save CONTENT as one note in the key's write target => a sentence."
  (guarded
   (lambda ()
     (let ((text (nlk:trimmed (or content ""))))
       (when (zerop (length text)) (fail "a Membase note needs its text"))
       (multiple-value-bind (value status) (save-response text :title title)
         (prog1 (save-answer (checked "save" value status))
           (setf *last-error* nil)))))))

(defun profile ()
  "The user's standing facts, newest first => a string."
  (guarded
   (lambda ()
     (multiple-value-bind (value status) (profile-response)
       (let ((texts (facts (checked "profile" value status))))
         (setf *last-error* nil)
         (if texts
             (with-output-to-string (out)
               (format out "The user's profile (newest first):~%")
               (dolist (text texts) (format out "- ~a~%" text)))
             "No standing facts are recorded about the user yet."))))))

;;; --- the model's side of them -----------------------------------------------------

(defun search-tool (call)
  (recall (search-call-query call) :limit (search-call-limit call)))

(defun save-tool (call)
  (remember (save-call-content call)))

(defun profile-tool (call)
  (declare (ignore call))
  (profile))

;;; --- the round: recall, and the opt-in capture -------------------------------------

(defun turn-key (turn)
  "TURN's id, whether the point handed over the turn or its id."
  (if (stringp turn) turn (nlk:durable-turn-turn-id turn)))

(defun turn-input (turn)
  "The words TURN was admitted with, or NIL."
  (nlk:json-value (nlk:events :turn-id (turn-key turn) :kind nlk::+kind-turn-started+
                               :as :payload)
                  :string "input"))

(defun date-stamp ()
  "Today, UTC, as YYYY-MM-DD."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun recall-for-turn (session input)
  "The block to inject for INPUT in SESSION, dialing only when this
conversation has not recalled this exact input recently; NIL when there is
nothing to inject, recall is refused, or the search failed."
  (unless *recall-refused*
    (let ((last (gethash session *seen*)))
      (when (and last (equal (car last) input)
                 (< (- (get-universal-time) (cdr last)) *recall-reuse-seconds*))
        (return-from recall-for-turn nil)))
    (setf (gethash session *seen*) (cons input (get-universal-time)))
    (handler-case
        (multiple-value-bind (value status) (search-response (clip input *search-chars*))
          (cond ((eql status 403)
                 ;; Withdrawn access is not retried every turn.
                 (setf *recall-refused* t
                       *last-error* (format nil "recall answered HTTP 403~@[ (~a)~]; automatic recall is off for this session"
                                             (sentence value)))
                 nil)
                ((not (answered-p status))
                 (setf *last-error* (format nil "recall answered HTTP ~a~@[: ~a~]" status (sentence value)))
                 nil)
                (t (let ((block (recall-block value)))
                     (when block (incf *recalled* (length (passages value))))
                     block))))
      (error (condition)
        (setf *last-error* (format nil "~a" condition))
        nil))))

(defun capture-turn (turn session input)
  "Save TURN's operator input into the key's write target once, titled by the
day; a failure is remembered for /membase and never stops a turn."
  (declare (ignore session))
  (unless (gethash (turn-key turn) *captured*)
    (when (> (hash-table-count *captured*) 500)
      (clrhash *captured*))
    (setf (gethash (turn-key turn) *captured*) t)
    (handler-case
        (multiple-value-bind (value status)
            (save-response (clip input *capture-chars*)
                           :title (format nil "Nodecode conversation ~a" (date-stamp)))
          (declare (ignore value))
          (unless (answered-p status)
            (setf *last-error* (format nil "capture answered HTTP ~a" status))))
      (error (condition)
        (setf *last-error* (format nil "~a" condition))))))

(defun on-round (op)
  "The :round point: TURN about to ask a model. The operator's words are
captured when capture_turns is on, and recalled into this very request."
  (let* ((session (getf op :session-id))
         (turn (getf op :turn))
         (input (and session turn (nlk:store-open-p) (configured-p) (turn-input turn))))
    (when (and input (plusp (length (nlk:trimmed input))))
      (when (setting :capture-turns) (capture-turn turn session input))
      (nlk:when-let (block (and (setting :auto-recall) (recall-for-turn session input)))
        (nlk:record-turn-context-prompt turn :text block)))))

;;; --- /membase ------------------------------------------------------------------

(defun status-line ()
  "One line: the endpoint, whether a key is there, the switches, what was
injected, and the last failure if one is still standing."
  (format nil "membase: ~a; key ~:[not set~;set~], auto-recall ~:[off~;on~], capture ~:[off~;on~], recall mode ~a; ~d passages injected this session~@[; last error: ~a~]"
          (base-url)
          (configured-p)
          (eq t (setting :auto-recall))
          (eq t (setting :capture-turns))
          (setting :recall-mode)
          *recalled*
          (and *last-error* (subseq *last-error* 0 (min 200 (length *last-error*))))))

(defun panel (line)
  "The dialog bare /membase answers: search the memory, save something into
it, read the profile, then the line as it stands."
  (nle:list-dialog
   :title "Membase" :summary line
   :rows (list (nle:ask-row "Search the memory" "Query" "/membase search"
                             :placeholder "what to look for" :verb "search")
               (nle:ask-row "Save a note" "Text" "/membase save"
                             :placeholder "what to remember" :verb "save")
               (nle:ask-row "Read the profile" "Profile" "/membase profile"
                             :placeholder "" :verb "profile"))))

(defun split-verb (args)
  "ARGS as (values HEAD REST): the first word, and the trimmed remainder."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text)))
    (values (if space (subseq text 0 space) text)
            (if space (nlk:trimmed (subseq text (1+ space))) ""))))

(defun run-slash (args session-id)
  "One /membase invocation => the answer's text, and typed alone the dialog
of what it does."
  (declare (ignore session-id))
  (multiple-value-bind (head rest) (split-verb args)
    (cond ((zerop (length head))
           (let ((line (status-line)))
             (values line (panel line))))
          ((string-equal head "status")
           (let ((line (status-line))) (values line (panel line))))
          ((string-equal head "search")
           (values (if (zerop (length rest))
                       "membase: search what? /membase search QUERY"
                       (recall rest))
                   (panel (status-line))))
          ((string-equal head "save")
           (values (if (zerop (length rest))
                       "membase: save what? /membase save TEXT"
                       (remember rest :title "Saved by the operator from /membase"))
                   (panel (status-line))))
          ((string-equal head "profile")
           (values (profile) (panel (status-line))))
          (t (values (format nil "membase: ~a is not one of its choices (search, save, profile, status)"
                             head)
                     (panel (status-line)))))))

(defun start ()
  "A fresh cell starts with no recall cached, no turn captured and no error
standing; a stop takes the state with it."
  (setf *seen* (make-hash-table :test #'equal)
        *captured* (make-hash-table :test #'equal)
        *recalled* 0
        *recall-refused* nil
        *last-error* nil)
  (nle:on-stop (lambda ()
                 (setf *seen* (make-hash-table :test #'equal)
                       *captured* (make-hash-table :test #'equal)
                       *recalled* 0
                       *recall-refused* nil
                       *last-error* nil))))

;;; --- the boundary payloads ---------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search the user's long-term memory in Membase: the passages their Memories hold on a question, most relevant first, each naming the Memory it came from. Use it when the answer may depend on what the user told you or on their material from before; cite the Memory name when you use a passage.")
  ("query" :text :required t :doc "the question, in natural language")
  ("limit" :integer :doc "how many passages in all, 1-50"))

(nlk:define-contract save-call
  (:doc "Save something to the user's Membase memory. Call it ONLY when the user asks you to remember something or plainly means it, and save it in the user's own words; never save your own answers, guesses or routine chatter. The note goes to the Memory this key writes to, which learns it on its next run, so it is searchable after that rather than instantly.")
  ("content" :text :required t :doc "what to remember, in the user's words"))

(defun no-arguments (description)
  "The schema of a tool whose call carries nothing."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:make-json-object))))

(defparameter +profile-schema+
  (no-arguments
   "Read the user's Membase profile: standing statements about who they are (name, role, lasting
preferences), newest first. Read it once near the start of a conversation when knowing the person
matters. It works only when the key's owner ticked the profile on the key.")
  "membase_profile takes nothing.")

;;; --- the one declaration ----------------------------------------------------------

(nle:define-cell membase
  (:section ("membase")
    (:guide "make a developer key in the Membase app (Connect > Developer keys; Read & write lets the agent save, and tick profile for membase_profile) and paste it in api_key, or export MEMBASE_API_KEY; auto_recall searches the user's memory before each turn, capture_turns also saves the operator's own words every turn")
    ("api_key" :secret
               :doc "the Membase developer key; MEMBASE_API_KEY answers when this is empty")
    ("base_url" :string :default "https://api.app.membase.com"
                :doc "the Membase API endpoint")
    ("auto_recall" :boolean :default t
                   :doc "search the user's memory before each turn and put the passages in that request")
    ("recall_mode" :choice :options '("full" "fast") :default "full"
                   :doc "full lets Membase's model pick the passages; fast is one round of retrieval with many more passages for this model to judge")
    ("recall_limit" :integer :default 5 :min 1
                     :doc "the most passages one turn's recall puts in front of the model")
    ("recall_max_chars" :integer :default 4000 :min 500
                         :doc "the ceiling on one turn's injected block")
    ("capture_turns" :boolean :default nil
                     :doc "save the operator's own words from every completed turn as one note; off saves only when the model is asked to")
    ("timeout" :integer :default 20 :min 1
               :doc "seconds one explicit call to Membase may take"))
  (:tool "membase_search" (nlk:contract 'search-call) 'search-tool)
  (:tool "membase_save" (nlk:contract 'save-call) 'save-tool)
  (:tool "membase_profile" +profile-schema+ 'profile-tool)
  (:command "membase" 'run-slash
            :description "Membase long-term memory: status, search, save, profile"
            :argument-hint "[search|save|profile|status] TEXT")
  (:help :membase "membase: long-term memory in Membase - membase:recall, membase:remember, membase:profile; /membase" +primer+)
  (:hook :round (nle:observer #'on-round "membase"))
  (:start #'start))
