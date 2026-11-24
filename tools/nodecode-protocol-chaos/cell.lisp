;;;; cell.lisp --- Protocol Chaos: the user's long-lived matters, on their own server.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of chaos-xxl's Hermes plugin (see NOTICE). The plugin has no
;;;; tools of its own: it hooks the turn, before the model answers and after
;;;; it has. That shape is kept here on Nodecode's seams -- the :round point
;;;; for what a round is told, the :frame point for what the turn said -- and
;;;; the original's own words travel over the same MCP endpoint with the same
;;;; bearer token:
;;;;
;;;;   context_pack          the directory of matters, paged, cached 30 seconds
;;;;   event_get_protocol    one matter by id, its accounts and entities
;;;;   event_note            record the user's words, into a matter or a new one
;;;;
;;;; The message is matched to matter titles on this machine (word overlap
;;;; weighted by how rare a word is among the titles); only the ids chosen go
;;;; to the server, so the message itself is never sent to read matters.
;;;;
;;;; What is recorded after a turn is copied verbatim, as the original does:
;;;; with capture "auto" the auxiliary model decides whether the message says
;;;; something that lasts, and where; with "match" the matter whose title
;;;; shares the most words takes it; with "off" nothing is written.

(defpackage #:nodecode-protocol-chaos
  (:use #:cl)
  (:export #:protocol-chaos-error #:protocol-chaos-error-detail))

(in-package #:nodecode-protocol-chaos)

(nlk:define-peripheral protocol-chaos :not-running t :lock "protocol-chaos")

(defparameter +header+
  "[Protocol Chaos · added by the client, not written by the user]"
  "The line the original puts over context it adds, so the model never reads it as the user's.")

(defparameter +overview-marker+ "Protocol Chaos · the user's matters"
  "How an overview already in the conversation is recognized.")

(defparameter +open-states+ '("emerging" "active" "waiting" "dormant")
  "The states of a matter a user still holds.")

(defparameter +overview-chars+ 3900
  "How much of the overview one turn's context carries.")

(defparameter +related-chars+ 5000
  "How much of the related matters one round's context carries.")

(defparameter +context-chars+ 9500
  "The whole block this cell adds to one round, the header included.")

(defparameter +related-min-score+ 1.0
  "How much a title must share with the message to be brought in.")

(defparameter +candidate-min-score+ 0.5
  "How much a title must share with the message to be offered to the recording decision.")

(defparameter +directory-page+ 200
  "One page of the directory, as the original asks for it.")

(defparameter +directory-max+ 2000
  "The most entries one directory read keeps.")

(defparameter +directory-fresh-seconds+ 30
  "How long a directory read is reused before it is asked for again.")

(defparameter +capture-max-chars+ 600
  "How much of a message one recording send carries.")

(defparameter +stop-words+
  '("the" "a" "an" "and" "or" "to" "of" "in" "on" "for" "with" "my" "me" "i" "is" "it" "this"
    "that" "be" "at" "by" "we" "you" "our" "your" "are" "was" "do" "does" "did" "can" "will"
    "just" "about" "from" "have" "has" "had" "not" "but" "so" "if" "as")
  "Words too common in a message or a title to say what a matter is about.")

(defvar *directory* nil
  "The server's directory as it was last read: a DIRECTORY-STATE, or NIL.")

(defvar *turns* nil
  "What each running turn's user message said, by turn id, until its completion.")

(defvar *shown* nil
  "Matter ids already shown in each turn, by turn id, so a round does not repeat one.")

(defvar *overview-turn* nil
  "The turn the overview was last added to.")

(defvar *queued* '()
  "Finished turns waiting to be recorded, (TURN-ID . TEXT), newest first.")

(defvar *worker* nil
  "The recording worker (NLK:WORKER-START) while the cell runs, or NIL.")

;;; --- the section, read back -------------------------------------------------------

(defun endpoint ()
  "The MCP endpoint the section names, without a trailing slash."
  (string-right-trim "/" (or (setting :url) "")))

(defun token ()
  "The bearer token: written in the section, or read from the variable it names,
else from PROTOCOL_CHAOS_TOKEN."
  (or (let ((written (setting :token))) (and written (plusp (length written)) written))
      (let ((variable (setting :token-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))
      (uiop:getenv "PROTOCOL_CHAOS_TOKEN")))

(defun refuse (format-control &rest args)
  "Refuse the section: the gateway renders this as the folder's REFUSED state."
  (error 'nlk:config-refusal :detail (apply #'format nil format-control args)))

(defun check-config ()
  "Refuse a section that could reach no server, naming the member."
  (unless (plusp (length (endpoint)))
    (refuse "the url of your Protocol Chaos server is not set: give \"url\" in the protocol-chaos ~
           section of config.jsonc, such as https://example.org:8443/mcp, then (restart-cells ~
           \"nodecode-protocol-chaos\")"))
  (unless (search "://" (endpoint))
    (refuse "the url ~s is not an http(s) address" (endpoint)))
  (unless (token)
    (refuse "no token: set \"token\" in the protocol-chaos section, or \"token_env\" naming the ~
           variable that holds it (PROTOCOL_CHAOS_TOKEN is read either way)")))

;;; --- the wire --------------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((hash-table-p answer) answer)
        (t (nlk:json-object))))

(defun answer-text (result)
  "The text of an MCP result's content blocks, joined."
  (with-output-to-string (out)
    (loop for block across (or (nlk:json-value result :array "content") #())
          when (hash-table-p block)
            do (let ((text (nlk:json-value block :string "text")))
                 (when text (write-string text out))))))

(defun json-text-p (text)
  "Whether TEXT is a JSON object or array, which a tool answers with data."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (and (plusp (length text))
         (let ((first (char text 0)))
           (or (char= first #\{) (char= first #\[))))))

(defun call-tool (tool arguments &key (timeout (setting :timeout-seconds)))
  "TOOL with ARGUMENTS on the server: the MCP result's decoded value.
A transport failure, a refused status or an MCP error signals."
  (let* ((body (nlk:json-object "jsonrpc" "2.0" "id" 1 "method" "tools/call"
                                 "params" (nlk:json-object "name" tool "arguments" arguments)))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :post (endpoint)
                    :headers (list (cons "authorization" (format nil "Bearer ~a" (or (token) "")))
                                   '("content-type" . "application/json")
                                   '("accept" . "application/json, text/event-stream")
                                   '("mcp-protocol-version" . "2025-06-18"))
                    :content (nlk:encode-json-object body)
                    :timeout timeout
                    :connect-timeout (min 6 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" tool (nle:transport-failure-label condition (endpoint)))))
    (unless (<= 200 status 299)
      (fail "~a: HTTP ~d" tool status))
    (let ((payload (decode-body answer)))
      (when (nlk:json-value payload :object "error")
        (fail "~a: MCP error ~a" tool (nlk:json-value payload :string "error" "message")))
      (let* ((result (nlk:json-value payload :object "result"))
             (text (and result (answer-text result))))
        (cond ((null result) (fail "~a: the server answered no result" tool))
              ((nlk:json-value result :boolean "isError")
               (let ((code (and text (json-text-p text) (ignore-errors (nlk:decode-json text)))))
                 (fail "~a: ~a" tool (or (nlk:json-value code :string "code") "ERROR"))))
              ((and text (json-text-p text)) (ignore-errors (nlk:decode-json text)))
              (t (or text "")))))))

(defstruct (directory-state (:constructor make-directory-state))
  "One read of the server's directory and when it was made."
  (entries '())
  (version "")
  (fetched 0))

(defun matter-directory (&key (max-age +directory-fresh-seconds+) force)
  "The server's directory of matters, in the server's order: read again only
when it may be stale, and then in full only when the server says it changed."
  (let ((state *directory*))
    (when (and state (not force)
               (<= (- (get-universal-time) (directory-state-fetched state)) max-age))
      (return-from matter-directory (directory-state-entries state)))
    (let* ((arguments (apply #'nlk:make-json-object
                             (append (list "limit" +directory-page+)
                                     (when (and state (plusp (length (directory-state-version state))))
                                       (list "sinceVersion" (directory-state-version state))))))
           (first (call-tool "context_pack" arguments))
           (entries (and state (directory-state-entries state)))
           (version (and state (directory-state-version state))))
      (unless (hash-table-p first)
        (fail "context_pack answered ~s, not an object" first))
      (unless (nlk:json-value first :boolean "unchanged")
        (let ((all (coerce (or (nlk:json-value first :array "events") #()) 'list))
              (cursor (nlk:json-value first :string "next")))
          (loop while (and cursor (< (length all) +directory-max+))
                do (let* ((page (call-tool "context_pack"
                                            (nlk:json-object "limit" +directory-page+ "cursor" cursor))))
                     (setf all (append all (coerce (or (nlk:json-value page :array "events") #()) 'list))
                           cursor (nlk:json-value page :string "next"))))
        (setf entries (remove-if-not (lambda (entry)
                                        (and (hash-table-p entry) (nlk:json-value entry :string "id")))
                                      all)
              version (or (nlk:json-value first :string "version") ""))))
      (setf *directory* (make-directory-state :entries entries :version version
                                               :fetched (get-universal-time)))
      entries)))

(defun matter (id)
  "One matter by ID: the server's event_get_protocol answer."
  (let ((result (call-tool "event_get_protocol" (nlk:json-object "eventId" id "accounts" 3))))
    (unless (and (hash-table-p result) (nlk:json-value result :object "event"))
      (fail "no matter ~s on the server" id))
    result))

(defun note-arguments (text &key id title parent)
  "The event_note arguments that record TEXT as the user's own words."
  (apply #'nlk:make-json-object
         (append (list "actor" (nlk:json-object "kind" "agent" "id" (setting :actor))
                       "accountSource" "user"
                       "text" text)
                 (cond (id (list "eventId" id))
                       (t (append (when (plusp (length title)) (list "title" title))
                                  (when parent (list "parentEventId" parent))))))))

(defun record (text &key id title parent)
  "Record TEXT on the server, in matter ID or in a new one titled TITLE; => the receipt."
  (prog1 (call-tool "event_note" (note-arguments text :id id :title (or title "") :parent parent)
                     :timeout 15)
    (setf *directory* nil)))

;;; --- matching and rendering, on this machine ----------------------------------------

(defun collapse-space (text)
  "TEXT with every run of white space as one space, trimmed."
  (let ((out (make-string-output-stream))
        (space nil))
    (loop for character across (string text)
          do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                 (setf space t)
                 (progn (when (and space (plusp (file-position out))) (write-char #\Space out))
                        (setf space nil)
                        (write-char character out))))
    (string-trim '(#\Space) (get-output-stream-string out))))

(defun clip (text &optional (limit 80))
  "TEXT as one line of at most LIMIT characters, an ellipsis where it was cut."
  (let ((flat (if (stringp text) (collapse-space text) (format nil "~a" (or text "")))))
    (if (<= (length flat) limit)
        flat
        (concatenate 'string (string-right-trim " " (subseq flat 0 (max 0 (1- limit)))) "…"))))

(defun cjk-p (character)
  "Whether CHARACTER is written without spaces between words."
  (let ((code (char-code character)))
    (or (<= #x3040 code #x30FF) (<= #x3400 code #x4DBF) (<= #x4E00 code #x9FFF)
        (<= #xAC00 code #xD7AF) (<= #xF900 code #xFAFF))))

(defun latin-word-p (character)
  "Whether CHARACTER is part of a word written with spaces around it."
  (or (digit-char-p character) (and (alpha-char-p character) (< (char-code character) 128))))

(defun tokens (text)
  "The words of TEXT, and for a script written without spaces, pairs of
adjacent characters; downcased, stop words and one-letter words left out."
  (let ((text (string-downcase (or text "")))
        (found '())
        (run '()))
    (flet ((flush-run ()
             (loop for (first second) on run
                   while second
                   do (let ((pair (coerce (list first second) 'string)))
                        (unless (member pair '("我们" "你们" "他们" "一个" "这个" "那个" "什么" "怎么")
                                        :test #'string=)
                          (push pair found))))
             (setf run '()))
           (flush-word (start end)
             (when (< start end)
               (let ((word (subseq text start end)))
                 (when (and (or (>= (length word) 2) (every #'digit-char-p word))
                            (not (member word +stop-words+ :test #'string=)))
                   (push word found))))))
      (let ((word-start nil) (index 0))
        (loop for character across text
              do (cond ((cjk-p character)
                        (when word-start (flush-word word-start index) (setf word-start nil))
                        (push character run))
                       ((latin-word-p character)
                        (when run (flush-run))
                        (unless word-start (setf word-start index)))
                       (t (when word-start (flush-word word-start index) (setf word-start nil))
                          (when run (flush-run))))
              do (incf index))
        (when word-start (flush-word word-start index))
        (when run (flush-run))))
    (remove-duplicates found :test #'string=)))

(defun entry-id (entry) (nlk:json-value entry :string "id"))

(defun entry-title (entry) (or (nlk:json-value entry :string "title") ""))

(defun entry-state (entry) (or (nlk:json-value entry :string "state") ""))

(defun entry-parents (entry)
  "ENTRY's parent ids."
  (coerce (or (nlk:json-value entry :array "parents") #()) 'list))

(defun rank (message entries minimum limit &optional exclude)
  "ENTRIES whose titles share words with MESSAGE, best first. A shared word
weighs by how few titles carry it, scaled so a word in one title alone
weighs one; a matter already closed weighs less."
  (let ((wanted (tokens message)))
    (when (null wanted)
      (return-from rank '()))
    (let* ((titles (mapcar (lambda (entry) (cons entry (tokens (entry-title entry)))) entries))
           (count (length titles))
           (frequency (make-hash-table :test #'equal)))
      (dolist (pair titles)
        (dolist (word (cdr pair)) (incf (gethash word frequency 0))))
      (let ((unique (log (/ (1+ count) 1.5))))
        (when (<= unique 0) (return-from rank '()))
        (let ((scored '()))
          (dolist (pair titles)
            (let ((entry (car pair)))
              (unless (member (entry-id entry) exclude :test #'equal)
                (let ((shared (intersection wanted (cdr pair) :test #'string=)))
                  (when shared
                    (let ((score (/ (loop for word in shared
                                          sum (log (/ (1+ count) (+ (gethash word frequency) 0.5))))
                                    unique)))
                      (unless (member (entry-state entry) +open-states+ :test #'string=)
                        (setf score (* score 0.75)))
                      (when (>= score minimum)
                        (push (cons score entry) scored))))))))
          (let ((ranked (mapcar #'cdr (stable-sort scored #'> :key #'car))))
            (subseq ranked 0 (min limit (length ranked)))))))))

(defun forest (entries)
  "ENTRIES as (DEPTH . ENTRY) pairs: each under the first of its parents
that is also listed, the server's order kept at every level."
  (let ((listed (mapcar #'entry-id entries))
        (children (make-hash-table :test #'equal))
        (roots '())
        (out '())
        (seen (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((parent (find-if (lambda (candidate)
                               (and (member candidate listed :test #'equal)
                                    (not (equal candidate (entry-id entry)))))
                             (entry-parents entry))))
        (if parent
            (push entry (gethash parent children))
            (push entry roots))))
    (labels ((walk (entry depth)
               (unless (gethash (entry-id entry) seen)
                 (setf (gethash (entry-id entry) seen) t)
                 (push (cons depth entry) out)
                 (dolist (child (reverse (gethash (entry-id entry) children)))
                   (walk child (min (1+ depth) 4))))))
      (dolist (root (reverse roots)) (walk root 0))
      ;; anything left -- a cycle a server should have refused -- still shows
      (dolist (entry entries) (walk entry 0)))
    (nreverse out)))

(defun fit-lines (lines budget more)
  "As many leading LINES as fit in BUDGET characters, then MORE saying how
many were left."
  (let ((text (format nil "~{~a~^~%~}" lines)))
    (if (<= (length text) budget)
        text
        (let ((kept (copy-list lines)))
          (loop while kept
                do (let ((note (format nil more (- (length lines) (length kept)))))
                     (when (<= (+ (length (format nil "~{~a~^~%~}" kept)) (length note) 1) budget)
                       (return-from fit-lines (format nil "~{~a~^~%~}~%~a" kept note)))
                     (setf kept (butlast kept))))
          ""))))

(defun progress-text (thing)
  "The text of a progress object, or NIL."
  (and (hash-table-p thing) (nlk:json-value thing :string "text")))

(defun overview-text (entries budget)
  "The overview the original puts before the model: open matters, the parts
of each indented under it."
  (let ((open (remove-if-not (lambda (entry)
                                (member (entry-state entry) +open-states+ :test #'string=))
                              entries))
        (head (list (format nil "~a, from their own Protocol Chaos repository (the client's ~
overview, not their words)." +overview-marker+)
                    "Open matters, most recently updated first; indented ones are parts of the one above.")))
    (if (null open)
        (format nil "~{~a~^~%~}~%- (none yet)" head)
        (let ((lines (loop for (depth . entry) in (forest open)
                           collect (let* ((parts (list (clip (or (entry-title entry) "(untitled)") 80)
                                                       (entry-state entry))))
                                     (when (progress-text (nlk:json-value entry :object "progress"))
                                       (setf parts (append parts
                                                           (list (clip (progress-text
                                                                        (nlk:json-value entry :object "progress"))
                                                                       60)))))
                                     (format nil "~a- ~{~a~^ · ~}" (make-string depth :initial-element #\Space)
                                             parts)))))
          (let ((body (fit-lines lines (- budget (length (format nil "~{~a~^~%~}" head)) 1)
                                  "- … and {} more (context_pack lists every matter)")))
            (if (plusp (length body))
                (format nil "~{~a~^~%~}~%~a" head body)
                (format nil "~{~a~^~%~}" head)))))))

(defun matter-text (result &key by-id children)
  "One matter as the lines the model reads: its title and id, its fields,
its progress, who and what it involves, its parents, its open parts and
the last of its words."
  (let* ((event (nlk:json-value result :object "event"))
         (id (nlk:json-value event :string "id"))
         (lines (list (format nil "- ~a · ~a [id:~a]"
                              (clip (or (nlk:json-value event :string "title") "(untitled)") 80)
                              (or (nlk:json-value event :string "state") "emerging")
                              id)))
         (facts (nlk:json-value event :object "facts"))
         (progress (nlk:json-value event :object "progress")))
    (dolist (key '("goal" "constraint" "plan"))
      (let ((value (and facts (nlk:json-value facts :string key))))
        (when (and value (plusp (length value)))
          (setf lines (append lines (list (format nil "  ~a: ~a" key (clip value 400))))))))
    (when (and progress (progress-text progress))
      (setf lines (append lines (list (format nil "  progress: ~a~@[ (~a)~]"
                                              (clip (progress-text progress) 200)
                                              (let ((at (nlk:json-value progress :string "timestamp")))
                                                (and at (subseq at 0 (min 10 (length at))))))))))
    (let* ((entities (coerce (or (nlk:json-value result :array "entities") #()) 'list))
           (names (mapcar (lambda (entity)
                            (cons (nlk:json-value entity :string "id")
                                  (nlk:json-value entity :string "name")))
                          (remove-if-not #'hash-table-p entities)))
           (involves (loop for ref across (or (nlk:json-value event :array "refs") #())
                           when (hash-table-p ref)
                             collect (or (nlk:json-value ref :string "criteria")
                                         (cdr (assoc (nlk:json-value ref :string "ref") names :test #'equal))
                                         (nlk:json-value ref :string "ref")))))
      (when (remove nil involves)
        (setf lines (append lines (list (format nil "  involves: ~{~a~^; ~}"
                                                (loop for item in involves
                                                      when item collect (clip item 60))))))))
    (dolist (parent (entry-parents event))
      (setf lines (append lines (list (format nil "  part of: ~a [id:~a]"
                                              (clip (or (cdr (assoc parent by-id :test #'equal))
                                                        "(a matter not listed)")
                                                    60)
                                              parent)))))
    (let ((parts (loop for entry in (gethash id children)
                       when (member (entry-state entry) +open-states+ :test #'string=)
                         collect entry into kept
                       finally (return (subseq kept 0 (min 5 (length kept)))))))
      (when parts
        (setf lines (append lines (list (format nil "  parts: ~{~a~^; ~}"
                                                (mapcar (lambda (part)
                                                          (format nil "~a [id:~a]"
                                                                  (clip (or (entry-title part) "(untitled)") 40)
                                                                  (entry-id part)))
                                                        parts)))))))
    (let ((accounts (coerce (or (nlk:json-value result :array "accounts") #()) 'list)))
      (dolist (account (last accounts (min 2 (length accounts))))
        (let ((text (and (hash-table-p account) (nlk:json-value account :string "text"))))
          (when (and text (plusp (length text)))
            (let ((at (nlk:json-value account :string "createdAt")))
              (setf lines (append lines (list (format nil "  said ~a~a: ~a"
                                                      (if at (subseq at 0 (min 10 (length at))) "")
                                                      (if (equal (nlk:json-value account :string "source")
                                                                 "user")
                                                          "" " (an assistant's note)")
                                                      (clip text 240))))))))))
    (format nil "~{~a~^~%~}" lines)))

(defun children-of (entries)
  "A parent id -> its listed children."
  (let ((out (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (dolist (parent (entry-parents entry))
        (push entry (gethash parent out))))
    out))

;;; --- what to keep after a turn -----------------------------------------------------

(defun candidate-text (entries)
  "The matters a recording decision may pick, one line each with a short
ref, as the original lists them to the model."
  (format nil "~{~a~^~%~}"
          (loop for (depth . entry) in (forest (remove-if-not
                                                 (lambda (entry)
                                                   (member (entry-state entry) +open-states+
                                                           :test #'string=))
                                                 entries))
                collect (format nil "~a~a ~a [~a]~@[ · ~a~]"
                                (make-string depth :initial-element #\Space)
                                (entry-id entry) (clip (entry-title entry) 80)
                                (entry-state entry)
                                (progress-text (nlk:json-value entry :object "progress"))))))

(defparameter +capture-instructions+
  "You decide whether one message from the user is saved in their personal repository of
long-lived matters, and where. Save it only when it changes something about one of their
matters (its aim, its wishes or limits, its plan, its progress or its state) or starts a new
one: an intention, plan, commitment, purchase, trip, target, arrangement, decision or
correction about their own life or work that will still matter after this conversation. Do not
save questions, requests for information or advice, hypotheticals, instructions or requests
for work, small talk, or one-off occurrences such as a meal, a workout or a payment. The
listed matters show their parts indented under them: put the words on the most specific
matter they are about and give its ref exactly as listed (such as m12). For a new matter give
a title in the user's language, at most 30 characters, that names it on its own -- what, and
when or which wherever a similar one could follow -- and, when it belongs under a listed
matter, that matter's ref as parentRef. words: the part of the message about this matter,
copied character for character; usually the whole message. Answer with one JSON object and
nothing else: {\"record\": true|false, \"ref\": \"m12\"|null, \"title\": null,
\"parentRef\": null, \"words\": \"...\"}.")

(defun json-part (text)
  "The JSON object inside TEXT: from its first brace to its last, or NIL."
  (let* ((text (or text ""))
         (start (position #\{ text))
         (end (position #\} text :from-end t)))
    (and start end (> end start) (subseq text start (1+ end)))))

(defun decide (text entries)
  "The recording decision: the auxiliary model's JSON answer, or NIL when it
answered nothing readable. The message and the matter titles are all it
sees, as in the original."
  (handler-case
      (let* ((today (multiple-value-bind (second minute hour date month year)
                         (decode-universal-time (get-universal-time))
                       (declare (ignore second minute hour))
                       (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))
             (answer (nle:complete
                      +capture-instructions+
                      (format nil "Today is ~a.~%~%The user's matters in the repository:~%~a~%~%The user's message:~%~a"
                              today (candidate-text entries) text)
                      :max-tokens 300))
             (decoded (and answer (json-part answer) (ignore-errors (nlk:decode-json (json-part answer))))))
        (and (hash-table-p decoded) decoded))
    (error (condition)
      (warn "protocol-chaos: the recording model answered nothing (~a)" condition)
      nil)))

(defun best-matter (text entries)
  "The id of the matter whose title shares the most with TEXT, or NIL."
  (let ((best (first (rank text entries +related-min-score+ 1))))
    (and best (entry-id best))))

(defun recorded-entry (text)
  "TEXT, the user's message of a finished turn, as the queue entry that
records it: (ID WORDS TITLE PARENT), or NIL when nothing is recorded."
  (when (and (stringp text) (plusp (length (string-trim " " text))))
    (let ((text (clip text +capture-max-chars+)))
        (handler-case
            (let* ((entries (matter-directory))
                   (mode (setting :capture))
                   (decision (and (equal mode "auto") (decide text entries))))
              (cond
                ((equal mode "off") nil)
                ((equal mode "match")
                 (let ((id (best-matter text entries)))
                   (and id (list id text))))
                ((and decision (eq t (nlk:json-value decision :boolean "record")))
                 (let* ((ref (nlk:json-value decision :string "ref"))
                        (parent (nlk:json-value decision :string "parentRef"))
                        ;; only a ref that was listed counts; anything else starts a matter
                        (id (and ref (find ref entries :key #'entry-id :test #'equal) ref))
                        (words (or (nlk:json-value decision :string "words") text)))
                   (list id (clip words +capture-max-chars+)
                         (nlk:json-value decision :string "title")
                         (and parent (find parent entries :key #'entry-id :test #'equal) parent))))
                (t (let ((id (best-matter text entries)))
                     (and id (list id text))))))
          (error (condition)
            (warn "protocol-chaos: a finished turn's decision failed (~a)" condition)
            nil)))))

(defun send-queued (entry)
  "Record one queued turn on the server, best effort."
  (destructuring-bind (id words &optional title parent) entry
    (handler-case
        (let ((receipt (record words :id id :title (or title (clip words 80)) :parent parent)))
          (format nil "recorded ~a" (or (nlk:json-value receipt :string "eventId") "a matter")))
      (error (condition)
        (warn "protocol-chaos: a finished turn was not recorded (~a)" condition)
        nil))))

(defun pop-queued (&aux (entry nil))
  (with-protocol-chaos-lock
    (when *queued* (setf entry (first *queued*) *queued* (rest *queued*))))
  entry)

(defun capture-step ()
  "The worker's step: record every queued turn, oldest first."
  (loop for entry = (pop-queued)
        while entry
        do (send-queued entry))
  nil)

;;; --- the two seams ----------------------------------------------------------------

(defun turn-user-text (payload)
  "The user's text of a turn.input_committed PAYLOAD."
  (nlk:fact-message-content payload))

(defun observe-frame (op)
  "Fold one published frame: a turn's user message kept, a finished turn queued."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when (and type turn *protocol-chaos*)
      (cond
        ((equal type "turn.input_committed")
         (let ((text (turn-user-text payload)))
           (when (and (stringp text) (plusp (length text)))
             (with-protocol-chaos-lock
               (setf (gethash turn *turns*) text)))))
        ((equal type "turn.completed")
         (let ((text (with-protocol-chaos-lock
                       (prog1 (gethash turn *turns*) (remhash turn *turns*)))))
           (when (and text (not (equal (setting :capture) "off")))
             (let ((entry (recorded-entry text)))
               (when entry
                 (with-protocol-chaos-lock
                   (setf *queued* (append (butlast *queued* 7) (list entry))))
                 (nlk:worker-poke *worker*))))))))))

(defun shown-in (turn)
  "The matter ids already shown in TURN."
  (with-protocol-chaos-lock (copy-list (gethash turn *shown*))))

(defun show-in (turn id)
  (with-protocol-chaos-lock
    (pushnew id (gethash turn *shown*) :test #'equal)))

(defun related-blocks (matches entries)
  "The MATCHES as matter blocks, paired with their ids, skipping any the
server could not read."
  (let ((by-id (mapcar (lambda (entry) (cons (entry-id entry) (entry-title entry))) entries))
        (children (children-of entries))
        (blocks '()))
    (dolist (entry matches)
      (let ((id (entry-id entry)))
        (handler-case
            (progn (push (cons id (matter-text (matter id) :by-id by-id :children children)) blocks)
                   (show-in *turn* id))
          (error (condition)
            (warn "protocol-chaos: matter ~a could not be read (~a)" id condition)))))
    (nreverse blocks)))

(defvar *turn* nil
  "The turn the round being observed belongs to, for SHOW-IN.")

(defun round-context (turn round)
  "The block this round adds: the overview on a turn's first round, then the
matters the message may be about. NIL when there is nothing to add."
  (let* ((entries (matter-directory))
         (parts '())
         (related (setting :related)))
    (when (and (setting :overview)
               (or (null round) (zerop round) (not (equal *overview-turn* turn))))
      (push (overview-text entries +overview-chars+) parts)
      (setf *overview-turn* turn))
    (let ((text (with-protocol-chaos-lock (gethash turn *turns*))))
      (when (and text (plusp (length (string-left-trim " " text))) (plusp related)
                 (not (char= (char (string-left-trim " " text) 0) #\/)))
        (let* ((*turn* turn)
               (matches (rank text entries +related-min-score+ related (shown-in turn)))
               (blocks (related-blocks matches entries)))
          (when blocks
            (push (format nil "Matters this message may be about (picked by the client from their titles):~%~{~a~^~%~}"
                          (mapcar #'cdr blocks))
                  parts)))))
    (when parts
      (let ((body (format nil "~a~%~%~{~a~^~%~%~}" +header+ (nreverse parts))))
        (if (> (length body) +context-chars+)
            (concatenate 'string (subseq body 0 (1- +context-chars+)) "…")
            body)))))

(defun observe-round (op)
  "At a model request: the overview and the matters this round may be about,
as context the client adds -- never as the user's words."
  (let ((turn (getf op :turn)) (round (getf op :round)))
    (when (and turn *protocol-chaos*
               (or (setting :overview) (plusp (setting :related))))
      (handler-case
          (let ((text (round-context turn round)))
            (when text
              (nlk:record-turn-context-prompt turn :text text)))
        (error (condition)
          (warn "protocol-chaos: no context this round (~a)" condition))))))

;;; --- the tools ---------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: protocol-chaos: ~a" condition)))))

(defun argument-text (args name &key (required t))
  "ARGS' NAME as a trimmed string."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (when required (fail "~a is required" (substitute #\_ #\- name))))))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *matters-schema*
  (tool-schema
   "The user's open long-lived matters from their own Protocol Chaos repository, as a tree with each matter's state and latest progress. Use it before answering anything about what the user is working on, planning or waiting for; protocol_chaos_matter takes one id from here for its goal, limits, plan and words."
   '())
  "The protocol_chaos_matters tool.")

(defparameter *matter-schema*
  (tool-schema
   "One matter in full: its goal, limits, plan, progress, who and what it involves, the larger matters it is part of, its open parts, and the last words recorded on it. Take the id from protocol_chaos_matters or from the context the client adds."
   '(("id" "string" "The matter's id, as [id:...] shows it." t)))
  "The protocol_chaos_matter tool.")

(defparameter *record-schema*
  (tool-schema
   "Record what the user just said, verbatim, in their Protocol Chaos repository: on the matter it is about (matter_id), or as a new matter (title). Call it the moment the user states something that will still matter after this conversation -- an intention, plan, commitment or correction -- with their words copied character for character. A finished turn is recorded automatically too; this is for words the user wants kept now."
   '(("text" "string" "The user's words, copied character for character." t)
     ("matter_id" "string" "The matter the words belong on; leave it out to start a new one." nil)
     ("title" "string" "For a new matter: what it is, at most 30 characters." nil)))
  "The protocol_chaos_record tool.")

(defun tool-matters (args)
  (declare (ignore args))
  (answering (overview-text (matter-directory) +overview-chars+)))

(defun tool-matter (args)
  (answering
    (let* ((id (argument-text args "id"))
           (entries (matter-directory)))
      (matter-text (matter id)
                   :by-id (mapcar (lambda (entry) (cons (entry-id entry) (entry-title entry))) entries)
                   :children (children-of entries)))))

(defun tool-record (args)
  (answering
    (let ((text (argument-text args "text"))
          (id (argument-text args "matter_id" :required nil))
          (title (argument-text args "title" :required nil)))
      (let ((receipt (record text :id id :title (or title (clip text 80)))))
        (format nil "Recorded~@[ on ~a~]~@[, starting ~a~]."
                (nlk:json-value receipt :string "eventId")
                (nlk:json-value receipt :string "title"))))))

;;; --- the manual and /protocol-chaos -------------------------------------------------

(defparameter +primer+
  "Protocol Chaos keeps the user's long-lived matters on their own server: what they are
working toward, the limits on it, the plan, the progress, the state, and the words they said.
Before a round the client adds an overview of the open matters (on a turn's first round) and
the matters whose titles share words with the message; after a finished turn the words are
recorded on the matter they are about. Runs beside any memory cell, not instead of it.
(help :protocol-chaos) is this page. The tools:
  protocol_chaos_matters        the open matters, as a tree with state and progress
  protocol_chaos_matter {id}    one matter in full: goal, limits, plan, progress, parts, words
  protocol_chaos_record {text, matter_id?, title?}   keep the user's words, verbatim
Only ids go to the server when matters are read; the message stays on this machine. A matter
is one line: '- title · state [id:...]', and the parts indented under the matter they belong
to. When the user asks to organize their repository, follow the organizer's order: read
event_words_since from a checkpoint (a file beside your config), context_pack for the
directory, then one event_record_expression per matter with expectedVersion, actor
{\"kind\":\"agent\",\"id\":<this client>}, accountId and text of the word it rests on, and
only the fields that change -- title, state, progress, facts, refs; event_add_parent for the
hierarchy, and entity_search then entity_record for the people and things involved. A
confirmed conflict (VERSION_CONFLICT) is read again and written once more, never retried
blind. The protocol-chaos section of config.jsonc names url, the token (or token_env), the
actor the token was issued for, and whether a finished turn is recorded: auto (the auxiliary
model decides), match (the matter whose title shares the most words takes it) or off."
  "What (help :protocol-chaos) answers while the cell runs.")

(defun status-line ()
  (format nil "Protocol Chaos at ~a as ~s: ~d open matter~:p of ~d, recording ~a, ~d to ~d related matter~:p a message."
          (endpoint) (setting :actor)
          (count-if (lambda (entry) (member (entry-state entry) +open-states+ :test #'string=))
                    (matter-directory))
          (length (matter-directory)) (setting :capture) 0 (setting :related)))

(defun run-command (args session-id)
  "One /protocol-chaos invocation: no argument the server and its counts, a
text the matters whose titles share words with it."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (handler-case
        (if (plusp (length text))
            (let* ((entries (matter-directory))
                   (matches (rank text entries 0.5 10)))
              (if matches
                  (format nil "~d matter~:p for ~s:~%~{~a~}"
                          (length matches) text
                          (mapcar (lambda (entry)
                                    (format nil "- ~a · ~a [id:~a]"
                                            (clip (entry-title entry) 80) (entry-state entry)
                                            (entry-id entry)))
                                  matches))
                  (format nil "no open matter shares words with ~s" text)))
            (status-line))
      (error (condition) (nle:failure (format nil "ERROR: protocol-chaos: ~a" condition))))))

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell protocol-chaos
  (:section ("protocol-chaos")
    (:guide "url is your Protocol Chaos server's MCP endpoint (such as https://example.org:8443/mcp); token is the bearer token it issued for this client, or token_env names the variable holding it; next step: /protocol-chaos shows the server and its matters")
    ("url" :string :required t
     :doc "your Protocol Chaos server's MCP endpoint, such as https://example.org:8443/mcp")
    ("token" :secret
     :doc "the bearer token that server issued for this client")
    ("token_env" :env
     :doc "the environment variable holding the token, when it is not written here (PROTOCOL_CHAOS_TOKEN is read either way)")
    ("actor" :string :default "nodecode"
     :doc "the client name the token was issued for; writes are stamped with it")
    ("capture" :choice :options '("auto" "match" "off") :default "auto"
     :doc "auto: the auxiliary model decides after each turn what to record and where; match: the matter whose title shares the most words takes it; off: record nothing")
    ("overview" :boolean :default t
     :doc "add an overview of the open matters to a turn's first round")
    ("related" :integer :default 3 :min 0
     :doc "how many matters whose titles share words with the message one round may bring in")
    ("timeout_seconds" :integer :default 8 :min 1
     :doc "one read's ceiling; a write gets fifteen seconds")
    ("capture_max_chars" :integer :default 600 :min 50
     :doc "how much of one message a recording send carries"))
  (:help :protocol-chaos
         "protocol-chaos: the user's long-lived matters on their own server, in context before the model answers and recorded after - (help :protocol-chaos)"
         +primer+)
  (:command "protocol-chaos" 'run-command
            :description "The Protocol Chaos server and the user's matters, or those matching a text"
            :argument-hint "[TEXT]")
  (:tool "protocol_chaos_matters" *matters-schema* 'tool-matters)
  (:tool "protocol_chaos_matter" *matter-schema* 'tool-matter)
  (:tool "protocol_chaos_record" *record-schema* 'tool-record)
  (:hook :round (nle:observer #'observe-round "nodecode-protocol-chaos"))
  (:hook :frame (nlk:observer #'observe-frame "nodecode-protocol-chaos"))
  (:start (lambda ()
            (check-config)
            (setf *directory* nil
                  *turns* (make-hash-table :test #'equal)
                  *shown* (make-hash-table :test #'equal)
                  *overview-turn* nil
                  *queued* '())
            (setf *worker* (nlk:worker-start "protocol-chaos-capture" #'capture-step :wake 60))
            (nle:on-stop (lambda ()
                           (when *worker* (nlk:worker-stop *worker*))
                           (setf *worker* nil
                                 *directory* nil
                                 *queued* '()
                                 *turns* nil
                                 *shown* nil))))))
