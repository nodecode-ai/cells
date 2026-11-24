;;;; cell.lisp --- Myeline: memory with receipts, four tools and /myeline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Sandbox-Group-LLC/myeline-hermes (see NOTICE). The
;;;; original is two manifests and a skill: an MCP server a Hermes agent
;;;; connects to, where every memory is one fact with its source, an AI can
;;;; only propose, a human approves, and a retraction carries a reason into an
;;;; audit log. That ritual is what is kept here; the store is one JSON file
;;;; on this computer instead of a remote cortex, so the receipts are the
;;;; user's own and nothing leaves the machine.
;;;;
;;;; Four tools, named as the original names them -- cortex_query,
;;;; cortex_remember, cortex_daily_card, cortex_retract -- and one command,
;;;; /myeline, where the person approves, dismisses, retires and reads the
;;;; audit log. The model cannot approve: approval is a human action, exactly
;;;; as in the original.

(defpackage #:nodecode-myeline
  (:use #:cl)
  (:nicknames #:myeline)
  (:export #:myeline-error #:myeline-error-detail))

(in-package #:nodecode-myeline)

(nlk:define-peripheral myeline :not-running t :lock "myeline")

(defconstant +max-fact+ 500 "A fact's length, in characters: one plain sentence.")
(defconstant +max-source+ 200 "A source's length, in characters.")
(defconstant +max-card+ 30 "How many lines one daily card may carry.")
(defconstant +review-shown+ 20 "How many pending facts /myeline review lists.")
(defconstant +audit-shown+ 10 "How many audit entries /myeline audit lists.")
(defconstant +page-limit+ 50 "The most facts one cortex_query answers.")

(defparameter *format-version* 1 "The receipts file's own format, so a later one can tell.")

;;; --- the receipts file -----------------------------------------------------------

(defun data-folder ()
  "The folder the receipts file lives in, as the section names it."
  (uiop:ensure-directory-pathname (or (setting :data-dir) "")))

(defun receipts-file ()
  (merge-pathnames "receipts.json" (data-folder)))

(defun now-text ()
  "Now, as UTC ISO-8601 to the second: what a fact's timestamps carry."
  (nlk:iso-time (nlk:unix-now) :millis nil))

(defun fact-json (fact)
  (nlk:json-object
   "id" (getf fact :id)
   "fact" (getf fact :text)
   "source" (getf fact :source)
   "state" (getf fact :state)
   "proposedAt" (getf fact :proposed)
   "approvedAt" (or (getf fact :approved) :null)
   "retiredAt" (or (getf fact :retired) :null)
   "reason" (or (getf fact :reason) :null)))

(defun audit-json (entry)
  (nlk:json-object
   "at" (getf entry :at)
   "what" (getf entry :kind)
   "id" (or (getf entry :id) :null)
   "fact" (or (getf entry :text) :null)
   "source" (or (getf entry :source) :null)
   "reason" (or (getf entry :reason) :null)))

(defun state-json (state)
  (nlk:json-object
   "format" *format-version*
   "facts" (coerce (mapcar #'fact-json (getf state :facts)) 'vector)
   "audit" (coerce (mapcar #'audit-json (getf state :audit)) 'vector)))

(defun fact-of (object)
  "One fact read back from the receipts file."
  (list :id (or (nlk:json-value object :string "id") "")
        :text (or (nlk:json-value object :string "fact") "")
        :source (or (nlk:json-value object :string "source") "")
        :state (or (nlk:json-value object :string "state") "pending")
        :proposed (or (nlk:json-value object :string "proposedAt") (now-text))
        :approved (nlk:json-value object :string "approvedAt")
        :retired (nlk:json-value object :string "retiredAt")
        :reason (nlk:json-value object :string "reason")))

(defun audit-of (object)
  (list :at (or (nlk:json-value object :string "at") (now-text))
        :kind (or (nlk:json-value object :string "what") "")
        :id (nlk:json-value object :string "id")
        :text (nlk:json-value object :string "fact")
        :source (nlk:json-value object :string "source")
        :reason (nlk:json-value object :string "reason")))

(defun read-state ()
  "The receipts as (:facts (FACT ...) :audit (ENTRY ...)), or an empty one
the first time. A file that does not decode is the error the person fixes,
never a silently empty memory."
  (let ((file (receipts-file)))
    (if (probe-file file)
        (let ((object (nlk:decode-json (uiop:read-file-string file))))
          (list :facts (loop for fact across (nlk:json-array object "facts") collect (fact-of fact))
                :audit (loop for entry across (nlk:json-array object "audit") collect (audit-of entry))))
        (list :facts '() :audit '()))))

(defun write-state (state)
  "STATE to the receipts file atomically, under the folder's lock."
  (let ((file (receipts-file)))
    (ensure-directories-exist file)
    (nlk:with-folder-lock ((data-folder))
      (nlk:write-file-atomically file (format nil "~a~%" (nlk:encode-json-object (state-json state)))
                                  :mode :private))
    state))

(defmacro with-receipts ((state) &body body)
  "BODY with STATE read from the receipts file and written back when it answers."
  `(with-myeline-lock
     (let ((,state (read-state)))
       (multiple-value-prog1 (progn ,@body)
         (write-state ,state)))))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR: myeline:' line the
model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: myeline: ~a" condition)))))

;;; --- arguments --------------------------------------------------------------------

(defun argument (args name)
  (and (hash-table-p args) (gethash name args)))

(defun argument-text (args name)
  (let ((value (argument args name)))
    (and (stringp value) value)))

(defun required-text (args name maximum)
  "NAME's text, non-empty and no longer than MAXIMUM, or a refusal naming it."
  (let ((value (argument-text args name)))
    (unless (and value (plusp (length (nlk:trimmed value))))
      (fail "~a is required" name))
    (let ((text (string-trim '(#\Space #\Tab #\Newline) value)))
      (when (> (length text) maximum)
        (fail "~a is too long: at most ~d characters" name maximum))
      text)))

(defun bounded-limit (args name default maximum)
  (let ((value (argument args name)))
    (cond ((null value) default)
          ((and (integerp value) (<= 1 value maximum)) value)
          (t (fail "~a must be an integer between 1 and ~d" name maximum)))))

;;; --- what a fact may say -----------------------------------------------------------

(defparameter *secret-names*
  '("password" "passphrase" "api key" "api-key" "apikey" "access token" "secret key"
    "secret" "token" "private key" "seed phrase" "recovery phrase" "card number" "cvv"
    "pin code" "iban" "account number")
  "The things Myeline refuses to remember, as the original refuses them.")

(defun secret-name-present-p (text name)
  "T when NAME occurs in TEXT with a value right after it: after a colon or
an equals sign, or after the word is/was."
  (let ((start 0))
    (loop for at = (search name text :start2 start)
          while at
          do (let* ((after (+ at (length name)))
                    (tail (string-left-trim '(#\Space #\Tab) (subseq text after (min (length text) (+ after 12))))))
               (when (or (and (plusp (length tail)) (find (char tail 0) ":="))
                         (and (>= (length tail) 4)
                              (member (subseq (string-downcase tail) 0 3) '("is " "was") :test #'string=)))
                 (return-from secret-name-present-p t)))
             (setf start (1+ at))
          finally (return nil))))

(defun long-token-p (text)
  "T when TEXT carries a run of 16 or more letters, digits, dashes and
underscores that mixes letters with digits without a space: a key or a hash."
  (loop with length = (length text) with index = 0
        while (< index length)
        do (if (or (alphanumericp (char text index)) (member (char text index) '(#\- #\_)))
               (let* ((end (or (position-if-not (lambda (character)
                                                  (or (alphanumericp character)
                                                      (member character '(#\- #\_))))
                                              text :start index)
                              length))
                     (run (subseq text index end)))
                 (when (and (>= (length run) 16)
                            (some #'alpha-char-p run)
                            (some #'digit-char-p run))
                   (return-from long-token-p t))
                 (setf index end))
               (incf index))
        finally (return nil)))

(defun card-number-p (text)
  "T when TEXT carries a run of 13 to 19 digits, spaces and dashes aside: a
card number."
  (loop with length = (length text) with index = 0
        while (< index length)
        do (if (digit-char-p (char text index))
               (let* ((end (or (position-if-not (lambda (character)
                                                  (or (digit-char-p character)
                                                      (member character '(#\Space #\-))))
                                              text :start index)
                              length))
                     (digits (remove-if-not #'digit-char-p (subseq text index end))))
                 (when (<= 13 (length digits) 19)
                   (return-from card-number-p t))
                 (setf index end))
               (incf index))
        finally (return nil)))

(defun secret-like-p (text)
  "T when TEXT looks like a credential or a card number, which Myeline refuses
at every setting."
  (let ((lower (string-downcase text)))
    (or (some (lambda (name) (secret-name-present-p lower name)) *secret-names*)
        (long-token-p text)
        (card-number-p text))))

(defun checked-fact (text)
  "TEXT refused when it looks like a secret, as the original refuses
passwords, API keys, card and bank numbers."
  (let ((trimmed (nlk:trimmed text)))
    (when (secret-like-p trimmed)
      (fail "~s looks like a password, a key or a card number, and Myeline refuses those; say where it is kept instead" trimmed))
    trimmed))

;;; --- the facts ---------------------------------------------------------------------

(defun normalize (text)
  "TEXT as identity is compared: lower case, runs of space read as one."
  (string-downcase
   (with-output-to-string (out)
     (let ((space nil))
       (loop for character across (string-trim '(#\Space #\Tab #\Newline) text)
             do (if (member character '(#\Space #\Tab #\Newline))
                    (setf space t)
                    (progn (when space (write-char #\Space out)) (setf space nil)
                           (write-char character out))))))))

(defun next-id (facts)
  "f-N, the first N no fact has."
  (loop for number from 1
        for id = (format nil "f-~d" number)
        unless (find id facts :key (lambda (fact) (getf fact :id)) :test #'string=)
          return id))

(defun find-fact (state id)
  (or (find id (getf state :facts) :test #'string= :key (lambda (fact) (getf fact :id)))
      (fail "no fact with id ~s; /myeline review and /myeline live name the ids" id)))

(defun live-facts (state)
  (remove-if-not (lambda (fact) (equal (getf fact :state) "live")) (getf state :facts)))

(defun pending-facts (state)
  (remove-if-not (lambda (fact) (equal (getf fact :state) "pending")) (getf state :facts)))

(defun newest-first (facts)
  "FACTS, the one approved (or proposed) most recently first."
  (stable-sort (copy-list facts) #'string>
               :key (lambda (fact) (or (getf fact :approved) (getf fact :proposed) ""))))

(defun search-facts (facts query)
  "FACTS whose text or source carries every word of QUERY, newest first."
  (let ((words (mapcar #'string-downcase (nlk:split-words (or query "")))))
    (newest-first
     (remove-if-not (lambda (fact)
                      (let ((text (string-downcase (getf fact :text)))
                            (source (string-downcase (getf fact :source))))
                        (every (lambda (word) (or (search word text) (search word source))) words)))
                    facts))))

(defun take (list limit)
  (loop for item in list for taken from 0 below limit collect item))

(defun audit (state kind fact &key reason)
  "STATE with one audit entry for KIND on FACT appended."
  (setf (getf state :audit)
        (append (getf state :audit)
                (list (list :at (now-text) :kind kind
                            :id (and fact (getf fact :id))
                            :text (and fact (getf fact :text))
                            :source (and fact (getf fact :source))
                            :reason reason))))
  state)

(defun stage-fact (state text source)
  "STATE with one pending fact TEXT from SOURCE: proposed, never live."
  (let ((fact (list :id (next-id (getf state :facts))
                     :text text :source source :state "pending"
                     :proposed (now-text))))
    (setf (getf state :facts) (append (getf state :facts) (list fact)))
    (audit state "proposed" fact)
    fact))

(defun existing-fact (state text)
  "The fact whose text is TEXT, whatever its state, or NIL: a fact is offered
once, never twice."
  (find (normalize text) (getf state :facts)
        :test #'string= :key (lambda (fact) (normalize (getf fact :text)))))

(defun live-ceiling (state)
  (let ((live (length (live-facts state))))
    (when (>= live (setting :max-live))
      (fail "the memory holds ~d live facts, its ceiling; retire one with cortex_retract or /myeline forget ID REASON" live))))

;;; --- the tools -----------------------------------------------------------------------

(defun tool-query (args)
  "cortex_query: answer from approved facts only, each with the source it came from."
  (answering
   (let* ((state (read-state))
          (limit (bounded-limit args "limit" 10 +page-limit+))
          (query (or (argument-text args "query") ""))
          (found (search-facts (live-facts state) query))
          (page (take found limit)))
     (nlk:encode-json-object
      (nlk:json-object
       "facts" (coerce (mapcar #'fact-json page) 'vector)
       "total" (length found)
       "pending" (length (pending-facts state))
       "note" (if (null found)
                  "No approved fact matches; say so rather than guessing."
                  "Approved facts only, each with its source."))))))

(defun tool-remember (args)
  "cortex_remember: stage one atomic fact with its source; it goes live only
when the person approves it."
  (answering
   (with-receipts (state)
     (let* ((text (checked-fact (required-text args "fact" +max-fact+)))
            (source (required-text args "source" +max-source+))
            (known (existing-fact state text)))
       (when known
         (return-from tool-remember
           (nlk:encode-json-object
            (nlk:json-object "id" (getf known :id) "state" (getf known :state)
                              "fact" (getf known :text) "duplicate" t
                              "note" "This fact is already here; it was not offered twice."))))
       (live-ceiling state)
       (let ((fact (stage-fact state text source)))
         (nlk:encode-json-object
          (nlk:json-object "id" (getf fact :id) "state" "pending" "duplicate" :false
                            "fact" (getf fact :text) "source" (getf fact :source)
                            "note" "Staged. The user approves it with /myeline approve ID; nothing reaches memory before that.")))))))

(defun card-line (args name prefix)
  (let ((value (argument args name)))
    (when value
      (unless (and (vectorp value) (not (stringp value)))
        (fail "~a must be an array of strings" name))
      (loop for line across value
            collect (format nil "~a~a" prefix (checked-fact (bounded-line line name)))))))

(defun bounded-line (line name)
  (unless (stringp line)
    (fail "every line of ~a must be a string" name))
  (let ((text (string-trim '(#\Space #\Tab #\Newline) line)))
    (unless (plusp (length text))
      (fail "a line of ~a is empty" name))
    (when (> (length text) +max-fact+)
      (fail "a line of ~a is longer than ~d characters" name +max-fact+))
    text))

(defun tool-daily-card (args)
  "cortex_daily_card: stage the day's card -- facts, decisions, open loops --
one line per fact, all pending."
  (answering
   (with-receipts (state)
     (let* ((source (or (argument-text args "source") "daily card"))
            (lines (append (card-line args "facts" "")
                           (card-line args "decisions" "Decision: ")
                           (card-line args "open_loops" "Open loop: "))))
       (when (null lines)
         (fail "a card carries at least one of facts, decisions or open_loops"))
       (when (> (length lines) +max-card+)
         (fail "a card carries at most ~d lines" +max-card+))
       (live-ceiling state)
       (let ((new (loop for line in lines
                        for known = (existing-fact state line)
                        collect (or known (stage-fact state line source)))))
         (nlk:encode-json-object
          (nlk:json-object
           "ids" (coerce (mapcar (lambda (fact) (getf fact :id)) new) 'vector)
           "staged" (length new)
           "state" "pending"
           "note" "Staged as one fact per line. The user swipes them through in /myeline review.")))))))

(defun tool-retract (args)
  "cortex_retract: retire one live fact for good, with the written reason that
stays in the audit log."
  (answering
   (when (null (setting :model-may-retract))
     (fail "the model may not retract here: the user retires facts with /myeline forget ID REASON"))
   (with-receipts (state)
     (let* ((fact (find-fact state (required-text args "id" 100)))
            (reason (required-text args "reason" +max-source+)))
       (unless (equal (getf fact :state) "live")
         (fail "fact ~s is ~a, not live; only a live fact is retracted, and a pending one is dismissed in /myeline review"
               (getf fact :id) (getf fact :state)))
       (setf (getf fact :state) "retired"
             (getf fact :retired) (now-text)
             (getf fact :reason) reason)
       (audit state "retracted" fact :reason reason)
       (nlk:encode-json-object
        (nlk:json-object "id" (getf fact :id) "fact" (getf fact :text)
                          "state" "retired" "reason" reason "retiredAt" (getf fact :retired)
                          "note" "Gone from recall for good; the reason stays in the audit log."))))))

;;; --- /myeline: the person's side -------------------------------------------------------

(defun status-line (state)
  (let ((live (live-facts state)) (pending (pending-facts state))
        (retired (count-if (lambda (fact) (equal (getf fact :state) "retired")) (getf state :facts))))
    (format nil "Myeline: ~d live fact~:p, ~d waiting for you, ~d retired.~%~
                 /myeline review shows what waits; /myeline approve ID makes one live; /myeline forget ID REASON retires one; /myeline audit reads the log.~%~
                 ~@[Next in the queue: ~a~]"
            (length live) (length pending) retired
            (let ((first (first (newest-first pending))))
              (and first (format nil "~s from ~a (id ~a)"
                                 (getf first :text) (getf first :source) (getf first :id)))))))

(defun review-text (state)
  (let ((pending (newest-first (pending-facts state))))
    (if (null pending)
        (format nil "Nothing waits for review; ~d live fact~:p." (length (live-facts state)))
        (with-output-to-string (out)
          (format out "~d fact~:p waiting for you, oldest offer first from the top as they were made:~%"
                  (length pending))
          (loop for fact in (take pending +review-shown+)
                do (format out "~a  ~s~%    from ~a, offered ~a~%"
                           (getf fact :id) (getf fact :text) (getf fact :source) (getf fact :proposed)))
          (format out "Approve one with /myeline approve ID; dismiss it with /myeline reject ID.~%")))))

(defun live-text (state)
  (let ((live (newest-first (live-facts state))))
    (if (null live)
        "No live facts yet."
        (with-output-to-string (out)
          (format out "~d live fact~:p:~%" (length live))
          (loop for fact in (take live +page-limit+)
                do (format out "~a  ~s~%    from ~a, approved ~a~%"
                           (getf fact :id) (getf fact :text) (getf fact :source) (getf fact :approved)))))))

(defun audit-text (state)
  (let ((entries (reverse (getf state :audit))))
    (if (null entries)
        "The audit log is empty."
        (with-output-to-string (out)
          (format out "The last ~d of ~d log entries:~%" (min +audit-shown+ (length entries)) (length entries))
          (loop for entry in (take entries +audit-shown+)
                do (format out "~a  ~a ~@[~a ~]~@[from ~a ~]~@[~s~]~@[ -- ~a~]~%"
                           (getf entry :at) (getf entry :kind)
                           (getf entry :id) (getf entry :source) (getf entry :text)
                           (getf entry :reason)))))))

(defun approve-ids (state ids)
  (if (null ids)
      (format nil "Name the fact to approve: /myeline approve ID (~d waiting)." (length (pending-facts state)))
      (with-output-to-string (out)
        (loop for id in ids
              do (let ((fact (find-fact state id)))
                   (if (equal (getf fact :state) "live")
                       (format out "~a is already live.~%" id)
                       (if (equal (getf fact :state) "pending")
                           (progn (setf (getf fact :state) "live"
                                        (getf fact :approved) (now-text))
                                  (audit state "approved" fact)
                                  (format out "~a is live: ~s~%" id (getf fact :text)))
                           (format out "~a is retired, not pending; it stays retired.~%" id))))))))

(defun reject-ids (state ids)
  (if (null ids)
      "Name the fact to dismiss: /myeline reject ID."
      (with-output-to-string (out)
        (loop for id in ids
              do (let ((fact (find-fact state id)))
                   (if (equal (getf fact :state) "pending")
                       (progn (setf (getf fact :state) "retired"
                                    (getf fact :retired) (now-text)
                                    (getf fact :reason) "dismissed at review")
                              (audit state "dismissed" fact :reason "dismissed at review")
                              (format out "~a is dismissed.~%" id))
                       (format out "~a is ~a, not waiting.~%" id (getf fact :state))))))))

(defun forget (state id reason)
  (if (null id)
      "Name the fact to retire: /myeline forget ID REASON."
      (let ((fact (find-fact state id)))
        (unless (equal (getf fact :state) "live")
          (return-from forget (format nil "~a is ~a, not live." id (getf fact :state))))
        (setf (getf fact :state) "retired"
              (getf fact :retired) (now-text)
              (getf fact :reason) reason)
        (audit state "retracted" fact :reason reason)
        (format nil "~a is retired for good, reason kept: ~s" id reason))))

(defun search-text (state query)
  (let ((found (search-facts (live-facts state) query)))
    (if (null found)
        (format nil "No live fact matches ~s." query)
        (with-output-to-string (out)
          (format out "~d live fact~:p for ~s:~%" (length found) query)
          (loop for fact in (take found +page-limit+)
                do (format out "~a  ~s~%    from ~a~%" (getf fact :id) (getf fact :text) (getf fact :source)))))))

(defun run-command (args session-id)
  "One /myeline invocation: no argument the state of the memory, `review` the
queue, `approve ID...` / `reject ID...` / `forget ID REASON` the person's acts,
`live` the facts in force, `audit` the log, anything else a search."
  (declare (ignore session-id))
  (running-settings)
  (let* ((typed (nlk:trimmed (or args "")))
         (words (nlk:split-words typed))
         (head (string-downcase (or (first words) "")))
         (rest-words (rest words)))
    (cond
      ((zerop (length typed)) (status-line (read-state)))
      ((string= head "review") (review-text (read-state)))
      ((string= head "live") (live-text (read-state)))
      ((string= head "audit") (audit-text (read-state)))
      ((string= head "approve")
       (with-receipts (state) (approve-ids state (mapcar #'string-downcase rest-words))))
      ((string= head "reject")
       (with-receipts (state) (reject-ids state (mapcar #'string-downcase rest-words))))
      ((string= head "forget")
       (with-receipts (state)
         (let ((reason (string-trim '(#\Space #\Tab)
                                    (format nil "~{~a~^ ~}" (rest rest-words)))))
           (forget state (let ((id (first rest-words))) (and id (string-downcase id)))
                   (if (plusp (length reason)) reason "retired by the user")))))
      (t (search-text (read-state) typed)))))

;;; --- the schemas -------------------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool
schema, the shape the model reads."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "additionalProperties" :false
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *query-schema*
  (tool-schema
   "Search the memory the user approved. Answers facts, each with the source it came from; a fact waiting in the review queue is not answered, so say nothing approved matches rather than guessing. Query before answering anything about the user's own history, preferences, decisions or projects."
   '(("query" "string" "Words to match in a fact or its source; empty answers the newest facts.")
     ("limit" "integer" "How many facts to answer, 1-50 (default 10).")))
  "The cortex_query tool.")

(defparameter *remember-schema*
  (tool-schema
   "Offer one atomic fact to the memory, with the source it came from. It is staged, not live: the user approves it in /myeline review. Offer only when the user says yes, once per fact. One plain sentence that stands alone tomorrow. Passwords, keys, card and bank numbers are refused."
   '(("fact" "string" "One sentence, at most 500 characters, that stands alone tomorrow." t)
     ("source" "string" "Where it came from: the conversation, a document, a page -- the receipt the user reads." t)))
  "The cortex_remember tool.")

(defparameter *daily-card-schema*
  (tool-schema
   "Stage the day's card at sign-off: what was learned, decided, and left open, one fact per line, all waiting for the user's review. Each line carries the same source."
   '(("facts" "array" "What was learned today, one sentence per line.")
     ("decisions" "array" "What was decided today, one sentence per line.")
     ("open_loops" "array" "What is still open, one sentence per line.")
     ("source" "string" "Where the card came from; \"daily card\" when absent.")))
  "The cortex_daily_card tool.")

(defparameter *retract-schema*
  (tool-schema
   "Retire one live fact for good. The reason is required and stays in the audit log; only when the user asks, and never for a fact merely staging."
   '(("id" "string" "The fact's id, from cortex_query." t)
     ("reason" "string" "Why it is no longer true, in the user's words: it stays in the audit log." t)))
  "The cortex_retract tool.")

(defparameter +primer+
  "Myeline is memory with receipts: every fact carries the source it came from, an AI can only offer a
fact, and the user approves it before it is remembered. (help :myeline) is this page; the tools are
the four below, and /myeline is the person's side.
  cortex_query       {query?, limit?}          approved facts only, each with its source
  cortex_remember    {fact, source}            stage one atomic sentence, with its receipt
  cortex_daily_card  {facts?, decisions?, open_loops?, source?}   the day's card, each line staged
  cortex_retract     {id, reason}              retire a live fact for good, reason kept
Offer to remember only when something durable comes up, ask once, and save only on the user's yes.
Search approved memory before answering questions about the user's own or their team's history,
preferences, decisions or projects; approved memory outranks recollection, and when nothing approved
matches, say so rather than guessing. Every save waits in the review queue -- /myeline review lists it,
/myeline approve ID makes one live, /myeline forget ID REASON retires one -- and nothing goes live
without the user's swipe. Retiring takes a reason, and the reason stays in the audit log
(/myeline audit). Myeline never reads conversations or files, and refuses passwords, API keys, card and
bank numbers. The myeline section of config.jsonc names data_dir, the folder the receipts file is kept
in."
  "What (help :myeline) answers while the cell runs.")

;;; --- the cell ------------------------------------------------------------------------------

(nle:define-cell myeline
  (:section ("myeline")
    (:guide "data_dir is the folder the receipts are kept in; next step: /myeline review shows what waits")
    ("data_dir" :path :required t
     :doc "the folder the receipts file is kept in, one memory per folder")
    ("max_live" :integer :default 2000 :min 1
     :doc "how many live facts the memory holds before it asks for a retraction")
    ("model_may_retract" :boolean :default t
     :doc "let cortex_retract retire a live fact; when off, only /myeline forget does"))
  (:help :myeline "myeline: memory with receipts - cortex_query, cortex_remember, cortex_daily_card, cortex_retract" +primer+)
  (:command "myeline" 'run-command
            :description "The memory's state, the review queue, approve, forget, audit, or a search"
            :argument-hint "review · approve ID · forget ID REASON · audit")
  (:tool "cortex_query" *query-schema* 'tool-query)
  (:tool "cortex_remember" *remember-schema* 'tool-remember)
  (:tool "cortex_daily_card" *daily-card-schema* 'tool-daily-card)
  (:tool "cortex_retract" *retract-schema* 'tool-retract)
  (:start (lambda ()
            ;; A memory needs a folder before it can have a file: a section
            ;; that names none refuses at start, naming the member.
            (unless (plusp (length (or (setting :data-dir) "")))
              (error 'nlk:config-refusal
                     :detail "myeline: data_dir names the folder the receipts are kept in; set it in the myeline section of config.jsonc or in /settings"))
            (ensure-directories-exist (receipts-file)))))
