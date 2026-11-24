;;;; cell.lisp --- Proof Run: capture a demonstrated run, then gate its reuse.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of raffieeey/hermes-proof-run for Nodecode. The original was a
;;;; Hermes plugin with a pre_tool_call gate, a post_tool_call observer and a
;;;; /proof command. Here one :TOOL point hook is both: it wraps the dispatch,
;;;; so it sees a call before the body runs and the answer after it.
;;;;
;;;; The loop is the original's, unchanged in meaning:
;;;;
;;;;   /proof-run start NAME [intent]   record the run that follows
;;;;   /proof-run stop                   the recording becomes a live contract
;;;;   /proof-run rebind NAME            re-demonstrate a changed run
;;;;   /proof-run learn NAME [N]         a contract from the last N calls
;;;;   /proof-run status | unbind NAME | cancel
;;;;
;;;; A contract holds the mutating calls of the demonstration -- the exact
;;;; arguments each was made with, and the fields each answer carried -- and
;;;; the reads made before each of them. After stop, a covered call runs only
;;;; when its arguments are one demonstrated call's and every read that
;;;; call's precondition names was made since, within max_age, with the same
;;;; answer. Reads are consumed by the next mutation. An answer that does not
;;;; match the demonstration marks the contract unproven, and its covered
;;;; calls are refused until it is rebound.
;;;;
;;;; Nothing here touches the network: contracts are one JSON file each under
;;;; the Nodecode home.

(defpackage #:nodecode-proof-run
  (:use #:cl)
  (:export #:start-cell))

(in-package #:nodecode-proof-run)

(nlk:define-peripheral proof-run :not-running t)

;;; --- the canonical form of a value ---------------------------------------
;;; Exact arguments, not ranges: two calls are the same call when their
;;; canonical forms are the same string, so the comparison, the file and
;;; the displayed message all read one thing.

(defun quote-json-string (text)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across text
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (if (< (char-code char) 32)
                      (format out "\\u~4,'0x" (char-code char))
                      (write-char char out)))))
    (write-char #\" out)))

(defun write-canonical (value out)
  (cond ((hash-table-p value)
         (write-char #\{ out)
         (let ((keys (sort (loop for key being the hash-keys of value
                                  collect (if (stringp key) key (format nil "~a" key)))
                           #'string<))
               (first t))
           (dolist (key keys)
             (unless first (write-char #\, out))
             (setf first nil)
             (write-string (quote-json-string key) out)
             (write-char #\: out)
             (write-canonical (gethash key value) out))
           (write-char #\} out)))
        ((stringp value) (write-string (quote-json-string value) out))
        ((vectorp value)
         (write-char #\[ out)
         (loop for index from 0 below (length value)
               do (when (plusp index) (write-char #\, out))
                  (write-canonical (aref value index) out))
         (write-char #\] out))
        ((eq value :null) (write-string "null" out))
        ((null value) (write-string "null" out))
        ((eq value t) (write-string "true" out))
        ((integerp value) (format out "~d" value))
        ((floatp value) (format out "~f" value))
        ((symbolp value) (write-canonical (string-downcase (symbol-name value)) out))
        (t (write-canonical (format nil "~a" value) out))))

(defun canonical (value)
  "VALUE's canonical JSON text: what two arguments are compared by."
  (with-output-to-string (out) (write-canonical value out)))

(defun display (value)
  "VALUE, short and on one line, for a message a model reads back."
  (let ((text (canonical value)))
    (substitute #\Space #\Newline
                (if (> (length text) 160) (concatenate 'string (subseq text 0 160) "...") text))))

;;; --- what the hook is running on ------------------------------------------

(defvar *contracts* (make-hash-table :test #'equal)
  "Contract id -> its contract, a decoded JSON object. Loaded at start.")

(defvar *facts* (make-hash-table :test #'equal)
  "Session id -> a hash of fact key -> (FIELDS . TICKS): the reads of this
session, in memory only, consumed by the next mutation.")

(defvar *recorders* (make-hash-table :test #'equal)
  "Session id -> the recording in progress there.")

(defvar *buffers* (make-hash-table :test #'equal)
  "Session id -> its last calls, newest first: what /proof-run learn reads.")

(defvar *session-override* nil
  "The session a call is filed under when this thread runs for no turn, as a
test does; NIL in a turn, where (NLE:TURN) answers the session.")

(defparameter +buffer-max+ 100 "How many calls of one session are kept to learn from.")
(defparameter +max-steps+ 500 "How many steps one contract may hold.")

(defun session-id ()
  (or *session-override* (getf (nle:turn) :session-id) "default"))

(defun ticks () (get-internal-real-time))

(defun seconds-since (then)
  (/ (- (ticks) then) (float internal-time-units-per-second)))

(defun tool-call-failed-p ()
  "Whether the dispatch this hook wraps recorded the call failed, read off
the engine's own flag rather than parsed back out of the result text."
  (let ((symbol (find-symbol "*TOOL-CALL-FAILED*" '#:nodecode.evolved)))
    (and symbol (boundp symbol) (symbol-value symbol) t)))

(defun mode ()
  "The mode this cell runs in; `off' whenever it is not running."
  (let ((value (handler-case (setting :mode) (error () nil))))
    (if (equal value "off") "off" "enforce")))

;;; --- calls, reads and answers ---------------------------------------------

(defun arguments (op)
  (let ((value (getf op :arguments)))
    (if (hash-table-p value) value (nlk:json-object))))

(defun form-text (args)
  (let ((form (and (hash-table-p args) (gethash "form" args))))
    (and (stringp form) form)))

(defun form-head (text)
  "The first symbol's name of TEXT's first form, downcased, or NIL: how an
eval call is told for a read of the image from a change to it."
  (when (and (stringp text) (plusp (length text)))
    (handler-case
        (let ((*package* (or (find-package '#:nodecode.evolved) *package*))
              (*read-eval* nil))
          (let ((form (read-from-string text nil nil)))
            (when (and (consp form) (symbolp (first form)))
              (string-downcase (symbol-name (first form))))))
      (error () nil)
      (storage-condition () nil))))

(defun read-call-p (tool args)
  "Whether this call observed the world rather than changed it: a read tool
by name, or an eval whose first verb the section counts as a read."
  (or (member tool (setting :reads) :test #'equal)
      (and (equal tool "eval")
           (let ((head (form-head (form-text args))))
             (and head (member head (setting :read-forms) :test #'equal))))))

(defun result-fields (answer)
  "ANSWER as the object a check reads: the JSON object it encodes, or its
text under one key."
  (let ((parsed (and (stringp answer)
                      (plusp (length answer))
                      (member (char answer 0) '(#\{ #\[))
                      (handler-case (nlk:decode-json answer :whole t) (error () nil)))))
    (if (hash-table-p parsed)
        parsed
        (nlk:json-object "text" (if (stringp answer) answer (format nil "~a" answer))))))

(defun fields (object)
  "OBJECT's members as (FIELD . VALUE), in key order."
  (let (pairs)
    (maphash (lambda (key value) (push (cons key value) pairs)) object)
    (sort pairs #'string< :key #'car)))

(defun fact-key (tool canon)
  "The key one read fact is kept under: its tool and its exact arguments."
  (format nil "~a~c~a" tool #\Nul canon))

(defun session-facts (session)
  (or (gethash session *facts*) (setf (gethash session *facts*) (make-hash-table :test #'equal))))

(defun observe-fact (session tool canon object)
  (setf (gethash (fact-key tool canon) (session-facts session)) (cons object (ticks))))

(defun fact-of (session pred)
  "The FIELDS the read PRED names was last observed with in SESSION, or NIL
when it was never read, or is older than max_age."
  (let* ((key (fact-key (gethash "tool" pred) (gethash "args" pred)))
         (entry (gethash key (session-facts session))))
    (when entry
      (destructuring-bind (object . observed) entry
        (when (<= (seconds-since observed) (max 1 (setting :max-age)))
          object)))))

;;; --- contracts on disk -----------------------------------------------------

(defun contracts-directory ()
  (let ((path (setting :directory)))
    (uiop:ensure-directory-pathname
     (if (stringp path) (merge-pathnames path (nlk:home)) (nlk:home "proof-run/")))))

(defun contract-file (id)
  (merge-pathnames (format nil "~a.json" id) (contracts-directory)))

(defun valid-name-p (name)
  (and (stringp name)
       (plusp (length name))
       (<= (length name) 64)
       (alphanumericp (char name 0))
       (every (lambda (char) (or (alphanumericp char) (member char '(#\- #\_)))) name)))

(defun check-name (name)
  (if (valid-name-p name)
      name
      (fail "invalid contract name ~s: letters, digits, hyphens and underscores, at most 64" name)))

(defun save-contract (contract)
  (let ((id (gethash "contract_id" contract)))
    (nlk:write-file-atomically (contract-file id)
                               (format nil "~a~%" (nlk:encode-json-object contract)))
    (setf (gethash id *contracts*) contract)
    contract))

(defun load-contracts ()
  (clrhash *contracts*)
  (let ((directory (contracts-directory)))
    (when (uiop:directory-exists-p directory)
      (dolist (path (sort (directory (merge-pathnames "*.json" directory)) #'string< :key #'namestring))
        (let ((object (handler-case (with-open-file (in path :external-format :utf-8)
                                      (nlk:decode-json in :whole t))
                        (error () nil))))
          (when (and (hash-table-p object)
                     (stringp (gethash "contract_id" object))
                     (vectorp (gethash "steps" object)))
            (setf (gethash (gethash "contract_id" object) *contracts*) object)))))))

(defun contract-ids ()
  (sort (loop for id being the hash-keys of *contracts* collect id) #'string<))

;;; --- the recording ---------------------------------------------------------

(defun check-arguments (value)
  "A demonstration whose call carried a credential is refused: exact
arguments are written to disk, so a secret would be too."
  (let ((text (canonical value)))
    (when (or (search "sk-" text) (search "ghp_" text) (search "Bearer " text)
              (search "api_key" text) (search "api-key" text) (search "token" text)
              (search "password" text) (search "secret" text))
      (fail "the recording carried a credential-shaped argument; record a run without one"))))

(defun fact-pred (tool canon field value)
  (nlk:json-object "tool" tool "args" canon "field" field "value" value))

(defun evidence-pred (field value)
  (nlk:json-object "field" field "value" value))

(defun begin-recording (id intent rebind)
  (let ((prior (gethash id *contracts*)))
    (when (and prior (not rebind))
      (fail "contract ~a already exists: /proof-run rebind ~a, or another name" id id))
    (when (and (null prior) rebind)
      (fail "no contract named ~a: /proof-run start ~a" id id))
    (let ((recorder (nlk:json-object "name" id "intent" intent
                                      "reads" '() "steps" '() "count" 0 "rejected" nil)))
      (when prior (setf (gethash "prior" recorder) prior))
      recorder)))

(defun record-call (recorder tool args canon answer failed)
  "One call into the recorder, as the original's post_tool_call did: a read
becomes a precondition source, a mutation a step."
  (setf (gethash "count" recorder) (1+ (gethash "count" recorder)))
  (handler-case (check-arguments args) (error () (setf (gethash "rejected" recorder) t)))
  (let ((object (and (not failed) (result-fields answer))))
    (cond
      (failed (setf (gethash "rejected" recorder) t))
      ((read-call-p tool args)
       (dolist (pair (fields object))
         (push (fact-pred tool canon (car pair) (cdr pair)) (gethash "reads" recorder))))
      ((null (fields object)) (setf (gethash "rejected" recorder) t))
      (t
       (let ((step (nlk:json-object "tool" tool "args" canon)))
         (setf (gethash "preconditions" step)
               (coerce (reverse (gethash "reads" recorder)) 'vector))
         (setf (gethash "success_evidence" step)
               (coerce (loop for (field . value) in (fields object)
                             collect (evidence-pred field value))
                       'vector))
         (push step (gethash "steps" recorder))
         (when (> (length (gethash "steps" recorder)) +max-steps+)
           (fail "the recording is longer than ~d mutating calls" +max-steps+))
         (setf (gethash "reads" recorder) '()))))))

(defun finish-recording (session)
  (let ((recorder (gethash session *recorders*)))
    (when (null recorder) (fail "no active recording in this session"))
    (remhash session *recorders*)
    (when (gethash "rejected" recorder)
      (fail "the recording held a failed or unusable call: /proof-run cancel, then record again"))
    (let ((steps (reverse (gethash "steps" recorder))))
      (when (null steps) (fail "the recording has no successful mutating call to guard"))
      (let* ((id (gethash "name" recorder))
             (prior (gethash "prior" recorder))
             (contract (nlk:json-object
                        "contract_id" id
                        "version" (if prior (1+ (gethash "version" prior)) 1)
                        "bound" t
                        "evidence_status" "proven"
                        "intent" (gethash "intent" recorder)
                        "recorded_calls" (gethash "count" recorder)
                        "guarded" (coerce (sort (remove-duplicates
                                                  (mapcar (lambda (step) (gethash "tool" step)) steps)
                                                  :test #'equal)
                 #'string<)
                 'vector)
                        "steps" (coerce steps 'vector))))
        (save-contract contract)))))

;;; --- the gate --------------------------------------------------------------

(defun covered-p (contract tool)
  (member tool (coerce (gethash "guarded" contract) 'list) :test #'equal))

(defun covered-step (contract tool canon)
  (loop for step across (gethash "steps" contract)
        when (and (equal (gethash "tool" step) tool)
                  (equal (gethash "args" step) canon))
          return step))

(defun precondition-failure (id step session)
  "Why STEP's preconditions do not hold in SESSION now, or NIL."
  (loop for pred across (gethash "preconditions" step)
        do (let ((what (format nil "contract ~a: precondition ~a.~a"
                               id (gethash "tool" pred) (gethash "field" pred)))
                 (object (fact-of session pred)))
             (cond
               ((null object)
                (return-from precondition-failure
                  (format nil "~a is unknown -- read it again in this session" what)))
               ((not (nth-value 1 (gethash (gethash "field" pred) object)))
                (return-from precondition-failure (format nil "~a is missing from the fresh read" what)))
               ((not (equal (canonical (gethash (gethash "field" pred) object))
                            (canonical (gethash "value" pred))))
                (return-from precondition-failure
                  (format nil "~a is ~a, demonstrated ~a"
                          what
                          (display (gethash (gethash "field" pred) object))
                          (display (gethash "value" pred)))))))))

(defun gate (tool args session recording)
  "The pre-dispatch verdict on one call:
  (values :block REASON)  -- it does not match, or its evidence is unproven
  (values :allow CONTRACT STEP) -- one demonstrated step, all its reads fresh
  (values nil)             -- no contract covers this tool"
  (let ((canon (canonical args)))
    (dolist (id (contract-ids))
      (let ((contract (gethash id *contracts*)))
        (when (and (eq (gethash "bound" contract) t)
                   (covered-p contract tool)
                   (not (and recording (equal id (gethash "name" recording)))))
          (when (equal (gethash "evidence_status" contract) "unproven")
            (return-from gate
              (values :block (format nil "contract ~a is unproven: /proof-run rebind ~a, or unbind it" id id))))
          (let ((step (covered-step contract tool canon)))
            (if (null step)
                (return-from gate
                  (values :block (format nil "contract ~a: arguments do not match a demonstrated ~a call" id tool)))
                (let ((reason (precondition-failure id step session)))
                  (if reason
                      (return-from gate (values :block reason))
                      (return-from gate (values :allow contract step)))))))))
    (values nil)))

(defun evidence-holds-p (step answer failed)
  (and (not failed)
       (or (not (setting :check-evidence))
           (let ((object (result-fields answer)))
             (loop for pred across (gethash "success_evidence" step)
                   always (multiple-value-bind (value present)
                              (gethash (gethash "field" pred) object)
                            (and present
                                 (equal (canonical value) (canonical (gethash "value" pred))))))))))

(defun mark-unproven (contract)
  (when (eq (gethash "bound" contract) t)
    (setf (gethash "evidence_status" contract) "unproven")
    (handler-case (save-contract contract) (error () nil))
    (nle:notice (format nil "proof-run: contract ~a answered differently from its demonstration and is unproven; /proof-run rebind ~a"
                        (gethash "contract_id" contract) (gethash "contract_id" contract))
                :key "nodecode-proof-run" :level :warning)))

;;; --- one call, wrapped -----------------------------------------------------

(defun remember-call (session tool args canon answer)
  (let ((buffer (cons (nlk:json-object "tool" tool "args" canon
                                       "arguments" args "answer" answer)
                      (gethash session *buffers*))))
    (setf (gethash session *buffers*)
          (if (> (length buffer) +buffer-max+) (subseq buffer 0 +buffer-max+) buffer))))

(defun observe (tool args canon answer session)
  (let ((failed (tool-call-failed-p))
        (reading (handler-case (read-call-p tool args) (error () nil))))
    (remember-call session tool args canon answer)
    (let ((recorder (gethash session *recorders*)))
      (when recorder
        (handler-case (record-call recorder tool args canon answer failed)
          (error () (setf (gethash "rejected" recorder) t)))))
    (when (and reading (not failed))
      (observe-fact session tool canon (result-fields answer)))))

(defun intercept (op next)
  "The :TOOL point: the gate in front of the dispatch, the observation
behind it. A refusal is the call's own failure, so no transcript reads it
as done."
  (let* ((tool (or (getf op :name) ""))
         (args (arguments op))
         (canon (canonical args))
         (session (session-id))
         (gating (equal (mode) "enforce"))
         (reading (and gating (handler-case (read-call-p tool args) (error () nil))))
         (recording (and gating (gethash session *recorders*))))
    (multiple-value-bind (action subject step)
        (if (or (not gating) reading)
            (values nil)
            (handler-case (gate tool args session recording)
              (error () (values :block "a contract could not be read; repair or re-record it"))))
      (when (eq action :block)
        (return-from intercept
          (nle:failure (format nil "ERROR: proof-run refused this call: ~a" subject))))
      ;; Reads are consumed before the dispatch, not after: two mutations
      ;; cannot both spend one observation.
      (when (and gating (not reading))
        (clrhash (session-facts session)))
      (let ((answer (funcall next op)))
        (handler-case (observe tool args canon answer session) (error () nil))
        (when (and (eq action :allow) subject step
                   (not (evidence-holds-p step answer (tool-call-failed-p))))
          (mark-unproven subject))
        answer))))

;;; --- what the command says -------------------------------------------------

(defparameter +usage+
  "Usage: /proof-run start <name> [intent] | stop | cancel | learn <name> [N] | rebind <name> | status | unbind <name>")

(defparameter +manual+
  (format nil "proof-run: record a run once, then gate its reuse.~%~
/proof-run start NAME [intent] records the session's calls: the reads it makes~%
and the mutating calls it makes, with their exact arguments.~%
/proof-run stop turns the recording into a live contract. From then on a call~%
of a guarded tool runs only when its arguments are one demonstrated call's and~%
every read that call depended on was made again since, within max_age, with the~%
same answer; reads are consumed by the next mutation.~%
/proof-run rebind NAME re-demonstrates a contract that changed.~%
/proof-run learn NAME [N] makes a contract from the last N calls (at most 100).~%
/proof-run status, unbind NAME, cancel.~2%~
A contract is one JSON file under the home; nothing leaves the machine."))

(defun split-words (text)
  (let ((words '())
        (start 0)
        (length (length text)))
    (loop for index from 0 to length
          do (when (or (= index length) (member (char text index) '(#\Space #\Tab #\Newline)))
               (when (> index start) (push (subseq text start index) words))
               (setf start (1+ index))))
    (nreverse words)))

(defun parse-count (text)
  (let ((count (and (every #'digit-char-p text) (parse-integer text))))
    (if (and count (<= 1 count +buffer-max+))
        count
        (fail "the count must be a whole number from 1 to ~d" +buffer-max+))))

(defun contract-json (contract)
  (nlk:json-object "name" (gethash "contract_id" contract)
                    "version" (gethash "version" contract)
                    "bound" (gethash "bound" contract)
                    "evidence" (gethash "evidence_status" contract)
                    "guarded" (gethash "guarded" contract)
                    "steps" (length (gethash "steps" contract))
                    "intent" (gethash "intent" contract)))

(defun status-text ()
  (let ((ids (contract-ids)))
    (if (null ids)
        "No contracts. /proof-run start NAME records one."
        (with-output-to-string (out)
          (format out "~d contract~:p, ~a:~%" (length ids) (namestring (contracts-directory)))
          (dolist (id ids)
            (let ((contract (gethash id *contracts*)))
              (format out "  ~a v~d bound=~a evidence=~a steps=~d gating ~{~a~^,~} -- ~a~%"
                      id (gethash "version" contract) (gethash "bound" contract)
                      (gethash "evidence_status" contract)
                      (length (gethash "steps" contract))
                      (coerce (gethash "guarded" contract) 'list)
                      (gethash "intent" contract))))))))

(defun learn-contract (session id count)
  (when (gethash session *recorders*)
    (fail "already recording in this session: /proof-run stop or cancel first"))
  (when (gethash id *contracts*)
    (fail "contract ~a already exists: /proof-run rebind ~a" id id))
  (let* ((buffer (gethash session *buffers*))
         (calls (reverse (if (> (length buffer) count) (subseq buffer 0 count) buffer))))
    (when (null calls) (fail "no tool calls in this session to learn from"))
    (let ((recorder (nlk:json-object "name" id
                                      "intent" (format nil "learned from ~d calls of this session" (length calls))
                                      "reads" '() "steps" '() "count" 0 "rejected" nil)))
      (setf (gethash session *recorders*) recorder)
      (dolist (call calls)
        (record-call recorder (gethash "tool" call) (gethash "arguments" call)
                     (gethash "args" call) (gethash "answer" call) nil))
      (let ((contract (finish-recording session)))
        (format nil "Contract '~a' is live (v~d), learned from ~d calls, gating ~{~a~^, ~}."
                (gethash "contract_id" contract) (gethash "version" contract) (length calls)
                (coerce (gethash "guarded" contract) 'list))))))

(defun proof-run-command (raw session)
  "The /proof-run command: the original's /proof verbs, one each."
  (let ((words (split-words (or raw ""))))
    (handler-case
        (cond
          ((null words) +usage+)
          ((and (equal (first words) "start") (>= (length words) 2))
           (when (gethash session *recorders*)
             (fail "already recording in this session: /proof-run stop or cancel"))
           (let ((id (check-name (second words))))
             (setf (gethash session *recorders*)
                   (begin-recording id (or (format nil "~{~a~^ ~}" (cddr words)) id) nil))
             (format nil "Recording '~a'. Do the task once, reads and calls, then /proof-run stop." id)))
          ((and (equal (first words) "rebind") (>= (length words) 2))
           (when (gethash session *recorders*)
             (fail "already recording in this session: /proof-run stop or cancel"))
           (let ((id (check-name (second words))))
             (setf (gethash session *recorders*)
                   (begin-recording id (or (format nil "~{~a~^ ~}" (cddr words)) id) t))
             (format nil "Rebinding '~a': the saved contract still gates every other session until you stop. Do the task once, then /proof-run stop." id)))
          ((equal (first words) "stop")
           (let ((contract (finish-recording session)))
             (format nil "Contract '~a' is live (v~d), gating ~{~a~^, ~}. Exact demonstrated arguments only; its reads must be fresh again."
                     (gethash "contract_id" contract) (gethash "version" contract)
                     (coerce (gethash "guarded" contract) 'list))))
          ((equal (first words) "cancel")
           (remhash session *recorders*)
           (remhash session *buffers*)
           (clrhash (session-facts session))
           "Recording cancelled in this session. Saved contracts are unchanged.")
          ((and (equal (first words) "learn") (<= 2 (length words) 3))
           (let ((id (check-name (second words))))
             (learn-contract session id (if (third words) (parse-count (third words)) +buffer-max+))))
          ((equal (first words) "status") (status-text))
          ((and (equal (first words) "unbind") (= (length words) 2))
           (let* ((id (check-name (second words)))
                  (contract (gethash id *contracts*)))
             (if (null contract)
                 (format nil "No contract named ~a." id)
                 (progn (setf (gethash "bound" contract) nil)
                        (save-contract contract)
                        (format nil "Contract ~a unbound: it no longer gates." id)))))
          (t +usage+))
      (proof-run-error (condition)
        (format nil "proof-run: ~a" (detail condition))))))

;;; --- what the model can read -----------------------------------------------

(nlk:define-contract proof-status-request
  (:doc "Proof Run's contracts: what is gated on this machine right now, each with its version, bound state and evidence status, and whether a recording is in progress.")
  ("session" :text :default "" :doc "the session whose recording state to report; empty for none"))

(defun proof-status-tool (request)
  (nlk:encode-json-object
   (nlk:json-object
    "contracts" (coerce (loop for id in (contract-ids) collect (contract-json (gethash id *contracts*))) 'vector)
    "recording" (let ((recorder (gethash (proof-status-request-session request) *recorders*)))
                   (if recorder (gethash "name" recorder) :null))
    "mode" (mode)
    "directory" (namestring (contracts-directory))
    "usage" +usage+
    "note" "a covered call runs only with a demonstrated call's exact arguments and its reads made fresh again")))

(nle:define-cell proof-run
  (:section ("proof-run")
    (:guide "nothing here is required; enforce gates a covered call, off only records. max_age is how long a read counts, in seconds; reads and read_forms name what is an observation rather than a step")
    ("mode" :choice :options '("enforce" "off") :default "enforce"
     :doc "enforce refuses a covered call that does not match its contract; off records nothing and gates nothing")
    ("max_age" :integer :default 60 :min 1
     :doc "seconds a read stays fresh; a guarded call needs its reads made again since")
    ("reads" :list :default '("proof_status" "web_search" "web_extract" "look" "help")
     :doc "tool names whose calls are observations a contract's preconditions read, not steps it gates")
    ("read_forms" :list :default '("help" "look" "config-get" "cells" "describe-cells" "index" "recall-entry" "search" "grep")
     :doc "the first verb of an eval form that makes it an observation, as sh is not")
    ("check_evidence" :boolean :default t
     :doc "compare a replayed call's answer with the demonstrated one; off gates on arguments and reads alone")
    ("directory" :path :doc "where contracts are kept, relative to the Nodecode home (default proof-run/)"))
  (:tool "proof_status" (nlk:contract 'proof-status-request) 'proof-status-tool)
  (:command "proof-run" 'proof-run-command
            :description "Record a demonstrated run, then gate its reuse"
            :argument-hint "start <name> | stop | cancel | learn <name> [N] | rebind <name> | status | unbind <name>")
  (:help :proof-run "proof-run: record a demonstrated run, then gate its reuse" +manual+)
  (:hook :tool #'intercept)
  (:start (lambda ()
            (setf *facts* (make-hash-table :test #'equal)
                  *recorders* (make-hash-table :test #'equal)
                  *buffers* (make-hash-table :test #'equal))
            (handler-case (load-contracts)
              (error () (clrhash *contracts*)))
            (let ((count (hash-table-count *contracts*)))
              (when (plusp count)
                (nle:notice (format nil "proof-run: ~d contract~:p gating" count)
                            :key "nodecode-proof-run")))
            (nle:on-stop (lambda ()
                           (nle:notice nil :key "nodecode-proof-run")
                           (clrhash *contracts*)
                           (clrhash *facts*)
                           (clrhash *recorders*)
                           (clrhash *buffers*))))))