;;;; router.lisp --- the roster, the two calls, and the one name they answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a stdio MCP server answering one `skill_select' call
;;;; with two Decisions calls to Jev (TypeSafe's System One). Stage one puts
;;;; the whole roster to one `choice' over the same one-line descriptions an
;;;; index shows, and asks three judgments on the request -- "does this turn
;;;; want a skill at all?". Stage two reranks the shortlist with each
;;;; candidate's own description and the start of its text, and one "does
;;;; this skill do the specific thing asked" judgment per candidate. Two
;;;; thresholds decide whether anything is named, and the answer is at most
;;;; one skill name, or an abstain.
;;;;
;;;; Every I/O seam is a variable, so the tests replace one holding nothing:
;;;;   *POST*    (lambda (url headers body timeout) => (values TEXT STATUS))
;;;;   *ROSTER*  NIL ranks the skills this image keeps; a list is ranked as
;;;;             it stands; a function of none is called.

(in-package #:nodecode-jev-skill-router)

;;; --- what the backend is -----------------------------------------------------

(defparameter +default-base+ "https://api.typesafe.ai"
  "The TypeSafe root the original sends to when nothing overrides it.")

(defparameter +endpoint+ "/v1/systemone"
  "The one route System One answers on: {state, model, questions} in, {answers} out.")

(defparameter +model+ "jev-latest"
  "The judgment model the original names.")

(defparameter +none+ "none_of_these"
  "The option the second choice carries, so a shortlist can say none of them fits.")

(defparameter +index-width+ 60
  "Characters of a description an index shows; the same line is the rubric.")

(defparameter +user-agent+ "nodecode-jev-skill-router/0.1"
  "What the cell calls itself to the backend.")

(defvar *post* (lambda (url headers body timeout)
                 (declare (ignore timeout))
                 (nlk:http :post url :headers headers :content body
                           :timeout (setting :timeout)))
  "What sends one request. (lambda (URL HEADERS BODY TIMEOUT) => (values TEXT STATUS)).")

(defvar *roster* nil
  "The seam a test replaces: NIL reads the skills this image keeps, a function
of none is called, and a list is ranked as it stands.")

;;; --- the roster ---------------------------------------------------------------

(defun skill-roster ()
  "The skills this image keeps, by name, as (NAME DESCRIPTION EXCERPT).
The same definitions an index lists: a skill another harness keeps and a
SKILL.md a folder links in are in that list too."
  (sort (loop for entry in (nlk:knowledge-entries)
              when (equal "DEFINE-SKILL" (nlk::knowledge-entry-operator entry))
                append (let ((name (nlk::knowledge-entry-name entry)))
                         (when (and name (plusp (length name)))
                           (list (list name
                                       (or (nlk::knowledge-entry-description entry) "")
                                       (or (nlk::knowledge-entry-body entry) ""))))))
        (lambda (left right) (string< (first left) (first right)))))

(defun current-roster ()
  "The roster a call ranks: what *ROSTER* holds, else the skills this image keeps."
  (cond ((null *roster*) (skill-roster))
        ((functionp *roster*) (funcall *roster*))
        (t *roster*)))

(defun index-line (description)
  "DESCRIPTION as an index shows it: one line, cut at +INDEX-WIDTH+."
  (let ((text (string-trim " " (substitute #\Space #\Newline (or description "")))))
    (if (> (length text) +index-width+)
        (concatenate 'string (subseq text 0 (- +index-width+ 3)) "...")
        text)))

(defun excerpt-of (entry length)
  "ENTRY's own text, the first LENGTH characters, whitespace folded."
  (let ((text (string-trim " " (substitute #\Space #\Newline (or (third entry) "")))))
    (if (> (length text) length) (subseq text 0 length) text)))

;;; --- the section -----------------------------------------------------------------

(defun named (key)
  "The section's KEY as a string with its edges trimmed, or NIL when empty."
  (let ((value (setting key)))
    (and (stringp value)
         (let ((text (string-trim " " value)))
           (and (plusp (length text)) text)))))

(defun from-environment (variable)
  "VARIABLE's value, trimmed, or NIL when the variable is unset or empty."
  (let ((value (and variable (plusp (length variable)) (uiop:getenv variable))))
    (and value
         (let ((text (string-trim " " value)))
           (and (plusp (length text)) text)))))

(defun base-url ()
  "The API root this cell sends to: the section's, else TypeSafe's own."
  (let ((configured (named :base-url)))
    (if configured (string-right-trim "/" configured) +default-base+)))

(defun adapter-url ()
  "The endpoint of the operator's own that forwards {state, questions}, or NIL."
  (let ((configured (named :adapter-url)))
    (and configured (string-right-trim "/" configured))))

(defun model ()
  "The judgment model this cell asks: the section's, else the original's."
  (or (named :model) +model+))

(defun api-key ()
  "The TypeSafe key, read from the variable the section names at every call."
  (from-environment (named :api-key-env)))

(defun adapter-token ()
  "The adapter's bearer token, read from the variable the section names."
  (from-environment (named :adapter-token-env)))

(defun endpoint-url ()
  "The one URL a call goes to: the adapter's, else base_url + /v1/systemone."
  (if (adapter-url)
      (concatenate 'string (adapter-url) "/")
      (concatenate 'string (base-url) +endpoint+)))

(defun bearer ()
  "The token a call carries: an adapter's own in adapter mode, else the key."
  (if (adapter-url) (adapter-token) (api-key)))

(defun missing-credential ()
  "One sentence saying what to set, when nothing is set; NIL when a call can go."
  (cond ((bearer) nil)
        ((adapter-url) (format nil "no adapter token: set ~a" (setting :adapter-token-env)))
        (t (format nil "no key: set ~a (or adapter_url and ~a)"
                   (setting :api-key-env) (setting :adapter-token-env)))))

(defun fraction (text fallback)
  "TEXT read as a fraction, FALLBACK when it is not a number."
  (let ((*read-eval* nil))
    (or (ignore-errors (let ((value (read-from-string text))) (and (realp value) (float value))))
        fallback)))

;;; --- JSON out ---------------------------------------------------------------------

(defun json-string (text stream)
  "TEXT as a JSON string on STREAM."
  (write-char #\" stream)
  (loop for char across (or text "")
        do (case char
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (t (if (< (char-code char) 32)
                    (format stream "\\u~4,'0x" (char-code char))
                    (write-char char stream)))))
  (write-char #\" stream))

(defun json-array (&rest items)
  "ITEMS written as one JSON array, even when empty."
  (cons :array items))

(defun json-write (value stream)
  "VALUE as JSON on STREAM: a string, a number, NIL, an object as a list of
(KEY . VALUE) conses whose keys are strings, an array as JSON-ARRAY, or a
plain list as an array of its members."
  (cond ((stringp value) (json-string value stream))
        ((integerp value) (format stream "~d" value))
        ((realp value) (format stream "~,3f" (float value)))
        ((null value) (write-string "null" stream))
        ((and (consp value) (eq (car value) :array))
         (write-char #\[ stream)
         (loop for member in (cdr value)
               for first = t then nil
               do (unless first (write-char #\, stream))
                  (json-write member stream))
         (write-char #\] stream))
        ((and (listp value) (every #'consp value))
         (write-char #\{ stream)
         (loop for member in value
               for first = t then nil
               do (unless first (write-char #\, stream))
                  (json-string (car member) stream)
                  (write-char #\: stream)
                  (json-write (cdr member) stream))
         (write-char #\} stream))
        ((listp value) (json-write (apply #'json-array value) stream))
        (t (json-string (princ-to-string value) stream))))

(defun json-text (value)
  "VALUE as the JSON text one answer carries."
  (with-output-to-string (stream) (json-write value stream)))

(defun noul (instructions)
  "A yes/no judgment over the same state, answered as a probability."
  (list (cons "type" "noul") (cons "instructions" instructions)))

(defun choice (instructions criteria)
  "One option out of CRITERIA, an alist of (NAME . RUBRIC)."
  (list (cons "type" "choice") (cons "instructions" instructions) (cons "criteria" criteria)))

;;; --- one request -----------------------------------------------------------------

(defun bounded (text length)
  "The first LENGTH characters of TEXT."
  (let ((text (or text "")))
    (subseq text 0 (min length (length text)))))

(defun ask (request questions)
  "One POST to the backend. => the decoded response object."
  (let* ((key (bearer))
         (body (json-text (list (cons "state" (list (cons "request" request)
                                                     (cons "recent_context" "")))
                                 (cons "model" (model))
                                 (cons "questions" questions))))
         (headers (list (cons "Content-Type" "application/json")
                        (cons "Accept" "application/json")
                        (cons "User-Agent" +user-agent+)
                        (cons "Authorization" (format nil "Bearer ~a" key)))))
    (multiple-value-bind (text status) (funcall *post* (endpoint-url) headers body
                                                (setting :timeout))
      (unless (and (integerp status) (<= 200 status 299))
        (fail "the backend answered ~a: ~a" status (bounded text 200)))
      (let ((decoded (handler-case (nlk:decode-json text)
                       (error () (fail "the backend answered something that is not JSON")))))
        (if (hash-table-p decoded)
            decoded
            (fail "the backend answered something that is not a JSON object"))))))

;;; --- reading the answers -------------------------------------------------------------

(defun answers-of-response (response)
  "RESPONSE's answers object, as a hash table, or NIL."
  (let ((answers (and (hash-table-p response) (gethash "answers" response))))
    (and (hash-table-p answers) answers)))

(defun answer-of (answers id)
  "ANSWERS' answer to question ID, as a hash table."
  (let ((answer (and (hash-table-p answers) (gethash id answers))))
    (and (hash-table-p answer) answer)))

(defun probabilities (answer)
  "ANSWER's distribution as a list of (NAME . PROBABILITY), or NIL."
  (let ((table (and answer (gethash "probabilities" answer))))
    (when (hash-table-p table)
      (loop for name being the hash-keys of table using (hash-value probability)
            collect (cons name (if (numberp probability) (float probability) 0.0))))))

(defun choice-of (answers id)
  "The option ANSWERS picked for ID, or NIL."
  (let ((answer (answer-of answers id)))
    (and answer (let ((picked (gethash "choice" answer))) (and (stringp picked) picked)))))

(defun noul-of (answers id)
  "The probability ANSWERS said yes to ID's judgment, or NIL."
  (let ((answer (answer-of answers id)))
    (and answer (let ((value (gethash "noul" answer))) (and (numberp value) (float value))))))

(defun rank (probabilities)
  "PROBABILITIES without the no-match option, highest first, ties by name."
  (sort (remove +none+ probabilities :key #'car :test #'equal)
        (lambda (left right)
          (if (= (cdr left) (cdr right))
              (string< (car left) (car right))
              (> (cdr left) (cdr right))))))

(defun usage-of (response)
  "RESPONSE's usage object as an alist of numbers, or NIL when it names none."
  (let ((usage (and (hash-table-p response) (gethash "usage" response))))
    (when (hash-table-p usage)
      (let ((kept (loop for key being the hash-keys of usage using (hash-value value)
                        when (numberp value)
                          collect (cons key value))))
        (and kept kept)))))

;;; --- the two stages --------------------------------------------------------------------

(defparameter +judgments+
  '(("gate::acts_on_user_system"
     . "Is the assistant being asked to act on the user's files, accounts, devices, or online services, rather than only to explain or advise?")
    ("gate::would_follow_documented_procedure"
     . "Would a careful expert answering this consult a specific documented procedure or set of commands, rather than answering from general understanding?")
    ("gate::prose_suffices"
     . "Could a knowledgeable generalist fully satisfy this request in prose, with no tools, no documentation, and no access to the user's files or accounts?"))
  "The three request judgments the gate is the mean of. The third points away from a skill.")

(defparameter +inverted+ '("prose_suffices")
  "The judgments whose yes means no skill is wanted.")

(defparameter +stage-one-instructions+
  "Which of these skills, if any, is the right one to load to help with the user's latest request?"
  "Stage one's one question, in the original's own words.")

(defparameter +stage-two-instructions+
  "Exactly one of these skills is the right one to load for the user's latest request. Which one? Read what each actually does, not just its name."
  "Stage two's one question, in the original's own words.")

(defun stage-one-questions (roster)
  "The question set stage one puts to the backend: the whole roster, and the gates."
  (append
   (list (cons "which"
               (choice +stage-one-instructions+
                       (loop for (name description) in roster
                             collect (cons name (index-line description))))))
   (loop for (id . instructions) in +judgments+ collect (cons id (noul instructions)))))

(defun choose-stage-one (request roster)
  "Stage one. => (values RANKED GATE GATE-VALUES RESPONSE), RANKED highest first
without the no-match option and GATE the mean of the three request judgments,
the third inverted."
  (let* ((response (ask request (stage-one-questions roster)))
         (answers (answers-of-response response))
         (ranked (rank (probabilities (answer-of answers "which"))))
         (gate-values (loop for (id . nil) in +judgments+
                            collect (cons (subseq id 6) (or (noul-of answers id) 0.0))))
         (oriented (loop for (name . value) in gate-values
                         collect (if (member name +inverted+ :test #'equal)
                                     (- 1.0 value)
                                     value)))
         (gate (if oriented (/ (reduce #'+ oriented) (length oriented)) 0.0)))
    (values ranked gate gate-values response)))

(defun rerank-questions (shortlist by-name)
  "Stage two's question set: the shortlist with each candidate's own text,
the no-match option, and one absolute judgment per candidate."
  (let ((criteria
          (loop for name in shortlist
                for entry = (gethash name by-name)
                for description = (or (second entry) "")
                for excerpt = (and entry (excerpt-of entry (setting :excerpt)))
                collect (cons name
                              (if (and excerpt (plusp (length excerpt)))
                                  (format nil "~a~%~%SKILL.md: ~a" description excerpt)
                                  description)))))
    (append
     (list (cons "which"
                 (choice +stage-two-instructions+
                         (append criteria
                                 (list (cons +none+ "None of these skills fit the request."))))))
     (loop for name in shortlist
           for entry = (gethash name by-name)
           collect (cons (format nil "fits::~a" name)
                         (noul (format nil "Does the skill '~a' do the specific thing the user's request asks for? It is described as: ~a"
                                       name (or (second entry) ""))))))))

(defun choose-stage-two (request shortlist roster)
  "Stage two. => (values WINNER FITS RESPONSE); WINNER is the option the
shortlist's own choice picked, or NIL, and FITS the absolute judgment each
candidate was given."
  (let ((by-name (make-hash-table :test #'equal)))
    (dolist (entry roster) (setf (gethash (first entry) by-name) entry))
    (let* ((response (ask request (rerank-questions shortlist by-name)))
           (answers (answers-of-response response))
           (picked (choice-of answers "which"))
           (fits (loop for name in shortlist
                        for value = (noul-of answers (format nil "fits::~a" name))
                        when value collect (cons name value))))
      (values (and picked (member picked shortlist :test #'equal) picked)
              fits
              response))))

;;; --- the decision -------------------------------------------------------------------

(defun suggestion-block (name)
  "The block the original hands back, for a caller that wants to put it after
the skill roster: bounded, and self-invalidating."
  (format nil "<skill_relevance>~%Relevant to the current request: ~a. Ignore this if it does not fit what the user actually asked for.~%</skill_relevance>"
          name))

(defun seconds-since (started)
  "The seconds between STARTED, an internal real time, and now."
  (let ((elapsed (/ (float (- (get-internal-real-time) started))
                    internal-time-units-per-second)))
    (/ (round (* elapsed 100.0)) 100.0)))

(defun best-fit (fits)
  "FITS' highest (NAME . PROBABILITY), or NIL."
  (let ((best nil))
    (dolist (candidate fits best)
      (when (or (null best) (> (cdr candidate) (cdr best)))
        (setf best candidate)))))

(defun suggest (request &optional (roster (current-roster)))
  "Which of ROSTER fits REQUEST, as the original's answer object: an alist of
(KEY . VALUE) conses ready for JSON. An empty request is an error; with no
credential the answer names what to set; every other failure signals
JEV-SKILL-ROUTER-ERROR, which the caller turns into text."
  (let ((result '())
        (started (get-internal-real-time)))
    (labels ((add (key &optional value)
               (setf result (nconc result (list (cons key value)))))
             (finish ()
               (add "seconds" (seconds-since started))
               (add "decision" (if (cdr (assoc "suggested" result :test #'equal)) "suggest" "abstain"))
               result))
      (let ((text (string-trim '(#\Space #\Tab #\Newline) (or request ""))))
        (add "roster_size" (length roster))
        (cond
          ((zerop (length text))
           (add "error" "request required")
           (add "decision" "abstain")
           (add "suggested" nil)
           result)
          ((missing-credential)
           (add "error" (missing-credential))
           (add "decision" "abstain")
           (add "suggested" nil)
           result)
          ((null roster)
           (add "decision" "abstain")
           (add "suggested" nil)
           (add "reason" "no skill is kept in this image yet")
           result)
          (t
           (multiple-value-bind (ranked gate gate-values first-response)
               (choose-stage-one text roster)
             (add "gate" gate)
             (add "gate_values" (loop for (name . value) in gate-values
                                       collect (cons name (/ (round (* value 1000.0)) 1000.0))))
             (let ((usage (usage-of first-response)))
               (when usage (add "usage_call1" usage)))
             (let ((gate-threshold (fraction (setting :gate) 0.30)))
               (cond
                 ((< gate gate-threshold)
                  (add "shortlist" (json-array))
                  (add "reason" (format nil "gate ~,2f is below ~a" gate (setting :gate)))
                  (finish))
                 ((null ranked)
                  (add "shortlist" (json-array))
                  (add "reason" "the roster nominated nothing")
                  (finish))
                 (t
                  (let ((shortlist (loop for entry in ranked
                                         repeat (max 1 (setting :shortlist))
                                         collect (car entry))))
                    (add "shortlist" (apply #'json-array shortlist))
                    (multiple-value-bind (winner fits second-response)
                        (choose-stage-two text shortlist roster)
                      (let ((usage (usage-of second-response)))
                        (when usage (add "usage_call2" usage)))
                      (add "fits" (loop for (name . value) in fits
                                         collect (cons name (/ (round (* value 1000.0)) 1000.0))))
                      (let ((best (best-fit fits))
                            (fits-threshold (fraction (setting :fits) 0.30)))
                        (if (and best (>= (cdr best) fits-threshold))
                            (progn
                              (add "suggested" (or winner (first shortlist)))
                              (add "suggestion_block" (suggestion-block (or winner (first shortlist))))
                              (finish))
                            (progn
                              (add "suggested" nil)
                              (add "reason" (format nil "no candidate fits: the best is ~,2f, below ~a"
                                                     (or (and best (cdr best)) 0.0) (setting :fits)))
                              (finish))))))))))))))))

(defun answer-json (value)
  "VALUE as the JSON text a call answers."
  (handler-case (json-text value)
    (error () "{\"error\":\"the answer could not be written\"}")))

(defun select-json (request)
  "One selection as the JSON text a tool call answers; a refusal is a field,
so a call never raises."
  (handler-case (json-text (suggest request))
    (jev-skill-router-error (condition)
      (json-text (list (cons "error" (jev-skill-router-error-detail condition))
                       (cons "decision" "abstain")
                       (cons "suggested" nil))))
    (error (condition)
      (json-text (list (cons "error" (princ-to-string condition))
                       (cons "decision" "abstain")
                       (cons "suggested" nil))))))
