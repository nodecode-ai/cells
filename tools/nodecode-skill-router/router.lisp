;;;; router.lisp --- the roster, the two-stage choice, and the line it names.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One POST /v1/systemone (TypeSafe's System One) per stage, as the
;;;; original does. Stage one puts the whole roster to one `choice' over the
;;;; same one-line descriptions the model's index shows, in chunks the API's
;;;; 255-option cap allows, and asks three request judgments; stage two puts
;;;; the shortlist to one `choice' plus one absolute "does this skill do the
;;;; specific thing asked" judgment per candidate. Two thresholds decide
;;;; whether anything is said at all, and the answer is one skill name.
;;;;
;;;; Every I/O seam is a variable, so the tests replace one holding nothing:
;;;;   *POST*       (lambda (url headers body timeout) => (values TEXT STATUS))
;;;;   *ROSTER*     (lambda () => ((NAME DESCRIPTION EXCERPT) ...))
;;;;   *TURN-TEXT*  (lambda (turn) => the request text, or NIL)
;;;;   *RECORD*     (lambda (turn text) => what records one reminder)

(in-package #:nodecode-skill-router)

(defparameter +base-url+ "https://api.typesafe.ai"
  "The TypeSafe API root the original sends to.")

(defparameter +model+ "jev-latest"
  "The judgment model the original names.")

(defparameter +endpoint+ "/v1/systemone"
  "The one route System One answers on: {state, model, questions} in, {answers} out.")

(defparameter +none+ "none_of_these"
  "The option every chunked choice carries, so a chunk with nothing relevant can say so.")

(defparameter +max-choices+ 255
  "The API's cap on one choice's options, discovered by the original against a 292-skill roster.")

(defparameter +index-width+ 60
  "Characters of a description the index shows; the same line goes to the router.")

(defvar *post* (lambda (url headers body timeout)
                 (declare (ignore timeout))
                 (nlk:http :post url :headers headers :content body :timeout (setting :timeout)))
  "What sends one request. (lambda (URL HEADERS BODY TIMEOUT) => (values TEXT STATUS)).")

(defvar *roster* nil
  "What the router ranks, the seam a test replaces: NIL reads the skills this image keeps, a function of none is called, and a list is ranked as it stands.")

(defvar *turn-text* (lambda (turn)
                      (and turn
                           (handler-case
                               (nlk:json-value (nlk:events :turn-id turn
                                                           :kind nlk::+kind-input-committed+
                                                           :as :payload)
                                               :string "message")
                             (error () nil))))
  "The request a turn was asked, for the round hook: (lambda (TURN) => TEXT or NIL).")

(defvar *record* (lambda (turn text)
                    (nlk:record-turn-context-prompt turn :text text))
  "What puts one line in the round about to be asked: (lambda (TURN TEXT) => anything).")

;;; --- the roster --------------------------------------------------------------------------

(defun skill-roster ()
  "The skills this image keeps, newest description first, as (NAME DESCRIPTION EXCERPT).
The same definitions the index section lists: a prose skill is one file in
a knowledge cell, and a skill another harness keeps is read where it lives."
  (loop for entry in (nlk:knowledge-entries)
        when (equal "DEFINE-SKILL" (nlk::knowledge-entry-operator entry))
          append (let ((name (nlk::knowledge-entry-name entry)))
                   (when (and name (plusp (length name)))
                     (list (list name
                                 (or (nlk::knowledge-entry-description entry) "")
                                 (or (nlk::knowledge-entry-body entry) "")))))))

(defun index-line (description)
  "DESCRIPTION as the index shows it: one line, cut at +INDEX-WIDTH+."
  (let ((text (string-trim " " (substitute #\Space #\Newline (or description "")))))
    (if (> (length text) +index-width+)
        (concatenate 'string (subseq text 0 (- +index-width+ 3)) "...")
        text)))

(defun excerpt-of (entry length)
  "ENTRY's own text, the first LENGTH characters, whitespace folded."
  (let ((text (string-trim " " (third entry))))
    (if (> (length text) length) (subseq text 0 length) text)))

;;; --- the request body --------------------------------------------------------------------

(defun json-string (text stream)
  "TEXT as a JSON string on STREAM."
  (write-char #\" stream)
  (loop for char across text
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

(defun json-write (value stream)
  "VALUE as JSON on STREAM: a string, an integer, NIL, an object as a list of
(KEY . VALUE) conses whose keys are strings, or an array as a list of those."
  (cond ((stringp value) (json-string value stream))
        ((integerp value) (format stream "~d" value))
        ((null value) (write-string "null" stream))
        ((and (listp value) (every #'consp value))
         (write-char #\{ stream)
         (loop for cell in value
               for first = t then nil
               do (unless first (write-char #\, stream))
                  (json-string (car cell) stream)
                  (write-char #\: stream)
                  (json-write (cdr cell) stream))
         (write-char #\} stream))
        ((listp value)
         (write-char #\[ stream)
         (loop for member in value
               for first = t then nil
               do (unless first (write-char #\, stream))
                  (json-write member stream))
         (write-char #\] stream))
        (t (json-string (princ-to-string value) stream))))

(defun json-text (value)
  "VALUE as the JSON text one request carries."
  (with-output-to-string (stream) (json-write value stream)))

(defun noul (instructions)
  "A yes/no judgment over the same state, answered as a probability."
  (list (cons "type" "noul") (cons "instructions" instructions)))

(defun choice (instructions criteria)
  "One option out of CRITERIA, an alist of (NAME . RUBRIC)."
  (list (cons "type" "choice") (cons "instructions" instructions) (cons "criteria" criteria)))

(defun fraction (text default)
  "TEXT read as a fraction, DEFAULT when it is not a number."
  (let ((*read-eval* nil))
    (or (ignore-errors (let ((value (read-from-string text))) (and (realp value) (float value))))
        default)))

(defun base-url ()
  "The API root this cell sends to: the section's, else TypeSafe's own."
  (let ((configured (setting :base-url)))
    (if (and configured (plusp (length configured)))
        (string-right-trim "/" configured)
        +base-url+)))

(defun model ()
  "The judgment model this cell asks: the section's, else the original's."
  (let ((configured (setting :model)))
    (if (and configured (plusp (length configured))) configured +model+)))

(defun api-key ()
  "The key the section names the variable of, or NIL. Read at every call."
  (let ((variable (setting :api-key-env)))
    (and variable (plusp (length variable))
         (let ((value (uiop:getenv variable)))
           (and value (plusp (length (string-trim " " value))) (string-trim " " value))))))

(defun keyed-p ()
  "Whether a request can be sent at all: a key, or a base of the operator's own."
  (or (api-key) (let ((configured (setting :base-url))) (and configured (plusp (length configured))))))

(defun ask (request questions)
  "One POST /v1/systemone. => the answers object, as decoded JSON."
  (let* ((key (api-key))
         (body (json-text (list (cons "state" (list (cons "request" request)
                                                     (cons "recent_context" "")))
                                 (cons "model" (model))
                                 (cons "questions" questions))))
         (headers (append (list (cons "Content-Type" "application/json")
                                (cons "Accept" "application/json")
                                (cons "User-Agent" "nodecode-skill-router/0.1"))
                          (when key (list (cons "Authorization" (format nil "Bearer ~a" key))))))
         (timeout (setting :timeout)))
    (multiple-value-bind (text status) (funcall *post* (concatenate 'string (base-url) +endpoint+)
                                                headers body timeout)
      (unless (and (integerp status) (<= 200 status 299))
        (fail "TypeSafe answered ~a: ~a" status (subseq (or text "") 0 (min 200 (length (or text ""))))))
      (let ((decoded (handler-case (nlk:decode-json text)
                        (error () (fail "TypeSafe answered something that is not JSON")))))
        (or (and (hash-table-p decoded) (gethash "answers" decoded))
            (fail "TypeSafe answered without an answers object"))))))

;;; --- reading the answers -----------------------------------------------------------------

(defun answers-of (answers id)
  "ANSWERS' answer to question ID, as a hash table."
  (let ((answer (and (hash-table-p answers) (gethash id answers))))
    (and (hash-table-p answer) answer)))

(defun probabilities (answer)
  "ANSWER's distribution as a list of (NAME . PROBABILITY), or NIL."
  (let ((table (and answer (hash-table-p answer) (gethash "probabilities" answer))))
    (when (hash-table-p table)
      (loop for name being the hash-keys of table using (hash-value probability)
            collect (cons name (if (numberp probability) (float probability) 0.0))))))

(defun choice-of (answers id)
  "The option ANSWERS picked for ID, or NIL."
  (let ((answer (answers-of answers id)))
    (and answer (let ((picked (gethash "choice" answer))) (and (stringp picked) picked)))))

(defun noul-of (answers id)
  "The probability ANSWERS said yes to ID's judgment, or NIL."
  (let ((answer (answers-of answers id)))
    (and answer (let ((value (gethash "noul" answer))) (and (numberp value) (float value))))))

(defun rank (probabilities)
  "PROBABILITIES without the no-match option, highest first, ties by name."
  (sort (remove +none+ probabilities :key #'car :test #'equal)
        (lambda (left right)
          (if (= (cdr left) (cdr right))
              (string< (car left) (car right))
              (> (cdr left) (cdr right))))))

(defun chunk (items size)
  "ITEMS in lists of at most SIZE."
  (loop for rest on items by (lambda (tail) (nthcdr size tail))
        while rest
        collect (subseq rest 0 (min size (length rest)))))

;;; --- the two stages ----------------------------------------------------------------------

(defparameter +judgments+
  '(("gate::acts_on_user_system"
     . "Is the assistant being asked to act on the user's files, accounts, devices, or online services, rather than only to explain or advise?")
    ("gate::would_follow_documented_procedure"
     . "Would a careful expert answering this consult a specific documented procedure or set of commands, rather than answering from general understanding?")
    ("gate::prose_suffices"
     . "Could a knowledgeable generalist fully satisfy this request in prose, with no tools, no documentation, and no access to the user's files or accounts?"))
  "The three request judgments the gate is the mean of. The third points away from a skill.")

(defparameter +inverted+ '("gate::prose_suffices")
  "The judgments whose yes means no skill is wanted.")

(defun stage-one-questions (group chunked first)
  "The question set one chunk of the roster is put to."
  (let ((criteria (loop for (name description) in group
                        collect (cons name (index-line description)))))
    (append
     (list (cons "which"
                 (choice "Which of these skills, if any, is the right one to load to help with the user's latest request?"
                         (if chunked
                             (append criteria (list (cons +none+ "None of these skills fit the request.")))
                             criteria))))
     (when first
       (loop for (id . instructions) in +judgments+ collect (cons id (noul instructions)))))))

(defun gate-value (answers)
  "The mean of the three request judgments, the third inverted; 1.0 when unanswered."
  (let ((values (loop for (id . nil) in +judgments+
                      for value = (noul-of answers id)
                      when value collect (if (member id +inverted+ :test #'equal) (- 1.0 value) value))))
    (if values (/ (reduce #'+ values) (length values)) 1.0)))

(defun choose-stage-one (request roster)
  "Stage one: every chunk ranked. => (values SHORTLIST GATE CHUNKS).
A chunk whose own P(none_of_these) is at least a half nominates nobody,
except the best chunk, so a shortlist is never empty when something fits."
  (let* ((groups (chunk roster (max 1 (setting :chunk))))
         (chunked (rest groups))
         (answers (loop for group in groups
                        for first = t then nil
                        collect (ask request (stage-one-questions group chunked first))))
         (ranks (mapcar (lambda (answer) (rank (probabilities (answers-of answer "which")))) answers))
         (none-pressure (mapcar (lambda (answer)
                                  (or (cdr (assoc +none+ (probabilities (answers-of answer "which"))
                                                  :test #'equal))
                                      0.0))
                                answers))
         (best (let ((winner 0) (best-probability -1.0))
                 (loop for rank in ranks
                       for index from 0
                       do (let ((top (and rank (cdr (first rank)))))
                            (when (and top (> top best-probability))
                              (setf best-probability top winner index))))
                 winner))
         (size (max 1 (setting :shortlist)))
         (shortlist '()))
    (loop for rank in ranks
          for index from 0
          do (unless (and (/= index best) (>= (nth index none-pressure) 0.5))
               (loop for (name . nil) in (subseq rank 0 (min size (length rank)))
                     unless (member name shortlist :test #'equal)
                       do (setf shortlist (append shortlist (list name))))))
    (values shortlist (gate-value (first answers)) (length groups))))

(defun rerank-questions (shortlist by-name)
  "Stage two's question set: the shortlist, and one absolute judgment each."
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
                 (choice "Exactly one of these skills is the right one to load for the user's latest request. Which one? Read what each actually does, not just its name."
                         (append criteria (list (cons +none+ "None of these skills fit the request."))))))
     (loop for name in shortlist
           for entry = (gethash name by-name)
           collect (cons (format nil "fits::~a" name)
                         (noul (format nil "Does the skill '~a' do the specific thing the user's request asks for? It is described as: ~a"
                                       name (or (second entry) ""))))))))

(defun choose-stage-two (request shortlist roster)
  "Stage two: the shortlist's own winner and its fit judgment."
  (let* ((by-name (make-hash-table :test #'equal))
         (answers (progn (dolist (entry roster) (setf (gethash (first entry) by-name) entry))
                         (ask request (rerank-questions shortlist by-name))))
         (picked (choice-of answers "which"))
         (fits (loop for name in shortlist
                      for value = (noul-of answers (format nil "fits::~a" name))
                      when value collect (cons name value))))
    (values (unless (or (null picked) (equal picked +none+)) picked) fits)))

;;; --- the decision ------------------------------------------------------------------------

(defun route-request (request &optional (roster (current-roster)))
  "Which of ROSTER fits REQUEST, as a plist:
  (:SKILL NAME or NIL :GATE G :SHORTLIST (...) :FITS ((NAME . P) ...) :CHUNKS N :REASON TEXT).
Nothing is sent when no key and no base of the operator's own are configured,
when the request is empty or a slash command, or when it is longer than
`suggest_chars' — the same silences the original keeps."
  (let ((text (string-trim '(#\Space #\Tab #\Newline) (or request ""))))
    (cond
      ((zerop (length text)) (list :skill nil :reason "the request is empty"))
      ((char= #\/ (char text 0)) (list :skill nil :reason "a slash command picks its own skill"))
      ((and (plusp (setting :suggest-chars)) (> (length text) (setting :suggest-chars)))
       (list :skill nil :reason "the request is longer than suggest_chars"))
      ((null roster) (list :skill nil :reason "no skill is kept in this image yet"))
      ((not (keyed-p)) (list :skill nil :reason "no key and no base_url of the operator's own"))
      (t
       (let ((fits-threshold (fraction (setting :fits) 0.40)))
         (multiple-value-bind (shortlist gate chunks) (choose-stage-one text roster)
           (let ((result (list :gate gate :chunks chunks :shortlist shortlist)))
             (cond
               ((< gate (fraction (setting :gate) 0.30))
                (append result (list :reason (format nil "gate ~,2f below the gate threshold" gate))))
               ((null shortlist)
                (append result (list :reason "the roster nominated nothing")))
               (t
                (multiple-value-bind (winner fits) (choose-stage-two text shortlist roster)
                  (let ((winner-fits (and winner (cdr (assoc winner fits :test #'equal)))))
                    (cond
                      ((null winner)
                       (append result (list :fits fits :reason "the shortlist picked no skill")))
                      ((and winner-fits (< winner-fits fits-threshold))
                       (append result (list :fits fits
                                            :reason (format nil "~a fits ~,2f, below the fits threshold"
                                                             winner winner-fits))))
                      (t
                       (append result (list :skill winner :fits fits
                                            :reason (format nil "~a fits ~,2f" winner winner-fits))))))))))))))))

(defun suggestion-block (name)
  "The one line a routing decision adds to the turn, bounded and self-invalidating."
  (format nil "<skill_relevance>~%Relevant to the current request: ~a. Ignore this if it does not fit what the user actually asked for.~%</skill_relevance>"
          name))

(defun record-suggestion (turn round)
  "At a turn's first round: the skill that fits it, as one reminder on the
round about to be asked. Every failure is silent — a router problem must
never break a turn."
  (when (and turn (eql 0 round) (keyed-p))
    (let* ((asked (funcall *turn-text* turn))
           (text (and asked (string-trim " " asked))))
      (when (and text (plusp (length text)))
        (let ((result (route-request text)))
          (when (getf result :skill)
            (funcall *record* turn (suggestion-block (getf result :skill)))))))))
