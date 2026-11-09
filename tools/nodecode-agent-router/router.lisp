;;;; router.lisp --- agents, their words, their score, and the one decision.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's router.py (HeuristicRouter) and models.py's three
;;;; declarations (AgentProfile, RouteRequest, RouteResult), written out here.
;;;; Nothing in this file leaves the process: an agent is data the caller
;;;; passes, the reading of a task is a tokenizer, and the decision is
;;;; arithmetic over both. That is the whole of the original's `local'
;;;; provider, and it is the one this cell ships.
;;;;
;;;; The order is the original's: the access policies first, then the score.
;;;; An agent a policy rejects is never scored, and no score can bring one
;;;; back.

(in-package #:nodecode-agent-router)

;;; --- one agent, as the registry declares it ----------------------------------------

(defstruct agent id name description capabilities tags tools permissions cost
  latency reliability enabled fallback-id max-actions scope)

(defun text-member (object key)
  "KEY's value on OBJECT when it is a string, else NIL."
  (let ((value (gethash key object)))
    (and (stringp value) value)))

(defun flag-member (object key default)
  "KEY's value as a boolean. An absent member reads DEFAULT, and JSON false is
a member that is there and NIL, which is not the same thing."
  (multiple-value-bind (value present) (gethash key object)
    (if present (not (null value)) default)))

(defun unit-member (object key default)
  "KEY's value as a number from 0 to 1, DEFAULT where it is absent."
  (let ((value (gethash key object)))
    (cond ((null value) default)
          ((and (realp value) (<= 0 value 1)) (coerce value 'double-float))
          (t (fail "~a is a number between 0 and 1, not ~s" key value)))))

(defun count-member (object key default)
  "KEY's value as a whole number of at least one, DEFAULT where it is absent."
  (let ((value (gethash key object)))
    (cond ((null value) default)
          ((and (integerp value) (>= value 1)) value)
          (t (fail "~a is a whole number of at least 1, not ~s" key value)))))

(defun strings-value (value what)
  "VALUE as a list of strings: a JSON array, or one comma-separated string."
  (cond ((null value) '())
        ((stringp value)
         (remove "" (mapcar (lambda (one) (string-trim " " one))
                             (uiop:split-string value :separator ","))
                 :test #'string=))
        ((vectorp value)
         (remove "" (loop for item across value
                           when (stringp item) collect (string-trim " " item))
                 :test #'string=))
        (t (fail "~a is an array of strings, or one comma-separated string" what))))

(defun hash-agent (object)
  "One agent declared as a JSON object, with the original's defaults."
  (unless (hash-table-p object)
    (fail "an agent is an object: {\"id\": \"coding-agent\", \"name\", \"description\", ~
capabilities, permissions, cost, latency, reliability}"))
  (let* ((id (nlk:trimmed (or (text-member object "id") "")))
         (fallback (nlk:trimmed (or (text-member object "fallback_id")
                                     (text-member object "fallback")
                                     ""))))
    (when (zerop (length id))
      (fail "every agent needs an id: {\"id\": \"coding-agent\", \"name\": \"Coding Agent\", ...}"))
    (make-agent :id id
                :name (or (text-member object "name") id)
                :description (or (text-member object "description") "")
                :capabilities (strings-value (gethash "capabilities" object) "capabilities")
                :tags (strings-value (gethash "tags" object) "tags")
                :tools (strings-value (gethash "tools" object) "tools")
                :permissions (strings-value (gethash "permissions" object) "permissions")
                :cost (unit-member object "cost" 0.5d0)
                :latency (unit-member object "latency" 0.5d0)
                :reliability (unit-member object "reliability" 0.7d0)
                :enabled (flag-member object "enabled" t)
                :fallback-id (and (plusp (length fallback)) fallback)
                :max-actions (count-member object "max_actions" 8)
                :scope (or (text-member object "scope")
                           (text-member object "namespace")
                           "default"))))

(defun plist-agent (row)
  "One agent written as a plist, for a form typed in eval."
  ;; A member the plist does not carry is a member the object does not carry:
  ;; setting every key would make an absent `:enabled' a NIL one, which reads
  ;; as an agent the registry disabled.
  (let ((object (make-hash-table :test #'equal)))
    (flet ((put (key value)
             (when value (setf (gethash key object) value)))
           (put-list (key value)
             (when value
               (setf (gethash key object)
                     (coerce (mapcar #'princ-to-string value) 'vector)))))
      (put "id" (getf row :id))
      (put "name" (getf row :name))
      (put "description" (getf row :description))
      (put-list "capabilities" (getf row :capabilities))
      (put-list "tags" (getf row :tags))
      (put-list "tools" (getf row :tools))
      (put-list "permissions" (getf row :permissions))
      (put "cost" (getf row :cost))
      (put "latency" (getf row :latency))
      (put "reliability" (getf row :reliability))
      (when (member :enabled row)
        (setf (gethash "enabled" object) (and (getf row :enabled) t)))
      (put "fallback_id" (getf row :fallback-id))
      (put "max_actions" (getf row :max-actions))
      (put "scope" (getf row :scope)))
    (hash-agent object)))

(defun agent-of (row)
  "A caller's agent, as JSON or as a plist, as one AGENT."
  (cond ((hash-table-p row) (hash-agent row))
        ((listp row) (plist-agent row))
        (t (fail "an agent is an object, not ~s" (type-of row)))))

;;; --- the words ----------------------------------------------------------------------

;; The original's list: the words that carry no signal in an English or a
;; French task, so one common word cannot outrank a capability.
(defparameter *stopwords*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (word (uiop:split-string
                   "au aux avec ce ces dans de des du elle en et eux il je la le les leur lui ma
                    mais me meme mes moi mon ne nos notre nous on ou par pas pour qu que qui sa se
                    ses son sur ta te tes toi ton tu un une vos votre vous c d j l a m n s t y
                    the a an and or of to in for with without is are was were be been this that"
                   :separator '(#\Space #\Tab #\Newline)))
      (unless (string= word "") (setf (gethash word table) t)))
    table)
  "Words dropped before scoring, the original's.")

(defun tokenize (text)
  "TEXT's lowercase words, stopwords dropped: the router's only reading of it."
  (let ((text (string-downcase (princ-to-string text)))
        (words '())
        (start nil))
    (flet ((flush (end)
             (when start
               (let ((word (subseq text start end)))
                 (when (and (or (> (length word) 1) (every #'digit-char-p word))
                            (not (gethash word *stopwords*)))
                   (pushnew word words :test #'string=)))
               (setf start nil))))
      (loop for index from 0 below (length text)
            for char = (char text index)
            do (if (alphanumericp char)
                   (unless start (setf start index))
                   (flush index)))
      (flush (length text)))
    (nreverse words)))

(defun agent-tokens (agent)
  "The words an agent is reachable by: its name and description, then every
capability and tag whole and word by word."
  (let ((tokens (tokenize (format nil "~a ~a" (agent-name agent) (agent-description agent)))))
    (dolist (value (append (agent-capabilities agent) (agent-tags agent)))
      (dolist (word (tokenize value))
        (pushnew word tokens :test #'string=))
      (let ((whole (string-downcase (nlk:trimmed value))))
        (unless (zerop (length whole)) (pushnew whole tokens :test #'string=))))
    tokens))

(defun lowered (values)
  (mapcar (lambda (one) (string-downcase (nlk:trimmed one))) values))

(defun overlap (words other)
  (count-if (lambda (word) (member word other :test #'string=)) words))

;;; --- one request --------------------------------------------------------------------

(defstruct request task capabilities permissions forbidden allow-fallback min-confidence
  alternatives failed scope)

(defun request-of (task &key agents required-capabilities required-permissions
                            forbidden-permissions (allow-fallback t) min-confidence
                            max-alternatives failed-agent-id scope)
  "The one request a call or a /route makes, the section answering what the
call leaves out. => (values REQUEST AGENTS), AGENTS the caller's own rows or
NIL for the configured registry."
  (let ((task (nlk:trimmed (or task "")))
        (failed (nlk:trimmed (or failed-agent-id ""))))
    (when (zerop (length task))
      (fail "a routing call needs a task: the words of the work to be done"))
    (values (make-request
             :task task
             :capabilities (lowered (strings-value required-capabilities "required_capabilities"))
             :permissions (lowered (strings-value required-permissions "required_permissions"))
             :forbidden (lowered (strings-value forbidden-permissions "forbidden_permissions"))
             :allow-fallback (and allow-fallback t)
             ;; The section's floor is a percent; a call may raise it, never lower it.
             :min-confidence (max (/ (min 100 (max 0 (setting :min-confidence))) 100.0d0)
                                  (coerce (or min-confidence 0.0d0) 'double-float))
             :alternatives (or max-alternatives (setting :max-alternatives))
             :failed (and (plusp (length failed)) failed)
             :scope (or scope "default"))
            (and agents (mapcar #'agent-of (coerce agents 'list))))))

;;; --- the policies, then the score ----------------------------------------------------

(defun hard-reject (agent request)
  "Why AGENT cannot take REQUEST whatever it scores, or NIL."
  (cond ((not (string= (agent-scope agent) (request-scope request))) "out_of_scope")
        ((not (agent-enabled agent)) "disabled")
        ((and (request-failed request) (string= (agent-id agent) (request-failed request)))
         "already_failed")
        ((let ((needed (request-capabilities request)))
           (and needed
                (not (subsetp needed (lowered (agent-capabilities agent)) :test #'string=))))
         "missing_capabilities")
        ((let ((needed (request-permissions request)))
           (and needed
                (not (subsetp needed (lowered (agent-permissions agent)) :test #'string=))))
         "missing_permissions")
        ((let ((forbidden (request-forbidden request)))
           (and forbidden
                (intersection forbidden (lowered (agent-permissions agent)) :test #'string=)))
         "forbidden_permissions")
        (t nil)))

(defun score-of (agent request task-tokens)
  "=> (values SCORE RELEVANCE COVERAGE): how much of the task AGENT's words
reach, how much of it its capabilities reach, and the weighted sum."
  (let* ((tokens (agent-tokens agent))
         (total (float (length task-tokens) 1d0))
         (relevance (if task-tokens (/ (overlap task-tokens tokens) total) 0.0d0))
         (coverage (cond ((request-capabilities request) 1.0d0)
                         ((and (agent-capabilities agent) task-tokens)
                          (/ (overlap task-tokens (lowered (agent-capabilities agent))) total))
                         (t 0.0d0))))
    ;; The original's weights: the task's own words first, then what the agent
    ;; can do, then how well it has held up, then what it costs.
    (values (+ (* 0.45d0 relevance)
               (* 0.25d0 coverage)
               (* 0.15d0 (agent-reliability agent))
               (* 0.10d0 (- 1.0d0 (agent-cost agent)))
               (* 0.05d0 (- 1.0d0 (agent-latency agent))))
            relevance
            coverage)))

(defstruct choice id name score reasons permissions capabilities max-actions)

(defstruct rejection id reason)

(defstruct route-result decision selected fallback alternatives rejected reasons abstain)

(defun collect (agents request)
  "Every agent the request's policies keep, scored best first.
=> (values ROWS REJECTED BY-ID), ROWS (SCORE RELEVANCE COVERAGE AGENT)."
  (let ((task-tokens (tokenize (request-task request)))
        (seen '())
        (by-id (make-hash-table :test #'equal))
        (rows '())
        (rejected '()))
    (dolist (agent agents)
      (if (member (agent-id agent) seen :test #'string=)
          (push (make-rejection :id (agent-id agent) :reason "duplicate_input") rejected)
          (progn
            (push (agent-id agent) seen)
            (setf (gethash (agent-id agent) by-id) agent)
            (let ((reason (hard-reject agent request)))
              (if reason
                  (push (make-rejection :id (agent-id agent) :reason reason) rejected)
                  (multiple-value-bind (score relevance coverage)
                      (score-of agent request task-tokens)
                    (push (list score relevance coverage agent) rows)))))))
    (values (sort rows (lambda (left right)
                         (let ((one (round (first left) 1d-6))
                               (other (round (first right) 1d-6)))
                           (cond ((/= one other) (> one other))
                                 (t (string< (agent-id (fourth left))
                                             (agent-id (fourth right))))))))
            (nreverse rejected)
            by-id)))

(defun choice-of (row)
  (destructuring-bind (score relevance coverage agent) row
    (make-choice :id (agent-id agent)
                 :name (agent-name agent)
                 :score score
                 :reasons (list (format nil "relevance=~,2f" relevance)
                                (format nil "capabilities=~,2f" coverage)
                                (format nil "reliability=~,2f" (agent-reliability agent))
                                (format nil "cost=~,2f" (agent-cost agent))
                                (format nil "latency=~,2f" (agent-latency agent))
                                (format nil "max_actions=~d" (agent-max-actions agent)))
                 :permissions (agent-permissions agent)
                 :capabilities (agent-capabilities agent)
                 :max-actions (agent-max-actions agent))))

(defun slice (rows start count)
  "ROWS from START, at most COUNT of them."
  (let ((end (min (length rows) (+ start (max 0 count)))))
    (and (< start end) (subseq rows start end))))

(defun choose (request rows rejected)
  "The decision when no agent has failed yet: the best one, a runner-up, or an
abstention that says why the best was not good enough."
  (if (null rows)
      (make-route-result :decision "abstain" :rejected rejected
                         :reasons '("no_candidate") :abstain "no_candidate")
      (let* ((top (first rows))
             (threshold (request-min-confidence request)))
        (if (< (choice-score top) threshold)
            (make-route-result :decision "abstain"
                               :alternatives (slice rows 0 (request-alternatives request))
                               :rejected rejected
                               :reasons (list (format nil "low_confidence:~,2f"
                                                       (choice-score top)))
                               :abstain "low_confidence")
            (make-route-result :decision "select"
                               :selected top
                               :fallback (and (request-allow-fallback request) (second rows))
                               :alternatives (slice rows 1 (request-alternatives request))
                               :rejected rejected
                               :reasons (cons "best_match" (choice-reasons top)))))))

(defun rejected-with (rejected id reason)
  "REJECTED with ID rejected for REASON, unless it is there already."
  (if (find id rejected :key #'rejection-id :test #'string=)
      rejected
      (append rejected (list (make-rejection :id id :reason reason)))))

(defun after-failure (request rows rejected by-id)
  "One agent has failed: answer what to try instead, in one hop, or say there
is nothing to try."
  (let* ((failed-id (request-failed request))
         (failed (gethash failed-id by-id))
         (rejected (if failed rejected
                       (rejected-with rejected failed-id "agent_absent"))))
    (if (not (request-allow-fallback request))
        (make-route-result :decision "abstain" :rejected rejected
                           :reasons '("fallback_disabled") :abstain "fallback_disabled")
        (let* ((preferred (and failed (agent-fallback-id failed)))
               (chosen (and preferred (find preferred rows :key #'choice-id :test #'string=)))
               (rejected (if (and preferred (null chosen))
                             (rejected-with rejected preferred "fallback_unavailable")
                             rejected))
               (chosen (or chosen (first rows))))
          (if (null chosen)
              (make-route-result :decision "abstain" :rejected rejected
                                 :reasons '("no_fallback") :abstain "no_fallback")
              (make-route-result
               :decision "fallback"
               :selected chosen
               :alternatives (slice (remove (choice-id chosen) rows
                                           :key #'choice-id :test #'string=)
                                    0 (request-alternatives request))
               :rejected rejected
               :reasons (list* (format nil "failed:~a" failed-id)
                               "bounded_fallback"
                               (choice-reasons chosen))))))))

(defun decide (agents request)
  "Rank AGENTS for REQUEST: a ROUTE-RESULT, a decision with its reasons and
never a condition -- a malformed call refuses, an agent that does not fit is
a rejection."
  (multiple-value-bind (candidates rejected by-id) (collect agents request)
    (let ((rows (mapcar #'choice-of candidates)))
      (if (request-failed request)
          (after-failure request rows rejected by-id)
          (choose request rows rejected)))))

;;; --- the answer, as JSON and as lines ------------------------------------------------

(defun rounded (score)
  (/ (float (round score 1d-6) 1d0) 1000000d0))

(defun choice-json (choice)
  (nlk:json-object "id" (choice-id choice)
                    "name" (choice-name choice)
                    "score" (rounded (choice-score choice))
                    "reasons" (coerce (choice-reasons choice) 'vector)
                    "permissions" (coerce (choice-permissions choice) 'vector)
                    "capabilities" (coerce (choice-capabilities choice) 'vector)
                    "max_actions" (choice-max-actions choice)))

(defun rejection-json (rejection)
  (nlk:json-object "id" (rejection-id rejection) "reason" (rejection-reason rejection)))

(defun result-object (result)
  "The decision as one JSON object: every candidate either chosen, a
runner-up, an alternative, or rejected with why."
  (nlk:json-object
   "decision" (route-result-decision result)
   "selected" (if (route-result-selected result)
                   (choice-json (route-result-selected result))
                   :null)
   "fallback" (if (route-result-fallback result)
                   (choice-json (route-result-fallback result))
                   :null)
   "alternatives" (coerce (mapcar #'choice-json (route-result-alternatives result)) 'vector)
   "reasons" (coerce (route-result-reasons result) 'vector)
   "abstain_reason" (or (route-result-abstain result) :null)
   "rejected" (coerce (mapcar #'rejection-json (route-result-rejected result)) 'vector)))

(defun result-json (result)
  "The same decision as the JSON text the agent_route tool answers."
  (nlk:encode-json-object (result-object result)))

(defun choice-line (choice)
  (format nil "~a (~,2f) - ~{~a~^ ~}"
          (choice-id choice) (rounded (choice-score choice)) (choice-reasons choice)))

(defun result-text (result)
  "The same decision as the lines /agents prints."
  (with-output-to-string (out)
    (let ((selected (route-result-selected result)))
      (cond ((string= (route-result-decision result) "abstain")
             (format out "abstain (~a)" (route-result-abstain result))
             (dolist (reason (route-result-reasons result))
               (format out "~%  ~a" reason)))
            (t
             (format out "~a ~a (~,2f)"
                     (route-result-decision result)
                     (choice-id selected)
                     (rounded (choice-score selected)))
             (format out "~%  ~{~a~^  ~}" (choice-reasons selected))))
      (when (route-result-fallback result)
        (format out "~%  fallback ~a (~,2f)"
                (choice-id (route-result-fallback result))
                (rounded (choice-score (route-result-fallback result)))))
      (when (route-result-alternatives result)
        (format out "~%  alternatives ~{~a~^, ~}"
                (mapcar (lambda (choice)
                          (format nil "~a (~,2f)" (choice-id choice)
                                  (rounded (choice-score choice))))
                        (route-result-alternatives result))))
      (when (route-result-rejected result)
        (dolist (rejection (route-result-rejected result))
          (format out "~%  rejected ~a: ~a" (rejection-id rejection)
                  (rejection-reason rejection)))))))
