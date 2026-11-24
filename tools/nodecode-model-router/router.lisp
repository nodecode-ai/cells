;;;; router.lisp --- the ranking, the hard filters, and the three decisions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The whole of it is deterministic and offline: a task's words are scored
;;;; against what a model says it is and can do, the caller's constraints
;;;; throw rows out before scoring rather than after, and what survives is
;;;; ordered by one strategy's numbers. Nothing here calls a model, writes a
;;;; message or reads a secret: a row names its key's variable, and whether
;;;; that variable is set is a fact of the process.
;;;;
;;;; A decision is one of three. SELECT takes the best row and, under it, one
;;;; fallback and the alternatives; FALLBACK answers what to use instead of a
;;;; model that just failed, honouring that row's own fallback_id for exactly
;;;; one hop; ABSTAIN is a decision too, and says which reason it has.

(in-package #:nodecode-model-router)

;;; --- a request ----------------------------------------------------------------

(defstruct (route-request (:constructor make-route-request))
  "What to route, and under which constraints."
  (task "" :type string)
  (required-capabilities '() :type list)
  (prefer "balanced" :type string)
  (min-quality 0.0 :type real)
  (max-cost 1.0 :type real)
  (max-latency 1.0 :type real)
  (min-context 0 :type integer)
  (allow-fallback t)
  (failed-model-id nil)
  (provider nil)
  (model nil)
  (scope "default" :type string)
  (max-alternatives 3 :type integer)
  ;; What a judge's opinion is worth against the strategy's own numbers; 0.0
  ;; while no judge is configured, so the ranking is the deterministic one.
  (judge-weight 0.0 :type real))

(defstruct (candidate (:constructor make-candidate (score relevance entry)) (:copier nil))
  score relevance entry)

(defstruct (selection (:constructor make-selection) (:copier nil))
  entry score (reasons '() :type list))

(defstruct (route-result (:constructor make-route-result) (:copier nil))
  (decision "abstain" :type string)
  selected fallback (alternatives '() :type list) (rejected '() :type list)
  (reasons '() :type list)
  (abstain-reason nil))

(defparameter *stopwords*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (word (uiop:split-string
                   "au aux avec ce ces dans de des du elle en et eux il je la le les leur lui ma mais me meme mes moi mon ne nos notre nous on ou par pas pour qu que qui sa se ses son sur ta te tes toi ton tu un une vos votre vous the a an and or of to in for with without is are was were be been this that"
                   :separator " "))
      (setf (gethash word table) t))
    table)
  "The words a task's meaning does not rest on, in both languages the
original refused to score.")

(defparameter *words* (ppcre:create-scanner "[a-z0-9àâäéèêëíìîïòóôöùúûüçñ]+")
  "What counts as one word of a task or of a model's description.")

(defun tokenize (text)
  "TEXT's words, lowercased, one letter and stopwords dropped."
  (let ((tokens '()))
    (ppcre:do-matches-as-strings (word *words* (string-downcase (or text "")))
      (when (and (or (> (length word) 1) (every #'digit-char-p word))
                 (not (gethash word *stopwords*)))
        (pushnew word tokens :test #'string=)))
    tokens))

(defun model-tokens (entry)
  "What a model says it is: its name, its id, its vendor and its capabilities."
  (let ((tokens (tokenize (format nil "~a ~a ~a ~a" (entry-name entry) (entry-model entry)
                                   (entry-provider entry) (entry-id entry)))))
    (dolist (capability (entry-capabilities entry))
      (pushnew (string-downcase capability) tokens :test #'string=)
      (setf tokens (union (tokenize capability) tokens :test #'string=)))
    tokens))

(defun relevance (task-tokens entry)
  "How much of the task's own vocabulary the model answers for, 0.0 to 1.0."
  (if task-tokens
      (/ (float (length (intersection task-tokens (model-tokens entry) :test #'string=)) 1.0)
         (length task-tokens))
      0.0))

;;; --- the strategies ------------------------------------------------------------

(defparameter +strategies+ '("balanced" "best" "cheapest" "fastest" "named")
  "What PREFER may say: four ways to rank, and one that pins a model.")

(defun strategy-score (entry prefer)
  "ENTRY's worth under PREFER, 0.0 to 1.0, before the task's own words."
  (let* ((quality (entry-quality entry))
         (cheap (- 1.0 (entry-cost entry)))
         (fast (- 1.0 (entry-latency entry))))
    (cond ((string= prefer "cheapest") (+ (* 0.7 cheap) (* 0.2 quality) (* 0.1 fast)))
          ((string= prefer "fastest") (+ (* 0.7 fast) (* 0.2 quality) (* 0.1 cheap)))
          ((string= prefer "best") (+ (* 0.7 quality) (* 0.15 fast) (* 0.15 cheap)))
          (t (+ (* 0.4 quality) (* 0.3 cheap) (* 0.3 fast))))))

(defun matches-pin (entry pin)
  "Whether PIN names ENTRY: its id, its model alone, or vendor:model."
  (let ((target (nlk:trimmed (or pin ""))))
    (and (plusp (length target))
         (or (string= (entry-id entry) target)
             (string= (entry-ref entry) target)
             (string= (entry-model entry) target)))))

;;; --- the hard filters ----------------------------------------------------------

(defun capability-set (entry)
  (mapcar (lambda (item) (string-downcase item)) (entry-capabilities entry)))

(defun hard-reject (entry request require-keys)
  "Why ENTRY cannot serve REQUEST at all, or NIL. A reason here is final: no
judge, no score and no fallback may put the row back."
  (let ((required (mapcar #'string-downcase (route-request-required-capabilities request))))
    (cond ((not (string= (entry-scope entry) (route-request-scope request))) "out_of_scope")
          ((not (entry-enabled entry)) "disabled")
          ((and (route-request-failed-model-id request)
                (string= (entry-id entry) (route-request-failed-model-id request)))
           "already_failed")
          ((and (route-request-provider request)
                (not (string= (entry-provider entry) (route-request-provider request))))
           "provider_mismatch")
          ((and (route-request-model request) (not (matches-pin entry (route-request-model request))))
           "pin_mismatch")
          ((and required (not (subsetp required (capability-set entry) :test #'string=)))
           "missing_capabilities")
          ((< (entry-quality entry) (route-request-min-quality request)) "below_min_quality")
          ((> (entry-cost entry) (route-request-max-cost request)) "above_max_cost")
          ((> (entry-latency entry) (route-request-max-latency request)) "above_max_latency")
          ((< (entry-context-window entry) (route-request-min-context request)) "below_min_context")
          ((and require-keys (not (entry-has-key entry))) "missing_key")
          (t nil))))

;;; --- one pass ------------------------------------------------------------------

(defparameter *relevance-weight* 0.15
  "What a task's own words are worth against a strategy's numbers, as the
original weighted them.")

(defun micro (value)
  "VALUE rounded to the six places the original compared scores at."
  (round (rational value) 1/1000000))

(defun collect (entries request &key (judge-scores '()) (require-keys nil))
  "Order ENTRYs for REQUEST. => (values CANDIDATES REJECTED), the candidates
best first, REJECTED one (id . reason) pair a row, in catalog order."
  (let ((task-tokens (tokenize (route-request-task request)))
        (weight (coerce (route-request-judge-weight request) 'single-float))
        (relevance-weight (coerce *relevance-weight* 'single-float))
        (seen (make-hash-table :test #'equal))
        (candidates '())
        (rejected '()))
    (dolist (entry entries)
      (let ((duplicate (gethash (entry-id entry) seen)))
        (setf (gethash (entry-id entry) seen) t)
        (let ((reason (if duplicate "duplicate_input"
                           (hard-reject entry request require-keys))))
          (if reason
              (push (cons (entry-id entry) reason) rejected)
              (let* ((match (relevance task-tokens entry))
                     (base (strategy-score entry (route-request-prefer request)))
                     (judged (let ((row (assoc (entry-id entry) judge-scores :test #'string=)))
                               (and row (coerce (cdr row) 'single-float))))
                     (score (+ (* (- 1.0 relevance-weight) base)
                               (* relevance-weight (coerce match 'single-float))
                               (if judged (* weight judged) 0.0))))
                (push (make-candidate score match entry) candidates))))))
    (values
     (sort candidates
           (lambda (a b)
             (let ((score-a (micro (candidate-score a)))
                   (score-b (micro (candidate-score b))))
               (cond ((/= score-a score-b) (> score-a score-b))
                     (t (string< (entry-id (candidate-entry a))
                                  (entry-id (candidate-entry b))))))))
     (nreverse rejected))))

;;; --- the decision ---------------------------------------------------------------

(defun selection-of (candidate judge-scores prefer)
  "The row a caller reads, with the reasons it was chosen."
  (let* ((entry (candidate-entry candidate))
         (reasons (list (format nil "prefer=~a" prefer)
                        (format nil "quality=~,2f" (entry-quality entry))
                        (format nil "cost=~,2f" (entry-cost entry))
                        (format nil "latency=~,2f" (entry-latency entry))
                        (format nil "relevance=~,2f" (candidate-relevance candidate)))))
    (let ((row (assoc (entry-id entry) judge-scores :test #'string=)))
      (when row (setf reasons (append reasons (list (format nil "judge=~,2f" (cdr row)))))))
    (make-selection :entry entry :score (candidate-score candidate) :reasons reasons)))

(defun take-n (list n)
  (subseq list 0 (min (length list) (max n 0))))

(defun rank (entries request &key (judge-scores '()) (require-keys nil))
  "Decide for REQUEST over ENTRIES. => a ROUTE-RESULT."
  (multiple-value-bind (candidates rejected)
      (collect entries request :judge-scores judge-scores :require-keys require-keys)
    (let ((rows (loop for candidate in candidates
                      collect (selection-of candidate judge-scores (route-request-prefer request)))))
      (cond ((route-request-failed-model-id request)
             (after-failure request entries rows rejected))
            ((string= (route-request-prefer request) "named")
             (named request rows rejected))
            ((null rows)
             (make-route-result :decision "abstain" :rejected rejected
                                :reasons '("no_candidate") :abstain-reason "no_candidate"))
            (t (let ((top (first rows))
                     (rest (rest rows)))
                 (make-route-result
                  :decision "select" :selected top
                  :fallback (and (route-request-allow-fallback request) rest (first rest))
                  :alternatives (take-n rest (route-request-max-alternatives request))
                  :rejected rejected
                  :reasons (cons "best_match" (selection-reasons top)))))))))

(defun named (request rows rejected)
  "PREFER=named: the caller pinned a model, so that one or nothing. An
unpinned named call abstains and names what it could have taken."
  (let ((pin (route-request-model request))
        (limit (route-request-max-alternatives request)))
    (unless (and pin (plusp (length (nlk:trimmed pin))))
      (return-from named
        (make-route-result :decision "abstain" :alternatives (take-n rows limit)
                            :rejected rejected :reasons '("named_without_pin")
                            :abstain-reason "named_without_pin")))
    (let* ((chosen (find-if (lambda (row) (matches-pin (selection-entry row) pin)) rows))
           (rest (and chosen (remove chosen rows))))
      (unless chosen
        (return-from named
          (make-route-result :decision "abstain" :alternatives (take-n rows limit)
                              :rejected rejected :reasons '("named_unavailable")
                              :abstain-reason "named_unavailable")))
      (make-route-result :decision "select" :selected chosen
                          :fallback (and (route-request-allow-fallback request) rest (first rest))
                          :alternatives (take-n rest limit)
                          :rejected rejected
                          :reasons (cons "named_pin" (selection-reasons chosen))))))

(defun after-failure (request entries rows rejected)
  "One model just failed: answer the next one, one hop, honouring the failed
row's own fallback model before the ranking."
  (let* ((failed-id (route-request-failed-model-id request))
         (failed (find failed-id entries :key #'entry-id :test #'string=))
         (rejected (if failed rejected
                        (append rejected (list (cons failed-id "model_absent"))))))
    (unless (route-request-allow-fallback request)
      (return-from after-failure
        (make-route-result :decision "abstain" :rejected rejected
                            :reasons '("fallback_disabled") :abstain-reason "fallback_disabled")))
    (let* ((preferred (and failed (entry-fallback-id failed)))
           (chosen (and preferred
                         (find preferred rows :key (lambda (row) (selection-id row))
                                             :test #'string=))))
      (when (and preferred (null chosen))
        (setf rejected (append rejected (list (cons preferred "fallback_unavailable")))))
      (setf chosen (or chosen (first rows)))
      (unless chosen
        (return-from after-failure
          (make-route-result :decision "abstain" :rejected rejected
                              :reasons '("no_fallback") :abstain-reason "no_fallback")))
      (make-route-result :decision "fallback" :selected chosen
                          :alternatives (take-n (remove chosen rows)
                                                 (route-request-max-alternatives request))
                          :rejected rejected
                          :reasons (append (list (format nil "failed:~a" failed-id)
                                                 "bounded_fallback")
                                           (selection-reasons chosen))))))

(defun selection-id (selection)
  (entry-id (selection-entry selection)))

;;; --- what a caller and the model read ---------------------------------------------

(defun selection-object (selection)
  "One selection as the JSON object the original's to_dict wrote."
  (let ((entry (selection-entry selection)))
    (nlk:make-json-object
     "id" (entry-id entry)
     "name" (entry-name entry)
     "provider" (entry-provider entry)
     "model" (entry-model entry)
     "base_url" (entry-base-url entry)
     "api_key_env" (entry-key-env entry)
     "has_key" (entry-has-key entry)
     "quality" (entry-quality entry)
     "cost" (entry-cost entry)
     "latency" (entry-latency entry)
     "capabilities" (coerce (entry-capabilities entry) 'vector)
     "context_window" (entry-context-window entry)
     "score" (selection-score selection)
     "reasons" (coerce (selection-reasons selection) 'vector))))

(defun result-object (result &key (show-rejected t))
  "RESULT as the JSON object a tool call answers."
  (let ((object
          (nlk:make-json-object
           "decision" (route-result-decision result)
           "selected" (if (route-result-selected result)
                          (selection-object (route-result-selected result)) :null)
           "fallback" (if (route-result-fallback result)
                          (selection-object (route-result-fallback result)) :null)
           "alternatives" (coerce (mapcar #'selection-object (route-result-alternatives result))
                                   'vector)
           "reasons" (coerce (route-result-reasons result) 'vector)
           "abstain_reason" (or (route-result-abstain-reason result) :null))))
    (when show-rejected
      (setf (gethash "rejected" object)
            (coerce (loop for (id . reason) in (route-result-rejected result)
                          collect (nlk:make-json-object "id" id "reason" reason))
                    'vector)))
    object))

(defun result-json (result &key (show-rejected t))
  (nlk:encode-json-object (result-object result :show-rejected show-rejected)))

(defun selection-line (selection &key (label "chosen"))
  "One selection as the lines /route prints: what it is, where it is reached,
which variable holds its key, and why it won."
  (let ((entry (selection-entry selection)))
    (format nil "~a: ~a  score ~,3f~%  POST ~a~%  key: ~a~@[ (set)~]~%  why: ~{~a~^, ~}"
            label (entry-ref entry) (selection-score selection)
            (entry-base-url entry) (entry-key-env entry)
            (and (find-if #'env-set-p (entry-key-envs entry)) t)
            (selection-reasons selection))))

(defun result-text (result)
  "RESULT as the lines an operator reads: what was chosen, why, what came
next, and what was thrown out."
  (with-output-to-string (out)
    (format out "decision: ~a~%" (route-result-decision result))
    (if (route-result-selected result)
        (progn
          (format out "~a~%" (selection-line (route-result-selected result)))
          (when (route-result-fallback result)
            (format out "fallback: ~a~%" (selection-id (route-result-fallback result))))
          (dolist (row (route-result-alternatives result))
            (format out "next: ~a  score ~,3f~%" (selection-id row) (selection-score row))))
        (format out "nothing to choose: ~a~%" (or (route-result-abstain-reason result) "unknown")))
    (when (route-result-reasons result)
      (format out "reasons: ~{~a~^, ~}~%" (route-result-reasons result)))
    (let ((rejected (route-result-rejected result)))
      (when rejected
        (format out "rejected ~d: ~{~a~^; ~}~%" (length rejected)
                (loop for (id . reason) in rejected
                      collect (format nil "~a (~a)" id reason)))))))

(defun catalog-lines (entries)
  "ENTRIES as the table `jev-model llms` printed: one line a model, with the
variable that would make it callable and whether this process holds it."
  (with-output-to-string (out)
    (let ((providers (remove-duplicates (mapcar #'entry-provider entries) :test #'string=)))
      (format out "~d models in ~d providers, ~d callable here~%"
              (length entries) (length providers)
              (count-if #'entry-has-key entries)))
    (dolist (entry entries)
      (format out "  ~a~%    q ~,2f  cost ~,2f  lat ~,2f  ~d ctx  ~a~@[ (set)~]~%"
              (entry-id entry) (entry-quality entry) (entry-cost entry) (entry-latency entry)
              (entry-context-window entry) (entry-key-env entry)
              (and (find-if #'env-set-p (entry-key-envs entry)) t)))))
