;;;; select.lisp --- the selector: tokens, scores, budget, abstention, one hop.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A deterministic, local, lexical ranking, carried from the original's
;;;; HeuristicSelector: the query's words against a tool's words, a score that
;;;; rewards overlap and lightly rewards a small tool, a hard budget, an
;;;; abstention when nothing clears the floor, and one bounded fallback hop.
;;;; No network, no model, no execution: one catalog and one query always
;;;; answer the same selection, and the reasons say why.
;;;;
;;;; The access policies come before the score, as the original insists: a
;;;; required or forbidden permission, a scope, a disabled flag and the tool
;;;; that already failed are hard rejections, and nothing about a query can
;;;; hand a tool a permission. Selection is not authorization.

(in-package #:nodecode-mcp-router)

;;; --- a candidate, as the catalog declares it ------------------------------------

(defstruct (tool-profile (:copier nil))
  "One tool a catalog offers: what the selector ranks, never what it runs."
  (id "" :type string)
  (name "" :type string)
  (description "" :type string)
  (server "" :type string)
  (tags '() :type list)
  (tokens 80 :type integer)
  (permissions '() :type list)
  (enabled t)
  (fallback-id nil)
  (max-calls 8 :type integer)
  (scope "default" :type string))

(defstruct (candidate (:copier nil))
  "One tool that survived the hard rejections, with the numbers it was ranked on."
  score relevance efficiency tool reasons)

;;; --- words ---------------------------------------------------------------------

(defparameter +stopwords+
  (let ((table (make-hash-table :test #'equal)))
    (dolist (word (uiop:split-string
                   "au aux avec ce ces dans de des du elle en et eux il je la le les leur lui ma
                    mais me meme mes moi mon ne nos notre nous on ou par pas pour qu que qui sa se
                    ses son sur ta te tes toi ton tu un une vos votre vous c d j l a m n s t y ete
                    the a an and or of to in for with without is are was were be been this that"
                   :separator " "))
      (when (plusp (length word))
        (setf (gethash word table) t)))
    table)
  "Words the lexical match ignores: the original's French and English list (ete for été).")

(defun extra-stopword-p (word)
  "T when the running section's extra_stopwords names WORD; nothing while the cell is stopped."
  (let ((settings *mcp-router*))
    (and settings
         (member word (getf settings :extra-stopwords) :test #'equal)
         t)))

(defun stopword-p (word)
  (or (gethash word +stopwords+) (extra-stopword-p word)))

(defun tokenize (text)
  "TEXT's words, lowercased, without the stopwords and without repeats: the one
lexical signal both a query and a tool are read into."
  (let ((words '()) (seen (make-hash-table :test #'equal))
        (text (or text "")) (start nil))
    (labels ((emit (end)
               (when start
                 (let ((word (string-downcase (subseq text start end))))
                   (when (and (or (> (length word) 1)
                                  (every #'digit-char-p word))
                              (not (stopword-p word))
                              (not (gethash word seen)))
                     (setf (gethash word seen) t)
                     (push word words)))
                 (setf start nil))))
      (loop for index from 0 below (length text)
            for character = (char text index)
            do (if (alphanumericp character)
                   (unless start (setf start index))
                   (emit index)))
      (emit (length text)))
    (nreverse words)))

(defun tool-words (tool)
  "The words a tool is matched by: its name, description and server, plus every tag whole."
  (let ((words '()) (seen (make-hash-table :test #'equal)))
    (flet ((add (word)
             (when (and (plusp (length word)) (not (gethash word seen)))
               (setf (gethash word seen) t)
               (push word words))))
      (dolist (word (tokenize (format nil "~a ~a ~a"
                                       (tool-profile-name tool)
                                       (tool-profile-description tool)
                                       (tool-profile-server tool))))
        (add word))
      (dolist (tag (tool-profile-tags tool))
        (let ((tag (string-downcase (nlk:trimmed tag))))
          (dolist (word (tokenize tag)) (add word))
          (add tag))))
    (nreverse words)))

(defun token-efficiency (tokens)
  "A light inversion of a tool's cost: a smaller tool is slightly preferred, never ahead of a match."
  (/ 1.0 (+ 1.0 (/ tokens 100.0))))

;;; --- the catalog a call carries -------------------------------------------------

(defun strings-of (object key)
  "OBJECT's members KEY as a list of strings: a JSON array, or the comma-separated
text the original also accepted."
  (multiple-value-bind (value present) (nlk:json-value object :any key)
    (cond ((not present) '())
          ((stringp value)
           (remove "" (mapcar #'nlk:trimmed (uiop:split-string value :separator ","))
                   :test #'equal))
          ((vectorp value) (loop for item across value when (stringp item) collect item))
          (t '()))))

(defun string-of (object key)
  (nlk:trimmed (or (nlk:json-value object :string key) "")))

(defun profile-of (given)
  "GIVEN -- a JSON object or a tool profile -- as a candidate's tool. A catalog that
breaks the declaration refuses here, before anything is ranked."
  (cond
    ((tool-profile-p given) given)
    (t
     (unless (hash-table-p given)
       (fail "every candidate tool is a JSON object with at least an id; got ~s" given))
     (let* ((id (string-of given "id"))
            (name (string-of given "name"))
            (tokens (or (nlk:json-value given :integer "tokens") 80))
            (max-calls (or (nlk:json-value given :integer "max_calls") 8))
            (fallback (string-of given "fallback_id")))
       (unless (plusp (length id))
         (fail "a candidate tool has no id: give each one the name its MCP server registers it under"))
       (unless (>= tokens 1)
         (fail "tool ~a declares tokens ~a: every tool costs at least 1" id tokens))
       (unless (>= max-calls 1)
         (fail "tool ~a declares max_calls ~a: give it at least 1" id max-calls))
       (make-tool-profile
        :id id
        :name (if (plusp (length name)) name id)
        :description (string-of given "description")
        :server (string-of given "server")
        :tags (strings-of given "tags")
        :tokens tokens
        :permissions (strings-of given "permissions")
        :enabled (multiple-value-bind (value present)
                     (nlk:json-value given :boolean "enabled")
                   (if present (and value t) t))
        :fallback-id (if (plusp (length fallback)) fallback nil)
        :max-calls max-calls
        :scope (let ((scope (string-of given "scope")))
                 (if (plusp (length scope))
                     scope
                     (let ((namespace (string-of given "namespace")))
                       (if (plusp (length namespace)) namespace "default")))))))))

;;; --- the access policies, ahead of the score ------------------------------------

(defun normalized (values)
  "VALUES as lowercased, trimmed permission names."
  (loop for value in (coerce (or values '()) 'list)
        for clean = (string-downcase (nlk:trimmed value))
        when (plusp (length clean)) collect clean))

(defun hard-reject (tool scope required forbidden failed-id)
  "Why TOOL cannot be selected at all, or NIL: the answer no score may undo."
  (let ((permissions (normalized (tool-profile-permissions tool))))
    (cond ((not (equal (tool-profile-scope tool) scope)) "out_of_scope")
          ((not (tool-profile-enabled tool)) "disabled")
          ((and failed-id (equal failed-id (tool-profile-id tool))) "already_failed")
          ((and required (null (subsetp required permissions :test #'equal)))
           "missing_permissions")
          ((and forbidden (intersection forbidden permissions :test #'equal))
           "forbidden_permissions")
          (t nil))))

(defun score-tool (tool query-words min-relevance)
  "TOOL scored against QUERY-WORDS: a candidate, or the reason it is below the floor."
  (let* ((words (tool-words tool))
         (relevance (if query-words
                         (/ (float (length (intersection query-words words :test #'equal)))
                            (float (length query-words)))
                         0.0))
         (efficiency (token-efficiency (tool-profile-tokens tool))))
    (if (and query-words (< relevance min-relevance))
        "low_relevance"
        (make-candidate :score (+ (* 0.7 relevance) (* 0.3 efficiency))
                         :relevance relevance
                         :efficiency efficiency
                         :tool tool))))

(defun collect-candidates (given scope required forbidden failed-id query-words min-relevance)
  "The candidates that clear the policies and the floor, the rejections, and the catalog
BY-ID."
  (let ((candidates '()) (rejections '())
        (by-id (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal)))
    (dolist (entry (coerce (or given '()) 'list))
      (let* ((tool (profile-of entry))
             (id (tool-profile-id tool))
             (reason (if (gethash id seen)
                         "duplicate_input"
                         (progn
                           (setf (gethash id seen) t (gethash id by-id) tool)
                           (hard-reject tool scope required forbidden failed-id)))))
        (if reason
            (push (cons id reason) rejections)
            (let ((scored (score-tool tool query-words min-relevance)))
              (if (stringp scored)
                  (push (cons id scored) rejections)
                  (push scored candidates))))))
    (values (sort (nreverse candidates) #'candidate-before-p) (nreverse rejections) by-id)))

;;; --- the budget -----------------------------------------------------------------

(defun row-id (row)
  (tool-profile-id (candidate-tool row)))

(defun candidate-before-p (left right)
  "The original's order: the higher score first, ties by id, compared as rounded."
  (let ((left-score (round (* 1.0d6 (candidate-score left))))
        (right-score (round (* 1.0d6 (candidate-score right)))))
    (if (= left-score right-score)
        (string< (row-id left) (row-id right))
        (> left-score right-score))))

(defun selected-of (row)
  "ROW as a selection: its numbers written as the reasons it was chosen."
  (let ((tool (candidate-tool row)))
    (setf (candidate-reasons row)
          (list (format nil "relevance=~,2f" (candidate-relevance row))
                (format nil "tokens=~a" (tool-profile-tokens tool))
                (format nil "efficiency=~,2f" (candidate-efficiency row))
                (format nil "max_calls=~a" (tool-profile-max-calls tool))))
    row))

(defun apply-budget (rows max-tools budget-tokens)
  "Take ROWS in order until MAX-TOOLS or BUDGET-TOKENS is reached.
=> (values SELECTED REJECTIONS TOKENS), the rejections max_tools and budget."
  (let ((selected '()) (rejections '()) (total 0))
    (dolist (row rows)
      (let ((tool (candidate-tool row)))
        (cond ((>= (length selected) max-tools)
               (push (cons (row-id row) "max_tools") rejections))
              ((> (+ total (tool-profile-tokens tool)) budget-tokens)
               (push (cons (row-id row) "budget") rejections))
              (t
               (push (selected-of row) selected)
               (incf total (tool-profile-tokens tool))))))
    (values (nreverse selected) (nreverse rejections) total)))

(defun preview-of (selected rows)
  "The top choice's declared fallback, when the policies left it standing and it is not
already chosen: the original's one preview, never a second selection."
  (when selected
    (let* ((top (find (row-id (first selected)) rows :key #'row-id :test #'equal))
           (fallback-id (and top (tool-profile-fallback-id (candidate-tool top))))
           (taken (mapcar #'row-id selected)))
      (when (and fallback-id (not (member fallback-id taken :test #'equal)))
        (let ((row (find fallback-id rows :key #'row-id :test #'equal)))
          (and row (selected-of row)))))))

(defun order-by-preferred (rows preferred rejections)
  "ROWS with PREFERRED first; REJECTIONS gains fallback_unavailable when the catalog lacks it."
  (if (null preferred)
      (values rows rejections)
      (let ((row (find preferred rows :key #'row-id :test #'equal)))
        (if row
            (values (cons row (remove preferred rows :key #'row-id :test #'equal)) rejections)
            (values rows (cons (cons preferred "fallback_unavailable") rejections))))))

;;; --- the answer -----------------------------------------------------------------

(defun selected-json (row)
  (let ((tool (candidate-tool row)))
    (nlk:json-object
     "id" (tool-profile-id tool)
     "name" (tool-profile-name tool)
     "server" (tool-profile-server tool)
     "score" (candidate-score row)
     "tokens" (tool-profile-tokens tool)
     "reasons" (coerce (candidate-reasons row) 'vector)
     "permissions" (coerce (tool-profile-permissions tool) 'vector)
     "max_calls" (tool-profile-max-calls tool))))

(defun rejected-json (rejections)
  (coerce (loop for (id . reason) in rejections
                collect (nlk:json-object "id" id "reason" reason))
          'vector))

(defun result-json (decision &key selected fallback rejections (total 0) reasons abstain
                                 (include-rejected t))
  "One selection as the JSON object the tool answers with. A selection that has no
fallback, or no abstention to explain, carries no key for one."
  (nlk:json-object
   "decision" decision
   "selected" (coerce (mapcar #'selected-json selected) 'vector)
   :opt "fallback" (and fallback (selected-json fallback))
   "total_tokens" total
   "reasons" (coerce (coerce reasons 'list) 'vector)
   :when include-rejected "rejected" (rejected-json rejections)
   :opt "abstain_reason" abstain))

;;; --- one selection --------------------------------------------------------------

(defun select-tools (query candidates &key (max-tools 8) (budget-tokens 800)
                                    (min-relevance 0.15)
                                    required-permissions forbidden-permissions
                                    failed-tool-id (allow-fallback t) (scope "default")
                                    (include-rejected t))
  "Select from CANDIDATES -- JSON objects, or tool profiles -- the tools that fit QUERY
inside MAX-TOOLS and BUDGET-TOKENS. => a JSON object: decision (select, fallback or
abstain), selected, an optional fallback preview, total_tokens, reasons, the rejected
candidates with their causes, and abstain_reason. Nothing is executed, and nothing is
authorized: a permission is what the catalog declares, never what a query asks for."
  (unless (<= 0.0 min-relevance 1.0)
    (fail "min_relevance is ~a: give a number between 0.0 and 1.0" min-relevance))
  (unless (plusp max-tools)
    (fail "max_tools is ~a: give at least 1" max-tools))
  (unless (plusp budget-tokens)
    (fail "budget_tokens is ~a: give at least 1" budget-tokens))
  (let ((query-words (tokenize (or query "")))
        (required (normalized required-permissions))
        (forbidden (normalized forbidden-permissions)))
    (multiple-value-bind (rows rejections by-id)
        (collect-candidates candidates scope required forbidden failed-tool-id
                             query-words min-relevance)
      (cond
        ((null failed-tool-id)
         (multiple-value-bind (selected more total)
             (apply-budget rows max-tools budget-tokens)
           (let ((rejections (append rejections more)))
             (if (null selected)
                 (result-json "abstain" :rejections rejections
                               :reasons '("no_candidate") :abstain "no_candidate"
                               :include-rejected include-rejected)
                 (result-json "select" :selected selected
                               :fallback (preview-of selected rows)
                               :rejections rejections :total total
                               :reasons (cons "best_match" (candidate-reasons (first selected)))
                               :include-rejected include-rejected)))))
        ((not allow-fallback)
         (result-json "abstain" :rejections rejections
                       :reasons '("fallback_disabled") :abstain "fallback_disabled"
                       :include-rejected include-rejected))
        (t
         ;; One hop, and only one: the failed tool's own fallback first, then the rest.
         (let* ((failed (gethash failed-tool-id by-id))
                (preferred (and failed (tool-profile-fallback-id failed))))
           (multiple-value-bind (ordered rejections)
               (order-by-preferred rows preferred
                                   (if failed rejections
                                       (cons (cons failed-tool-id "tool_absent") rejections)))
             (multiple-value-bind (selected more total)
                 (apply-budget ordered max-tools budget-tokens)
               (let ((rejections (append rejections more)))
                 (if (null selected)
                     (result-json "abstain" :rejections rejections
                                   :reasons '("no_fallback") :abstain "no_fallback"
                                   :include-rejected include-rejected)
                     (result-json "fallback" :selected selected :rejections rejections
                                   :total total
                                   :reasons (cons (format nil "failed:~a" failed-tool-id)
                                                  (cons "bounded_fallback"
                                                        (candidate-reasons (first selected))))
                                   :include-rejected include-rejected)))))))))))
