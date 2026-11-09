;;;; routing.lisp --- the operator's routing table, read from a JSON file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Mirrors config.py of the original: camelCase and snake_case both read,
;;;; a tier the file leaves empty stays empty, and the loader never invents a
;;;; model list. Without a host catalogue every configured row is eligible,
;;;; which is the catalog edition the original ships.

(in-package #:nodecode-jev-routing)

;;; --- JSON walking ---------------------------------------------------------------

(defun jget (object &rest path)
  "Walk PATH down JSON objects and arrays; NIL when a step is missing."
  (let ((value object))
    (dolist (key path value)
      (cond ((hash-table-p value)
             (multiple-value-bind (inner present) (gethash key value)
               (setf value (and present inner))))
            ((and (vectorp value) (not (stringp value)) (integerp key))
             (setf value (and (< -1 key (length value)) (aref value key))))
            (t (setf value nil))))))

(defun jkey (object &rest names)
  "The first NAME OBJECT carries -- camelCase, then snake_case -- else NIL."
  (loop for name in names
        when (and (hash-table-p object) (nth-value 1 (gethash name object)))
          return (gethash name object)))

(defun jnum (value)
  "VALUE as a double when it is a JSON number, else NIL (a boolean is not one)."
  (and (numberp value) (float value 1d0)))

(defun jstr (value)
  "VALUE as a non-empty string, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun jlist (raw)
  "RAW, a JSON array, as a list; NIL when it is not one."
  (and (vectorp raw) (not (stringp raw)) (coerce raw 'list)))

;;; --- the table ------------------------------------------------------------------

(defstruct (target (:constructor make-target))
  "One row of a chain: the provider and model the operator would run."
  (provider "" :read-only t)
  (model "" :read-only t)
  thinking
  min-tier
  (priority 0 :read-only t))

(defstruct (router (:constructor make-router))
  "The routing table, as the file states it."
  path
  (routes (make-hash-table :test 'equal))
  (kind-models (make-hash-table :test 'equal))
  (kind-minimum-tier (make-hash-table :test 'equal))
  (pricing (make-hash-table :test 'equal))
  (task-kinds (make-hash-table :test 'equal))
  (confidence-threshold 0.34d0)
  (min-prompt-chars 12)
  (soft-ratio 0.7d0)
  (hard-ratio 0.9d0)
  daily
  monthly
  (free-enabled nil)
  (free-policy "fallback-only")
  free-pool)

(defparameter +tiers+ #("quick" "standard" "high" "premium" "xpremium")
  "The capability tiers, lowest first. A tier name outside this reads as standard.")

(defparameter +task-kinds+
  '(("plan" . "Deciding what to build, sequencing work, or designing an approach before editing")
    ("implement" . "Writing or changing code, scripts, or configuration to produce a concrete result")
    ("write" . "Producing prose, documentation, comments, or other non-code content from scratch")
    ("debug" . "Diagnosing a failure, error, or unexpected behavior and finding its root cause")
    ("refactor" . "Restructuring existing code without changing intended behavior")
    ("review" . "Auditing code, a diff, a document, or a plan for problems and risks")
    ("research" . "Searching, reading, and synthesizing external information or unfamiliar APIs")
    ("explain" . "Answering a question or explaining how something works")
    ("operate" . "Running commands, tooling, git, deploys, or environment setup")
    ("chat" . "Small talk, acknowledgements, or a request with no real work attached"))
  "The task kinds the routing question offers, as the original states them.")

(defun target-of (object)
  "OBJECT as a TARGET, or NIL when it names no model."
  (let ((model (jstr (jkey object "model"))))
    (when model
      (make-target :provider (or (jstr (jkey object "provider")) "")
                   :model model
                   :thinking (jstr (jkey object "thinkingLevel" "thinking_level"))
                   :min-tier (jstr (jkey object "minTier" "min_tier"))
                   :priority (round (or (jnum (jkey object "priority")) 0d0))))))

(defun chain-of (raw)
  "RAW, a JSON array of rows, as a list of TARGETs."
  (loop for item in (jlist raw)
        for target = (and (hash-table-p item) (target-of item))
        when target collect target))

(defun expand-tilde (path)
  "PATH with a leading ~ or ~/ read as the home directory."
  (cond ((equal path "~") (namestring (user-homedir-pathname)))
        ((and (> (length path) 1) (char= (char path 0) #\~)
              (member (char path 1) '(#\/ #\\)))
         (namestring (merge-pathnames (subseq path 2) (user-homedir-pathname))))
        (t path)))

(defun config-path ()
  "The routing JSON the section names, or a refusal saying where to set one."
  (or (let ((path (jstr (setting :config-path))))
        (and path (expand-tilde path)))
      (fail "no routing table yet: set config_path in the jev-routing section of the config to the routing JSON, then (restart-cells \"nodecode-jev-routing\")")))

(defun load-router (path)
  "PATH as a ROUTER; a file that cannot be read or is not a JSON object refuses."
  (let ((raw (handler-case (nlk:decode-json (uiop:read-file-string path))
               (error (condition)
                 (fail "the routing table ~a could not be read: ~a" path condition)))))
    (unless (hash-table-p raw)
      (fail "the routing table ~a is not a JSON object" path))
    (let* ((router (make-router :path path))
           (routes (or (jkey raw "routes") (make-hash-table :test 'equal)))
           (kinds (or (jkey raw "kindModels" "kind_models") (make-hash-table :test 'equal)))
           (floors (or (jkey raw "kindMinimumTier" "kind_minimum_tier") (make-hash-table :test 'equal)))
           (budget (or (jkey raw "budget") (make-hash-table :test 'equal)))
           (free (or (jkey raw "free") (make-hash-table :test 'equal)))
           (kinds-raw (or (jkey raw "taskKinds" "task_kinds") (make-hash-table :test 'equal))))
      (dolist (tier (coerce +tiers+ 'list))
        (setf (gethash tier (router-routes router)) (chain-of (jkey routes tier))))
      (when (hash-table-p kinds)
        (maphash (lambda (kind chain) (setf (gethash kind (router-kind-models router)) (chain-of chain)))
                 kinds))
      (when (hash-table-p floors)
        (maphash (lambda (kind tier)
                   (when (stringp tier) (setf (gethash kind (router-kind-minimum-tier router)) tier)))
                 floors))
      (dolist (pair +task-kinds+)
        (setf (gethash (car pair) (router-task-kinds router)) (cdr pair)))
      (when (hash-table-p kinds-raw)
        (maphash (lambda (kind text)
                   (when (jstr text) (setf (gethash kind (router-task-kinds router)) (jstr text))))
                 kinds-raw))
      (setf (router-confidence-threshold router)
            (or (jnum (jkey raw "confidenceThreshold" "confidence_threshold")) 0.34d0)
            (router-min-prompt-chars router)
            (round (or (jnum (jkey raw "minPromptChars" "min_prompt_chars")) 12d0))
            (router-soft-ratio router)
            (float (or (jnum (jkey budget "softRatio" "soft_ratio")) 0.7d0) 1d0)
            (router-hard-ratio router)
            (float (or (jnum (jkey budget "hardRatio" "hard_ratio")) 0.9d0) 1d0)
            (router-daily router)
            (let ((value (jnum (jkey budget "dailyUsd" "daily_usd"))))
              (and value (plusp value) value))
            (router-monthly router)
            (let ((value (jnum (jkey budget "monthlyUsd" "monthly_usd"))))
              (and value (plusp value) value))
            (router-free-enabled router) (and (jkey free "enabled") t)
            (router-free-policy router)
            (or (jstr (jkey free "policy")) "fallback-only")
            (router-free-pool router) (chain-of (jkey free "pool")))
      (let ((pricing (jkey raw "pricing")))
        (when (hash-table-p pricing)
          (maphash (lambda (key row)
                     (when (and (hash-table-p row) (search "/" key))
                       (setf (gethash key (router-pricing router))
                             (list :input (jnum (jkey row "input"))
                                   :output (jnum (jkey row "output"))
                                   :cache-read (jnum (jkey row "cache_read" "cached"))
                                   :cache-write (jnum (jkey row "cache_write"))))))
                   pricing)))
      router)))

(defun pricing-of (router provider model)
  "The operator's per-million rates for PROVIDER/MODEL, or NIL when unpriced."
  (gethash (format nil "~a/~a" (or provider "") (or model "")) (router-pricing router)))
