;;;; cell-test.lisp --- the fallback cell against canned catalogs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every candidate is a canned (PROVIDER . MODEL) pair, the catalog reader is
;;;; off, the chain reader answers a chain already present (so nothing seeds
;;;; unless a test says so) and the writer records instead of writing: no test
;;;; depends on a provider being configured, on models.dev, or on the operator's
;;;; config.

(in-package #:nodecode.test)

(define-test-slice "fallback" "FALLBACK-CELL-" :start nodecode-fallback:start-cell)

;;; --- the fixture ------------------------------------------------------------------

(defparameter +fb-models+
  '(("openrouter" . "meta-llama/llama-3.3-70b-instruct")
    ("openrouter" . "qwen/qwen3-coder:free")
    ("gemini" . "gemini-2.5-flash")
    ("gemini" . "gemini-2.5-pro")
    ("openai" . "gpt-4o-mini")
    ("openai" . "gpt-5.4"))
  "The candidates every test ranks: a 70B with a small window, a 1M-window flash
and pro pair, a free coder, an 8B mini and a version whose id carries no size.")

(defvar *fb-writes* nil
  "What this test's stub writer recorded, newest first: a list of entry lists.")

(defmacro with-fallback-fixture (&body body)
  "BODY with the canned candidates as the only source, no catalog, a chain
already present, and a writer that records into *FB-WRITES*."
  `(let* ((writes (list nil))
          (models (copy-list +fb-models+))
          (nodecode-fallback:*candidates* (lambda () (copy-list models)))
          (nodecode-fallback:*catalog* nil)
          (nodecode-fallback:*chain-reader*
            (lambda () (list (list :provider "openai" :model "gpt-5.4"))))
          (nodecode-fallback:*chain-writer*
            (lambda (entries)
              (setf (car writes) (cons entries (car writes)))
              (format nil "failover.chain: set ~d entr~:p" (length entries)))))
     (let ((*fb-writes* writes))
       ,@body)))

;;; --- the lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "fallback"
  (:fixture with-fallback-fixture)
  (:help :fallback)
  (:command "fallback")
  (:running
   (is (nle::find-tool "fallback_resolve") "the ranking tool is registered")
   (is (nle::find-tool "fallback_apply") "the chain tool is registered")
   (is (nle::find-tool "fallback_status") "the status tool is registered")
   (is (equal "largest_parameter_count" (nodecode-fallback::setting :sort-by)) "ranking default"))
  (:stopped
   (is (null (nle::find-tool "fallback_resolve")) "stop unregisters the tools")
   (is (null (nle::find-registered-command "fallback")) "and takes /fallback out"))
  (:refused ("sort_by" "fastest") ("max_candidates" 0))
  (:idle fallback:fallback-error (fallback:resolve)))

;;; --- the ranking ------------------------------------------------------------------

(deftest fallback-cell-ranks-by-size-then-window ()
  "The default order: the biggest, the longer window breaking a tie, each line
saying where its number came from."
  (with-fallback-fixture
    (with-cell-stop ((fallback-start))
      (let ((text (fallback:resolve)))
        (is (search "1. gemini/gemini-2.5-pro" text) "70B and a 1M window wins the tie on window")
        (is (search "2. openrouter/meta-llama/llama-3.3-70b-instruct" text) "the other 70B follows")
        (is (search "context 1M" text) "a window is shown as it was published")
        (is (search "70.0B" text) "and a size too")
        (is (search "ranked by largest parameter count" text) "the strategy is named"))
      (let ((latest (fallback:resolve :sort-by "latest")))
        (is (search "1. openai/gpt-5.4" latest) "latest ranks by version, no date needed")))))

(deftest fallback-cell-filters-by-context-price-and-size ()
  "A criterion that needs a number leaves an unknown one out, and free_only keeps
what the id marks free."
  (with-fallback-fixture
    (with-cell-stop ((fallback-start))
      (let ((text (fallback:resolve :min-context "200k")))
        (is (search "gemini-2.5-pro" text))
        (is (search "gpt-5.4" text))
        (is (null (search "llama-3.3-70b" text)) "a 128k window is under 200k"))
      (let ((text (fallback:resolve :free-only t)))
        (is (search "qwen/qwen3-coder:free" text))
        (is (search "1. openrouter/qwen/qwen3-coder:free" text) "the free model ranks first")
        (is (null (search "2. " text)) "and is the only one"))
      (let ((text (fallback:resolve :provider "gemini")))
        (is (null (search "openrouter" text)) "one provider searches one provider")
        (is (search "gemini-2.5-flash" text)))
      (is (null (search "gemini" (fallback:resolve :exclude-providers '("gemini")))) "excluded")
      (is (search "No model survives" (fallback:resolve :min-params "500b")) "nothing survives"))))

;;; --- the chain ---------------------------------------------------------------------

(deftest fallback-cell-writes-the-chain-it-ranked ()
  (with-fallback-fixture
    (with-cell-stop ((fallback-start))
      (let ((text (fallback:apply-chain :max-candidates 2)))
        (is (search "failover.chain" text) "the answer names the key it wrote")
        (is (= 2 (length (first (car *fb-writes*)))) "two entries, as asked")
        (is (equal "gemini" (getf (first (first (car *fb-writes*))) :provider)) "best first"))
      (is (search "no model survives" (refusal-text fallback:fallback-error
                                         (fallback:apply-chain :min-params "500b"))) "refused"))))

(deftest fallback-cell-seeds-an-empty-chain-once ()
  "auto_sync fills the chain at start when the config holds none, and a chain
somebody wrote is never touched."
  (with-fallback-fixture
    (let ((nodecode-fallback:*chain-reader* (lambda () nil)))
      (with-cell-stop ((fallback-start))
        (is (await (:timeout 3) (car *fb-writes*)) "the empty chain is filled at start")
        (is (= 3 (length (first (car *fb-writes*)))) "with the section's three")
        (is (equal "gemini" (getf (first (first (car *fb-writes*))) :provider)) "best first"))))
  (with-fallback-fixture
    (with-cell-stop ((fallback-start))
      (sleep 0.3)
      (is (null (car *fb-writes*)) "a chain already in the config is left alone"))))

(deftest fallback-cell-writes-the-shared-configs-chain ()
  "The real writer: the key the engine walks, written and taken out again."
  (let* ((file (merge-pathnames "config.jsonc"
                                (uiop:ensure-directory-pathname (temp-path "fallback-chain"))))
         (nodecode.evolved::*shared-config-path* file))
    (funcall nodecode-fallback:*chain-writer*
             (list (list :provider "openrouter" :model "qwen/qwen3-coder:free")))
    (let ((chain (nodecode-fallback::current-chain)))
      (is (= 1 (length chain)) "one entry in failover.chain")
      (is (equal "openrouter" (getf (first chain) :provider)) "the provider is written")
      (is (equal "qwen/qwen3-coder:free" (getf (first chain) :model)) "and the id exact"))
    (funcall nodecode-fallback:*chain-writer* nil)
    (is (null (nodecode-fallback::current-chain)) "clearing takes the key out")))

;;; --- the surfaces the model and the operator use ----------------------------------------

(deftest fallback-cell-tools-and-command-answer ()
  (with-fallback-fixture
    (with-cell-stop ((fallback-start))
      (let ((text (fb-tool "fallback_resolve" (nlk:json-object "min_params" "30b"
                                                              "sort_by" "greatest_context"))))
        (is (search "gemini/gemini-2.5-pro" text) "the schema's members reach the ranking")
        (is (null (search "gpt-4o-mini" text)) "and its filters hold"))
      (let ((text (fb-tool "fallback_apply" (nlk:json-object "max_candidates" 2))))
        (is (search "failover.chain" text) "the tool wrote the chain")
        (is (= 2 (length (first (car *fb-writes*)))) "two entries"))
      (is (search "chain" (fb-tool "fallback_status" (nlk:json-object))) "status answers")
      (is (search "ranked by" (fallback-slash "resolve min_params=30b")) "the slash command ranks")
      (is (search "failover.chain" (fallback-slash "apply count=2")) "and applies")
      (is (search "(none" (fallback-slash "clear")) "and clears")
      (is (search "/fallback nope" (refusal-text fallback:fallback-error (fallback-slash "nope"))) "names") )))

;;; --- helpers -----------------------------------------------------------------------------

(defun fb-tool (name object)
  "Call the registered tool NAME as the engine does: the argument object through
the tool's own schema."
  (let* ((entry (nle::find-tool name))
         (schema (second entry))
         (function (third entry)))
    (funcall function
             (if (or (hash-table-p schema) (and (consp schema) (keywordp (first schema))))
                 object
                 (nlk:contract-decode schema object)))))

(defun fallback-slash (args)
  "One /fallback LINE, as the shell types it."
  (nodecode-fallback::run-command args "fallback-test-session"))
