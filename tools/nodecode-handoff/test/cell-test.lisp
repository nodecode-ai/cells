;;;; cell-test.lisp --- the four actions, the page they file, /handoff.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing is stubbed and nothing dials: a handoff is a folder under the
;;;; scratch home this run owns, so a test files and reads real documents.
;;;; What is proved is the wiring the loader makes -- the tool, the command,
;;;; the manual, the refusals -- and that a filed handoff is canonical,
;;;; scrubbed, immutable, and still there after a stop and a start.

(in-package #:nodecode.test)

(define-test-slice "handoff" "HANDOFF-CELL-" :recipe "handoff"
  :start nodecode-handoff:start-cell)

(define-cell-lifecycle-tests "handoff"
  (:help :handoff)
  (:command "handoff")
  (:refused ("max_string_kib" 0))
  (:idle nodecode-handoff:handoff-error
         (handoff:handoff-text)
         (handoff:handoffs-text)))

;;; --- helpers ------------------------------------------------------------------

(defmacro with-fresh-store (&body body)
  "BODY over an empty store, so one test never reads another's handoff."
  `(let ((dir (nlk:home "handoff/")))
     (when (uiop:directory-exists-p dir)
       (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))
     ,@body))

(defun handoff-call (&rest pairs)
  "The semantic_handoff tool as the registry dispatches it: PAIRS alternating
key and value, decoded through the tool's own contract, then run."
  (let ((entry (nle::find-tool "semantic_handoff")))
    (unless entry (error "the semantic_handoff tool is not registered"))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun handoff-command-text (&optional (argument ""))
  "/handoff ARGUMENT, run the way a shell with no model runs it."
  (let ((entry (nle::find-registered-command "handoff")))
    (unless entry (error "/handoff is not registered"))
    (funcall (nle::slash-command-headless entry) argument "s1")))

(defun test-workspace ()
  "A workspace real enough for the tool's own check: an existing directory."
  (let ((dir (nlk:home "workspace/")))
    (ensure-directories-exist (merge-pathnames "probe" dir))
    (uiop:native-namestring dir)))

(defun sample-state (&rest extra)
  "A canonical state, then EXTRA members on top of it."
  (apply #'nlk:make-json-object
         "schema_version" 1
         "goal" "carry the parser work into a fresh session"
         "constraints_preferences" (vector "no new dependencies")
         "progress" (nlk:make-json-object
                     "done" (vector "read the old parser")
                     "in_progress" (vector "port the lexer")
                     "pending" (vector "port the printer"))
         "key_decisions" (vector "one function per token")
         "critical_context" (vector "the suite runs with the system's own runner")
         "next_steps" (vector "port the printer, then run the suite")
         extra))

(defun answer-ref (text)
  "The handoff:// reference the create answer begins with."
  (let* ((start (search "handoff://" text))
         (end (and start (position #\Space text :start start))))
    (unless (and start end) (error "no reference in ~s" text))
    (subseq text start end)))

(defun document-path (ref)
  "Where the handoff REF's document is filed."
  (multiple-value-bind (project id) (nodecode-handoff::parse-ref ref)
    (merge-pathnames "document.md" (nodecode-handoff::record-dir project id))))

;;; --- the tool ------------------------------------------------------------------

(deftest handoff-cell-a-state-files-a-canonical-page-and-reads-it-back
  (with-fresh-store
    (with-cell-stop ((handoff-start))
      (let* ((text (handoff-call "action" "create" "workspace" (test-workspace)
                                  "name" "parser-work" "state" (sample-state)))
             (ref (answer-ref text)))
        (is (search "parser-work" text) "the answer names the handoff")
        (is (search "## Goal" text) "and carries the page it filed")
        (is (search "carry the parser work into a fresh session" text) "with the goal")
        (is (search "- no new dependencies" text) "the constraints")
        (is (search "1. port the printer, then run the suite" text) "and the numbered next steps")
        (is (search "- Provenance: written from the model's account" text) "marking what the document is worth")
        (let ((read (handoff-call "action" "read" "ref" ref)))
          (is (search "carry the parser work into a fresh session" read) "read answers the same page")
          (is (search "valid" read) "and says it is valid")
          (is (search "Trust: untrusted project data" read) "with the trust line a fresh session must read"))
        (is (search "valid -- all six sections present"
                    (handoff-call "action" "validate" "ref" ref)) "validate agrees")))))

(deftest handoff-cell-the-same-request-files-nothing-twice-and-another-state-conflicts
  (with-fresh-store
    (with-cell-stop ((handoff-start))
      (let ((first (handoff-call "action" "create" "workspace" (test-workspace)
                                 "request_id" "attempt-1" "state" (sample-state)))
            (second (handoff-call "action" "create" "workspace" (test-workspace)
                                  "request_id" "attempt-1" "state" (sample-state))))
        (is (equal (answer-ref first) (answer-ref second)) "one reference for one request")
        (is (search "already there" second) "and the second call says nothing was rewritten"))
      (let ((conflict (handoff-call "action" "create" "workspace" (test-workspace)
                                    "request_id" "attempt-1"
                                    "state" (sample-state "goal" "a different goal"))))
        (is (search "ERROR: " conflict) "another state under the same id is a failed call")
        (is (search "different handoff" conflict) "saying the reference is taken"))
      (is (= 1 (length (uiop:directory* (nlk:home "handoff/projects/*/*/document.md")))) "and only one document was filed"))))

(deftest handoff-cell-credential-looking-text-never-reaches-the-page
  (with-fresh-store
    (with-cell-stop ((handoff-start))
      (let* ((text (handoff-call "action" "create" "workspace" (test-workspace)
                                 "state" (sample-state
                                           "critical_context"
                                           (vector "API_KEY=sk-abcdefghijklmnop1234"
                                                   "auth: Bearer abcdefghijklmnop"
                                                   "the private key is planted here"))))
             (ref (answer-ref text))
             (page (handoff-call "action" "read" "ref" ref)))
        (is (search "redacted" text) "the answer counts what was scrubbed")
        (is (null (search "sk-abcdefghijklmnop1234" page)) "the token is gone from the page")
        (is (null (search "abcdefghijklmnop" page)) "and so is the bearer run")
        (is (search "[REDACTED]" page) "with the marker in its place")
        (is (search "the private key is planted here" page) "while plain prose is kept")))))

(deftest handoff-cell-a-document-that-lost-a-section-reads-invalid
  (with-fresh-store
    (with-cell-stop ((handoff-start))
      (let* ((text (handoff-call "action" "create" "workspace" (test-workspace)
                                 "state" (sample-state)))
             (ref (answer-ref text))
             (path (document-path ref)))
        (is (probe-file path) "the document was filed under the home")
        (nlk:write-file-atomically path "## Goal\n\nhand-edited\n")
        (let ((answer (handoff-call "action" "validate" "ref" ref)))
          (is (search "invalid" answer) "a document missing its sections is invalid")
          (is (search "## Next Steps" answer) "naming the one it lost"))
        (is (search "invalid" (handoff-call "action" "read" "ref" ref)) "and read says so too")))))

(deftest handoff-cell-a-refusal-names-what-is-wrong
  (with-fresh-store
    (with-cell-stop ((handoff-start))
      (is (search "workspace must be an absolute path"
                  (handoff-call "action" "create" "state" (sample-state))) "a create with no workspace is refused")
      (is (search "create needs state"
                  (handoff-call "action" "create" "workspace" (test-workspace))) "and one with no state")
      (is (search "a ref reads handoff://" (handoff-call "action" "read" "ref" "nope")) "a ref that is not one is refused")
      (is (search "no handoff at handoff://aaaaaaaaaa/bbbbbbbbbbbb"
                  (handoff-call "action" "read" "ref" "handoff://aaaaaaaaaa/bbbbbbbbbbbb")) "and a reference this store does not hold says which")
      (is (search "state needs progress"
                  (handoff-call "action" "create" "workspace" (test-workspace)
                                "state" (nlk:make-json-object "goal" "no progress here"))) "a state with no progress is refused, not filed")
      (let ((schema (second (nle::find-tool "semantic_handoff"))))
        (is (hash-table-p (nlk:contract-schema schema)) "the tool is registered against a declared contract")
        (is (signals-error nlk:contract-refusal
              (nlk:contract-decode schema (nlk:make-json-object "action" "levitate"))) "and an action outside the vocabulary is refused before the body runs")))))

(deftest handoff-cell-a-filed-handoff-outlives-a-stop-and-a-start
  (with-fresh-store
    (let ((stop (handoff-start)))
      (handoff-call "action" "create" "workspace" (test-workspace)
                    "state" (sample-state))
      (funcall stop))
    (with-cell-stop ((handoff-start))
      (let ((text (handoff:handoff-text)))
        (is (search "carry the parser work into a fresh session" text) "the page is read off disk")
        (is (search "## Progress" text) "with its progress section")))))

;;; --- /handoff ------------------------------------------------------------------

(deftest handoff-cell-slash-shows-the-newest-and-lists-the-recent
  (with-fresh-store
    (with-cell-stop ((handoff-start))
      (is (search "none yet" (handoff-command-text)) "with nothing filed, /handoff says so")
      (let ((one (answer-ref (handoff-call "action" "create" "workspace" (test-workspace)
                                         "request_id" "one" "state" (sample-state)))))
        (is (search "carry the parser work" (handoff-command-text)) "otherwise it shows the newest page")
        (is (search one (handoff-command-text one)) "and a reference shows that one"))
      (let ((two (answer-ref (handoff-call "action" "create" "workspace" (test-workspace)
                                         "request_id" "two"
                                         "state" (sample-state "goal" "the second piece of work")))))
        (is (search "the second piece of work" (handoff-command-text two)) "the reference the second one filed shows that page")
        (let ((listed (handoff-command-text "list")))
          (is (search "2 handoffs" listed) "list counts them")
          (is (and (search "handoff://" listed) (search two listed)) "naming what the store holds"))
        (is (search "semantic_handoff tool" (handoff-command-text "help")) "help answers the manual")
        (is (search two (handoff:handoffs-text)) "and the eval verb lists what the command lists")))))
