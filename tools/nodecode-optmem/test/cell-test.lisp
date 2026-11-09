;;;; cell-test.lisp --- lifecycle, the store, the decay tree, the tools, /optmem.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and the store it keeps: a
;;;; healthy start that installs the eight tools, the hook and the command and
;;;; a stop that takes them all back; a section member of the wrong shape
;;;; refused; each tool answering through a store of its own (a scratch folder,
;;;; so nothing touches the operator's memory); the 320-byte record the
;;;; upstream `memo' tool reads; the decay tree's naps, zoom and forget; the
;;;; regex and BM25 recall modes; and the digest a request carries.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defmacro with-optmem-store ((place) &body body)
  "BODY with PLACE naming a fresh store folder, removed on unwind: every test
keeps its own memory, and none reaches the operator's."
  `(let ((,place (namestring (temp-path "optmem-store"))))
     (nlk:with-cleanup ((ignore-errors (nlk:remove-tree ,place)))
       ,@body)))

(defun optmem-dir (store) (uiop:ensure-directory-pathname store))

(defun optmem-file (store name)
  (merge-pathnames name (optmem-dir store)))

(defun optmem-bytes (store name)
  "How many bytes STORE's file NAME holds, or NIL when it is absent."
  (with-open-file (in (optmem-file store name) :element-type '(unsigned-byte 8)
                                              :if-does-not-exist nil)
    (and in (file-length in))))

(defun optmem-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun optmem-answer (name arguments)
  "Tool NAME's answer, decoded: a tool answers JSON text, or an ERROR line."
  (let ((text (optmem-tool name arguments)))
    (unless (and (plusp (length text)) (char= (char text 0) #\{))
      (error "~a answered ~a" name text))
    (nlk:decode-json text)))

(defun optmem-args (&rest pairs)
  (apply #'nlk:make-json-object pairs))

(defun optmem-note (text)
  (optmem-tool "optmem_note" (optmem-args "text" text)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "optmem" "OPTMEM-CELL-" :start nodecode-optmem:start-cell)

(define-cell-lifecycle-tests "optmem"
  (:fixture with-optmem-store (store))
  (:config "store" store)
  (:hooks :frame 'nle::read-harness-sections)
  (:help :optmem)
  (:command "optmem")
  (:running (is (nle::find-tool "optmem_note") "optmem_note is registered")
            (is (nle::find-tool "optmem_recall"))
            (is (nle::find-tool "optmem_wake"))
            (is (nle::find-tool "optmem_nap"))
            (is (nle::find-tool "optmem_zoom"))
            (is (nle::find-tool "optmem_forget"))
            (is (nle::find-tool "optmem_config"))
            (is (nle::find-tool "optmem_import"))
            (is (equal "auto" (nodecode-optmem::setting :recall-mode)) "the default mode")
            (is (= 96 (nodecode-optmem::setting :wake-lines)) "the default digest budget")
            (is (probe-file (optmem-file store "LOG.txt")) "the store exists as soon as the cell runs"))
  (:stopped (is (null (nle::find-tool "optmem_note")) "stop unregisters the tools"))
  (:refused ("recall_mode" "semantic") ("wake_lines" 0))
  (:idle nodecode-optmem:optmem-error (nodecode-optmem::run-command "" nil)))

;;; --- the store ----------------------------------------------------------------

(deftest optmem-cell-note-writes-the-record-memo-reads ()
  (with-optmem-store (store)
    (with-cell-stop ((optmem-start "store" store))
      (let ((answer (optmem-answer "optmem_note" (optmem-args "text" "the deploy ships on Friday"))))
        (is (equal "#0" (nlk:json-value answer :string "saved_as")))
        (is (equal "added" (nlk:json-value answer :string "status"))))
      (is (= 320 (optmem-bytes store "LOG.txt"))
          "one record is 320 bytes, the width the upstream memo tool writes")
      (let ((wake (optmem-answer "optmem_wake" (nlk:json-object))))
        (is (= 1 (nlk:json-value wake :integer "count")))
        (is (search "the deploy ships on Friday"
                    (aref (nlk:json-value wake :array "context") 0))))
      (is (search "the deploy ships on Friday"
                  (cdr (assoc "optmem" (nle::read-harness-sections "s1") :test #'string=)))
          "the digest rides every request as the optmem block")
      (let ((too-long (optmem-tool "optmem_note"
                                    (optmem-args "text" (make-string 400 :initial-element #\x)))))
        (is (search "ERROR: optmem:" too-long) "an over-long memory is a repair, not a crash")
        (is (search "limit 280" too-long)))
      (is (= 320 (optmem-bytes store "LOG.txt")) "and nothing was written"))))

(deftest optmem-cell-recall-by-pattern-and-by-meaning ()
  (with-optmem-store (store)
    (with-cell-stop ((optmem-start "store" store))
      (optmem-note "the paywall shipped in March")
      (optmem-note "caçula means the youngest child")
      (optmem-note "the deploy ships on Friday")
      (let ((regex (optmem-answer "optmem_recall" (optmem-args "query" "deploy" "mode" "regex"))))
        (is (equal "regex" (nlk:json-value regex :string "mode_used")))
        (is (= 1 (nlk:json-value regex :integer "total_matches")))
        (is (search "ships on Friday"
                    (nlk:json-value (aref (nlk:json-value regex :array "results") 0)
                                    :string "text"))))
      (let ((prose (optmem-answer "optmem_recall" (optmem-args "query" "what does cacula mean"))))
        (is (equal "token" (nlk:json-value prose :string "mode_used"))
            "a sentence is routed to token search, never compiled as a regex")
        (is (search "caçula" (nlk:json-value (aref (nlk:json-value prose :array "results") 0)
                                              :string "text"))
            "accent folding finds caçula from cacula"))
      (let ((ranked (optmem-answer "optmem_recall" (optmem-args "query" "caçula" "mode" "bm25"))))
        (is (equal "bm25" (nlk:json-value ranked :string "mode_used")))
        (is (= 1 (nlk:json-value ranked :integer "count"))))
      (is (search "ERROR: optmem:"
                  (optmem-tool "optmem_recall" (optmem-args "query" "(" "mode" "regex")))
          "an explicitly broken pattern is reported, not silently empty"))))

;;; --- the decay tree --------------------------------------------------------------

(deftest optmem-cell-naps-compress-and-zoom-recovers ()
  (with-optmem-store (store)
    (with-cell-stop ((optmem-start "store" store "wake_lines" 2))
      (dotimes (index 4)
        (optmem-note (format nil "memory ~a about the release" index)))
      (let ((before (optmem-answer "optmem_wake" (nlk:json-object))))
        (is (null (nlk:json-value before :boolean "complete"))
            "a block without its summary cannot wake")
        (is (search "Compress memories #0-1" (nlk:json-value before :string "nap_prompt"))
            "the answer carries the next nap, as memo wake refuses"))
      (is (equal "compressed"
                 (nlk:json-value (optmem-answer "optmem_nap"
                                                 (optmem-args "lo" 0 "hi" 2
                                                              "summary" "two memories about the release"))
                                 :string "status")))
      (is (equal "compressed"
                 (nlk:json-value (optmem-answer "optmem_nap"
                                                 (optmem-args "lo" 2 "hi" 4
                                                              "summary" "two more memories about the release"))
                                 :string "status")))
      (let ((after (optmem-answer "optmem_wake" (nlk:json-object))))
        (is (nlk:json-value after :boolean "complete")
            "the digest is complete once both blocks are compressed")
        (is (= 2 (nlk:json-value after :integer "count")))
        (is (search "#0-1 two memories about the release"
                    (aref (nlk:json-value after :array "context") 0))
            "the older block reads as its summary, the newest verbatim"))
      (let ((zoomed (optmem-answer "optmem_zoom" (optmem-args "lo" 0 "hi" 4))))
        (is (= 2 (length (nlk:json-value zoomed :array "halves"))))
        (is (search "two more memories"
                    (aref (nlk:json-value zoomed :array "halves") 1))
            "zoom opens the node, never flattening it to raw lines"))
      (let ((forgotten (optmem-answer "optmem_forget" (optmem-args "lo" 0 "hi" 2))))
        (is (= 2 (nlk:json-value forgotten :integer "count"))
            "the block and its sibling are dropped, with every block built on them")
        (is (= 0 (optmem-bytes store "TREE/2")) "TREE/2 is truncated where they lived"))
      (is (search "ERROR: optmem:" (optmem-tool "optmem_forget" (optmem-args "lo" 0 "hi" 2)))
          "a second forget reports the truth instead of a false success")
      (is (null (nlk:json-value (optmem-answer "optmem_wake" (nlk:json-object))
                                 :boolean "complete"))
          "and the digest is incomplete again, as it should be"))))

;;; --- the knobs -------------------------------------------------------------------

;;; --- the knobs -------------------------------------------------------------------

(deftest optmem-cell-config-shows-and-refuses ()
  (with-optmem-store (store)
    (with-cell-stop ((optmem-start "store" store))
      (let ((changed (optmem-answer "optmem_config"
                                     (optmem-args "changes" (vector "WAKE_LINES=2")))))
        (is (nlk:json-value changed :boolean "changed"))
        (is (= 2 (nlk:json-value changed :integer "effective_wake_budget"))
            "a change reports the budget it made")
        (is (= 2 (nodecode-optmem::wake-budget)) "and the engine runs on it"))
      (is (search "read-only"
                  (optmem-tool "optmem_config" (optmem-args "changes" (vector "ENTRY_CHARS=100"))))
          "a memo-parity display value is refused, never silently persisted")
      (is (search "invalid knob"
                  (optmem-tool "optmem_config" (optmem-args "changes" (vector "NOPE=1")))))
      (is (search "must be a positive integer"
                  (optmem-tool "optmem_config" (optmem-args "changes" (vector "WAKE_LINES=0")))))
      (let ((shown (nlk:json-value (optmem-answer "optmem_config" (nlk:json-object))
                                   :array "config")))
        (is (= 5 (length shown)) "every knob is shown")
        (is (= 96 (nlk:json-value (aref shown 0) :integer "default"))
            "with its default, as memo config prints them")))))

;;; --- import ------------------------------------------------------------------------

(deftest optmem-cell-imports-a-file-of-dated-memories ()
  (with-optmem-store (store)
    (with-cell-stop ((optmem-start "store" store "wake_lines" 8))
      (write-temp-file (optmem-file store "imports/history.txt")
                       (format nil "2024-01-02 the first memory~%2024-01-03 the second memory~%"))
      (let ((answer (optmem-answer "optmem_import" (optmem-args "file" "history.txt"))))
        (is (equal "imported" (nlk:json-value answer :string "status")))
        (is (= 2 (nlk:json-value answer :integer "count"))))
      (let ((wake (optmem-answer "optmem_wake" (nlk:json-object))))
        (is (= 2 (nlk:json-value wake :integer "count")))
        (is (search "the first memory"
                    (aref (nlk:json-value wake :array "context") 0))
            "an imported memory is an ordinary memory"))
      (is (search "bare file name"
                  (optmem-tool "optmem_import" (optmem-args "file" "../LOG.txt")))
          "the tool reads one bare name inside <store>/imports, never a path"))))

;;; --- the automatic nap ---------------------------------------------------------------

(deftest optmem-cell-auto-nap-compresses-from-what-a-block-holds ()
  (with-optmem-store (store)
    (with-cell-stop ((optmem-start "store" store "auto_nap" t))
      (dolist (text '("the client approved the budget"
                      "we deploy to staging"
                      "the paywall shipped"
                      "a decision was replaced"))
        (optmem-note text))
      (is (null (nodecode-optmem::tree-get 0 2)) "nothing is compressed before the tenth turn")
      (dotimes (index 10)
        (cell-publish "nodecode-optmem"
                       (cell-frame-op "turn.completed" :turn (format nil "t~a" index))))
      (let ((summary (nodecode-optmem::tree-get 0 2)))
        (is (stringp summary) "the tenth finished turn compressed the next pending block")
        (is (search "|" summary) "a local extractive summary: no model call, nothing invented")))))

;;; --- /optmem -------------------------------------------------------------------------

(deftest optmem-cell-the-command-shows-the-store-or-searches-it ()
  (with-optmem-store (store)
    (with-cell-stop ((optmem-start "store" store))
      (let ((answer (cell-entry "nodecode-optmem" "optmem" "")))
        (is (search "OptMem: 0 memories" answer))
        (is (search store answer) "the status names the store it runs on"))
      (optmem-note "the deploy ships on Friday")
      (is (search "the deploy ships on Friday"
                  (cell-entry "nodecode-optmem" "optmem" "deploy"))
          "a query searches the log from the shell")
      (is (search "No memory matched"
                  (cell-entry "nodecode-optmem" "optmem" "zzzznothing"))))))
