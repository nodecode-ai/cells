;;;; cell-test.lisp --- the lifecycle, the spill, every fetch mode, /rescuer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and its whole surface: a
;;;; healthy section installs one :TOOL hook, the rescuer_fetch tool and the
;;;; /rescuer command and a stop takes all three back; a small result is left
;;;; alone; a result over max_result_chars comes back as a handle, and every
;;;; mode reads that blob back; a kept-out tool is never spilled; a blob that
;;;; is not there answers an error line rather than a condition; and a section
;;;; whose numbers are out of range is the loader's loud refusal.

(in-package #:nodecode.test)

;;; --- lifecycle --------------------------------------------------------------

(define-cell-lifecycle-tests "toolaria"
  (:hooks :tool)
  (:command "rescuer")
  (:running (is (plusp (length (nodecode-toolaria:status))))
            (is (uiop:directory-exists-p (nodecode-toolaria:store-dir))
                "the store folder is made at start"))
  (:refused ("max_result_chars" 10)
            ("store_path" 42)))

;;; --- the spill ---------------------------------------------------------------

(deftest toolaria-cell-small-result-is-left-alone (with-cell-stop ((toolaria-start)))
  (is (equal "ok" (run-tool-call "web_extract" "ok"))
      "a result under the threshold goes to the model unchanged"))

(deftest toolaria-cell-oversized-result-becomes-a-handle (with-cell-stop ((toolaria-start)))
  (let* ((text (sample-text))
         (handle (run-tool-call "web_extract" text)))
    (is (< (length handle) (length text)) "the model reads the handle, not the result")
    (is (uiop:string-prefix-p "[Toolaria: tool result rescued." handle) "the handle opens the same way")
    (is (search "tool=web_extract" handle) "and names the tool")
    (is (search "NOT the full output" handle) "and says the preview is a preview")
    (is (search "mode=..." handle) "and says how to fetch more")
    (let ((id (handle-id handle)))
      (is (plusp (length (or id ""))) "and carries one blob handle")
      (is (equal text (nodecode-toolaria:fetch id :mode "full"))
          "which reads the whole result back"))))

(deftest toolaria-cell-a-kept-out-tool-is-never-spilled (with-cell-stop ((toolaria-start)))
  (let ((text (sample-text)))
    (is (equal text (run-tool-call "eval" text))
        "eval's result is the operator's, and stays as it was")
    (is (equal text (run-tool-call "rescuer_fetch" text))
        "and the fetch tool's own answers are never re-spilled")))

(deftest toolaria-cell-one-text-is-one-blob (with-cell-stop ((toolaria-start)))
  (let* ((text (sample-text))
         (first (run-tool-call "web_extract" text))
         (again (run-tool-call "web_extract" text)))
    (is (equal (handle-id first) (handle-id again)) "content addresses the blob")
    (is (equal first again) "and the handle is the same handle")))

;;; --- the fetch modes ----------------------------------------------------------

(deftest toolaria-cell-stat-and-range-answer (with-cell-stop ((toolaria-start)))
  (let* ((text (sample-text))
         (id (handle-id (run-tool-call "web_extract" text)))
         (stat (nodecode-toolaria:fetch id :mode "stat")))
    (is (search id stat) "stat names the blob")
    (is (search "web_extract" stat) "and the tool it came from")
    (is (search "chars" stat) "and its size"))
  (let* ((text (sample-text))
         (id (handle-id (run-tool-call "web_extract" text)))
         (range (nodecode-toolaria:fetch id :mode "range" :start 5 :want 3)))
    (is (search "line 5:" range) "range starts where it was asked to")
    (is (search "line 7:" range) "and runs on")
    (is (not (search "line 9:" range)) "and stops at count")))

(deftest toolaria-cell-grep-search-and-outline-answer (with-cell-stop ((toolaria-start)))
  (let* ((text (sample-text))
         (id (handle-id (run-tool-call "web_extract" text)))
         (grep (nodecode-toolaria:fetch id :mode "grep" :pattern "payment failed"))
         (search (nodecode-toolaria:fetch id :mode "search" :query "gateway timeout"))
         (outline (nodecode-toolaria:fetch id :mode "outline")))
    (is (search "line 7:" grep) "grep answers the matching line")
    (is (search "1 matching line" grep) "and counts them")
    (is (search "line 60:" search) "search ranks the line that answers the words")
    (is (search "blocks" outline) "outline maps the blob")
    (is (search "line 0:" outline) "and maps its blocks to line numbers")))

(deftest toolaria-cell-a-missing-or-bad-handle-is-an-error-line (with-cell-stop ((toolaria-start)))
  (is (search "Error: no rescued blob"
              (nodecode-toolaria:fetch "000000000000" :mode "stat"))
      "a swept blob is an error line, not a condition")
  (is (search "Error: invalid blob id"
              (nodecode-toolaria:fetch "../../etc/passwd" :mode "stat"))
      "a handle that is not 12 hex characters is refused"))

;;; --- /rescuer ------------------------------------------------------------------

(deftest toolaria-cell-rescuer-reports-the-store (with-cell-stop ((toolaria-start)))
  (let ((before (nodecode-toolaria:status)))
    (is (search "Toolaria:" before) "the command answers the store's line")
    (is (search "ttl" before) "with the switches on it"))
  (run-tool-call "web_extract" (sample-text))
  (is (search "1 blob" (nodecode-toolaria:status)) "one rescue, one blob"))

(deftest toolaria-cell-the-manual-names-the-modes (with-cell-stop ((toolaria-start)))
  (let ((text nodecode-toolaria::+primer+))
    (is (search "rescuer_fetch" text) "the manual names the tool")
    (is (search "max_result_chars" text) "and the threshold")))
