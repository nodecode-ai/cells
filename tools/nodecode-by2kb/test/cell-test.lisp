;;;; cell-test.lisp --- the six tools, the routed command, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring against a stubbed program:
;;;; that each verb asks by2kb the argv the plugin asked it, and reads the
;;;; JSON it answers the way the plugin read it -- a recommendation list with
;;;; the numbers to reply with, a selection that names its jobs, one video's
;;;; three files, a staged operation read page by page and handed back whole,
;;;; and a job's state; that a run which fails, or prints nothing, is a
;;;; refusal the model can read; and that a start with no program refuses.
;;;; Nothing here needs by2kb installed and none of it touches the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *calls* '()
  "The ARGS of every run this test stubbed, newest first.")

(defvar *responder* nil
  "What a stubbed run answers: a function of ARGS => (VALUES STDOUT EXIT-CODE).")

(defvar *written* nil
  "What the last submitted answer file held, read while the program ran.")

(defparameter *stub-program* nil
  "The stand-in program's path, made once a run.")

(defun executable-script (path &rest lines)
  "Write LINES to PATH, one per line, make it executable, and answer its
native namestring."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-line "#!/bin/sh" out)
    (dolist (line lines) (write-line line out)))
  (uiop:run-program (list "chmod" "755" (uiop:native-namestring path))
                     :output nil :error-output nil)
  (uiop:native-namestring path))

(defun by2kb-stub-program ()
  "A program the cell accepts as by2kb: it answers one JSON object, so a start
finds something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (executable-script (merge-pathnames "by2kb-stub/by2kb" (uiop:temporary-directory))
                               "cat >/dev/null"
                               "printf '%s' '{}'"))))

(defmacro with-run-stub ((&key (answer "{}") (code 0)) &body body)
  "BODY with the program stubbed: every run answers ANSWER with CODE, and each
one pushes its ARGS on *CALLS*. A test that needs several answers setfs
*RESPONDER* instead."
  `(let ((*calls* '())
         (*written* nil)
         (*responder* (lambda (args) (declare (ignore args)) (values ,answer ,code)))
         (nodecode-by2kb::*run* (lambda (args)
                                   (push args *calls*)
                                   (funcall *responder* args))))
     ,@body))

(defun by2kb-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) (nlk:contract-decode (second entry) arguments))))

(defun by2kb-asked ()
  "The stubbed runs, oldest first."
  (reverse *calls*))

(defun argument (args flag)
  "What ARGS carries after FLAG."
  (nth (1+ (position flag args :test #'equal)) args))

;;; --- the slice ------------------------------------------------------------------

(define-test-slice "by2kb" "BY2KB-CELL-" :start nodecode-by2kb:start-cell
  :defaults (("by2kb" (by2kb-stub-program))))

(define-cell-lifecycle-tests "by2kb"
  (:help :by2kb)
  (:command "by2kb")
  (:running (is (nle::find-tool "by2kb_search") "by2kb_search is registered")
            (is (nle::find-tool "by2kb_select"))
            (is (nle::find-tool "by2kb_video"))
            (is (nle::find-tool "by2kb_next"))
            (is (nle::find-tool "by2kb_submit"))
            (is (nle::find-tool "by2kb_status"))
            (is (stringp (nodecode-by2kb::program)) "the program the section names")
            (is (= 3600 (nodecode-by2kb::setting :timeout-seconds)) "an hour a run"))
  (:stopped (is (null (nle::find-tool "by2kb_search")) "stop unregisters the tools")
            (is (null (nle::find-tool "by2kb_status"))))
  (:refused ("by2kb" "definitely-no-such-program-xyz") ("timeout_seconds" 5))
  (:idle nodecode-by2kb:by2kb-error (nodecode-by2kb:search "anything")))

(deftest by2kb-cell-a-section-naming-no-program-says-how-to-install-it ()
  (is-carrying (text (refusal-text nlk:config-refusal
                                    (by2kb-start "by2kb" "definitely-no-such-program-xyz")))
               ("no such program on PATH") ("pipx install")))

;;; --- discovery --------------------------------------------------------------------

(deftest by2kb-cell-a-topic-search-answers-numbers-under-the-scope ()
  (with-run-stub ()
    (with-cell-stop ((by2kb-start "scope" "conv-1"))
      (setf *responder*
            (lambda (args)
              (declare (ignore args))
              (values "{\"status\":\"awaiting_selection\",\"session_id\":\"s-1\",\"message\":\"1. A talk -- https://example.test/a\"}" 0)))
      (is-carrying (answer (nodecode-by2kb:search "TiDB vector search"))
                   ("1. A talk") ("s-1") ("Reply with the numbers"))
      (is (equal '("search" "discover" "TiDB vector search" "--scope" "conv-1" "--json")
                 (first *calls*)) "the argv the plugin ran, scope included"))))

(deftest by2kb-cell-a-selection-takes-the-lists-numbers ()
  (with-run-stub ()
    (with-cell-stop ((by2kb-start "scope" "conv-1"))
      (setf *responder*
            (lambda (args)
              (if (equal (second args) "latest")
                  (values "{\"status\":\"awaiting_selection\",\"session_id\":\"s-9\"}" 0)
                  (values "{\"results\":[{\"number\":1,\"status\":\"enrichment_pending\",\"job_id\":\"j-1\"},{\"number\":3,\"status\":\"completed\",\"message\":\"already in the base\"}]}" 0))))
      (is-carrying (answer (nodecode-by2kb:select "1,3"))
                   ("1. enrichment_pending") ("job j-1") ("3. completed")
                   ("call by2kb_next"))
      (is (equal '("search" "select" "s-9" "1,3" "--scope" "conv-1"
                   "--enricher" "external_agent" "--json")
                 (first *calls*)) "the numbers went to the list's own id")
      (is (equal '("search" "latest" "--scope" "conv-1" "--json") (second *calls*)) "the open list was asked for first"))))

(deftest by2kb-cell-a-number-with-no-open-list-is-a-repair ()
  (with-run-stub (:answer "{}")
    (with-cell-stop ((by2kb-start "scope" "conv-2"))
      (is-carrying (text (refusal-text nodecode-by2kb:by2kb-error
                                       (nodecode-by2kb:select "1")))
                   ("no open recommendation list" "a number before any search is a repair, not a crash")))))

;;; --- one video, and the staged summaries -------------------------------------------

(deftest by2kb-cell-one-video-becomes-three-files ()
  (let* ((dir (merge-pathnames "by2kb-files/" (uiop:temporary-directory)))
         (abstract (merge-pathnames "abstract.md" dir))
         (raw (merge-pathnames "raw.md" dir))
         (updated (merge-pathnames "updated.md" dir)))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree dir :validate t)))
      (ensure-directories-exist abstract)
      (with-open-file (out abstract :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
        (format out "---~%title: a talk~%---~%the abstract body~%"))
      (with-run-stub ()
        (with-cell-stop ((by2kb-start))
          (setf *responder*
                (lambda (args)
                  (declare (ignore args))
                  (values (format nil "{\"status\":\"completed\",\"job_id\":\"j-2\",~
\"artifacts\":{\"raw_md\":~s,\"abstract_md\":~s,\"updated_md\":~s}}"
                                  (uiop:native-namestring raw)
                                  (uiop:native-namestring abstract)
                                  (uiop:native-namestring updated))
                          0)))
          (is-carrying (answer (nodecode-by2kb:video "https://www.bilibili.com/video/BV1x"))
                       ("Video ready (job j-2)") ("the abstract body")
                       ("transcript") ("notes"))
          (is (equal '("ingest" "https://www.bilibili.com/video/BV1x"
                       "--enricher" "external_agent" "--json")
                     (first *calls*)) "the plugin's own ingest argv"))))))

(deftest by2kb-cell-a-staged-operation-is-read-page-by-page ()
  (with-run-stub ()
    (with-cell-stop ((by2kb-start))
      (setf *responder*
            (lambda (args)
              (if (equal (second args) "next")
                  (values "{\"status\":\"needs_input\",\"operation\":{\"id\":\"op-1\",\"request_file\":\"request-x.json\",\"max_output_bytes\":800,\"timeout_s\":60}}" 0)
                  (let* ((field (argument args "--field"))
                         (offset (parse-integer (argument args "--offset"))))
                    (if (zerop offset)
                        (values (format nil "{\"operation_id\":\"op-1\",\"field\":~s,\"offset\":0,\"next_offset\":3,\"total_chars\":6,\"eof\":false,\"text\":\"sys\"}" field) 0)
                        (values (format nil "{\"operation_id\":\"op-1\",\"field\":~s,\"offset\":3,\"next_offset\":6,\"total_chars\":6,\"eof\":true,\"text\":\"tem\"}" field) 0))))))
      (is-carrying (answer (nodecode-by2kb:next "j-3"))
                   ("Operation op-1 of job j-3") ("at most 800 bytes") ("60s")
                   ("--- system prompt ---") ("system") ("--- user prompt ---"))
      (is (= 5 (length *calls*)) "one next, then two pages of each prompt")
      (is (equal "user_prompt" (argument (first *calls*) "--field")) "the user prompt was read last"))))

(deftest by2kb-cell-an-answer-goes-back-whole-and-its-file-goes ()
  (with-run-stub ()
    (with-cell-stop ((by2kb-start))
      (setf *responder*
            (lambda (args)
              (setf *written* (uiop:read-file-string (argument args "--output-file")))
              (values "{\"status\":\"accepted\"}" 0)))
      (is-carrying (answer (nodecode-by2kb:submit "j-4" "op-2" "# the notes"))
                   ("accepted for job j-4") ("call by2kb_next"))
      (is (equal "# the notes" *written*) "the program read exactly the answer")
      (let ((argv (first *calls*)))
        (is (equal "op-2" (argument argv "--operation-id")))
        (is (equal "nodecode" (argument argv "--provider")) "the section's identity")
        (is (null (probe-file (argument argv "--output-file"))) "the temporary answer file is gone")))))

;;; --- a run that fails ---------------------------------------------------------------

(deftest by2kb-cell-a-failed-run-is-a-refusal-with-the-programs-own-message ()
  (with-run-stub (:answer "{\"status\":\"error\",\"message\":\"search is disabled in [search]\"}"
                  :code 2)
    (with-cell-stop ((by2kb-start))
      (is-carrying (text (refusal-text nodecode-by2kb:by2kb-error
                                       (nodecode-by2kb:search "x")))
                   ("search is disabled" "the program's own message is the model's repair")))))

(deftest by2kb-cell-a-run-that-prints-nothing-is-a-refusal-too ()
  (with-run-stub (:answer "" :code 0)
    (with-cell-stop ((by2kb-start))
      (is-carrying (text (refusal-text nodecode-by2kb:by2kb-error
                                       (nodecode-by2kb:video "https://youtu.be/x")))
                   ("by2kb printed nothing")))))

;;; --- /by2kb ------------------------------------------------------------------------

(deftest by2kb-cell-the-command-routes-what-the-operator-typed ()
  (with-run-stub ()
    (with-cell-stop ((by2kb-start "scope" "conv-3"))
      (is (cl:search "by2kb_search" (nodecode-by2kb::run-slash "" nil)) "an empty command says what the cell can do")
      (setf *responder*
            (lambda (args)
              (if (equal (second args) "latest")
                  (values "{\"status\":\"awaiting_selection\",\"session_id\":\"s-3\"}" 0)
                  (values "{\"results\":[{\"number\":2,\"status\":\"duplicate\",\"message\":\"already filed\"}]}" 0))))
      (is-carrying (answer (nodecode-by2kb::run-slash "2" nil))
                   ("2. duplicate"))
      (is (equal '("search" "select" "s-3" "2" "--scope" "conv-3"
                   "--enricher" "external_agent" "--json")
                 (first *calls*)) "a bare number took the list's numbers"))))
