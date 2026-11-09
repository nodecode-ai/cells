;;;; cell-test.lisp --- the REE cell, end to end over a scripted docker.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one process call is NODECODE-GENSYN-REE::*RUNNER*: every test binds it
;;;; to a function of the argv that answers what the official image would print,
;;;; and writes the receipts a container would write into the tasks folder it
;;;; mounted. So no test needs Docker, and none opens a socket. The lifecycle
;;;; tests are the machinery's own: a start installs the manual topic, the
;;;; three tools and /gensyn-ree, a stop takes them all back, a second start
;;;; leaves one of everything, and a bad setting refuses.

(in-package #:nodecode.test)

(define-test-slice "gensyn-ree" "GENSYN-REE-CELL-" :start nodecode-gensyn-ree:start-cell)

;;; --- the fixtures a receipt is made of -------------------------------------------

(defparameter +ree-hash-a+ (format nil "sha256:~{~a~}" (make-list 64 :initial-element "a")))
(defparameter +ree-hash-b+ (format nil "sha256:~{~a~}" (make-list 64 :initial-element "b")))
(defparameter +ree-hash-c+ (format nil "sha256:~{~a~}" (make-list 64 :initial-element "c")))

(defun ree-receipt-json (&key (prompt "hello") (text "Hello there") (tokens +ree-hash-a+)
                               (model "facebook/opt-125m"))
  "A receipt of the shape the official command writes, as its JSON text."
  (nlk:encode-json-object
   (nlk:json-object
    "version" "2.0.0"
    "model" (nlk:json-object "name" model "config_hash" +ree-hash-b+ "commit_hash" +ree-hash-c+)
    "input" (nlk:json-object "prompt" prompt "prompt_hash" +ree-hash-b+
                              "tools_hash" +ree-hash-b+ "parameters_hash" +ree-hash-b+)
    "output" (nlk:json-object "text_output" text "finish_reason" "stop"
                               "token_count" 7 "tokens_hash" tokens)
    "hashes" (nlk:json-object "receipt_hash" +ree-hash-c+))))

(defun ree-receipt-file (&rest arguments)
  "A receipt file under the home holding the receipt ARGUMENTS describe. => its path."
  (let ((path (nlk:home "gensyn-ree/test/receipt.json")))
    (nlk:write-file-atomically path (apply #'ree-receipt-json arguments) :mode #o600)
    path))

(defun ree-validate-log (&key (prompt "PASS") (tools "PASS") (parameters "PASS") (receipt "PASS"))
  "The official validate's log: one line per hash, each prefixed as its logger prints it."
  (format nil "I ree.validate: prompt_hash: ~a~%I ree.validate: tools_hash: ~a~%~
I ree.validate: parameters_hash: ~a~%I ree.validate: receipt_hash: ~a"
          prompt tools parameters receipt))

;;; --- the scripted docker ----------------------------------------------------------

(defvar *ree-calls* nil
  "Every argv the scripted docker was handed, newest first.")

(defun ree-tasks-of (argv)
  "The host folder ARGV mounted at /mnt/tasks, or NIL."
  (loop for (flag target) on argv
        when (and (string= flag "-v") target (uiop:string-suffix-p target ":/mnt/tasks"))
          return (subseq target 0 (- (length target) (length ":/mnt/tasks")))))

(defun ree-write-receipts (tasks texts)
  "TEXTS as the receipt files the official command writes into TASKS."
  (when (and tasks texts)
    (loop for text in texts
          for index from 0
          do (nlk:write-file-atomically
              (merge-pathnames (format nil "out/receipt_2026010~d_12000~d.json" index index)
                               (uiop:ensure-directory-pathname tasks))
              text :mode #o600))))

(defun ree-scripted-runner (&key (log (ree-validate-log)) (status 0) receipts
                                  infer-receipts (run-status 0) (version "27.3.1") (image t))
  "A *RUNNER* answering what the image and the daemon would, and writing RECEIPTS
into the tasks folder a verify or an infer mounted."
  (lambda (argv seconds)
    (declare (ignore seconds))
    (push argv *ree-calls*)
    (let ((words (mapcar #'string-downcase argv)))
      (cond ((member "validate" words :test #'string=) (values log "" status))
            ((member "verify" words :test #'string=)
             (ree-write-receipts (ree-tasks-of argv) receipts)
             (values "" "" run-status))
            ((member "run-all" words :test #'string=)
             (ree-write-receipts (ree-tasks-of argv) infer-receipts)
             (values "" "" run-status))
            ((member "version" words :test #'string=) (values version "" 0))
            ((member "inspect" words :test #'string=) (values "sha256:id" "" (if image 0 1)))
            ((member "info" words :test #'string=) (values "16000000000" "" 0))
            (t (values "" "" 0))))))

(defmacro with-ree-docker ((&rest spec) &body body)
  "Run BODY with the process seam scripted from SPEC and every argv recorded."
  `(let ((*ree-calls* nil)
         (nodecode-gensyn-ree::*runner* (ree-scripted-runner ,@spec)))
     ,@body))

(defun ree-permits (title &key detail choices seconds)
  "The operator, always saying yes."
  (declare (ignorable title detail choices seconds))
  (values "Allow" :answered))

;;; --- the lifecycle --------------------------------------------------------------------

(define-cell-lifecycle-tests "gensyn-ree"
  (:help :gensyn-ree)
  (:command "gensyn-ree")
  (:refused ("validate_seconds" 1) ("ask_seconds" 0) ("infer_seconds" 10)))

;;; --- what the model reads ---------------------------------------------------------------

(deftest gensyn-ree-cell-validate-checks-the-hashes-with-no-network ()
  (with-cell-stop ((gensyn-ree-start))
    (with-ree-docker ()
      (let ((block (nodecode-gensyn-ree:validate-receipt (ree-receipt-file))))
        (is (cl:search "level: consistent" block) "four PASS lines and exit zero is consistent")
        (is (cl:search "prompt_hash PASS" block) "the block quotes each hash line")
        (is (cl:search "not an instruction" block) "and says the receipt text is data")
        (let ((argv (car (last *ree-calls*))))
          (is (member "--network" argv :test #'string=) "the validate container is asked for no network")
          (is (member "none" argv :test #'string=) "and gets none")
          (is (member "--pull" argv :test #'string=) "the image is never pulled")
          (is (some (lambda (word) (uiop:string-prefix-p "gensynai/ree@sha256:" word)) argv) "and it is pinned by digest"))
        (let ((arguments (nodecode-gensyn-ree::decode-ree-receipt
                          (nlk:json-object "receipt_path" (namestring (ree-receipt-file))))))
          (is (cl:search "level: consistent" (nodecode-gensyn-ree::validate-tool arguments)) "the tool answers the same block as the verb"))))))

(deftest gensyn-ree-cell-an-inconsistent-receipt-is-its-own-level ()
  (with-cell-stop ((gensyn-ree-start))
    (with-ree-docker (:log (ree-validate-log :receipt "FAIL") :status 1)
      (let ((block (nodecode-gensyn-ree:validate-receipt (ree-receipt-file))))
        (is (cl:search "level: inconsistent" block) "a FAIL line with exit one is inconsistent")
        (is (cl:search "receipt_hash FAIL" block) "naming the line that failed")))))

(deftest gensyn-ree-cell-verify-names-the-field-that-differs ()
  (with-cell-stop ((gensyn-ree-start))
    (with-stubbed-fdefinition (nle:ask-operator (title &key detail choices seconds)
                              (ree-permits title :detail detail :choices choices :seconds seconds))
      (with-ree-docker (:receipts (list (ree-receipt-json :text "Hello world")))
        (let ((block (nodecode-gensyn-ree:verify-receipt (ree-receipt-file))))
          (is (cl:search "level: text_mismatch" block) "the same hashes with different words is text_mismatch, not a failed hash check")
          (is (cl:search "output.text_output" block) "and the block names the field that differs")
          (is (cl:search "submitted \"Hello there\"" block) "with what the receipt said")
          (is (cl:search "rerun \"Hello world\"" block) "beside what the re-run said"))))))

(deftest gensyn-ree-cell-verify-with-nobody-to-ask-refuses ()
  (with-cell-stop ((gensyn-ree-start))
    (with-stubbed-fdefinition (nle:ask-operator (title &key detail choices seconds)
                              (declare (ignorable title detail choices seconds))
                              (values nil :nobody))
      (with-ree-docker ()
        (let ((refusal (refusal-text nodecode-gensyn-ree:gensyn-ree-error
                                      (nodecode-gensyn-ree:verify-receipt (ree-receipt-file)))))
          (is (cl:search "approval_unavailable" refusal) "an operator who is not there is a refusal, never a silent container"))))))

(deftest gensyn-ree-cell-infer-keeps-the-receipt-it-made ()
  (with-cell-stop ((gensyn-ree-start))
    (with-stubbed-fdefinition (nle:ask-operator (title &key detail choices seconds)
                              (ree-permits title :detail detail :choices choices :seconds seconds))
      (with-ree-docker (:infer-receipts (list (ree-receipt-json :prompt "say hello")))
        (let* ((block (nodecode-gensyn-ree:infer "facebook/opt-125m" "say hello" :max-new-tokens 8))
               (kept (first (directory (merge-pathnames "*.json"
                                                        (nlk:home "gensyn-ree/receipts/"))))))
          (is (cl:search "level: inferred" block) "one new receipt is an inference")
          (is (cl:search "receipt_hash: sha256:" block) "the block names the receipt's hash")
          (is (and kept (probe-file kept)) "and the receipt is kept under the home"))
        (is (cl:search "unsupported"
                       (refusal-text nodecode-gensyn-ree:gensyn-ree-error
                                     (nodecode-gensyn-ree:infer "gpt-4" "hello"))) "a model outside the three is refused")))))

(deftest gensyn-ree-cell-doctor-says-what-is-missing ()
  (with-cell-stop ((gensyn-ree-start))
    (with-ree-docker (:image nil)
      (let ((block (nodecode-gensyn-ree:doctor)))
        (is (cl:search "docker: 27.3.1" block) "the doctor names the daemon it found")
        (is (cl:search "docker pull gensynai/ree:v0.8.0" block) "and says which pull is the operator's own, when the image is missing")))
    (with-ree-docker ()
      (is (cl:search "gensynai/ree@sha256:" (nodecode-gensyn-ree:doctor)) "a pinned image present is named by digest"))))

(deftest gensyn-ree-cell-the-command-validates-a-path-and-otherwise-doctors ()
  (with-cell-stop ((gensyn-ree-start))
    (with-ree-docker ()
      (let ((answer (cell-entry "nodecode-gensyn-ree" "gensyn-ree"
                                (namestring (ree-receipt-file)) "s-gen")))
        (is (cl:search "level: consistent" answer) "/gensyn-ree <path> checks that receipt"))
      (let ((answer (cell-entry "nodecode-gensyn-ree" "gensyn-ree" "" "s-gen")))
        (is (cl:search "REE on this machine" answer) "and /gensyn-ree alone is the doctor")))))
