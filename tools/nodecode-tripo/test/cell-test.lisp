;;;; cell-test.lisp --- lifecycle, the tools, /tripo, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the section (a healthy start, a start that could
;;;; reach no account, a member out of range), each tool through the seam the
;;;; model calls it by -- its URL, its body and its answer -- a task read and
;;;; a file saved, and the refusals that send nothing. Every request is a
;;;; stubbed NLK:HTTP, so nothing reaches Tripo or the network.

(in-package #:nodecode.test)

;;; --- the stubbed wire -----------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-tripo-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Tripo."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected tripo request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun tripo-asked ()
  (reverse *requests*))

(defun tripo-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun tripo-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun tripo-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun tripo-header (request name)
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

;;; --- lifecycle -------------------------------------------------------------------

(define-test-slice "tripo" "TRIPO-CELL-" :start nodecode-tripo:start-cell)

(define-cell-lifecycle-tests "tripo"
  (:fixture with-tripo-wire)
  (:config "api_key" "tsk_test" "base_url" "https://tripo.test")
  (:help :tripo)
  (:command "tripo")
  (:running (is (nle::find-tool "tripo_generate_model") "tripo_generate_model is registered")
            (is (nle::find-tool "tripo_task"))
            (is (nle::find-tool "tripo_download"))
            (is (nle::find-tool "tripo_balance"))
            (is (equal "https://tripo.test" (nodecode-tripo::root)))
            (is (= 60 (nodecode-tripo::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "tripo_generate_model")) "stop unregisters the tools"))
  (:refused ("timeout_seconds" 0) ("base_url" "ftp://tripo.test")))

(deftest tripo-cell-a-section-without-a-key-refuses ()
  (with-tripo-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (tripo-start "base_url" "https://tripo.test")))
                   ("no key" "TRIPO_API_KEY"))
      (is (null (nle::find-tool "tripo_generate_model")) "a refused start installed nothing"))))

;;; --- the tools -------------------------------------------------------------------

(deftest tripo-cell-a-text-prompt-starts-a-task ()
  (with-tripo-wire
    (with-cell-stop ((tripo-start "api_key" "tsk_test" "base_url" "https://tripo.test"))
      (tripo-answer "{\"code\":0,\"data\":{\"task_id\":\"task_1\"}}")
      (let ((text (tripo-tool "tripo_generate_model" (nlk:json-object "prompt" "a brass key"))))
        (is (search "task_1" text))
        (is (search "text to 3D" text))
        (is (search "about 20 credits" text) "the answer states what it costs"))
      (let ((request (first (tripo-asked))))
        (is (= 1 (length (tripo-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://tripo.test/generation/text-to-model" (second request)))
        (is (equal "Bearer tsk_test" (tripo-header request "authorization")))
        (let ((body (tripo-body request)))
          (is (equal "a brass key" (nlk:json-value body :string "prompt")))
          (is (eq t (nlk:json-value body :boolean "texture")) "textured by default")
          (is (equal "v3.1-20260211" (nlk:json-value body :string "model"))))))))

(deftest tripo-cell-a-call-that-could-not-be-priced-sends-nothing ()
  (with-tripo-wire
    (with-cell-stop ((tripo-start "api_key" "tsk_test" "base_url" "https://tripo.test"))
      (is-carrying (answer (tripo-tool "tripo_generate_model"
                                        (nlk:json-object "prompt" "a key"
                                                         "image" "https://example.test/a.png")))
                   ("exactly one of prompt"))
      (is-carrying (answer (tripo-tool "tripo_retopology"
                                        (nlk:json-object "input" "task_1")))
                   ("face_limit"))
      (is (null (tripo-asked)) "nothing was sent, so nothing was spent"))))

(deftest tripo-cell-a-task-is-read-and-its-file-saved ()
  (with-tripo-wire
    (with-cell-stop ((tripo-start "api_key" "tsk_test" "base_url" "https://tripo.test"))
      (let ((dir (merge-pathnames "tripo-test/" (uiop:temporary-directory))))
        (ensure-directories-exist dir)
        (setf *answer-fn*
              (lambda (method url args)
                (declare (ignore method args))
                (cond ((search "/tasks/" url)
                       (values "{\"code\":0,\"data\":{\"task_id\":\"task_1\",\"status\":\"success\",\"progress\":100,\"credits_consumed\":20,\"output\":{\"model_url\":\"https://cdn.tripo.test/m.glb\"}}}"
                               200))
                      (t (values "glTF-bytes" 200)))))
        (let ((read (tripo-tool "tripo_task" (nlk:json-object "task_id" "task_1"))))
          (is (search "success" read))
          (is (search "20 credits consumed" read))
          (is (search "https://cdn.tripo.test/m.glb" read) "the output file is named, not downloaded"))
        (let ((saved (tripo-tool "tripo_download"
                                  (nlk:json-object "task_id" "task_1"
                                                   "save_dir" (namestring dir)
                                                   "name" "brass-key"))))
          (is (search "saved 1 file" saved))
          (let ((target (merge-pathnames "brass-key.glb" dir)))
            (is (probe-file target) "the model is on disk")
            (is (equal "glTF-bytes" (uiop:read-file-string target))))
          ;; a second save of the same name makes a new one, never overwrites
          (tripo-tool "tripo_download" (nlk:json-object "task_id" "task_1"
                                                           "save_dir" (namestring dir)
                                                           "name" "brass-key"))
          (is (probe-file (merge-pathnames "brass-key (2).glb" dir)) "never overwrites"))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest tripo-cell-the-balance-and-the-command ()
  (with-tripo-wire
    (with-cell-stop ((tripo-start "api_key" "tsk_test" "base_url" "https://tripo.test"))
      (tripo-answer "{\"code\":0,\"data\":{\"balance\":123.5,\"frozen\":4}}")
      (let ((text (tripo-tool "tripo_balance" (nlk:json-object))))
        (is (search "123.5" text))
        (is (search "4 frozen" text)))
      (tripo-answer "{\"code\":0,\"data\":{\"balance\":123.5,\"frozen\":4}}")
      (is (search "123.5" (cell-entry "nodecode-tripo" "tripo" "")))
      (let ((request (first (tripo-asked))))
        (is (eq :get (first request)))
        (is (equal "https://tripo.test/account/balance" (second request)))))))

(deftest tripo-cell-a-refused-key-is-a-repair ()
  (with-tripo-wire
    (with-cell-stop ((tripo-start "api_key" "tsk_test" "base_url" "https://tripo.test"))
      (tripo-answer "{\"code\":1002,\"message\":\"Invalid API key\"}" 401)
      (is-carrying (answer (tripo-tool "tripo_balance" (nlk:json-object)))
                   ("ERROR: tripo:" "HTTP 401") ("Invalid API key") ("check \"api_key\"")))))
