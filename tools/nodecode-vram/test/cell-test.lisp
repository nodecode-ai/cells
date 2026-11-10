;;;; cell-test.lisp --- lifecycle, the three tools, /vram, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, members under their floors or an address without a scheme
;;;; refused), the three tools through the seam the model calls them by --
;;;; their URL, their body, the answer they build -- the estimate and the
;;;; eviction plan, the nvidia-smi fallback to Ollama's own figures, an HTTP
;;;; failure read back as text, and /vram with each of its words. Every
;;;; request is a stubbed NLK:HTTP and the GPU reader a bound *RUN*, so
;;;; nothing reaches Ollama, the network, a GPU or the machine's programs.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *vram-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *vram-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defvar *vram-smi* "24576, 5120"
  "What the stubbed nvidia-smi prints, or NIL for a machine without one.")

(defmacro with-vram-wire (&body body)
  "BODY with NLK:HTTP stubbed and the GPU reader bound: a request is logged in
*VRAM-REQUESTS* and answered by *VRAM-ANSWER-FN*; nvidia-smi is *VRAM-SMI*."
  `(let ((*vram-requests* '())
         (*vram-answer-fn* (lambda (method url args)
                              (declare (ignore method args))
                              (error "unexpected request to ~a" url)))
         (nodecode-vram::*run* (lambda (argv timeout)
                                 (declare (ignore argv timeout))
                                 (if *vram-smi*
                                     (values *vram-smi* 0)
                                     (values "" 127)))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *vram-requests*)
                                (funcall *vram-answer-fn* method url args))
       ,@body)))

(defun vram-wire (&rest routes)
  "An answer per URL suffix: ROUTES of (SUFFIX TEXT &optional STATUS)."
  (lambda (method url args)
    (declare (ignore method args))
    (let ((hit (find-if (lambda (route) (uiop:string-suffix-p url (first route))) routes)))
      (unless hit (error "unexpected request to ~a" url))
      (values (second hit) (or (third hit) 200)))))

(defun vram-set-wire (&rest routes)
  (setf *vram-answer-fn* (apply #'vram-wire routes)))

(defun vram-asked ()
  "The stubbed requests, oldest first."
  (reverse *vram-requests*))

(defun vram-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun vram-ps-json (&rest rows)
  "An /api/ps answer carrying ROWS of (NAME SIZE SIZE-VRAM CONTEXT EXPIRES-AT)."
  (nlk:encode-json-object
   (nlk:json-object "models"
                     (coerce (loop for (name size vram ctx expires) in rows
                                   collect (nlk:json-object "name" name "size" size
                                                             "size_vram" vram
                                                             "context_length" ctx
                                                             "expires_at" expires))
                             'vector))))

(defun vram-tags-json (&rest rows)
  "An /api/tags answer carrying ROWS of (NAME SIZE), as Ollama lists what it has."
  (nlk:encode-json-object
   (nlk:json-object "models"
                     (coerce (loop for (name size) in rows
                                   collect (nlk:json-object "name" name "size" size))
                             'vector))))

(defun vram-show-json (&optional (info nil))
  "An /api/show answer: the model's shape, as Ollama reports it."
  (nlk:encode-json-object
   (nlk:json-object "model_info"
                     (or info
                         (nlk:json-object
                          "general.architecture" "llama"
                          "llama.block_count" 32
                          "llama.attention.head_count" 32
                          "llama.attention.head_count_kv" 8
                          "llama.embedding_length" 4096
                          "llama.attention.key_length" 128
                          "llama.attention.value_length" 128)))))

(defun vram-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "vram" "VRAM-CELL-"
  :start nodecode-vram:start-cell)

(define-cell-lifecycle-tests "vram"
  (:fixture with-vram-wire)
  (:help :vram)
  (:command "vram")
  (:running (is (nle::find-tool "vram_status") "vram_status is registered")
            (is (nle::find-tool "vram_fit") "vram_fit is registered")
            (is (nle::find-tool "vram_unload") "vram_unload is registered")
            (is (equal "http://127.0.0.1:11434" (nodecode-vram::setting :ollama-url)) "the section's default Ollama")
            (is (equal "nvidia-smi" (nodecode-vram::setting :smi-path)) "and the program the GPU is read with"))
  (:stopped (is (null (nle::find-tool "vram_status")) "stop unregisters the status tool")
            (is (null (nle::find-tool "vram_fit")) "and the other two")
            (is (null (nle::find-tool "vram_unload")) "all of them"))
  (:refused ("ollama_url" "localhost:11434") ("hold_pct" 0)
            ("kv_cache_type" "f32") ("reserve_mb" -1))
  (:idle nodecode-vram:vram-error
         (nodecode-vram:status)
         (nodecode-vram:fit "qwen3:8b")))

;;; --- status -------------------------------------------------------------------

(deftest vram-cell-status-names-the-gpu-and-the-models (with-vram-wire)
  (vram-set-wire (list "/api/ps"
                        (vram-ps-json (list "llama3:8b" 5000000000 4000000000 4096
                                             "2026-11-08T12:04:00.5Z"))))
  (with-cell-stop ((vram-start))
    (is-carrying (answer (vram-tool "vram_status" (nlk:json-object)))
                 ("5.0 GB of 24.0 GB" "the pool and what is used of it")
                 ("21%" "as a share")
                 ("ok" "the level word")
                 ("llama3:8b" "the loaded model")
                 ("0.9 GB in system RAM" "what spilled out of the pool")
                 ("until 2026-11-08 12:04" "and when it goes")))
  (is (equal "http://127.0.0.1:11434/api/ps" (second (first (vram-asked)))) "one request, on the url the section names"))

(deftest vram-cell-without-nvidia-smi-uses-ollamas-figures (with-vram-wire)
  (let ((*vram-smi* nil))
    (vram-set-wire (list "/api/ps"
                          (vram-ps-json (list "llama3:8b" 5000000000 4000000000 nil nil))))
    (with-cell-stop ((vram-start))
      (is-carrying (answer (vram-tool "vram_status" (nlk:json-object)))
                   ("GPU pool size unknown" "the fallback says so")
                   ("3.7 GB held by 1 model" "with what Ollama itself reports")
                   ("(until unloaded)" "and no expiry it knows")))))

;;; --- fit ----------------------------------------------------------------------

(deftest vram-cell-fit-says-a-small-model-fits (with-vram-wire)
  (vram-set-wire (list "/api/ps" (vram-ps-json))
                 (list "/api/tags" (vram-tags-json (list "qwen3:8b" 2000000000)))
                 (list "/api/show" (vram-show-json)))
  (with-cell-stop ((vram-start))
    (is-carrying (answer (vram-tool "vram_fit"
                                     (nlk:json-object "model" "qwen3:8b" "context" 4096)))
                 ("qwen3:8b at context 4096" "the model and the context")
                 ("weights plus the cache its shape implies" "how the estimate is made")
                 ("Fits" "the decision")
                 ("5.0 of 24.0 GB" "against the pool"))))

(deftest vram-cell-fit-names-a-model-to-unload-when-it-does-not-fit (with-vram-wire)
  (let ((*vram-smi* "8192, 4096"))
    (vram-set-wire (list "/api/ps"
                          (vram-ps-json (list "llama3:8b" 7000000000 6000000000 4096 nil)))
                   (list "/api/tags" (vram-tags-json (list "big:70b" 6000000000)))
                   (list "/api/show" (vram-show-json)))
    (with-cell-stop ((vram-start))
      (is-carrying (answer (vram-tool "vram_fit" (nlk:json-object "model" "big:70b")))
                   ("Not yet" "the decision")
                   ("unloading llama3:8b" "the one model that would make room")
                   ("vram_unload" "and the tool that does it")))))

(deftest vram-cell-fit-says-nothing-else-may-go (with-vram-wire)
  (let ((*vram-smi* "8192, 4096"))
    (vram-set-wire (list "/api/ps"
                          (vram-ps-json (list "llama3:8b" 7000000000 6000000000 4096 nil)))
                   (list "/api/tags" (vram-tags-json (list "huge:200b" 40000000000)))
                   (list "/api/show" (vram-show-json)))
    (with-cell-stop ((vram-start "pinned_models" (vector "llama3:8b")))
      (is-carrying (answer (vram-tool "vram_fit" (nlk:json-object "model" "huge:200b")))
                   ("Not yet" "it does not fit")
                   ("nothing else loaded may be unloaded" "a pinned model is not a candidate")
                   ("runs it on the CPU" "and Ollama spills it")))))

;;; --- unload -------------------------------------------------------------------

(deftest vram-cell-unload-posts-keep-alive-zero (with-vram-wire)
  (vram-set-wire (list "/api/ps"
                        (vram-ps-json (list "llama3:8b" 5000000000 4000000000 4096 nil)))
                 (list "/api/generate" "{\"done\":true}"))
  (with-cell-stop ((vram-start))
    (is-carrying (answer (vram-tool "vram_unload" (nlk:json-object "model" "llama3:8b")))
                 ("Unloaded llama3:8b" "the model by name")
                 ("loads again on its next call" "and what that means")))
  (let ((request (find :post (vram-asked) :key #'first)))
    (is (equal :post (first request)) "the unload is a POST")
    (is (equal "http://127.0.0.1:11434/api/generate" (second request)) "to the endpoint the original used")
    (is (equal "llama3:8b" (nlk:json-value (vram-request-body request) :string "model")) "naming the model")
    (is (eql 0 (nlk:json-value (vram-request-body request) :integer "keep_alive")) "with keep_alive 0, which is what unloads it")))

(deftest vram-cell-unload-refuses-a-pinned-or-missing-model (with-vram-wire)
  (vram-set-wire (list "/api/ps"
                        (vram-ps-json (list "llama3:8b" 5000000000 4000000000 4096 nil)))
                 (list "/api/generate" "{\"done\":true}"))
  (with-cell-stop ((vram-start "pinned_models" (vector "llama3:8b")))
    (is-carrying (answer (vram-tool "vram_unload" (nlk:json-object "model" "llama3:8b")))
                 ("ERROR:" "a refusal the model reads")
                 ("pinned" "saying why"))
    (is-carrying (answer (vram-tool "vram_unload" (nlk:json-object "model" "qwen3:8b")))
                 ("is not loaded" "and a name Ollama does not hold")
                 ("llama3:8b" "is answered with what is")))
  (is (null (remove-if (lambda (url) (uiop:string-suffix-p url "/api/ps")) (mapcar #'second (vram-asked)))) "neither refusal sent a generate call"))

(deftest vram-cell-an-http-failure-is-text-not-a-crash (with-vram-wire)
  (vram-set-wire (list "/api/ps" "{\"error\":\"boom\"}" 500))
  (with-cell-stop ((vram-start))
    (is-carrying (answer (vram-tool "vram_fit" (nlk:json-object "model" "qwen3:8b")))
                 ("ERROR:" "a refusal")
                 ("500" "naming the status")
                 ("boom" "and what Ollama said"))))

;;; --- /vram --------------------------------------------------------------------

(deftest vram-cell-the-command-answers-each-of-its-words (with-vram-wire)
  (vram-set-wire (list "/api/ps"
                        (vram-ps-json (list "llama3:8b" 5000000000 4000000000 4096 nil)))
                 (list "/api/tags" (vram-tags-json (list "qwen3:8b" 2000000000)))
                 (list "/api/show" (vram-show-json))
                 (list "/api/generate" "{\"done\":true}"))
  (with-cell-stop ((vram-start))
    (is-carrying (answer (cell-entry "nodecode-vram" "vram" ""))
                 ("5.0 GB of 24.0 GB" "no word: the status"))
    (is-carrying (answer (cell-entry "nodecode-vram" "vram" "models"))
                 ("llama3:8b" "models: only the loaded ones"))
    (is-carrying (answer (cell-entry "nodecode-vram" "vram" "settings"))
                 ("ollama_url: http://127.0.0.1:11434" "settings: the address")
                 ("hold_pct: 95" "and the hold line"))
    (is-carrying (answer (cell-entry "nodecode-vram" "vram" "fit qwen3:8b"))
                 ("Fits" "fit: a model by name"))
    (is-carrying (answer (cell-entry "nodecode-vram" "vram" "evict llama3:8b"))
                 ("Unloaded llama3:8b" "evict: the action"))
    (is-carrying (answer (cell-entry "nodecode-vram" "vram" "nonsense"))
                 ("Usage: /vram" "an unknown word answers the usage"))))
