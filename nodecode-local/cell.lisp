;;;; cell.lisp --- the cell: omp's tiny local models on a lane of their own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No lane of the organism reaches a model through a worker's socket, so the
;;;; cell registers one, local, whose stream is STREAM-ROUND below: the
;;;; conversation as omp's local-inference transport flattens it
;;;; (local-inference-api.ts), one chat request to the model's worker
;;;; (worker.lisp), the text it answers as the round's message. Two hooks,
;;;; each declining for every provider but local:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries the local row: this cell's
;;;;                          lane package and omp's eight text models, with
;;;;                          no tool calls, as omp seeds them
;;;;   :CREDENTIAL            no key: a worker takes none
;;;;
;;;; The models cannot call tools, so /models (which lists turn models) does
;;;; not offer them; they serve side work, the job omp gives them (titles,
;;;; memory, small completions): /model-aux local lfm2.5-230m makes one the
;;;; auxiliary model that names sessions and writes recaps.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "local": {"backend": "auto", "omp_command": "omp", "python": "",
;;;;             "runtime_dir": "", "spawn": true}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-local)

;;; --- the catalog ---------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
the local row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with the local row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog."
  (setf *catalog* (cons nil nil)))

(defun credential (op next)
  "The :CREDENTIAL answer for local: none to send (allow-unauthenticated)."
  ;; Never NEXT for local: the ladder behind this point would lend it
  ;; OPENAI_API_KEY, which nothing here sends but a status line would show.
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential "local" :public)
      (funcall next op)))

;;; --- the request ------------------------------------------------------------------

(defun join-turns (left right)
  "LEFT and RIGHT, a blank line between them, either alone when the other is empty."
  (cond ((zerop (length left)) right)
        ((zerop (length right)) left)
        (t (format nil "~a~%~%~a" left right))))

(defun worker-messages (context)
  "CONTEXT as the worker's chat (buildLocalInferenceMessages): the system
prompt, a history system message as system, each assistant turn's text
folded into the user turn after it (the worker's chat templates alternate
system and user only), a tool result as the user's."
  (let ((messages '())
        (pending "")
        (system (nle::compiled-turn-context-system-prompt context)))
    (when (plusp (length system))
      (push (nlk:json-object "role" "system" "content" system) messages))
    (loop for message across (coerce (nle::request-messages context) 'vector)
          for role = (nlk:json-value message :string "role")
          for text = (nle::content-text (gethash "content" message))
          do (cond ((equal role "assistant") (setf pending (join-turns pending text)))
                   ((equal role "system") (push (nlk:json-object "role" "system" "content" text) messages))
                   (t (let ((content (join-turns pending text)))
                        (setf pending "")
                        (when (plusp (length content))
                          (push (nlk:json-object "role" "user" "content" content) messages))))))
    (when (plusp (length pending))
      (push (nlk:json-object "role" "user" "content" pending) messages))
    (coerce (nreverse messages) 'vector)))

(defun max-new-tokens (config)
  "The tokens a round may generate: the configured ceiling, else 256, kept
within 1 and 1024 (COMPLETION_DEFAULT_MAX_NEW_TOKENS, COMPLETION_MAX_NEW_TOKENS)."
  (min (max 1 (or (nle::effective-provider-config-max-completion-tokens config) 256)) 1024))

;;; --- the lane ---------------------------------------------------------------------

(defun stream-round (context &key (on-part nle::*turn-part-fn*))
  "The lane's stream: one chat request to the model's worker, the text it
generates as the round's message. => (values MESSAGE USAGE FINISH-REASON REQUEST-JSON)"
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model (nle::effective-provider-config-model config))
         (backend (backend))
         (asm (nle::make-lane-assembly :on-part on-part)))
    (unless (tiny-model model)
      (error 'nle::provider-config-error
             :status 404
             :detail (format nil "Local inference cannot run model ~a/~a; it runs ~{~a~^, ~}"
                             +provider+ model (mapcar #'first +tiny-models+))))
    (when (and (eq backend :onnx) (onnx-refusal model))
      (error 'nle::provider-config-error
             :status 400
             :detail (format nil "~a runs on the MLX backend only (local.backend \"mlx\"): ~a"
                             model (onnx-refusal model))))
    (let ((stream (worker-stream model backend))
          (text nil)
          (request nil))
      (unwind-protect
           (multiple-value-setq (text request)
             (chat stream (worker-messages context) (max-new-tokens config)
                   (max 300 (nle::effective-provider-config-request-timeout config))))
        (ignore-errors (close stream :abort t)))
      (nle::emit-stream-part on-part :stream-start)
      (when (zerop (length text))
        (error 'nle::provider-error :status 500 :scope :contract :detail "Local inference returned no output."))
      (nle::assembly-text-delta asm "txt-0" text)
      (nle::assembly-close-spans asm :order '(:tools :reasoning :text))
      (nle::flush-thinking-tag asm)
      (let ((content (get-output-stream-string (nle::lane-assembly-content asm))))
        (nle::emit-stream-part on-part :finish)
        (values (nlk:json-object "role" "assistant"
                                 "content" (if (string= content "") :null content)
                                 :when (nle::lane-assembly-reasoning-seen-p asm) "reasoning_content"
                                 (get-output-stream-string (nle::lane-assembly-reasoning asm)))
                nil
                "stop"
                (sb-ext:string-to-octets (nlk:encode-json-object request) :external-format :utf-8))))))

(defun register-lane ()
  "The local lane: omp's tiny-model workers under this provider's name."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'stream-round
                            ;; the family only names the env ladder's
                            ;; default key, which the credential hook never
                            ;; falls to
                            :family :openai-completions
                            :reasoning-carry :text
                            :default-endpoint +base+
                            :path ""
                            :npm +npm+)))

(defun unregister-lane ()
  "Take the local lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))

(defun start ()
  "Register the lane; on the way down take it out and drop the merged catalog."
  (forget-catalog)
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (nle:on-stop #'forget-catalog))

(nle:define-cell local
  (:section ("local")
    (:guide "omp's tiny models run in omp's own workers: with omp installed, the ONNX worker starts from omp_command; on Apple silicon, backend mlx runs omp's mlx-server.py under python (omp's mlx-lm venv by default); make one the auxiliary model with /model-aux local lfm2.5-230m")
    ("backend" :choice :options '("auto" "onnx" "mlx") :default "auto"
     :doc "which worker serves a model: auto is MLX when PI_TINY_DEVICE says mlx, else ONNX, as omp chooses")
    ("omp_command" :string :default "omp"
     :doc "the omp executable that starts an ONNX worker (omp __omp_worker_tiny_inference)")
    ("python" :string :default ""
     :doc "the Python with mlx-lm that runs an MLX worker; empty is omp's mlx-lm venv, else python3")
    ("runtime_dir" :string :default ""
     :doc "where the workers own their sockets; empty is omp's ($XDG_STATE_HOME/omp/run/tiny or ~/.omp/run/tiny)")
    ("spawn" :boolean :default t
     :doc "whether a round starts a worker that is not running, or only uses one already up"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential))
