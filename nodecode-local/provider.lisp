;;;; provider.lisp --- what the local provider is: omp's tiny models, and where their workers live.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; providers/local.kdl (the seed rows and their kinds), coding-agent/src/
;;;; tiny/models.ts (each tiny model's ONNX and MLX weights), title-protocol.ts
;;;; (a worker's socket and log names), mlx-runtime.ts (the mlx-lm venv and the
;;;; weights directory), utils/src/dirs.ts (where omp keeps them), and the
;;;; bundled rows of catalog/src/models.json, which models.json in this folder
;;;; carries.
;;;;
;;;; omp's `local' provider is not an endpoint. Its models run on the machine,
;;;; each in a worker process of its own that owns a Unix socket named after
;;;; the model and speaks newline-delimited JSON (title-protocol.ts): the ONNX
;;;; worker is the omp binary itself re-entered as `omp
;;;; __omp_worker_tiny_inference' (transformers.js over onnxruntime-node, which
;;;; only omp's Bun runtime carries), and the MLX worker is a Python script,
;;;; mlx-server.py, run by a venv holding mlx-lm (Apple silicon only). A worker
;;;; serves every process on the machine and exits on its own after fifteen
;;;; idle minutes. Of the thirteen rows, eight are text models (kind `tiny');
;;;; Kokoro is speech and Parakeet and Whisper are transcription, which other
;;;; omp workers run and this cell does not.

(in-package #:nodecode-local)

(defparameter +base+ "local://inference"
  "The base omp's rows name: no address, the lane's own marker.")

(defparameter +worker-arg+ "__omp_worker_tiny_inference"
  "The hidden omp subcommand that boots the ONNX worker (TINY_WORKER_ARG).")

(defparameter +mlx-lm-version+ "0.31.3"
  "The mlx-lm release omp's MLX venv pins (MLX_LM_VERSION).")

(defparameter +idle-seconds+ (* 15 60)
  "How long an idle worker lives (TINY_WORKER_IDLE_MS, MLX_IDLE_SECONDS).")

;;; key, ONNX repo, MLX repo, reasoning, why the ONNX backend refuses it
(defparameter +tiny-models+
  '(("lfm2.5-230m" "LiquidAI/LFM2.5-230M-ONNX" "LiquidAI/LFM2.5-230M-MLX-4bit" nil nil)
    ("lfm2.5-350m" "onnx-community/LFM2.5-350M-ONNX" "LiquidAI/LFM2.5-350M-MLX-4bit" nil nil)
    ("falcon-h1-90m" "onnx-community/Falcon-H1-Tiny-90M-Instruct-ONNX" "mlx-community/Falcon-H1-Tiny-90M-Instruct-4bit" nil nil)
    ("qwen3-1.7b" "onnx-community/Qwen3-1.7B-ONNX" "mlx-community/Qwen3-1.7B-4bit" t
     "onnxruntime-node does not support Qwen3 RotaryEmbedding cache updates in onnx-community/Qwen3-1.7B-ONNX")
    ("llama3.2:3b" "onnx-community/Llama-3.2-3B-Instruct-ONNX" "mlx-community/Llama-3.2-3B-Instruct-4bit" nil nil)
    ("gemma-3-1b" "onnx-community/gemma-3-1b-it-ONNX" "mlx-community/gemma-3-1b-it-4bit" nil nil)
    ("qwen2.5-1.5b" "onnx-community/Qwen2.5-1.5B-Instruct" "mlx-community/Qwen2.5-1.5B-Instruct-4bit" nil nil)
    ("lfm2-1.2b" "onnx-community/LFM2-1.2B-ONNX" "mlx-community/LFM2-1.2B-4bit" nil nil))
  "omp's tiny models (TINY_TITLE_LOCAL_MODELS, TINY_MEMORY_LOCAL_MODELS): the
text models a worker serves.")

(defun tiny-model (model-id)
  "MODEL-ID's spec, or NIL when it is no tiny model."
  (assoc model-id +tiny-models+ :test #'equal))

(defun mlx-repo (model-id) (third (tiny-model model-id)))

(defun onnx-refusal (model-id) (fifth (tiny-model model-id)))

;;; --- where omp keeps its workers --------------------------------------------------

(defun home () (user-homedir-pathname))

(defun config-root ()
  "omp's config root: ~/.omp, or ~/$PI_CONFIG_DIR."
  (merge-pathnames (format nil "~a/" (or (nle::credential-env "PI_CONFIG_DIR") ".omp")) (home)))

(defun xdg-root (variable)
  "$VARIABLE/omp/ when that directory exists (dirs.ts resolveIf), else NIL;
omp reads the XDG roots on Linux and macOS only."
  (alexandria:when-let (value (and (member (uiop:operating-system) '(:linux :macosx))
                                   (nle::credential-env variable)))
    (let ((root (uiop:ensure-directory-pathname (merge-pathnames "omp/" (uiop:ensure-directory-pathname value)))))
      (and (uiop:directory-exists-p root) root))))

(defun default-runtime-dir ()
  "Where omp's tiny workers own their sockets (getTinyWorkerRuntimeDir):
$XDG_STATE_HOME/omp/run/tiny/, else ~/.omp/run/tiny/."
  (merge-pathnames "run/tiny/" (or (xdg-root "XDG_STATE_HOME") (config-root))))

(defun tiny-models-dir ()
  "Where omp caches tiny-model weights (getTinyModelsCacheDir):
$XDG_CACHE_HOME/omp/cache/tiny-models/, else ~/.omp/agent/cache/tiny-models/."
  (alexandria:if-let (xdg (xdg-root "XDG_CACHE_HOME"))
    (merge-pathnames "cache/tiny-models/" xdg)
    (merge-pathnames "agent/cache/tiny-models/" (config-root))))

(defun mlx-model-dir (model-id)
  "The weights directory of MODEL-ID's MLX repo: tiny-models/mlx/<org>--<name>/."
  (merge-pathnames (format nil "mlx/~a/" (substitute-string (mlx-repo model-id) "/" "--")) (tiny-models-dir)))

(defun mlx-python ()
  "omp's mlx-lm venv interpreter when it is there, else NIL."
  (let ((python (merge-pathnames (format nil "tiny-mlx-runtime/mlx-lm-~a/bin/python" +mlx-lm-version+)
                                 (uiop:pathname-parent-directory-pathname (tiny-models-dir)))))
    (and (probe-file python) (uiop:native-namestring python))))

(defun substitute-string (text old new)
  "TEXT with the first OLD replaced by NEW."
  (let ((at (search old text)))
    (if at (concatenate 'string (subseq text 0 at) new (subseq text (+ at (length old)))) text)))

(defun worker-name (model-id backend)
  "The socket and log name of MODEL-ID's BACKEND worker: <model>-<backend>,
anything but [A-Za-z0-9._-] an underscore."
  (map 'string (lambda (char) (if (or (alphanumericp char) (find char "._-")) char #\_))
       (format nil "~a-~(~a~)" model-id backend)))

;;; --- the catalog --------------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string (asdf:system-relative-pathname "nodecode-local" "models.json")))
  "omp's bundled local rows, read when this file loads: a vector of objects.")

(defun catalog-model (row)
  "ROW as the catalog keeps a model: no window, no price, and no tool calls,
as omp seeds them (supports-tools #false)."
  (nle::make-catalog-model
   (nlk:json-value row :string "name")
   nil nil
   (or (nlk:json-value row :array "input") #("text"))
   #("text")
   (sort (remove-if-not #'nle::effort-rank (coerce (nlk:json-array row "efforts") 'list))
         #'< :key #'nle::effort-rank)
   (nlk:json-value row :boolean "reasoning")
   nil
   nil
   nil))

(defun catalog-row (&optional prior)
  "The local provider as a models.dev provider: this cell's lane package,
omp's marker base, no key, and its eight text models (the speech and
transcription rows left out) over PRIOR's."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          for id = (nlk:json-value row :string "id")
          when (tiny-model id)
            do (setf (gethash id models) (catalog-model row)))
    (nlk:json-object "name" "Local models"
                     "npm" +npm+
                     "api" +base+
                     "env" #()
                     "models" models)))
