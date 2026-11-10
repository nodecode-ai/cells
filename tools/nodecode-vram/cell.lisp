;;;; cell.lisp --- Ollama's GPU room and the local models loaded in it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of jimmy77733/coffee-vram (see NOTICE). The original guards
;;;; the local Ollama calls a harness makes: before one it estimates whether
;;;; the model fits next to what is already loaded, holds the call while an
;;;; idle model is unloaded, and shows the numbers. Three of those four are
;;;; questions and one is an action, and all four are here: vram_status (the
;;;; GPU and what /api/ps holds), vram_fit (the estimator and the decision),
;;;; vram_unload (the keep_alive: 0 the original's evict sent), and /vram for
;;;; the operator. The middleware that queued live calls, the keep-warm loop
;;;; and the desktop chip are the harness's own surfaces and have no
;;;; counterpart here (NOTICE).
;;;;
;;;; Ollama is asked on the url the section names, and nothing else is
;;;; dialled: /api/ps, /api/tags, /api/show, /api/generate. The GPU numbers
;;;; come from nvidia-smi when it is installed, else from Ollama's own
;;;; figures -- the fallback order the original used.

(defpackage #:nodecode-vram
  (:use #:cl)
  (:nicknames #:vram)
  (:export #:status #:fit #:unload #:status-text #:models-text #:settings-text
           #:*primer* #:vram-error #:vram-error-detail))

(in-package #:nodecode-vram)

(nlk:define-peripheral vram :not-running t)

;;; --- the words -------------------------------------------------------------------

(defparameter +mib+ (expt 1024 2)
  "A mebibyte: nvidia-smi reports whole mebibytes.")

(defparameter +gib+ (expt 1024 3)
  "A gibibyte: Ollama reports bytes, people read gigabytes.")

(defparameter +kv-cache-bytes+
  '((f16 . 2d0) (q8_0 . 1.0625d0) (q4_0 . 0.5625d0))
  "Bytes a cached KV element costs, per cache type: q8_0 and q4_0 carry one
f16 scale per 32-element block, as the original counted them.")

(defparameter +default-context+ 4096
  "The context Ollama loads with when neither the server nor the model says
otherwise -- Ollama's own default, as the original used it.")

(defparameter +usage+ "Usage: /vram [status | models | settings | fit <model> [context] | evict <model>]"
  "What /vram answers when it does not know the word.")

(defparameter +primer+
  "vram: the GPU room on this machine and the local Ollama models loaded in it.
  vram_status {}                    the GPU, what is loaded, the room left
  vram_fit {model, context?}        whether a model fits now, and what to unload if not
  vram_unload {model}              unload one idle model from Ollama (keep_alive 0)
Check vram_status before switching to a different local model: loading one that
does not fit unloads another, or runs partly on the CPU. /vram is the
operator's way to the same answers. (help :vram) has this while the cell runs."
  "What (help :vram) answers while the cell runs.")

(defparameter +setting-defaults+
  '((:ollama-url . "http://127.0.0.1:11434") (:smi-path . "nvidia-smi")
    (:gpu-total-mb . 0) (:reserve-mb . 2048) (:warn-pct . 85) (:hold-pct . 95)
    (:kv-cache-type . "f16") (:estimate-overhead-pct . 10) (:pinned-models . nil))
  "What each setting reads as when the section leaves it out or blank: the
declaration's own defaults, repeated here because a present-but-empty section
answers \"\" for a string member rather than the declared default.")

(defparameter *setting-rows*
  '((:ollama-url "ollama_url") (:smi-path "smi_path") (:gpu-total-mb "gpu_total_mb")
    (:reserve-mb "reserve_mb") (:warn-pct "warn_pct") (:hold-pct "hold_pct")
    (:kv-cache-type "kv_cache_type") (:estimate-overhead-pct "estimate_overhead_pct")
    (:pinned-models "pinned_models"))
  "The settings /vram settings prints, in the order the section declares them.")

;;; --- the section, read back --------------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES with this cell's own defaults under them -- an empty
section reads as every default -- and the Ollama address checked once, here:
a url without a scheme would be dialled as a file path."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (loop for (key . default) in +setting-defaults+
          for value = (getf plist key)
          do (setf (getf plist key)
                   (cond ((null value) default)
                         ((stringp value) (or (nonblank value) default))
                         (t value))))
    (let ((root (string-right-trim "/" (getf plist :ollama-url))))
      (unless (or (uiop:string-prefix-p "http://" root)
                  (uiop:string-prefix-p "https://" root))
        (nlk:config-error "ollama_url ~s has no http:// or https:// scheme: set it to the address your Ollama answers on, http://127.0.0.1:11434 unless you moved it" root))
      (setf (getf plist :ollama-url) root))
    (when (> (getf plist :warn-pct) (getf plist :hold-pct))
      (setf (getf plist :warn-pct) (getf plist :hold-pct)))
    plist))

;;; --- one request ------------------------------------------------------------------

(defun headers ()
  "The headers an Ollama call carries: JSON in and out, nothing else."
  (list (cons "content-type" "application/json")
        (cons "accept" "application/json")))

(defun ask (method path &key body (timeout 3) (decode t))
  "PATH on the configured Ollama, as METHOD, with BODY as its JSON => the
decoded answer (or T when DECODE is NIL), or a refusal saying what Ollama
said. An unreachable Ollama is a refusal too, naming the url."
  (let* ((url (concatenate 'string (setting :ollama-url) path))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (if body
              (nlk:http :post url :headers (headers)
                        :content (nlk:encode-json-object body)
                        :timeout timeout :connect-timeout (min 3 timeout))
              (nlk:http :get url :headers (headers)
                        :timeout timeout :connect-timeout (min 3 timeout))))
      (error (condition)
        (fail "Ollama at ~a did not answer: ~a" url
              (nlk:one-line (princ-to-string condition)))))
    (let ((text (nlk:body-text answer)))
      (unless (and (integerp status) (< status 300))
        (fail "Ollama answered HTTP ~a on ~a~@[: ~a~]" status path
              (and (nonblank text) (nlk:clip (nlk:one-line text) 300 :disclose t))))
      (cond ((not decode) t)
            ((null (nonblank text)) (nlk:make-json-object))
            (t (handler-case (nlk:decode-json text)
                 (error ()
                   (fail "Ollama answered ~a with something that is not JSON: ~a"
                         path (nlk:clip (nlk:one-line text) 300 :disclose t)))))))))

;;; --- what /api/ps holds -----------------------------------------------------------

(defun whole (value)
  "VALUE as an integer, 0 when it is not a number."
  (if (realp value) (truncate value) 0))

(defun parse-ps (payload)
  "PAYLOAD's models as plists of :NAME :SIZE :SIZE-VRAM :CONTEXT-LENGTH
and :EXPIRES-AT, in the order Ollama lists them."
  (loop for entry across (nlk:json-array payload "models")
        for name = (or (nlk:json-value entry :string "name")
                       (nlk:json-value entry :string "model"))
        when name
          collect (list :name name
                        :size (whole (nlk:json-value entry :number "size"))
                        :size-vram (whole (or (nlk:json-value entry :number "size_vram")
                                              (nlk:json-value entry :number "size")))
                        :context-length (nlk:json-value entry :integer "context_length")
                        :expires-at (nlk:json-value entry :string "expires_at"))))

(defun loaded-models ()
  "/api/ps as plists, or a refusal naming what Ollama said."
  (parse-ps (ask :get "/api/ps" :timeout 2)))

(defun probe ()
  "Ollama's loaded models => (VALUES LOADED REACHABLE-P); an Ollama that
does not answer is not an error here, it is one of the answers."
  (handler-case (values (loaded-models) t)
    (vram-error () (values nil nil))))

(defun matching (name models)
  "The entry in MODELS NAME names: itself, or the one tag of a bare model
name (qwen3 asks for qwen3:8b). NIL when nothing matches."
  (let ((name (nonblank name)))
    (and name
         (or (find name models :key (lambda (model) (getf model :name)) :test #'string=)
             (find-if (lambda (model)
                        (uiop:string-prefix-p (concatenate 'string name ":")
                                               (getf model :name)))
                      models)))))

;;; --- the GPU numbers ---------------------------------------------------------------

(defun run-cli (argv timeout)
  "Run ARGV with no shell of ours, its output and its errors to one scratch
file => (VALUES OUTPUT EXIT). EXIT is 127 for a program that is not
installed and 124 for one that outlived TIMEOUT seconds."
  (let* ((path (merge-pathnames (format nil "nodecode-vram-~36r.out" (random most-positive-fixnum))
                                 (uiop:temporary-directory)))
         (process (ignore-errors
                   (sb-ext:run-program (first argv) (rest argv)
                                       :search t :wait nil :input nil
                                       :output path :error :output))))
    (if (null process)
        (values "" 127)
        (let ((deadline (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second)))
              (killed nil))
          (loop
            (cond ((not (sb-ext:process-alive-p process)) (return))
                  ((> (get-internal-real-time) deadline)
                   (setf killed t)
                   (ignore-errors (sb-ext:process-kill process 9))
                   (return))
                  (t (sleep 1/20))))
          (ignore-errors (sb-ext:process-wait process))
          (unwind-protect
               (values (if (probe-file path) (or (uiop:read-file-string path) "") "")
                       (if killed 124 (or (sb-ext:process-exit-code process) 1)))
            (ignore-errors (delete-file path)))))))

(defparameter *run* #'run-cli
  "How the cell runs nvidia-smi: (funcall *RUN* ARGV TIMEOUT) => (VALUES OUTPUT EXIT).
A test binds its own, so no test needs a GPU or the program.")

(defun parse-smi (output)
  "OUTPUT's first row of whole mebibytes => (TOTAL USED) in bytes, or NIL."
  (loop for line in (uiop:split-string (or output "") :separator '(#\Newline))
        for parts = (remove "" (mapcar (lambda (part) (string-trim " " part))
                                        (uiop:split-string line :separator '(#\, #\Space)))
                            :test #'string=)
        when (and (= (length parts) 2)
                  (every (lambda (number)
                           (and (plusp (length number)) (every #'digit-char-p number)))
                         parts))
          return (list (* (parse-integer (first parts)) +mib+)
                       (* (parse-integer (second parts)) +mib+))))

(defun smi-memory ()
  "nvidia-smi's whole-GPU total and used bytes => (TOTAL USED), or NIL when
the program is not there or answers nothing usable."
  (let ((argv (list (setting :smi-path)
                     "--query-gpu=memory.total,memory.used"
                     "--format=csv,noheader,nounits")))
    (multiple-value-bind (output exit) (funcall *run* argv 5)
      (and (eql exit 0) (parse-smi output)))))

(defun memory (loaded)
  "The pool the fit check budgets against => (VALUES TOTAL USED SOURCE), in
bytes. TOTAL is 0 when nothing can say how big the pool is, and then the
answer is Ollama's own figures only."
  (let* ((ollama (loop for model in loaded sum (getf model :size-vram) into bytes
                        finally (return bytes)))
         (override (setting :gpu-total-mb))
         (smi (smi-memory)))
    (cond
      ((and (integerp override) (plusp override))
       (values (* override +mib+) (or (second smi) ollama) (if smi "nvidia-smi" "ollama")))
      (smi (values (first smi) (second smi) "nvidia-smi"))
      (t (values 0 ollama "ollama")))))

(defun cap (total)
  "The highest `used' the fit check allows: the reserve and the hold line,
whichever is lower."
  (if (plusp total)
      (max 0 (min (- total (* (setting :reserve-mb) +mib+))
                   (truncate (* total (setting :hold-pct)) 100)))
      0))

(defun pct (part whole)
  "PART as a share of WHOLE, 0 when WHOLE is 0."
  (if (plusp whole) (* 100d0 (/ part whole)) 0d0))

(defun gb (bytes)
  "BYTES as the gigabytes people read."
  (format nil "~,1f" (/ (float bytes 1d0) +gib+)))

(defun level (used total)
  "The word the original's status chip was coloured by."
  (let ((share (round (pct used total))))
    (cond ((zerop total) "unknown")
          ((>= share (setting :hold-pct)) "hold")
          ((>= share (setting :warn-pct)) "warn")
          (t "ok"))))

;;; --- the estimator ------------------------------------------------------------------

(defun model-file-size (model)
  "MODEL's weights on disk, from /api/tags, or 0 when the tag list misses it."
  (let ((tags (ask :get "/api/tags" :timeout 5)))
    (loop for entry across (nlk:json-array tags "models")
          for name = (or (nlk:json-value entry :string "name")
                         (nlk:json-value entry :string "model"))
          when (and name (or (string= name model)
                             (uiop:string-prefix-p (concatenate 'string model ":") name)))
            return (whole (nlk:json-value entry :number "size"))
          finally (return 0))))

(defun model-info (model)
  "MODEL's model_info from /api/show, or NIL when Ollama has no shape for it."
  (let ((answer (ask :post "/api/show" :body (nlk:json-object "model" model)
                     :timeout 10)))
    (nlk:json-value answer :object "model_info")))

(defun first-int (value)
  "VALUE as the integer it is, or the largest of the integers it holds, or
NIL: a per-layer array marks the cache-free layers with 0."
  (cond ((integerp value) value)
        ((vectorp value)
         (let ((ints (remove-if-not #'integerp (coerce value 'list))))
           (and ints (reduce #'max ints))))
        (t nil)))

(defun kv-bytes-per-token (info kv-type)
  "The KV cache bytes one token of context costs, or NIL when INFO lacks the
shape. A hybrid model caches only every full_attention_interval-th layer;
counting every layer would overestimate it about four times, as the original
noted."
  (let ((arch (nlk:json-value info :string "general.architecture")))
    (when arch
      (flet ((key-of (name)
               (nlk:json-value info :any (format nil "~a.~a" arch name))))
        (let* ((layers (first-int (key-of "block_count")))
               (heads (first-int (key-of "attention.head_count")))
               (raw-kv (key-of "attention.head_count_kv"))
               (embedding (first-int (key-of "embedding_length")))
               (key-len (or (first-int (key-of "attention.key_length"))
                            (and embedding heads (plusp heads) (floor embedding heads))))
               (value-len (or (first-int (key-of "attention.value_length")) key-len)))
          (when (and layers key-len value-len (plusp layers) (plusp key-len))
            (let ((head-layers
                    (if (vectorp raw-kv)
                        (loop for head across raw-kv
                              when (and (integerp head) (plusp head)) sum head)
                        (let ((kv-heads (or (first-int raw-kv) heads))
                              (interval (first-int (key-of "full_attention_interval"))))
                          (and kv-heads
                               (* kv-heads (if (and interval (> interval 1))
                                               (floor layers interval)
                                               layers)))))))
              (when (and head-layers (plusp head-layers))
                (truncate (* head-layers (+ key-len value-len)
                             (or (cdr (assoc kv-type +kv-cache-bytes+ :test #'string-equal))
                                 (cdr (assoc 'f16 +kv-cache-bytes+)))))))))))))

(defun effective-context (asked resident)
  "The context Ollama will actually load with: the one asked for, else the one
a resident model is loaded at, else Ollama's default."
  (or (and asked (plusp asked) asked)
      (getf resident :context-length)
      +default-context+))

(defun estimate (model context resident)
  "MODEL at CONTEXT => a plist of :WEIGHTS :KV :OVERHEAD :TOTAL :SOURCE
:CONTEXT in bytes. A model the cell has seen loaded uses its measured size,
as the original's observed path did."
  (let* ((per-token (kv-bytes-per-token (model-info model) (setting :kv-cache-type)))
         (observed (and resident (plusp (whole (getf resident :size)))))
         (previous (getf resident :context-length))
         (weights (if observed (whole (getf resident :size)) (model-file-size model)))
         (grow (if observed (max 0 (- context (or previous context))) context))
         (kv (if per-token (truncate (* grow per-token)) 0))
         (overhead (truncate (* (+ weights kv) (setting :estimate-overhead-pct)) 100)))
    (list :weights weights :kv kv :overhead overhead :total (+ weights kv overhead)
          :source (cond (observed "observed") (per-token "formula") (t "weights-only"))
          :context context)))

(defun evictable (loaded model)
  "The loaded models a fit check for MODEL may unload: everything else that
is not pinned, the one expiring soonest first."
  (sort (remove-if (lambda (other)
                     (or (string= (getf other :name) model)
                         (member (getf other :name) (setting :pinned-models)
                                 :test #'string-equal)))
                   (copy-list loaded))
        #'string< :key (lambda (other) (or (getf other :expires-at) ""))))

(defun plan-eviction (loaded model need free)
  "The fewest loaded models to unload so NEED fits in FREE, the one expiring
soonest first => (VALUES NAMES FREED), or NIL when no subset makes room."
  (let ((names '()) (freed 0))
    (dolist (other (evictable loaded model) (values nil nil))
      (push (getf other :name) names)
      (incf freed (getf other :size-vram))
      (when (<= need (+ free freed))
        (return (values (nreverse names) freed))))))

(defun until (stamp)
  "STAMP, an Ollama expiry, as the words a person reads."
  (let ((stamp (nonblank stamp)))
    (cond ((null stamp) "until unloaded")
          ((and (>= (length stamp) 16) (char= (char stamp 10) #\T))
           (format nil "until ~a ~a" (subseq stamp 0 10) (subseq stamp 11 16)))
          (t (format nil "until ~a" stamp)))))

(defun assure-running ()
  "Refuse while the cell is not started, with the cell's own condition: a verb
answered off a section that is not in the config would read half a cell."
  (running-settings)
  t)

;;; --- the answers --------------------------------------------------------------------

(defun model-lines (loaded)
  "LOADED as one line each: the room it takes, what spills, its context,
when it goes."
  (if (null loaded)
      (list "  no models loaded")
      (loop for model in loaded
            for spill = (max 0 (- (whole (getf model :size)) (getf model :size-vram)))
            collect (format nil "  ~a  ~a GB in VRAM~@[, ~a GB in system RAM~]~@[, ctx ~a~]  (~a)"
                            (getf model :name) (gb (getf model :size-vram))
                            (and (plusp spill) (gb spill))
                            (getf model :context-length)
                            (if (member (getf model :name) (setting :pinned-models)
                                        :test #'string-equal)
                                "pinned"
                                (until (getf model :expires-at)))))))

(defun reserve-line ()
  (format nil "~,1f" (/ (setting :reserve-mb) 1024.0)))

(defun status-text (loaded total used source reachable)
  "The lines vram_status and /vram answer with."
  (with-output-to-string (out)
    (if (plusp total)
        (format out "~a GB of ~a GB used (~d%, ~a; warn ~d%, hold ~d%) -- from ~a~%"
                (gb used) (gb total) (round (pct used total)) (level used total)
                (setting :warn-pct) (setting :hold-pct) source)
        (format out "GPU pool size unknown (no nvidia-smi and no gpu_total_mb): Ollama reports ~a GB held by ~d model~:p~%"
                (gb used) (length loaded)))
    (format out "Ollama ~a: ~a; ~d model~:p loaded, ~a GB of that in VRAM~%"
            (setting :ollama-url) (if reachable "answering" "not answering")
            (length loaded)
            (gb (loop for model in loaded sum (getf model :size-vram) into bytes
                       finally (return bytes))))
    (dolist (line (model-lines loaded)) (write-line line out))
    (when (plusp total)
      (format out "Room before the hold line: ~a GB of ~a GB (reserve ~a GB)~%"
              (gb (max 0 (- (cap total) used))) (gb total) (reserve-line)))))

(defun models-text (loaded reachable)
  "The loaded models alone, for `vram models'."
  (with-output-to-string (out)
    (unless reachable (format out "Ollama is not answering.~%"))
    (if (null loaded)
        (format out "No models loaded.~%")
        (dolist (line (model-lines loaded)) (write-line line out)))))

(defun settings-text ()
  "The section as it stands, a line a key."
  (with-output-to-string (out)
    (loop for (key label) in *setting-rows*
          for value = (setting key)
          do (format out "~a: ~a~%" label
                     (cond ((null value) "(none)")
                           ((listp value) (if value (format nil "~{~a~^, ~}" value) "(none)"))
                           (t value))))))

(defun status (&key full)
  "The GPU, what Ollama holds and the room left => the lines the model reads."
  (assure-running)
  (multiple-value-bind (loaded reachable) (probe)
    (multiple-value-bind (total used source) (memory loaded)
      (let ((text (status-text loaded total used source reachable)))
        (if full
            (format nil "~a~%Settings~%~a" (string-right-trim '(#\Newline) text)
                    (settings-text))
            text)))))

(defun fit (model &key context)
  "Whether MODEL at CONTEXT fits the room the GPU has now => the lines the
model reads: the estimate, the room, and either a go or what to unload."
  (assure-running)
  (let ((asked (nonblank model)))
    (unless asked
      (fail "a fit check needs a model name, e.g. qwen3:8b"))
    (let* ((loaded (loaded-models))
           (resident (matching asked loaded))
           (name (or (and resident (getf resident :name)) asked))
           (ctx (effective-context context resident))
           (est (estimate name ctx resident))
           (mem (multiple-value-list (memory loaded)))
           (total (first mem))
           (used (second mem))
           (source (third mem))
           (free (max 0 (- (cap total) used)))
           (need (if resident 0 (getf est :total))))
      (with-output-to-string (out)
        (format out "~a at context ~d: about ~a GB (~a GB weights, ~a GB KV cache, ~a GB margin; ~a)~%"
                name ctx (gb (getf est :total)) (gb (getf est :weights)) (gb (getf est :kv))
                (gb (getf est :overhead))
                (cond ((string= (getf est :source) "observed") "its measured size")
                      ((string= (getf est :source) "formula") "weights plus the cache its shape implies")
                      (t "weights only: Ollama reported no shape for it")))
        (if (plusp total)
            (format out "GPU now: ~a of ~a GB used (~d%, ~a; from ~a); room before the hold line ~a GB (reserve ~a GB, hold ~d%)~%"
                    (gb used) (gb total) (round (pct used total)) (level used total) source
                    (gb free) (reserve-line) (setting :hold-pct))
            (format out "GPU pool size unknown: only Ollama's own figures are known~%"))
        (cond
          (resident
           (format out "Already loaded: no room is needed, only its context matters.~%"))
          ((and (plusp total) (<= need free))
           (format out "Fits: ~a GB needed, ~a GB free; about ~d% would be used afterwards~a.~%"
                   (gb need) (gb free) (round (pct (+ used need) total))
                   (if (>= (round (pct (+ used need) total)) (setting :warn-pct))
                       (format nil ", past the ~d% warn line" (setting :warn-pct))
                       "")))
          (t
           (multiple-value-bind (names freed)
               (if (plusp total) (plan-eviction loaded name need free) (values nil nil))
             (cond
               (names
                (format out "Not yet: ~a GB needed, ~a GB free before the hold line; unloading ~{~a~^, ~} (~a GB) would make room -- vram_unload takes one name at a time.~%"
                        (gb need) (gb free) names (gb freed)))
               (t
                (format out "Not yet: ~a GB needed, ~a GB free before the hold line~@[, and nothing else loaded may be unloaded~]; Ollama puts what does not fit in system RAM and runs it on the CPU.~%"
                        (gb need) (gb free) (and (plusp total) t)))))))))))

(defun unload (model)
  "Unload MODEL from Ollama, as the original's /vram evict did => a line saying
so, or a refusal naming what is loaded."
  (assure-running)
  (let ((asked (nonblank model)))
    (unless asked
      (fail "an unload needs a model name; /vram models lists what is loaded"))
    (let* ((loaded (loaded-models))
           (entry (matching asked loaded))
           (name (and entry (getf entry :name))))
      (unless entry
        (fail "~a is not loaded. Loaded: ~a" asked
              (or (format nil "~{~a~^, ~}" (mapcar (lambda (model) (getf model :name)) loaded))
                  "none")))
      (when (member name (setting :pinned-models) :test #'string-equal)
        (fail "~a is pinned in the vram section: unpin it there before unloading it" name))
      (ask :post "/api/generate" :body (nlk:json-object "model" name "keep_alive" 0)
           :timeout 10 :decode nil)
      (format nil "Unloaded ~a (~a GB was in VRAM); it loads again on its next call."
              name (gb (getf entry :size-vram))))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (vram-error (condition)
       (nle:failure (format nil "ERROR: ~a" (vram-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Ollama: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun run-command (args session-id)
  "/vram [status|models|settings|fit <model> [context]|evict <model>]: the
operator's own way to the same answers."
  (declare (ignore session-id))
  (assure-running)
  (let* ((raw (nlk:trimmed (or args "")))
         (parts (uiop:split-string raw :separator '(#\Space)))
         (word (string-downcase (or (first parts) "")))
         (rest (nlk:trimmed (format nil "~{~a~^ ~}" (rest parts)))))
    (cond
      ((member word '("" "status") :test #'string=) (answering (status)))
      ((string= word "models") (answering (multiple-value-call #'models-text (probe))))
      ((string= word "settings") (answering (settings-text)))
      ((string= word "fit")
       (if (plusp (length rest))
           (let* ((words (uiop:split-string rest :separator '(#\Space)))
                  (context (ignore-errors (parse-integer (or (second words) "") :junk-allowed t))))
             (answering (fit (first words) :context context)))
           +usage+))
      ((member word '("evict" "unload") :test #'string=)
       (if (plusp (length rest)) (answering (unload rest)) +usage+))
      (t +usage+))))

;;; --- the three boundaries ----------------------------------------------------------

(nlk:define-contract vram-status-call
  (:doc "The GPU memory on this machine and the local Ollama models loaded in it, with the room left before the hold line. Read it before switching to a different local model: loading one that does not fit unloads another, or runs partly on the CPU. Read-only.")
  ("full" :boolean :default nil
          :doc "also list this cell's own settings"))

(nlk:define-contract vram-fit-call
  (:doc "Whether a local Ollama model fits the GPU room left right now, and what to unload if it does not. The estimate is the model's weights plus the KV cache its shape implies, plus a margin; a model already loaded needs no room. A model larger than the pool is not an error: Ollama puts the rest in system RAM.")
  ("model" :text :required t
           :doc "the model name as Ollama lists it, e.g. qwen3:8b")
  ("context" :integer
             :doc "the context it would load at; omit for the one a loaded copy uses, else 4096"))

(nlk:define-contract vram-unload-call
  (:doc "Unload one idle model from Ollama so its GPU room frees (POST /api/generate with keep_alive 0). It loads again on its next call; a model pinned in the vram section is refused, and so is one Ollama does not list as loaded.")
  ("model" :text :required t
           :doc "the model to unload, as Ollama lists it"))

(defun status-tool (call)
  (answering (status :full (vram-status-call-full call))))

(defun fit-tool (call)
  (answering (fit (vram-fit-call-model call) :context (vram-fit-call-context call))))

(defun unload-tool (call)
  (answering (unload (vram-unload-call-model call))))

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell vram
  (:section ("vram")
    (:guide "nothing here is required: the cell reads your Ollama on http://127.0.0.1:11434 and nvidia-smi when it is installed; ollama_url moves it to another host, gpu_total_mb sets the pool by hand on a driver that misreports it; next step: ask whether a local model fits")
    ("ollama_url" :string :default "http://127.0.0.1:11434"
     :doc "the Ollama this cell reads and unloads on")
    ("smi_path" :string :default "nvidia-smi"
     :doc "the program the GPU numbers are read with; Ollama's own figures are used when it is missing")
    ("gpu_total_mb" :integer :default 0 :min 0
     :doc "the GPU pool in MB, for a driver that misreports it; 0 detects it")
    ("reserve_mb" :integer :default 2048 :min 0
     :doc "memory the fit check leaves for the desktop, in MB")
    ("warn_pct" :integer :default 85 :min 1
     :doc "the usage share above which the status says warn")
    ("hold_pct" :integer :default 95 :min 1
     :doc "the usage share the fit check budgets up to")
    ("kv_cache_type" :choice :options '("f16" "q8_0" "q4_0") :default "f16"
     :doc "match OLLAMA_KV_CACHE_TYPE so the KV cache estimate is the one loaded")
    ("estimate_overhead_pct" :integer :default 10 :min 0
     :doc "the safety margin an estimate carries, in percent")
    ("pinned_models" :list
     :doc "model names this cell never suggests or performs an unload of"))
  (:settings #'read-settings)
  (:help :vram
         "vram: the GPU room and the local Ollama models loaded in it (vram_status, vram_fit, vram_unload)"
         +primer+)
  (:tool "vram_status" (nlk:contract 'vram-status-call) #'status-tool)
  (:tool "vram_fit" (nlk:contract 'vram-fit-call) #'fit-tool)
  (:tool "vram_unload" (nlk:contract 'vram-unload-call) #'unload-tool)
  (:command "vram" 'run-command
            :description "The GPU room and the local Ollama models loaded in it"
            :argument-hint "[status|models|settings|fit <model>|evict <model>]"))

