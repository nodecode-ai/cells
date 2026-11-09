;;;; provider.lisp --- what Hopper is: the list it serves, its catalog row, its key.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of mykeura's Hopper (see NOTICE): the plugin's one idea is
;;;; that the model picker shows the OpenRouter ids a person wrote down, and
;;;; nothing else. Hermes carries that as a provider whose fetch-models
;;;; answers a text file; here the same list is the provider's catalog row
;;;; (so /models, /connect and a turn all see it) and its listing (so the
;;;; picker never falls back to OpenRouter's whole roster):
;;;;
;;;;   ~/.nodecode/hopper/models.txt     one OpenRouter model id a line,
;;;;                                     # a comment, blank lines skipped
;;;;
;;;; OpenRouter speaks the OpenAI chat wire at https://openrouter.ai/api/v1,
;;;; which is the chat lane; the model id rides the wire exactly as the line
;;;; wrote it, `:free' and routing suffixes included.

(in-package #:nodecode-hopper)

(defparameter +base+ "https://openrouter.ai/api/v1"
  "Where OpenRouter is served: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("OPENROUTER_API_KEY")
  "The environment variables a key is read from: OpenRouter's own, so one
key serves Hopper and OpenRouter alike.")

(defparameter +key-page+ "https://openrouter.ai/settings/keys"
  "Where a key is made by hand.")

(defparameter +default-models+ '("inclusionai/ling-3.0-flash-vl:free")
  "What a fresh list starts with: the original plugin's DEFAULT_MODELS.")

(defparameter +file-header+
  "# Hopper's OpenRouter models: one model id a line.
# A line beginning with # is a comment, and a blank line is skipped.
"
  "The comment block every list Hopper writes opens with.")

;;; --- the list file ----------------------------------------------------------------

(defun models-file (&optional (configured (setting :models-file)))
  "The file the list lives in: the section's models_file when it names one,
else models.txt in the organism's home."
  (let ((configured (and (stringp configured) (nlk:trimmed configured))))
    (if (and configured (plusp (length configured)))
        (pathname (nlk:expand-home configured))
        (nlk:home "hopper/models.txt"))))

(defun model-id-p (id)
  "Whether ID is an OpenRouter model id Hopper can serve: not blank, no
whitespace in it, and a provider slash."
  (and (stringp id)
       (plusp (length id))
       (notany (lambda (character)
                 (member character '(#\Space #\Tab #\Return #\Newline) :test #'char=))
               id)
       (find #\/ id)))

(defun read-models (&optional (path (models-file)))
  "The ids PATH lists, in file order and deduplicated; comments, blanks and
lines that are not model ids are skipped, the way the original's CLI read
them."
  (let ((ids '()) (seen (make-hash-table :test 'equal)))
    (when (probe-file path)
      (dolist (raw (uiop:split-string (uiop:read-file-string path) :separator '(#\Newline)))
        (let ((id (string-trim '(#\Space #\Tab #\Return) raw)))
          (when (and (plusp (length id))
                     (char/= #\# (char id 0))
                     (model-id-p id)
                     (not (gethash id seen)))
            (setf (gethash id seen) t)
            (push id ids)))))
    (nreverse ids)))

(defun write-models (ids &optional (path (models-file)))
  "Write IDS to PATH, one a line, atomically: the next read sees the whole
new list or the whole old one."
  (let ((text (with-output-to-string (out)
                (write-string +file-header+ out)
                (dolist (id ids) (write-line id out)))))
    (ensure-directories-exist path)
    (nlk:write-file-atomically path text :mode #o600)
    path))

(defun ensure-models-file (&optional (path (models-file)))
  "PATH, written with the default list when nothing is there yet."
  (unless (probe-file path)
    (write-models +default-models+ path))
  path)

(defun curated-models ()
  "The ids the picker is to offer: the file's, never a guess."
  (read-models (ensure-models-file)))

;;; --- the catalog -----------------------------------------------------------------

(defun bare-name (id)
  "ID without its provider namespace: how a row no source names is labelled."
  (let ((slash (position #\/ id :from-end t)))
    (if slash (subseq id (1+ slash)) id)))

(defun published-model (catalog id)
  "What CATALOG publishes for OpenRouter's own copy of ID, or NIL: a model
models.dev knows keeps its window, ladder and price instead of losing them
to the list's bare id."
  ;; The row's models are CATALOG-MODELs, not objects, so the last step is
  ;; GETHASH's own: JSON-VALUE's :OBJECT would read a struct as absent.
  (let ((models (nlk:json-value catalog :object "openrouter" "models")))
    (and models (gethash id models))))

(defun fallback-model (id)
  "A catalog model for ID claiming only what the id says: it takes a tool
call and answers text, and its window is unknown rather than a guess."
  (nle::make-catalog-model (bare-name id) nil nil #("text") #("text")
                            nil nil nil t nil))

(defun catalog-model (id catalog)
  "ID as the catalog keeps a model: what CATALOG publishes for it, else
FALLBACK-MODEL's row."
  (or (published-model catalog id) (fallback-model id)))

(defun catalog-row (catalog)
  "Hopper as a models.dev provider: the chat lane's package, this section's
base, and exactly the ids the list names."
  (let ((models (make-hash-table :test 'equal)))
    (dolist (id (curated-models))
      (setf (gethash id models) (catalog-model id catalog)))
    (nlk:json-object "name" "Hopper"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

;;; --- the key ---------------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for hopper: OPENROUTER_API_KEY, else none. A key
/connect saved in auth.json answers before this point."
  ;; Never NEXT for hopper: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to
  ;; OpenRouter. The key never expires, so a probe costs nothing.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the listing -----------------------------------------------------------------

(defun listing-rows (&optional (catalog (nle::models-catalog-table)))
  "The curated ids as LIST-PROVIDER-MODELS rows, each sized and named by
what the catalog publishes for it; an unknown id rides unsized."
  (loop for id in (curated-models)
        for model = (published-model catalog id)
        for window = (and model (nle::catalog-model-context model))
        collect (list :id id
                      :display (and model (nle::catalog-model-name model))
                      :context-window (and window (plusp window) window))))

(defun listing (next provider &rest args)
  "LIST-PROVIDER-MODELS advice: hopper lists the ids its file names, so the
picker never falls back to OpenRouter's whole roster. Asked with :KEY --
/connect's key check -- it asks OpenRouter itself and keeps none of the
rows, so a key is still checked where it is spent."
  (if (equal provider +provider+)
      (if (getf args :key)
          (multiple-value-bind (rows reason) (apply next provider args)
            (declare (ignore rows))
            (values nil reason))
          (values (listing-rows) nil))
      (apply next provider args)))
