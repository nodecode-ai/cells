;;;; provider.lisp --- what Command Code is: its address, its key, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from NanamiMio/hermes-commandcode-oauth (see NOTICE):
;;;; endpoints.py (the origin guard), auth.py (the CLI credential file),
;;;; model_metadata.py (the model snapshot) and __init__.py (the catalog row).
;;;;
;;;; Command Code (commandcode.ai) serves two wires. The documented one is an
;;;; OpenAI-compatible Provider API under /provider/v1; the one the Command Code
;;;; CLI itself speaks -- and the one a CLI account's key is good for -- is
;;;; POST /alpha/generate, an NDJSON event stream. This cell rides the CLI's
;;;; wire (wire.lisp) with the key the CLI already stored, which is the whole
;;;; point of the original: an account signed in through the CLI, used from
;;;; here.

(in-package #:nodecode-commandcode)

(defparameter +base+ "https://api.commandcode.ai"
  "Where Command Code is served: the base the lane appends /alpha/generate to.")

(defparameter +env+ '("COMMANDCODE_CLI_TOKEN")
  "The environment variables a Command Code key is read from, in order.")

(defparameter +default-cli-auth-file+ "~/.commandcode/auth.json"
  "Where the Command Code CLI keeps the key its sign-in stored.")

(defparameter +key-page+ "https://commandcode.ai"
  "Where an account is made and a key can be seen.")

(defun normalized-base (base)
  "BASE as the endpoint guard leaves it (endpoints.py API-ORIGIN): trimmed, no
query, its scheme and host -- a configured base_url may be the Provider API
form (…/provider/v1) or a bare origin, and either reduces to the origin."
  (let ((value (nlk:trimmed (or base ""))))
    (if (zerop (length value))
        +base+
        (let ((trimmed (string-right-trim "/" value)))
          (loop for marker in (list "/provider/v1" "/alpha")
                when (and (>= (length trimmed) (length marker))
                          (string= marker trimmed :start2 (- (length trimmed) (length marker))))
                  do (setf trimmed (subseq trimmed 0 (- (length trimmed) (length marker)))))
          (let ((cut (or (position #\? trimmed) (length trimmed))))
            (string-right-trim "/" (subseq trimmed 0 cut)))))))

(defun canonical-model-id (model)
  "MODEL without the vendor prefix a picker may hand us: `commandcode/<id>'
and its spellings go back to the id the endpoint lists."
  (let ((clean (nlk:trimmed (or model ""))))
    (loop for prefix in '("commandcode/" "command-code/" "commandcode-oauth/")
          when (and (> (length clean) (length prefix)) (uiop:string-prefix-p prefix clean))
            do (return (subseq clean (length prefix)))
          finally (return clean))))

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-commandcode" "models.json")))
  "The CLI's model snapshot, read when this file loads: a vector of objects.
The live account catalog (LIST-MODELS) is asked first; these rows fill what it
does not answer with.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun model-images-p (model-id)
  "Whether MODEL-ID takes images: its bundled row's input lists them; a model
the rows do not carry is taken to, the way the CLI's registry falls back."
  (let ((row (model-row model-id)))
    (if row (find "image" (nlk:json-array row "input") :test #'equal) t)))

(defun catalog-model (row)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (flet ((value (type key) (nlk:json-value row type key)))
    (let ((cost (value :object "cost")))
      (nle::make-catalog-model
       (value :string "name")
       (value :integer "context")
       (value :integer "output")
       (or (value :array "input") #("text"))
       #("text")
       (sort (remove-if-not #'nle::effort-rank (coerce (or (value :array "efforts") #()) 'list))
             #'< :key #'nle::effort-rank)
       (value :boolean "reasoning")
       nil
       t
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "Command Code as a models.dev provider: this cell's lane package, this
section's base, the key variable, and the bundled models over PRIOR's (the
row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Command Code"
                     "npm" +npm+
                     "api" (normalized-base (setting :base-url))
                     "env" (coerce +env+ 'vector)
                     "models" models)))

;;; --- the credential ----------------------------------------------------------------

(defun cli-auth-path ()
  "The CLI credential file this section names, with a leading ~ expanded."
  (let ((text (nlk:trimmed (format nil "~a" (setting :cli-auth-file)))))
    (cond ((zerop (length text)) (uiop:subpathname (user-homedir-pathname) ".commandcode/auth.json"))
          ((and (> (length text) 2) (string= "~/" text :end2 2))
           (uiop:subpathname (user-homedir-pathname) (subseq text 2)))
          ((and (plusp (length text)) (char= #\~ (char text 0)))
           (uiop:subpathname (user-homedir-pathname) (subseq text 1)))
          (t (pathname text)))))

(defun cli-key ()
  "The key the Command Code CLI stored, or NIL: auth.py READ-CLI-TOKEN reads
`apiKey' out of ~/.commandcode/auth.json and never writes it back."
  (let ((path (ignore-errors (cli-auth-path))))
    (when (and path (probe-file path))
      (let ((data (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))
        (when (hash-table-p data)
          (or (nlk:json-value data :text "apiKey")
              (nlk:json-value data :text "api_key")
              (nlk:json-value data :text "accessToken")
              (nlk:json-value data :text "access_token")))))))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun configured-key ()
  "The key this cell resolves without asking /connect: the environment's, else
the CLI's own store, else NIL."
  (or (env-key) (and (setting :read-cli-login) (cli-key))))
