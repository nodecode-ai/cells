;;;; provider.lisp --- what Fortytwo is: its address, its key, its model, its
;;;; headers and its reasoning efforts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from Fortytwo-Network's Hermes plugin (see NOTICE):
;;;; plugin.yaml and __init__.py, which registered one provider profile -
;;;; base https://api.fortytwo.network/v1, the chat-completions wire, one
;;;; model (qwen/qwen3.8-27b, text and image in, streaming, tools, reasoning,
;;;; 262,144 tokens of context), two attribution headers, and Hermes's wider
;;;; effort ladder rounded down onto Fortytwo's own four rungs.
;;;;
;;;; Deliberately as narrow as the original. One model id goes on the wire as
;;;; it is; the headers ride only a round whose provider is `fortytwo'; no key
;;;; is read from anywhere but FORTYTWO_API_KEY (or the key /connect saved);
;;;; nothing is fetched, spawned or rewritten.

(in-package #:nodecode-fortytwo)

(defparameter +base+ "https://api.fortytwo.network/v1"
  "Where Fortytwo is served: the base the chat lane appends /chat/completions to.")

(defparameter +model+ "qwen/qwen3.8-27b"
  "The one model Fortytwo offers here, named on the wire as the original named it.")

(defparameter +model-name+ "Qwen3.8 27B"
  "What the picker calls +MODEL+.")

(defparameter +context-tokens+ 262144
  "The context window the original advertised for +MODEL+, in tokens.")

(defparameter +env+ '("FORTYTWO_API_KEY")
  "The environment variable a Fortytwo key is read from. A key /connect saved
in auth.json answers before this one does.")

(defparameter +referer+ "https://github.com/Fortytwo-Network/fortytwo-hermes"
  "The HTTP-Referer the original sent: the integration's own page.")

(defparameter +title+ "Nodecode"
  "The X-Title this client sends. The original named Hermes there; this cell
names the application it runs in, never another product.")

(defparameter +owned-headers+ '("HTTP-Referer" "X-Title")
  "The headers this cell owns: a stale copy of one is dropped before it is set.")

(defparameter +wire-efforts+ '("low" "medium" "xhigh")
  "The effort rungs Fortytwo takes under their own names. Its weakest rung,
`none', is the disabled one, which Nodecode spells `off'.")

(defparameter +effort-rounding+ '(("minimal" . "none") ("high" . "medium")
                                  ("max" . "xhigh") ("ultra" . "xhigh"))
  "Hermes's wider rungs, rounded down onto the nearest rung Fortytwo takes:
the original's own table.")

(defparameter +effort-levels+ '("off" "low" "medium" "xhigh")
  "The ladder the picker offers for +MODEL+: Fortytwo's four rungs, weakest
first, `off' for its `none'.")

(defun attribution-headers ()
  "The attribution a Fortytwo round carries: the integration's page, and the
application this round runs in."
  (list (cons "HTTP-Referer" +referer+)
        (cons "X-Title" +title+)))

(defun wire-effort (effort)
  "EFFORT, a rung of Nodecode's ladder, as the value the Fortytwo API takes
for `reasoning_effort'; NIL when the field is left off."
  ;; The same rule the original applied: a rung Fortytwo takes goes as it is,
  ;; a wider one rounds down to the nearest it takes, a disabled one is said
  ;; as `none', and an unset one leaves the API default in charge.
  (when (and (stringp effort) (plusp (length effort)))
    (cond ((string-equal effort "off") "none")
          ((string-equal effort "none") "none")
          ((member effort +wire-efforts+ :test #'string-equal) (string-downcase effort))
          (t (or (cdr (assoc effort +effort-rounding+ :test #'string-equal))
                 (string-downcase effort))))))

(defun model-row ()
  "The one catalog entry this provider offers: +MODEL+ with the window, the
modalities, the tool calls and the effort ladder the original advertised."
  (nle::make-catalog-model +model-name+ +context-tokens+ nil
                            #("text" "image") #("text") +effort-levels+
                            t nil t nil))

(defun catalog-row (&optional prior)
  "Fortytwo as a models.dev provider: the chat lane's package, this section's
base, the key variable, and the one model over PRIOR's (the row models.dev
itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (setf (gethash +model+ models) (model-row))
    (nlk:json-object "name" "Fortytwo"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The key FORTYTWO_API_KEY holds, or NIL."
  (some #'nle::credential-env +env+))
