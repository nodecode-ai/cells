;;;; provider.lisp --- what Liquid Inference is: its address, its key, the cap
;;;; it puts on a request, and the one model it names.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of architect-xyz's hermes-liquid-inference (see
;;;; NOTICE): its provider registration (__init__.py in the original, lines
;;;; 20-33), which is the whole of what that plugin is.
;;;;
;;;; The exchange speaks the OpenAI chat wire at
;;;; https://router.inference.ai.exchange/v1, and models.dev already publishes
;;;; it under the id `liquid' with the model `liquid.auto'. What the original
;;;; adds over a plain OpenAI-compatible endpoint, and this cell keeps:
;;;;
;;;;   - every request carries X-Title, naming the client it comes from
;;;;   - every request carries x-liquid-cap-usd, the most that request may be
;;;;     charged: 1 USD unless the section says otherwise, so a request the
;;;;     auction routes to an expensive offer cannot run up an uncapped bill
;;;;
;;;; A top-level reasoning_effort rides only when the operator picks a level,
;;;; which is the chat lane's own rule: the original's
;;;; build_api_kwargs_extras said nothing more than that.

(in-package #:nodecode-liquid)

(defparameter +base+ "https://router.inference.ai.exchange/v1"
  "Where the exchange is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("LIQUID_API_KEY")
  "The environment variables a buyer key is read from, in order.")

(defparameter +key-page+ "https://inference.ai.exchange"
  "Where a buyer key with the spend scope is made.")

(defparameter +title+ "Nodecode"
  "The client every request names in X-Title: the original named its own harness there.")

(defparameter +cap+ "1.00"
  "The most one request may be charged, in USD, when the section names none.")

(defparameter +model+ "liquid.auto"
  "The exchange's own model id: it auctions each request to the eligible offer
with the lowest estimated charge.")

;;; --- the catalog row ------------------------------------------------------------

(defun bundled-model ()
  "The one row the exchange names, for the catalog when models.dev publishes
no row of its own: the exchange answers in text and takes tools, and says
nothing about a context window."
  (nle::make-catalog-model "Liquid Auto" nil nil #("text") #("text") nil t nil t nil))

(defun catalog-row (&optional prior)
  "Liquid Inference as a catalog provider: models.dev's own row (its name, its
lane, its model rows) with this section's base over its address and the
exchange's key variable, and with the exchange's one model present even when
models.dev is silent about it."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (unless (gethash +model+ models)
      (setf (gethash +model+ models) (bundled-model)))
    (nlk:json-object "name" (or (nlk:json-value prior :text "name") "Liquid Inference")
                     "npm" (or (nlk:json-value prior :text "npm") "@ai-sdk/openai-compatible")
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

;;; --- the cap ---------------------------------------------------------------------

(defun cap-valid-p (text)
  "Whether TEXT is a charge the exchange reads as money: digits with at most one
dot, and not zero. A cap of zero leaves no offer eligible, and a malformed one
is the uncapped bill the header exists to prevent."
  (and (stringp text)
       (plusp (length text))
       (let ((dots 0) (digits 0) (zero t) (valid t))
         (loop for character across text
               do (cond ((digit-char-p character)
                         (incf digits)
                         (unless (char= character #\0) (setf zero nil)))
                        ((char= character #\.) (incf dots))
                        (t (setf valid nil))))
         (and valid (plusp digits) (<= dots 1) (not zero)))))

(defun cap ()
  "The charge one request is capped at, as the section writes it."
  (setting :cap-usd))

;;; --- the key ---------------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for liquid: the key LIQUID_API_KEY holds, else none.
A key /connect saved in auth.json answers before this point does."
  ;; Never NEXT for liquid: the ladder behind this point falls back to the chat
  ;; family's default variable, and would send OPENAI_API_KEY to the exchange.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the request -----------------------------------------------------------------

(defun request-headers ()
  "The headers every request to the exchange carries: the client it comes from,
and the most it may be charged."
  `(("X-Title" . ,+title+)
    ("x-liquid-cap-usd" . ,(cap))))
