;;;; cell.lisp --- the cell: Liquid Inference among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but liquid, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog's liquid row keeps models.dev's name,
;;;;                          lane and models, and takes this section's base, so
;;;;                          /connect offers the exchange, /models lists it and
;;;;                          a turn resolves its address the way it does any
;;;;                          catalog provider's
;;;;   :CREDENTIAL            LIQUID_API_KEY, and no other variable, else
;;;;                          nothing: a key /connect saved answers before this
;;;;                          point does
;;;;   WALK-PROVIDER-STREAM   every request carries X-Title and
;;;;                          x-liquid-cap-usd, the most it may be charged
;;;;
;;;;   /liquid                                                 the exchange as it stands
;;;;
;;;; Config, a sibling top-level key:
;;;;   "liquid": {"base_url": "https://router.inference.ai.exchange/v1",
;;;;              "cap_usd": "1.00"}
;;;; A vetoed section ("enabled": false) installs nothing, and Liquid Inference
;;;; is then whatever models.dev alone makes of it: a provider a turn can reach
;;;; with a key, but with no cap and no client name on the request.

(in-package #:nodecode-liquid)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a liquid round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Liquid Inference's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with this section's base on the
exchange's row, made once per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a liquid round names its client and its cap."
  (if (ours-p config)
      (apply next fold :headers (append headers (request-headers))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(defun start ()
  "Refuse a cap the exchange would not read as money, then start clean."
  (unless (cap-valid-p (cap))
    (error 'nlk:config-refusal
           :detail (format nil "liquid.cap_usd must be a positive decimal in USD, got ~s"
                           (cap))))
  (forget-catalog)
  (nle:on-stop #'forget-catalog))

(defun run-command (args session-id)
  "The /liquid command: the exchange as this cell stands."
  (declare (ignore args session-id))
  (values (format nil "Liquid Inference: ~a (model ~a), each request capped at ~a USD. ~a"
                  (setting :base-url) +model+ (cap)
                  (if (env-key)
                      "The key comes from LIQUID_API_KEY."
                      "No key yet: /connect liquid saves one, or set LIQUID_API_KEY."))
          nil))

(defparameter +manual+
  "liquid: Liquid Inference is an auction exchange for LLM inference - the exchange sends each request to the eligible offer with the lowest estimated charge. Set the provider to liquid and the model to liquid.auto, then give it a buyer key with the spend scope: /connect liquid saves one, or set LIQUID_API_KEY. Every request is capped at liquid.cap_usd, 1.00 USD unless the section says otherwise, and carries X-Title, so a request the auction routes to an expensive offer cannot run up an uncapped bill. A reasoning level you pick rides as a top-level reasoning_effort, and one you leave off sends none. liquid.base_url points the cell at a relay; /liquid answers how it stands."
  "What (help :liquid) answers: the manual topic, not the request's one line.")

(nle:define-cell liquid
  (:section ("liquid")
    (:guide "put a buyer key with the spend scope in LIQUID_API_KEY, or save it with /connect liquid; cap_usd is the most one request may be charged, in USD; base_url is where the exchange, or a relay of it, answers")
    ("base_url" :string :default +base+
     :doc "the exchange API base the chat lane appends /chat/completions to")
    ("cap_usd" :string :default +cap+
     :doc "the most one request may be charged, in USD, sent as x-liquid-cap-usd"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "liquid" #'run-command
            :description "Liquid Inference: how the exchange stands")
  (:help :liquid "liquid: the auction exchange, its cap, its key" +manual+))
