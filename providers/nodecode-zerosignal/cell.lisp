;;;; cell.lisp --- the cell: ZeroSignal among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but zerosignal:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries ZeroSignal's row: the chat
;;;;                          lane's package, this section's base, and the
;;;;                          models the local proxy last named (the curated
;;;;                          fallback while it has named none), so /connect
;;;;                          offers it, /models lists its models and a turn
;;;;                          resolves its lane and address the way it does
;;;;                          any catalog provider's
;;;;   :CREDENTIAL            ZEROSIGNAL_API_KEY when the environment sets
;;;;                          it, else the placeholder the proxy ignores; a
;;;;                          key /connect saved in auth.json answers first
;;;;   REQUEST-BODY           reasoning_effort clamped down onto the levels
;;;;                          the serving node declares, omitted where it
;;;;                          declares that it does not reason at all, and
;;;;                          passed through untouched for a model the proxy
;;;;                          has not named
;;;;
;;;; Config, a sibling top-level key:
;;;;   "zerosignal": {"base_url": "http://127.0.0.1:9376/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-zerosignal)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a zerosignal round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* nil
  "(CATALOG LIVE-STAMP . MERGED): the last root the core answered, the
catalog read that row was built from, and that catalog with ZeroSignal's row
in it.")

(defun catalog (next)
  "MODELS-CATALOG-TABLE advice: the catalog with ZeroSignal's row, made once
per catalog the core reads and once more when the proxy's own catalog
changes."
  (let ((root (funcall next)) (stamp (car-safe *live*)) (memo *catalog*))
    (if (and memo (eq (first memo) root) (eql (second memo) stamp))
        (third memo)
        (let ((merged (make-hash-table :test 'equal)))
          (when (hash-table-p root)
            (maphash (lambda (id provider) (setf (gethash id merged) provider)) root))
          (setf (gethash +provider+ merged)
                (catalog-row (and (hash-table-p root) (gethash +provider+ root))))
          (setf *catalog* (list root stamp merged))
          merged))))

(defun forget-catalog ()
  "Drop the merged catalog and the proxy's map, so a restart with another
base builds both again."
  (setf *catalog* nil)
  (forget-live))

(defun credential (op next)
  "The :CREDENTIAL answer for zerosignal: ZEROSIGNAL_API_KEY when set, else
the placeholder the proxy ignores."
  ;; Never NEXT for zerosignal: the ladder behind this point falls back to the
  ;; chat family's default variable and would send OPENAI_API_KEY to the proxy.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (env-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential +placeholder-key+ :public))
      (funcall next op)))

(defun clamp-rung (effort levels)
  "EFFORT clamped down onto LEVELS: the strongest level at or below it, or
NIL when every declared level is above it (then the field is omitted and the
node's own default stands). Never climbs: a rung the node does not declare is
never sent in its place."
  (let* ((rank (rung-rank effort))
         (below (and rank (remove-if (lambda (level) (> (rung-rank level) rank)) levels))))
    (when below (first (last (sort (copy-list below) #'< :key #'rung-rank))))))

(defun clamp-reasoning (body config)
  "BODY, a chat request for CONFIG's model, with reasoning_effort clamped
onto the levels the proxy declares for it."
  (let* ((model (nle::effective-provider-config-model config))
         (levels (live-levels model))
         (requested (gethash "reasoning_effort" body)))
    (cond
      ;; a model the proxy has not named: pass through, as the original does
      ((null levels) body)
      ;; it declares that it does not reason: no reasoning parameter at all
      ((eq levels :none) (remhash "reasoning_effort" body) body)
      ;; the operator disabled reasoning (`off' is omitted from the wire), and
      ;; this node spells the disabled tier: say so rather than nothing
      ((and (null requested)
            (string-equal (or (nle::effective-provider-config-reasoning-effort config) "")
                          "off")
            (member "none" levels :test #'string-equal))
       (setf (gethash "reasoning_effort" body) "none")
       body)
      ((null requested) body)
      ((member requested levels :test #'string-equal) body)
      (t (let ((clamped (clamp-rung requested levels)))
           (if clamped
               (setf (gethash "reasoning_effort" body) clamped)
               (remhash "reasoning_effort" body))
           body)))))

(defun body (next context &aux (request (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a zerosignal round asks for a rung its node declares."
  (when (and (ours-p config) (hash-table-p request))
    (clamp-reasoning request config))
  request)

(nle:define-cell zerosignal
  (:section ("zerosignal")
    (:guide "start and fund the proxy first (zs-proxy proxy start; zs-proxy fund); base_url is where it listens; ZEROSIGNAL_API_KEY may be any non-empty value, since the wallet pays and the proxy ignores it")
    ("base_url" :string :default +base+
     :doc "the proxy's OpenAI base; the chat lane appends /chat/completions"))
  (:start (lambda ()
            (forget-catalog)
            ;; one bounded read at start, so the first picker already knows the
            ;; rungs; a down proxy answers nothing and start goes on
            (refresh-live)
            (nle:on-stop #'forget-catalog)
            (let ((worker (nlk:worker-start "zerosignal-catalog" #'refresh-live
                                            :wake +refresh-seconds+)))
              (nle:on-stop (lambda () (nlk:worker-stop worker))))))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body))
