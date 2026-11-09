;;;; cell.lisp --- the cell: Fortytwo among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but fortytwo:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Fortytwo's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable and qwen/qwen3.8-27b, so /connect offers
;;;;                          it, /models lists it and a turn resolves its
;;;;                          lane and address the way it does any catalog
;;;;                          provider's
;;;;   :CREDENTIAL            a key from FORTYTWO_API_KEY and no other
;;;;                          variable; a key /connect saved in auth.json
;;;;                          answers before this point does
;;;;   REQUEST-BODY           a round asks for one of Fortytwo's four efforts:
;;;;                          the narrower rungs go as they are, the wider ones
;;;;                          round down, and an unset effort is left out
;;;;   WALK-PROVIDER-STREAM   the request carries the two attribution headers
;;;;
;;;; Config, a sibling top-level key:
;;;;   "fortytwo": {"base_url": "https://api.fortytwo.network/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and Fortytwo is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-fortytwo)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a fortytwo round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Fortytwo's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Fortytwo's row, made once per
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

(defun credential (op next)
  "The :CREDENTIAL answer for fortytwo: the key FORTYTWO_API_KEY holds, else
none."
  ;; Never NEXT for fortytwo: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to
  ;; Fortytwo.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a fortytwo round asks for one of Fortytwo's four
efforts, and for none when the selection names none."
  (when (and (ours-p config) (hash-table-p body))
    (alexandria:if-let (effort (wire-effort
                                 (nle::effective-provider-config-reasoning-effort config)))
      (setf (gethash "reasoning_effort" body) effort)
      (remhash "reasoning_effort" body)))
  body)

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a fortytwo round carries the attribution
headers, and no stale copy of the two this cell owns."
  (if (ours-p config)
      (apply next fold
             :headers (append (remove-if (lambda (header)
                                           (member (car header) +owned-headers+
                                                   :test #'string-equal))
                                         headers)
                              (attribution-headers))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell fortytwo
  (:section ("fortytwo")
    (:guide "make a key at https://platform.fortytwo.network; save it with /connect or set FORTYTWO_API_KEY; base_url is where Fortytwo is served")
    ("base_url" :string :default +base+
     :doc "the Fortytwo API base the chat lane appends /chat/completions to"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk))
