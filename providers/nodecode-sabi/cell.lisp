;;;; cell.lisp --- the cell: Sabi among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but sabi:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Sabi's row - the chat lane's
;;;;                          package, this section's base, and the one alias -
;;;;                          so /models lists it and a turn resolves its lane
;;;;                          and address the way any catalog provider's does
;;;;   :CREDENTIAL            no key: a loopback proxy holds the upstream
;;;;                          credential itself, and another family's variable
;;;;                          (OPENAI_API_KEY) is never sent to it
;;;;   WALK-PROVIDER-STREAM   the request carries X-Sabi-Client and, when the
;;;;                          session id is opaque, X-Sabi-Session
;;;;
;;;; Config, a sibling top-level key:
;;;;   "sabi": {"base_url": "http://127.0.0.1:8787/v1", "context_tokens": 65536}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-sabi)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a sabi round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Sabi's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Sabi's row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (let ((models (make-hash-table :test 'equal)))
                (alexandria:when-let (published (nlk:json-value (and (hash-table-p base) (gethash +provider+ base))
                                                      :object "models"))
                  (maphash (lambda (id model) (setf (gethash id models) model)) published))
                (setf (gethash +alias+ models) (model-row))
                (nlk:json-object "name" "Sabi"
                                 "npm" "@ai-sdk/openai-compatible"
                                 "api" (setting :base-url)
                                 "env" #()
                                 "models" models)))
        (setf *catalog* (cons base merged))
        merged)))

(defun credential (op next)
  "The :CREDENTIAL answer for sabi: a public one, so no key is ever asked
for and none of another family's is sent."
  ;; Never NEXT for sabi: the ladder behind this point falls back to the chat
  ;; family's own variable, and would send OPENAI_API_KEY to the proxy.
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential "public" :public)
      (funcall next op)))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a sabi round carries the attribution headers,
and no stale copy of the three this cell owns."
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

(nle:define-cell sabi
  (:section ("sabi")
    (:guide "run the Sabi proxy on this machine (its default base is http://127.0.0.1:8787/v1), then pick sabi/sabi-code; there is no key here - the upstream credential belongs to the proxy")
    ("base_url" :string :default +base+
     :doc "the Sabi Chat Completions base the chat lane appends /chat/completions to")
    ("context_tokens" :integer :default 65536 :min 1
     :doc "the context window a turn on the alias is sized against"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::walk-provider-stream #'walk))
