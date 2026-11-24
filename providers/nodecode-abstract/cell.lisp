;;;; cell.lisp --- the cell: Abstract among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but abstract:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Abstract's row: the Responses
;;;;                          lane's package, this section's base, and the
;;;;                          three published models over whatever models.dev
;;;;                          published, so /connect offers it, /models lists
;;;;                          its models and a turn resolves its lane and
;;;;                          address the way it does any catalog provider's
;;;;   :CREDENTIAL            a key from ABSTRACT_API_KEY, and no other
;;;;                          variable; a key /connect saved in auth.json
;;;;                          answers before this point does
;;;;   LIST-PROVIDER-MODELS   the listing is the roster, so the picker never
;;;;                          offers the background lanes Abstract's own
;;;;                          /models also names
;;;;   WALK-PROVIDER-STREAM   a round with no key refuses in words, naming
;;;;                          the variable to set, before it earns a 401
;;;;
;;;; Config, a sibling top-level key:
;;;;   "abstract": {"base_url": "https://api.abstract.inc/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and Abstract is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-abstract)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an abstract round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Abstract's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Abstract's row, made once
per catalog the core reads."
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
  "The :CREDENTIAL answer for abstract: the key ABSTRACT_API_KEY holds, else
none."
  ;; Never NEXT for abstract: the ladder behind this point falls back to the
  ;; Responses family's default variable, and would offer OPENAI_API_KEY.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun walk (next fold &rest keys &key config &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: an abstract round with no key refuses in
words, naming the variable, instead of asking the far end to say 401."
  (if (ours-p config)
      (progn
        (require-key (nle::effective-provider-config-api-key config))
        (apply next fold keys))
      (apply next fold keys)))

(defun listing (next provider &rest keys &key key &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Abstract's listing is its roster, answered
without a request, the way the plugin disables live model listing. Asked with
KEY, which only /connect's key check does, it says why the key was not checked."
  (if (equal provider +provider+)
      (values (listing-rows)
              (and key "Abstract is asked nothing before a turn: its models come from the static roster, so the first turn tries the key"))
      (apply next provider keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell abstract
  (:section ("abstract")
    (:guide "make a key at https://platform.abstract.inc/; save it with /connect or set ABSTRACT_API_KEY; base_url is where the Responses API is served")
    ("base_url" :string :default +base+
     :doc "the API base the Responses lane appends /responses to"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::list-provider-models #'listing)
  (:hook :credential #'credential)
  (:hook 'nle::walk-provider-stream #'walk))
