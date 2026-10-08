;;;; cell.lisp --- the cell: Muse Code among the organism's providers, and its sign-in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Five hooks, each declining for every provider but muse-code, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE    the catalog carries Muse Code's row: the
;;;;                           Responses lane's package, this section's base,
;;;;                           and omp's bundled Muse Spark models over
;;;;                           whatever models.dev published, so /models lists
;;;;                           them and a turn resolves the openai-responses
;;;;                           lane and https://api.meta.ai/v1/responses
;;;;   :CREDENTIAL             the Model API key the sign-in minted, from
;;;;                           auth.json; a key /connect saved answers before
;;;;                           this point does. It never falls through to the
;;;;                           lane's family default, which would send
;;;;                           OPENAI_API_KEY to Meta
;;;;   LIST-PROVIDER-MODELS    the live listing asked as omp's discovery asks
;;;;                           it: the minted key and x-api-version
;;;;   RESPONSES-REQUEST-BODY  no tool_choice, which Meta refuses
;;;;   WALK-PROVIDER-STREAM    x-api-version 1.0.0
;;;;   /muse-code              login (a device code), logout, status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "muse-code": {"base_url": "https://api.meta.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and Muse Code is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-muse-code)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a muse-code round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Muse Code's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Muse Code's row, made once
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
  "The :CREDENTIAL answer for muse-code: the minted key, else the keyless
placeholder."
  (if (equal (getf op :provider) +provider+)
      (or (token-credential op) (nle:make-credential "public" :public))
      (funcall next op)))

(defun listing (next provider &rest keys &key key base &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Meta's listing, asked as omp asks it."
  (if (equal provider +provider+)
      (list-models (or key (ignore-errors (nle:credential-key (nle::resolve-provider-credential provider))))
                   (or base (setting :base-url)))
      (apply next provider keys)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "RESPONSES-REQUEST-BODY advice: a muse-code round sends no tool_choice."
  (if (and (ours-p config) (hash-table-p body))
      (muse-body body)
      body))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a muse-code round names the API version."
  (if (ours-p config)
      (apply next fold :headers (append headers `(("x-api-version" . ,+api-version+)))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(defun start ()
  "Begin from a fresh catalog, and stop a sign-in in progress with the cell."
  (forget-catalog)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop #'cancel-flow))

(nle:define-cell muse-code
  (:section ("muse-code")
    (:guide "sign in with /muse-code login (a Muse subscription's Meta account; open the address, enter the code); base_url is where Meta's Model API is served")
    ("base_url" :string :default +base+
     :doc "the Meta Model API base the Responses lane appends /responses to"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::list-provider-models #'listing)
  (:hook 'nle::responses-request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "muse-code" 'run-command
            :description "Sign in to Muse Code with a device code"
            :argument-hint "login | logout | status"
            :session nil
            :complete 'complete-command))
