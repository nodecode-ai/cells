;;;; cell.lisp --- the cell: AI Stupid Level among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but aistupidlevel:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries AI Stupid Level's row: the
;;;;                          chat lane's package, this section's base, the
;;;;                          router key variable, and the routing modes, so
;;;;                          /connect offers it, /models lists it and a turn
;;;;                          resolves its lane and address the way it does any
;;;;                          catalog provider's; the core's own listing then
;;;;                          reads GET /v1/models with the key and offers the
;;;;                          live catalog, pinned models included
;;;;   :CREDENTIAL            a key from ASL_ROUTER_KEY, and no other
;;;;                          variable; a key /connect saved answers before
;;;;                          this point does
;;;;
;;;; Config, a sibling top-level key:
;;;;   "aistupidlevel": {"base_url": "https://aistupidlevel.info/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-aistupidlevel)

(defvar *catalog* nil
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
AI Stupid Level's row in it; NIL before the first read.")

(defun catalog (next &aux (base (funcall next)))
  "MODELS-CATALOG-TABLE advice: the catalog with AI Stupid Level's row, made
once per catalog the core reads."
  (if (and *catalog* (eq (car *catalog*) base))
      (cdr *catalog*)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* nil))

(defun credential (op next)
  "The :CREDENTIAL answer for aistupidlevel: the key ASL_ROUTER_KEY holds,
else none. A key /connect saved in auth.json answers before this point."
  ;; Never NEXT for aistupidlevel: the ladder behind this point falls back to
  ;; the chat family's default variable, and would send OPENAI_API_KEY to the
  ;; router.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun start ()
  "Start clean, and leave no merged catalog behind a stop."
  (forget-catalog)
  (nle:on-stop #'forget-catalog))

(nle:define-cell aistupidlevel
  (:section ("aistupidlevel")
    (:guide "make a router key at https://aistupidlevel.info/router/keys and save your provider keys in the router settings on the same site; save the key with /connect or as ASL_ROUTER_KEY; base_url is where the router is served")
    ("base_url" :string :default +base+
     :doc "the AI Stupid Level API base the chat lane appends /chat/completions to"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential))
