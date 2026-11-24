;;;; cell.lisp --- the cell: Ace Data Cloud among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but acedatacloud:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries the provider's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable and the two verified models, so /connect
;;;;                          offers it, /models lists them and a turn resolves
;;;;                          its lane and address the way it does any catalog
;;;;                          provider's
;;;;   :CREDENTIAL            a key from ACEDATACLOUD_API_KEY and no other
;;;;                          variable; a key /connect saved in auth.json
;;;;                          answers before this point does
;;;;   LIST-PROVIDER-MODELS   the account's /models answer narrowed to those
;;;;                          two ids, as the original narrowed its picker; a
;;;;                          catalog that answers nothing offers the same two
;;;;
;;;; Config, a sibling top-level key:
;;;;   "acedatacloud": {"base_url": "https://api.acedata.cloud/openai"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-acedatacloud)

;;; --- the catalog ---------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Ace Data Cloud's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Ace Data Cloud's row, made
once per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged) (catalog-row))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

;;; --- the credential ----------------------------------------------------------------

(defun credential (op next)
  "The :CREDENTIAL answer for acedatacloud: the key ACEDATACLOUD_API_KEY
holds, else none."
  ;; Never NEXT for acedatacloud: the ladder behind this point falls back to
  ;; the chat family's default variable, and would send OPENAI_API_KEY to Ace.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the listing ----------------------------------------------------------------------

(defun listing-rows (rows)
  "ROWS narrowed to the verified ids, in the picker's order; the bundled two
when the catalog lists none of them."
  (or (loop for entry in +models+
            for row = (find (car entry) rows :key (lambda (row) (getf row :id)) :test #'equal)
            when row collect row)
      (bundled-rows)))

(defun http-refusal-p (reason)
  "Whether REASON is the endpoint's own refusal rather than a transport fact."
  (and (stringp reason) (uiop:string-prefix-p "HTTP " reason)))

(defun fetch-listing (base key)
  "(values ROWS REASON): the models GET <BASE>/models lists for KEY, or NIL and
why the endpoint answered nothing."
  (multiple-value-bind (body status)
      (nle::http-fetch (format nil "~a/models" (string-right-trim "/" base))
                        :headers `(("accept" . "application/json")
                                   ("authorization" . ,(format nil "Bearer ~a" key)))
                        :timeout (setting :timeout-seconds))
    (if (not (stringp body))
        (values nil (or (and (stringp status) status) "empty listing"))
        (nle::parse-provider-listing body))))

(defun listing (next provider &rest keys &key key base &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Ace Data Cloud's answer, narrowed to the two
verified ids; a catalog that cannot be read offers them anyway."
  (if (not (equal provider +provider+))
      (apply next provider keys)
      (let ((key (or key (listing-key))))
        (if (or (null key) (zerop (length key)))
            ;; The picker keeps its two ids on a box with no key yet, as the
            ;; original's did; /connect always passes the key it is checking.
            (values (bundled-rows)
                    (format nil "no key: Ace Data Cloud lists its models to a key (~a)"
                            +key-page+))
            (multiple-value-bind (rows reason) (fetch-listing (or base (setting :base-url)) key)
              (cond ((and rows (null reason)) (values (listing-rows rows) nil))
                    ;; The endpoint refused the key: /connect reads a nil first
                    ;; value as a refusal, and the reason is the core's shape.
                    ((http-refusal-p reason) (values nil reason))
                    ;; Nothing was asked or nothing could be read: the verified
                    ;; two stand in, and the reason says they went unchecked.
                    (t (values (bundled-rows) (or reason "no listing")))))))))

(nle:define-cell acedatacloud
  (:section ("acedatacloud")
    (:guide "make a key in the Ace Data Cloud console (Applications, a General application's API Key) and save it with /connect, or set ACEDATACLOUD_API_KEY; base_url is the chat base the lane appends /chat/completions to")
    ("base_url" :string :default +base+
     :doc "the Ace Data Cloud chat base the lane appends /chat/completions to")
    ("timeout_seconds" :integer :default 10 :min 1
     :doc "one model-listing fetch's ceiling in seconds"))
  (:start (lambda ()
            (forget-catalog)
            (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::list-provider-models #'listing))
