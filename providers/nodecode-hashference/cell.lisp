;;;; cell.lisp --- the cell: Hashference among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One hook, declining for no one because it adds a row rather than answering
;;;; for one:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Hashference's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the models a turn can run on, each
;;;;                          with the input and output limits its catalog
;;;;                          states, so /connect offers it, /models lists its
;;;;                          models and a turn resolves its lane and address
;;;;                          the way it does any catalog provider's.
;;;;
;;;; A worker fetches the live catalog once at start, and again after each wait
;;;; while it stays unreachable; a landed fetch lets the worker end, and the
;;;; curated list answers in the meantime. The original plugin cached the
;;;; catalog for ten minutes; a fetch at start with a ten-minute retry is the
;;;; same answer for a catalog that changes this rarely, and one request fewer.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "hashference": {"base_url": "https://hashference.com/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and Hashference is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-hashference)

(nle:define-cell hashference
  (:section ("hashference")
    (:guide "make a key at https://hashference.com/keys; save it with /connect or set HASHFERENCE_API_KEY; base_url is where the OpenAI-compatible API is served, and the model catalog is read from its /models")
    ("base_url" :string :default +base+
     :doc "the OpenAI-compatible base the chat lane appends /chat/completions to, and the catalog is read from"))
  (:start (lambda ()
            ;; a start begins from no rows and no merged catalog, so a restart
            ;; after a fix fetches again and no test inherits the last start's
            (setf *live-rows* nil)
            (forget-catalog)
            (let* ((base (setting :base-url))
                   (worker (nlk:worker-start "hashference-catalog" (lambda () (refresh base))
                                             :wake 600 :prime t)))
              (nle:on-stop (lambda ()
                             (nlk:worker-stop worker)
                             (setf *live-rows* nil)
                             (forget-catalog))))))
  (:hook 'nle::models-catalog-table #'catalog))
