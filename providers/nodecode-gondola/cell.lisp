;;;; cell.lisp --- the cell: Gondola among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One hook, declining for no one because it adds a row rather than answering
;;;; for one:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Gondola's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the chat-capable models, so
;;;;                          /connect offers it, /models lists its models and
;;;;                          a turn resolves its lane and address the way it
;;;;                          does any catalog provider's.
;;;;
;;;; A worker fetches the live catalog once at start, and again after each wait
;;;; while it stays unreachable; a landed fetch lets the worker end, and the
;;;; curated list answers in the meantime.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "gondola": {"base_url": "https://api.gondola-ai.com/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-gondola)

(nle:define-cell gondola
  (:section ("gondola")
    (:guide "mint a key at https://gondola-ai.com/wallet?tab=keys and fund the wallet with USDC on Base; save the key with /connect or set GONDOLA_API_KEY; base_url is where the OpenAI-compatible API is served")
    ("base_url" :string :default +base+
     :doc "the OpenAI-compatible base the chat lane appends /chat/completions to"))
  (:start (lambda ()
            ;; a start begins from no rows and no merged catalog, so a restart
            ;; after a fix fetches again and no test inherits the last start's
            (setf *live-rows* nil)
            (forget-catalog)
            (let* ((base (setting :base-url))
                   (worker (nlk:worker-start "gondola-catalog" (lambda () (refresh base))
                                             :wake 1800 :prime t)))
              (nle:on-stop (lambda ()
                             (nlk:worker-stop worker)
                             (setf *live-rows* nil)
                             (forget-catalog))))))
  (:hook 'nle::models-catalog-table #'catalog))
