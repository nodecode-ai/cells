;;;; cell.lisp --- the cell: CoreWeave Inference among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but coreweave, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries CoreWeave's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the models /coreweave read, so
;;;;                          /connect offers it, /models lists its models and
;;;;                          a turn resolves its lane and address the way it
;;;;                          does any catalog provider's
;;;;   :CREDENTIAL            a key from COREWEAVE_API_KEY, and no other
;;;;                          variable; a key /connect saved answers before
;;;;                          this point does
;;;;
;;;;   /coreweave [TEXT]      read GET <base>/models and list the models this
;;;;                          key can use; without a key, where to make one
;;;;
;;;; Config, a sibling top-level key:
;;;;   "coreweave": {"base_url": "https://api.inference.wandb.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-coreweave)

(defun start ()
  "Start clean, and leave no merged catalog behind a stop."
  (forget-catalog)
  (nle:on-stop #'forget-catalog))

(nle:define-cell coreweave
  (:section ("coreweave")
    (:guide "make an inference-enabled W&B API key at https://wandb.ai/authorize; save it with /connect or set COREWEAVE_API_KEY; /coreweave reads the live model list and puts it in /models; base_url is where CoreWeave Serverless Inference is served")
    ("base_url" :string :default +base+
     :doc "the CoreWeave Inference API base the chat lane appends /chat/completions to"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:command "coreweave" #'run-command
            :description "CoreWeave Inference: list the models this key can use"))
