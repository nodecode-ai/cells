;;;; cell.lisp --- the cell: CloudGPU among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but cloudgpu, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries CloudGPU's row - the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the bundled models with the ones
;;;;                          /cloudgpu read over them - so /connect offers
;;;;                          it, /models lists its models and a turn
;;;;                          resolves its lane and address the way it does
;;;;                          any catalog provider's
;;;;   :CREDENTIAL            a key from CLOUDGPU_API_KEY, and no other
;;;;                          variable; a key /connect saved answers before
;;;;                          this point does
;;;;
;;;;   /cloudgpu [TEXT]       read GET <base>/models and list the models
;;;;
;;;; Config, a sibling top-level key:
;;;;   "cloudgpu": {"base_url": "https://cloudgpu.app/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-cloudgpu)

(nle:define-cell cloudgpu
  (:section ("cloudgpu")
    (:guide "make a key at https://cloudgpu.app/api and save it with /connect or as CLOUDGPU_API_KEY; /cloudgpu lists the models the API publishes; base_url is where CloudGPU is served")
    ("base_url" :string :default +base+
     :doc "the CloudGPU API base the chat lane appends /chat/completions to"))
  (:settings 'read-settings)
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:command "cloudgpu" #'run-command
            :description "CloudGPU: list the models the API publishes"))
