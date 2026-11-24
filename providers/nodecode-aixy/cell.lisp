;;;; cell.lisp --- the cell: Aixy among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, both declining for every provider but aixy:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Aixy's row: the chat
;;;;                           lane's package, this section's base, and the
;;;;                           key variable, so /connect offers Aixy, /models
;;;;                           lists its models and a turn on it resolves its
;;;;                           lane and address the way it does any catalog
;;;;                           provider's
;;;;   WALK-PROVIDER-STREAM   the request carries this turn's conversation id
;;;;                           in the session header Aixy Activity groups by
;;;;
;;;; And one command: /aixy answers what the gateway serves (GET {base}/models
;;;; with the project key, the call the plugin's setup makes) and where that
;;;; key comes from.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "aixy": {"base_url": "https://api.aixy-gateway.com/v1",
;;;;            "session_header": "X-Aixy-Session-ID"}
;;;; A vetoed section ("enabled": false) installs nothing, and Aixy is then
;;;; whatever models.dev alone makes of it.

(in-package #:nodecode-aixy)

(nle:define-cell aixy
  (:section ("aixy")
    (:guide "make a project key in Aixy, save it with /connect, Aixy (or set AIXY_API_KEY), then pick a model in /models and try /aixy; base_url is where the gateway answers")
    ("base_url" :string :default +base+
     :doc "the Aixy gateway base: the chat lane appends /chat/completions, the listing /models")
    ("session_header" :string :default "X-Aixy-Session-ID"
     :doc "the header this turn's conversation id rides, so Aixy Activity groups its requests; \"none\" sends none"))
  (:help :aixy "aixy: Aixy's inference gateway as a provider - /connect, /models, /aixy" +manual+)
  (:command "aixy" 'run-command
            :description "Aixy: the gateway, its key, and the models it serves"
            :argument-hint "MODELS|KEY")
  (:start (lambda ()
            (check-config)
            (forget-catalog)
            (nle:on-stop (lambda ()
                           (forget-catalog)
                           (setf *base* nil)))))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::walk-provider-stream #'walk))
