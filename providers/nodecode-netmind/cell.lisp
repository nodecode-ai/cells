;;;; cell.lisp --- the cell: NetMind among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One hook, declining for every provider but netmind:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries NetMind's row: the chat
;;;;                           lane's package, this section's base, the key
;;;;                           variable and the bundled roster with each
;;;;                           model's window, answer cap, vision, reasoning
;;;;                           and tool calls, so /connect offers NetMind,
;;;;                           /models lists its models and a turn resolves
;;;;                           its lane and address the way it does any
;;;;                           catalog provider's
;;;;
;;;; And one command: /netmind answers the roster and where the key comes
;;;; from, without asking anything.
;;;;
;;;; The key needs no hook of its own: NETMIND_API_KEY is the name the core
;;;; derives for a provider called netmind (PROVIDER-ENV-KEY), so the
;;;; environment ladder answers it, and the chat family's OPENAI_API_KEY rides
;;;; only where OpenAI's own lane goes, never to api.netmind.ai.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "netmind": {"base_url": "https://api.netmind.ai/inference-api/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and NetMind is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-netmind)

(nle:define-cell netmind
  (:section ("netmind")
    (:guide "make a key at https://www.netmind.ai/; save it with /connect, NetMind (or set NETMIND_API_KEY); then pick a model in /models and try /netmind; base_url is where NetMind is served")
    ("base_url" :string :default +base+
     :doc "the NetMind API base: the chat lane appends /chat/completions"))
  (:help :netmind "netmind: NetMind's inference API as a provider - /connect, /models, /netmind" +manual+)
  (:command "netmind" 'run-command
            :description "NetMind: the models it serves, and where the key comes from"
            :argument-hint "MODELS|KEY")
  (:start (lambda ()
            (check-config)
            (forget-catalog)
            (nle:on-stop (lambda ()
                           (forget-catalog)
                           (setf *base* nil)))))
  (:hook 'nle::models-catalog-table #'catalog))
