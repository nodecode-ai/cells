;;;; cell.lisp --- the cell: AnyRouter among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but anyrouter, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries AnyRouter's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the tool-capable routes /anyrouter
;;;;                          read (with the catalog's own prices) over the
;;;;                          original's fallback rows, so /connect offers
;;;;                          it, /models lists its models and a turn
;;;;                          resolves its lane and address the way it does
;;;;                          any catalog provider's
;;;;   :CREDENTIAL            a key from ANYROUTER_API_KEY, and no other
;;;;                          variable; a key /connect saved answers before
;;;;                          this point does
;;;;   REQUEST-BODY           the session id, the routing preferences, and
;;;;                          reasoning as AnyRouter's own object rather than
;;;;                          the OpenAI spelling
;;;;   WALK-PROVIDER-STREAM   the app-attribution headers, only when the
;;;;                          section asks for them
;;;;
;;;;   /anyrouter [TEXT]      read GET /v1/models and list the routes that
;;;;                          take tool calls
;;;;
;;;; Config, a sibling top-level key:
;;;;   "anyrouter": {"base_url": "https://anyrouter.dev/api/v1",
;;;;                  "attribution": false, "only": ["anthropic"],
;;;;                  "sort": "price", "allow_fallbacks": "default"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-anyrouter)

(defun start ()
  "Start clean, and leave no merged catalog behind a stop."
  (forget-catalog)
  (nle:on-stop #'forget-catalog))

(nle:define-cell anyrouter
  (:section ("anyrouter")
    (:guide "make a key at https://anyrouter.dev and save it with /connect or set ANYROUTER_API_KEY; /anyrouter reads the live catalog and lists the routes that take tool calls; only, ignore and order are upstream routing preferences; attribution credits Nodecode's traffic in AnyRouter's public app rankings and is off by default")
    ("base_url" :string :default +base+
     :doc "the AnyRouter API base the chat lane appends /chat/completions to")
    ("attribution" :boolean :default nil
     :doc "whether every request carries AnyRouter's app-attribution headers, which credit Nodecode's traffic in its public app rankings")
    ("only" :list
     :doc "the upstreams AnyRouter may route to, and no other")
    ("ignore" :list
     :doc "the upstreams AnyRouter must not route to")
    ("order" :list
     :doc "the upstreams AnyRouter tries first, in order")
    ("sort" :choice :options '("default" "price" "throughput" "latency") :default "default"
     :doc "how AnyRouter picks among the upstreams that can serve a model: cheapest, fastest to start, or lowest latency; default leaves it to AnyRouter")
    ("allow_fallbacks" :choice :options '("default" "on" "off") :default "default"
     :doc "whether AnyRouter may fall back to another upstream when the preferred one cannot serve the request: default leaves it to AnyRouter")
    ("max_price" :integer :min 0
     :doc "the most, in whole US dollars per million tokens, a route may cost"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "anyrouter" #'run-command
            :description "AnyRouter: list the routes that take tool calls"))
