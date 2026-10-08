;;;; addon.lisp --- the add-on: two advices on one lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A hook on RESOLVE-PROVIDER-CREDENTIAL puts the client header on every
;;;; request the served lane makes, and a hook on LIST-PROVIDER-MODELS adds
;;;; Cline's feed to that lane's listing (feed.lisp). The listing is fetched by
;;;; the gateway, whose image started this folder, so an attached shell's
;;;; /models shows the rows too.
;;;;
;;;; Config, a sibling top-level key next to `websearch' and `cron':
;;;;   "cline": {"enabled": true, "provider": "cline-pass",
;;;;             "client_type": "nodecode", "buckets": ["clinePass", "free"]}
;;;; A vetoed section ("enabled": false) installs neither hook: the lane then
;;;; lists and dials exactly as it did before this folder existed.

(in-package #:nodecode-cline)

(nle:define-addon cline
  (:section ("cline")
    (:guide "provider is the lane a Cline API key serves; buckets are the feed's lists its /models picker gains")
    ("provider" :string :default "cline-pass"
     :doc "the provider id whose requests carry the client header and whose listing gains the feed")
    ("client_type" :string :default "nodecode"
     :doc "what X-CLIENT-TYPE names this client; api.cline.bot serves its free models only to a request that names one")
    ("feed" :string :default "https://api.cline.bot/api/v1/ai/cline/recommended-models"
     :doc "the public feed of the models Cline serves by plan")
    ("buckets" :list :default (vector "clinePass" "free")
     :doc "the feed's lists the listing takes: clinePass, free, recommended, clineCloud"))
  (:hook 'nle::resolve-provider-credential #'identify)
  (:hook 'nle::list-provider-models #'list-with-feed))
