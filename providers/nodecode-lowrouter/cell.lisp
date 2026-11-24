;;;; cell.lisp --- the cell: LowRouter among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Two hooks, and one command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries LowRouter's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the routes this cell read -- the
;;;;                          `auto/' and `alias/' chat models that take tool
;;;;                          calls and fit a turn -- with the plugin's four
;;;;                          fallbacks answering until the live catalog has,
;;;;                          so /connect offers it, /models lists its models
;;;;                          and a turn resolves its lane and address the way
;;;;                          it does any catalog provider's
;;;;   :CREDENTIAL            a key from LOWROUTER_API_KEY, and no other
;;;;                          variable; without one a public credential, since
;;;;                          /v1/models answers anonymously; a key /connect
;;;;                          saved answers before this point does
;;;;
;;;;   /lowrouter [models]    your credit balance in euros, or the live
;;;;                          catalog's routes taken into the picker
;;;;
;;;; A worker fetches the catalog once at start, and again after each wait
;;;; while it stays unreachable; a landed fetch lets the worker end, and the
;;;; four fallbacks answer in the meantime.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "lowrouter": {"base_url": "https://api.lowrouter.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and LowRouter is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-lowrouter)

(defun start ()
  "Start clean, fetch the catalog once, and leave no worker or merged catalog
behind a stop."
  (setf *live-rows* nil)
  (forget-catalog)
  (let* ((base (base))
         (worker (nlk:worker-start "lowrouter-catalog" (lambda () (refresh base))
                                   :wake 3600 :prime t)))
    (nle:on-stop (lambda ()
                   (nlk:worker-stop worker)
                   (setf *live-rows* nil)
                   (forget-catalog)))))

(nle:define-cell lowrouter
  (:section ("lowrouter")
    (:guide "make a key at https://lowrouter.ai/app/keys and save it with /connect or set LOWROUTER_API_KEY; the picker offers the auto/ and alias/ chat routes that take tool calls and fit a turn; /lowrouter shows your balance in the currency LowRouter bills in")
    ("base_url" :string :default +base+
     :doc "the OpenAI-compatible base the chat lane appends /chat/completions to, and this cell reads /models and /credits from"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:command "lowrouter" #'run-command
            :description "LowRouter: your credit balance, and the routes the picker offers"))
