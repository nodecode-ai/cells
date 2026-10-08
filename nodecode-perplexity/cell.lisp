;;;; cell.lisp --- the primer, the section, the /perplexity command, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verb through the manual: while the cell runs,
;;;; (help :perplexity) answers the primer below, and every session's help
;;;; section carries one line naming it. The operator signs in with
;;;; /perplexity. Nothing hooks the provider path: omp serves no chat model
;;;; under this id (provider.lisp says why).
;;;;
;;;; Config, a sibling top-level key:
;;;;   "perplexity": {"model": "experimental", "api_model": "sonar-pro",
;;;;                  "borrow_app_session": true}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-perplexity)

(defparameter +primer+
  "Perplexity search is available through the nodecode-perplexity cell: one Lisp function, called
through eval, that returns a string.
  (perplexity:search \"query\" &key n recency domains after before language)
      Perplexity searches the web live and answers the query: its answer, then the numbered
      sources it read (title, url, snippet, date), then related questions, then who answered.
      n caps the sources listed. recency is \"hour\", \"day\", \"week\", \"month\" or \"year\".
      domains is a list of hosts to keep, \"-host\" to drop one, at most 20. after and before
      are \"YYYY-MM-DD\" bounds and outrank recency. language is a two-letter code (\"en\").
      It asks with the operator's Perplexity sign-in (the account's Pro/Max models), else
      PERPLEXITY_COOKIES, else an API key, else anonymously.
Use it for a question that wants a synthesized, cited answer, and cite the urls it lists. One
search answers one question: read the answer before asking again. ERROR: PERPLEXITY-ERROR names
the problem; when it says no credential answered, tell the operator /perplexity login EMAIL."
  "What (help :perplexity) answers while the cell runs.")

(defun start ()
  "On stop, drop a sign-in waiting for its code."
  (setf *pending* nil)
  (nle:on-stop (lambda () (setf *pending* nil))))

(nle:define-cell perplexity
  (:section ("perplexity")
    (:guide "sign in with /perplexity login EMAIL and type the mailed code with /perplexity code CODE (on a Mac with the legacy Perplexity app, /perplexity login borrows its session); or set PERPLEXITY_COOKIES to a browser's Cookie header, or save an API key with /connect or PERPLEXITY_API_KEY; model is the subscription model a signed-in search asks for, api_model the one a key search asks for")
    ("model" :string :default "experimental"
     :doc "the subscription model a signed-in or cookie search asks Perplexity for (experimental is Sonar)")
    ("api_model" :string :default "sonar-pro"
     :doc "the API model a key search asks api.perplexity.ai for")
    ("borrow_app_session" :boolean :default t
     :doc "on macOS, whether /perplexity login takes the legacy Perplexity app's session before it mails a code"))
  (:start #'start)
  (:help :perplexity "perplexity:search answers a question from a live Perplexity web search, with its sources" +primer+)
  (:command "perplexity" 'run-command
            :description "Perplexity (Pro/Max) sign-in: login [EMAIL], code CODE, logout, status"
            :argument-hint "login [EMAIL] | code CODE | logout | status"
            :session nil))
