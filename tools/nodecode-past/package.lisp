;;;; package.lisp --- one package, and the words the wire uses.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Named after the system: the loader finds START-CELL as the exported
;;;; symbol of package NODECODE-PAST, and every contribution is filed under
;;;; "nodecode-past", which is the folder's name.

(defpackage #:nodecode-past
  (:nicknames #:past)
  (:use #:cl)
  (:export #:start-cell #:stop-cell))

(in-package #:nodecode-past)

;;; NLK:DEFINE-PERIPHERAL gives this package its condition, PAST-ERROR and
;;; FAIL, and -- with :NOT-RUNNING -- *PAST*, RUNNING-SETTINGS and the refusal
;;; every verb answers while the cell is not started. :LOCK is the one lock the
;;; capture and the flush share.
(nlk:define-peripheral past :not-running t :lock "past")

(defparameter +version+ "0.1.0"
  "The cell's own version, as /past prints it.")

(defparameter +default-origin+ "https://api.past.dev"
  "past.dev's API, where the original connector goes unless another address is set.")

(defparameter +marker+ "=== past · recalled from memory ==="
  "The line a recall block opens with, which a turn's own text is cleaned of.")

(defparameter +label+ "Nodecode"
  "The label every sitting is filed under in past.")

(defparameter +query-chars+ 1000
  "How much of a message a recall asks with, as the original's QUERY_MAX_CHARS.")

(defparameter +line-chars+ 400
  "How much of one memory a recalled line quotes, as the original's own cut.")

(defparameter +secret-patterns+
  (list "\\b(?:past_sk|past_mk|sk-ant|sk-|ghp_|gho_|ghu_|ghs_|github_pat|xox[baprs]|AKIA|ASIA|glpat)-?[A-Za-z0-9_\\-]{12,}"
        "\\bBearer\\s+[A-Za-z0-9._\\-]{20,}"
        "\\beyJ[A-Za-z0-9._\\-]{20,}"
        "-----BEGIN [A-Z ]*PRIVATE KEY-----[\\s\\S]*?-----END [A-Z ]*PRIVATE KEY-----")
  "The obvious shapes of keys and tokens, which never leave the machine: the
original connector's SECRET_PATTERNS, the one that names its variable last.")

(defparameter +assignment-pattern+
  "\\b[A-Z0-9_]*(?:SECRET|PASSWORD|TOKEN|API_?KEY|PRIVATE_?KEY|CREDENTIAL)[A-Z0-9_]*\\s*[=:]\\s*\\S+"
  "VARIABLE=value, where the variable's name is kept and its value is not.")

(defparameter +injected+ "<memory-context>[\\s\\S]*?</memory-context>"
  "What Hermes or past put into a turn that the person did not say.")
