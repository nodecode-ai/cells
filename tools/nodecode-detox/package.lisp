;;;; package.lisp --- the DETOX package: the four verbs, the words, the one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of kellycryptos's hermes-detox: the tool-output
;;;; firewall. One package, nicknamed DETOX, so an eval form reads
;;;; (detox:status); the slash command is /detox and the manual topic is
;;;; (help :detox). Every exported verb answers a STRING and every refusal is
;;;; DETOX-ERROR, which the eval snippet renders as "ERROR: DETOX-ERROR: <why>".
;;;;
;;;; The values this cell runs on derive from the one `detox' section in
;;;; cell.lisp (NLE:DEFINE-CELL) and are read with (SETTING :min-size).

(defpackage #:nodecode-detox
  (:use #:cl)
  (:nicknames #:detox)
  (:export
   ;; the verbs: every one answers a string
   #:inspect-evidence #:search-evidence #:status #:purge #:run-slash
   ;; the :tool point hook, so a test can drive it with a stub
   #:intercept
   ;; the one condition
   #:detox-error))

(in-package #:nodecode-detox)

(nlk:define-peripheral detox :not-running t :lock "detox")
