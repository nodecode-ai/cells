;;;; package.lisp --- the DREAMING package: the verbs, the data a layer may reword.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every refusal is DREAMING-ERROR, rendered by the eval snippet as
;;;; "ERROR: DREAMING-ERROR: <detail>". Each verb answers a string.

(defpackage #:nodecode-dreaming
  (:use #:cl)
  (:nicknames #:dreaming)
  (:export
   ;; verbs, every one a string
   #:sight #:report #:reject #:diary-tail #:run-slash
   ;; the pass itself, so a layer can schedule it differently
   #:nightly-step #:pass #:state-path #:diary-path #:read-state
   ;; the words a layer may reword
   #:+memory-types+ #:+diary-name+
   ;; the one condition
   #:dreaming-error))
