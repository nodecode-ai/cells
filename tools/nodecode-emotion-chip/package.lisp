;;;; package.lisp --- the one package this cell defines.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The package is named after the system, and its nickname is the short
;;;; name the slash command and the manual topic use: emotion-chip.

(defpackage #:nodecode-emotion-chip
  (:nicknames #:emotion-chip)
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           ;; the renderer on its own: a score in, a dialect's text and
           ;; params out, no network and no state of its own.
           #:parse-score #:render #:render-score #:dialect-for #:guide-text
           #:strip-score
           #:cue #:cue-p #:cue-raw #:cue-words #:cue-rung
           #:span #:span-p #:span-op #:span-text
           #:event #:event-p #:event-id
           #:speaker #:speaker-p #:speaker-id
           #:plain #:plain-p #:plain-text
           #:sentence #:sentence-p #:sentence-tokens
           #:header #:header-p #:header-raw #:header-fields
           #:dropped #:dropped-p #:dropped-kind #:dropped-value #:dropped-reason
           #:score #:score-p #:score-header #:score-sentences #:score-source #:score-dropped
           #:rendered #:rendered-p #:rendered-text #:rendered-params
           #:rendered-notes #:rendered-dropped #:rendered-dialect #:rendered-group))
