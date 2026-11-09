;;;; package.lisp --- the NODECODE-BLUESKY-FEED package: three reads and their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it. Nicknamed BLUESKY so an eval form reads
;;;; (bluesky:author-feed "bsky.app"). Every verb answers a string; a refusal
;;;; is BLUESKY-FEED-ERROR, which the eval snippet draws as "ERROR: BLUESKY-FEED-ERROR: ...".

(defpackage #:nodecode-bluesky-feed
  (:use #:cl)
  (:nicknames #:bluesky)
  (:export
   ;; verbs, every one a string
   #:profile-get #:author-feed #:author-watch
   ;; lifecycle, derived from the declaration in cell.lisp
   #:start-cell #:stop-cell
   ;; the one condition
   #:bluesky-feed-error))

(in-package #:nodecode-bluesky-feed)

;;; DEFINE-PERIPHERAL gives this package its condition BLUESKY-FEED-ERROR, FAIL for a
;;; refusal in words, the settings variable *BLUESKY-FEED* and RUNNING-SETTINGS.
;;; SETTING, which client.lisp and cell.lisp read, comes from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last.
(nlk:define-peripheral bluesky-feed :not-running t)
