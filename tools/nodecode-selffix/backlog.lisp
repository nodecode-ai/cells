;;;; backlog.lisp --- what the home keeps failing at, counted. Placeholder until the backlog lands.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; (BACKLOG &key LIMIT) answers items, most frequent first, each a plist
;;;; (:SOURCE :KEY :COUNT :LAST :EVIDENCE), read from what every install
;;;; records: failed turns in the store, notes the experience cell keeps
;;;; writing, /qa notes. The cycle takes them in this order. This placeholder
;;;; answers none, so a cycle finds nothing to do.

(in-package #:nodecode-selffix)

(defun backlog (&key limit)
  "Items to try, most frequent first; none until the backlog lands."
  (declare (ignore limit))
  '())
