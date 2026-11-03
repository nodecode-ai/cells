;;;; repro.lisp --- prove an item: a test that fails today. Placeholder until the repro lands.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; (REPRO ITEM &key HOME) answers (:TEST PATH :SHA256 HEX :FAILS BOOL
;;;; :OUTPUT STRING): a test file written for ITEM, the hash of that file,
;;;; whether it fails as the item says, and what it printed. It runs in a
;;;; throwaway home under HOME with keys and network off.

(in-package #:nodecode-selffix)

(defun repro (item &key home)
  "Not built yet: a cycle that reaches it records a failed cycle."
  (declare (ignore item home))
  (fail "repro is not built yet"))
