;;;; support.lisp --- blackbox test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Blackbox tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a BLACKBOX-CELL-
;;;; name prefix; RUN-BLACKBOX-TESTS runs exactly that slice.
;;;;
;;;; Nothing is stubbed and nothing needs stubbing: the cell reads the text
;;;; and the command it is handed, opens no file (bar a `threat_list' the test
;;;; writes itself), dials nothing and looks at no part of the machine. The
;;;; only state a test makes is a config section, through BLACKBOX-START.

(in-package #:nodecode.test)

(define-test-slice "blackbox" "BLACKBOX-CELL-" :start nodecode-blackbox:start-cell)

(defun blackbox-write-threat-list (lines)
  "A file holding LINES, one a line, in the scratch home; answer its path."
  (let ((path (merge-pathnames "blackbox-threats.txt" (uiop:temporary-directory))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (dolist (line lines) (write-line line out)))
    (namestring path)))
