;;;; support.lisp --- the redteam-gates test runner and the helpers every test reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under a REDTEAM-GATES name
;;;; prefix; RUN-REDTEAM-GATES-TESTS runs exactly that slice. Nothing here
;;;; dials anything: a gate reads the artifacts of a scratch workspace and
;;;; appends a row to its gate-log.md, and the one seam a test reaches for is
;;;; the cell's own tool, called through the wire a provider's call comes
;;;; through.

(in-package #:nodecode.test)

(define-test-slice "redteam-gates" "REDTEAM-GATES" :start nodecode-redteam-gates:start-cell)

(defun rg-workspace ()
  "A fresh, empty scratch directory to hold one test's artifacts."
  (let ((path (format nil "~a/redteam-gates-~(~36r~)/"
                       (uiop:temporary-directory) (random most-positive-fixnum))))
    (ensure-directories-exist path)
    path))

(defmacro with-workspace ((place) &body body)
  "Run BODY with PLACE a fresh scratch workspace, removed on unwind."
  (let ((path (gensym "WORKSPACE")))
    `(let ((,path (rg-workspace)))
       (nlk:with-cleanup ((ignore-errors
                            (uiop:delete-directory-tree
                             (uiop:ensure-directory-pathname ,path) :validate t)))
         (let ((,place ,path))
           ,@body)))))

(defun rg-write (workspace name text)
  "TEXT written to NAME under WORKSPACE, the directory made; answers the path."
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname workspace))))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string text stream))
    path))

(defun rg-tool (id name arguments)
  "A tool call as the provider's call makes it: the answer's text."
  (nle::message-content (execute-wire-call id name arguments)))

(defun rg-log-text (workspace)
  "The gate log under WORKSPACE, or NIL when there is none."
  (nlk:read-text (merge-pathnames "gate-log.md" (uiop:ensure-directory-pathname workspace))))

(defparameter +rg-assets+
  "# assets

| asset | note |
|---|---|
| app.example.com | WAF in front, rate limit 10/s |
| api.example.com | no rate limit, batch endpoint |
"
  "An assets.md a pentest baseline gate accepts.")

(defparameter +rg-evidence+
  "# evidence index

Every row names the tool-plane it came from; an MCP tool is marked as one.

| id | evidence |
|---|---|
| E1 | version banner from a plain request |
"
  "An evidence-index.md a baseline gate accepts.")
