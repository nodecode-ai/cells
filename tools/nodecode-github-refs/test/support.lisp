;;;; support.lisp --- the test slice: a stubbed api.github.com.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; GITHUB-REFS-CELL- name prefix; RUN-GITHUB-REFS-TESTS runs exactly that
;;;; slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-GITHUB-REFS::HTTP, the
;;;; cell's only way to the network. WITH-WIRE replaces it with canned
;;;; answers and records every call, so a test asserts on the request and
;;;; nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "github-refs" "GITHUB-REFS-CELL-" :start nodecode-github-refs:start-cell)

;;; --- the canned API --------------------------------------------------------------

(defparameter +issue-42+
  "{\"number\":42,\"title\":\"Allow plugins to register context references\",\"html_url\":\"https://github.com/octo/tools/issues/42\",\"state\":\"closed\",\"state_reason\":\"completed\",\"locked\":false,\"comments\":3,\"created_at\":\"2026-05-15T09:00:00Z\",\"closed_at\":\"2026-08-13T17:30:00Z\",\"user\":{\"login\":\"pat\"},\"labels\":[{\"name\":\"type/feature\"},{\"name\":\"P3\"}],\"assignees\":[{\"login\":\"sam\"}],\"milestone\":{\"title\":\"Phase 1\"},\"body\":\"## Summary\\n\\nAttach a reference. <!-- a template hint -->\\n\\n```\\n<!-- kept in a fence -->\\n```\\n\\nSigned-off-by: nobody\",\"comments_url\":\"https://api.github.com/repos/octo/tools/issues/42/comments\"}"
  "Issue octo/tools#42: closed with a reason, labels, an assignee, a
milestone, a body carrying an HTML comment and a fenced one.")

(defparameter +comments-page+
  "[{\"user\":{\"login\":\"ann\"},\"created_at\":\"2026-05-16T10:00:00Z\",\"body\":\"Adopted as a Phase-1 item.\"},{\"user\":{\"login\":\"bo\"},\"created_at\":\"2026-06-01T11:00:00Z\",\"body\":\"+1, we need this. <!-- hidden marker -->\"}]"
  "What the first page of comments answers: two comments, the second with a
hidden marker.")

(defparameter +comments-last+
  "[{\"user\":{\"login\":\"cy\"},\"created_at\":\"2026-08-13T12:00:00Z\",\"body\":\"Shipped in 0.21.\"}]"
  "What the newest page of comments answers: one comment.")

(defparameter +pull-77+
  "{\"number\":77,\"title\":\"Register context references\",\"html_url\":\"https://github.com/octo/tools/pull/77\",\"state\":\"open\",\"comments\":0,\"created_at\":\"2026-07-01T08:00:00Z\",\"user\":{\"login\":\"pat\"},\"body\":\"The provider seam. <b>not html</b>\",\"pull_request\":{\"url\":\"https://api.github.com/repos/octo/tools/pulls/77\"},\"comments_url\":\"https://api.github.com/repos/octo/tools/issues/77/comments\"}"
  "Pull request octo/tools#77 as the issue endpoint answers it.")

(defparameter +pull-detail+
  "{\"merged\":true,\"merged_at\":\"2026-08-13T17:30:00Z\",\"draft\":false,\"commits\":3,\"additions\":120,\"deletions\":4,\"changed_files\":2,\"head\":{\"label\":\"octo:context-refs\"},\"base\":{\"label\":\"octo:main\"}}"
  "The pull request's own details: merged, its branches and its size.")

(defparameter +moved-9+
  "{\"number\":9,\"title\":\"Moved issue\",\"html_url\":\"https://github.com/neworg/newrepo/issues/9\",\"state\":\"open\",\"comments\":0,\"created_at\":\"2026-01-02T03:00:00Z\",\"user\":{\"login\":\"dee\"},\"body\":\"\",\"comments_url\":\"https://api.github.com/repos/neworg/newrepo/issues/9/comments\"}"
  "An issue whose repository was renamed: the url says another owner.")

(defparameter +absent+
  "{\"message\":\"Not Found\",\"documentation_url\":\"https://docs.github.com/rest\"}"
  "What a number that does not exist answers.")

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:method :url :headers).")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS HEADERS), or NIL for the
fixtures above.")

(defun stub-answer (url)
  "The fixture URL answers with."
  (cond ((cl:search "&page=2" url) (values +comments-last+ 200 nil))
        ((cl:search "/comments" url) (values +comments-page+ 200 nil))
        ((cl:search "/pulls/77" url) (values +pull-detail+ 200 nil))
        ((cl:search "/issues/77" url) (values +pull-77+ 200 nil))
        ((cl:search "/issues/42" url) (values +issue-42+ 200 nil))
        ((cl:search "/issues/9" url) (values +moved-9+ 200 nil))
        (t (values +absent+ 404 nil))))

(defun stub-http (method url &key headers timeout)
  (declare (ignore timeout))
  (push (list :method method :url url :headers headers) *calls*)
  (if *answer* (funcall *answer* (first *calls*)) (stub-answer url)))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-github-refs::http
                                (method url &key headers timeout)
                                (stub-http method url :headers headers :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defmacro with-gh-refs ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((github-refs-start ,@pairs))
       ,@body)))

(defun gh-refs-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the requests a call made ---------------------------------------------

(defun call-urls ()
  "The url of every request the stub took, newest first."
  (mapcar (lambda (call) (getf call :url)) *calls*))

(defun calls-matching (needle)
  "The calls whose url carries NEEDLE, newest first."
  (remove-if-not (lambda (call) (cl:search needle (getf call :url))) *calls*))

(defun call-header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))
