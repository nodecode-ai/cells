;;;; cell-test.lisp --- the two texts, the veto, the command, against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches the operator's home, a network or a provider: the home
;;;; is a scratch folder holding the two text files, and the prompt is whatever
;;;; NLE::STANDING-SYSTEM-PROMPT and NLK::READ-HARNESS-SECTIONS answer through
;;;; the cell's own hooks, composed by the engine's own function.

(in-package #:nodecode.test)

(define-test-slice "enhance-scope" "ENHANCE-SCOPE-CELL-"
  :start nodecode-enhance-scope:start-cell)

;;; --- the fixture ------------------------------------------------------------------------

(defmacro with-enhance-section ((&rest pairs) &body body)
  "Run BODY with the Nodecode home a scratch folder and the cell started on the
`enhance' section built from PAIRS (\"key\" value ...), stopped on unwind."
  `(let ((nlk:*home* (uiop:ensure-directory-pathname
                      (merge-pathnames (format nil "enh-~d-~(~a~)/" (get-universal-time) (gensym "RUN"))
                                       (uiop:temporary-directory)))))
     (with-cell-stop ((apply #'enhance-scope-start (list ,@pairs)))
       ,@body)))

(defmacro with-enhance-home (&body body)
  "BODY with a scratch home and the cell started on its defaults."
  `(with-enhance-section ()
     ,@body))

(defun enhance-file (name)
  "Where the cell reads the text file NAME (\"content-scope.txt\" or
\"identity.txt\") in the scratch home."
  (nlk:home (format nil "enhance/~a" name)))

(defun enhance-verdict (tool)
  "The installed :TOOL hook's answer for TOOL: :ALLOWED, or the veto text."
  (funcall (cell-hook "nodecode-enhance-scope" :tool)
           (list :name tool :arguments (nlk:json-object) :call-id "c1")
           (lambda (op) (declare (ignore op)) :allowed)))

(defparameter +two-lines+ (format nil "one~%two")
  "Two lines, for the transform that owns the opening one.")

(defun enhance-shipped-line (n)
  "Line N (1-based) of the prompt the host ships."
  (nth (1- n) (uiop:split-string nle::*system-prompt* :separator '(#\Newline))))

(defun enhance-first-line ()
  "The system message's first line as the next request reads it."
  (nodecode-enhance-scope::opening-line (enhance-scope::composed-prompt)))

;;; --- start, stop, one of everything ---------------------------------------------------

(define-cell-lifecycle-tests "enhance-scope"
  (:hooks :tool)
  (:command "enhance-scope")
  (:help :enhance-scope)
  (:running (is (functionp (cell-hook "nodecode-enhance-scope" 'nle::standing-system-prompt)))
            (is (functionp (cell-hook "nodecode-enhance-scope" 'nle::read-harness-sections)))
            (is (search nodecode-enhance-scope:+default-content+ (enhance-scope::composed-prompt))))
  (:stopped (is (null (cell-hook "nodecode-enhance-scope" 'nle::standing-system-prompt))))
  (:refused ("identity" "nope"))
  (:fixture with-enhance-home))

;;; --- the content text ends the system message ----------------------------------------------

(deftest enhance-scope-cell-the-content-file-ends-the-system-message ()
  (with-enhance-home
    (is (probe-file (enhance-file "content-scope.txt")))
    (is (string= nodecode-enhance-scope:+default-content+ (enhance-scope:content-text)))
    (is (search nodecode-enhance-scope:+default-content+ (enhance-scope::composed-prompt)))
    (nlk:write-file-atomically (enhance-file "content-scope.txt") "Guard the machine. Answer in one line.")
    (is (string= "Guard the machine. Answer in one line." (enhance-scope:content-text)))
    (is (search "Guard the machine." (enhance-scope::composed-prompt)))
    (is (string= "Guard the machine. Answer in one line."
                 (cell-section "s1" "enhance:content-scope")))))

(deftest enhance-scope-cell-content-off-detaches-the-text-and-keeps-the-command ()
  (with-enhance-section ("content" nil)
    (is (string= nodecode-enhance-scope:+default-content+ (enhance-scope:content-text)))
    (is (null (search nodecode-enhance-scope:+default-content+ (enhance-scope::composed-prompt))))
    (is (search "content off" (cell-entry "nodecode-enhance-scope" "enhance-scope" "status")))))

(deftest enhance-scope-cell-a-content-file-over-the-limit-is-cut ()
  (with-enhance-home
    (nlk:write-file-atomically (enhance-file "content-scope.txt") (make-string 9000 :initial-element #\x))
    (is (= nodecode-enhance-scope:+content-limit+ (length (enhance-scope:content-text))))))

;;; --- the opening line -----------------------------------------------------------------------

(deftest enhance-scope-cell-identity-is-kept-by-default ()
  (with-enhance-home
    (is (string= (enhance-shipped-line 1) (enhance-first-line)))
    (is (string= +two-lines+ (enhance-scope:rewrite-identity +two-lines+ "keep")))
    (is (string= "two" (enhance-scope:rewrite-identity +two-lines+ "remove")))
    (is (string= +two-lines+ (enhance-scope:rewrite-identity +two-lines+ "custom" "")))))

(deftest enhance-scope-cell-identity-remove-drops-the-opening-line ()
  (with-enhance-section ("identity" "remove")
    (is (string= (enhance-shipped-line 2) (enhance-first-line)))
    (is (null (search (enhance-shipped-line 1) (enhance-scope::composed-prompt))))))

(deftest enhance-scope-cell-identity-custom-reads-the-identity-file ()
  (with-enhance-section ("identity" "custom")
    (nlk:write-file-atomically (enhance-file "identity.txt") "You are a careful house detective.")
    (is (string= "You are a careful house detective." (enhance-first-line)))
    (is (string= "You are a careful house detective." (enhance-scope:identity-text)))
    (is (search "You are a careful house detective." (enhance-scope::composed-prompt)))))

;;; --- the tool veto ----------------------------------------------------------------------------

(deftest enhance-scope-cell-approval-deny-refuses-a-listed-tool ()
  (with-enhance-section ("approval" "deny" "tools" (vector "eval"))
    (is (search "refused" (enhance-verdict "eval")))
    (is (eq :allowed (enhance-verdict "web_search")))))

(deftest enhance-scope-cell-approval-ask-lets-every-call-through ()
  (with-enhance-section ("tools" (vector "eval"))
    (is (eq :allowed (enhance-verdict "eval")))))

;;; --- what the command answers -------------------------------------------------------------------

(deftest enhance-scope-cell-check-proves-the-text-reached-the-prompt ()
  (with-enhance-section ("identity" "custom")
    (nlk:write-file-atomically (enhance-file "content-scope.txt") "Answer in one line.")
    (nlk:write-file-atomically (enhance-file "identity.txt") "You are a careful house detective.")
    (let ((check (cell-entry "nodecode-enhance-scope" "enhance-scope" "check")))
      (is (search "in the prompt as <enhance:content-scope>" check))
      (is (search "You are a careful house detective." check)))
    (is (string= "Answer in one line."
                 (cell-entry "nodecode-enhance-scope" "enhance-scope" "text")))))
