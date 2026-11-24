;;;; cell-test.lisp --- the tool, the command, the verb, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed program: that
;;;; its start installs the tool, the command and the manual and its stop takes
;;;; them back, that each of the three asks defuddle the argv the plugin asked
;;;; ("parse" "--json" URL) and reads the JSON the plugin read, that an address
;;;; which could be read as a flag, a page that is no article, an answer that is
;;;; no JSON and a read past its deadline each say why, and that a missing
;;;; program refuses the start. One test runs a real /bin/sh program, for the
;;;; argv a stub cannot prove. Nothing here needs defuddle installed or a
;;;; network.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "defuddle"
  (:help :defuddle)
  (:command "defuddle")
  (:running (is (= 60 (nodecode-defuddle::setting :timeout-seconds)) "a minute by default")
            (is (plusp (length (nodecode-defuddle::setting :program))) "and a program is named"))
  (:refused ("program" "definitely-no-such-program-xyz") ("timeout_seconds" 0))
  (:idle defuddle:defuddle-error (defuddle:extract "https://example.org/a-bridge")))

;;; --- the tool ---------------------------------------------------------------------

(deftest defuddle-cell-the-tool-answers-with-the-article-and-its-metadata ()
  (with-cell-stop ((defuddle-start))
    (with-defuddle-stub ()
      (is-present (entry (nle::find-tool "defuddle_extract"))
        "defuddle_extract is registered"
        (is (hash-table-p (nlk:contract-schema (second entry))) "against a declared contract")
        (let* ((call (nlk:contract-decode (second entry) (nlk:make-json-object "url" +bridge+)))
               (answer (funcall (third entry) call)))
          (is (search "# A Bridge in the Fog" answer) "the article's markdown is the answer")
          (is (search +bridge+ answer) "with the page's address")
          (is (search "812 words" answer) "and the word count the page reported")
          (is (search "> How one town rebuilt it" answer) "the description is quoted, as the plugin wrote it")
          (is (search "published 2024-03-01" answer) "one line says what the page says about itself")
          (is (equal '("parse" "--json" "https://example.org/a-bridge") (last-args)) "the argv is the plugin's")
          (is (search "defuddle" (first (last-run))) "and the configured program is what ran"))))))

(deftest defuddle-cell-a-positive-clip-cuts-and-says-how-much-is-left ()
  (with-cell-stop ((defuddle-start))
    (with-defuddle-stub ()
      (let* ((entry (nle::find-tool "defuddle_extract"))
             (call (nlk:contract-decode (second entry)
                                         (nlk:make-json-object "url" +bridge+ "max_chars" 40)))
             (answer (funcall (third entry) call)))
        (is (search "clipped at 40 of" answer) "the cut is said out loud")
        (is (search "max_chars 0" answer) "with the way to read the rest")
        (is (eql 0 (search "A Bridge" answer)) "and the answer opens with the page")))))

;;; --- the command ------------------------------------------------------------------

(deftest defuddle-cell-the-command-reads-a-page-and-asks-for-one-with-none ()
  (with-cell-stop ((defuddle-start))
    (with-defuddle-stub ()
      (let ((answer (cell-entry "nodecode-defuddle" "defuddle" +bridge+)))
        (is (search "A Bridge in the Fog" answer) "the operator reads the page")
        (is (equal '("parse" "--json" "https://example.org/a-bridge") (last-args)) "through the same argv"))
      (let ((before *runs*))
        (is (search "type a URL" (cell-entry "nodecode-defuddle" "defuddle" "")) "no argument asks for one")
        (is (equal before *runs*) "and reads nothing")))))

;;; --- every refusal ----------------------------------------------------------------

(deftest defuddle-cell-every-way-a-page-fails-says-why ()
  (with-cell-stop ((defuddle-start))
    (with-defuddle-stub (:exit 2 :stderr "could not resolve example.org")
      (is (search "could not resolve" (defuddle-refusal +bridge+)) "the program's own message rides it"))
    (with-defuddle-stub (:stdout "")
      (is (search "printed nothing" (defuddle-refusal +bridge+)) "silence is named"))
    (with-defuddle-stub (:stdout "not json at all")
      (is (search "not JSON" (defuddle-refusal +bridge+)) "an answer that is no JSON says so"))
    (with-defuddle-stub (:stdout (defuddle-json :omit '("contentMarkdown")))
      (is (search "no article" (defuddle-refusal +bridge+)) "a page with no markdown is no article"))
    (with-defuddle-stub (:timed-out t)
      (is (search "did not finish" (defuddle-refusal +bridge+)) "a read past its deadline says so"))))

(deftest defuddle-cell-an-address-that-could-be-a-flag-is-refused ()
  (with-cell-stop ((defuddle-start))
    (with-defuddle-stub ()
      (is (search "http://" (defuddle-refusal "--json")) "a leading dash never reaches the program")
      (is (search "non-empty" (defuddle-refusal "  ")) "nor does an empty address")
      (is (null *runs*) "nothing was run for either"))))

;;; --- the real program -------------------------------------------------------------

(deftest defuddle-cell-the-program-is-really-run-and-its-argv-carries-the-address ()
  (let ((directory (merge-pathnames "defuddle-running/" (uiop:temporary-directory))))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree directory :validate t)))
      (with-cell-stop ((defuddle-start "program" (defuddle-recording-program directory)))
        (let ((answer (defuddle:extract +bridge+)))
          (is (search "A Bridge in the Fog" answer) "the program's answer is the article"))
        (let ((argv (uiop:read-file-string (merge-pathnames "argv" directory))))
          (is (search "parse" argv) "it was asked to parse")
          (is (search +bridge+ argv) "the address as an argument"))))))
