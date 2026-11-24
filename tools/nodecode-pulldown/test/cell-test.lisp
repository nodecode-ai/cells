;;;; cell-test.lisp --- the tool, the command, the verb, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed program: that
;;;; its start installs the tool, the command and the manual and its stop takes
;;;; them back, that the tool and the verb ask pulldown the argv a user's shell
;;;; would ask ("get" "--json-output" "--meta" "--detail" ... URL) and read the
;;;; JSON the plugin read -- title, address, status, routing, description,
;;;; content -- that detail, render, max_chars and the cache folder each reach
;;;; the program, that a page clipped, a page that was cached and a page that
;;;; wants the renderer each say so, that an address that is no http(s), a
;;;; page the program could not read, an answer that is no JSON and a read past
;;;; its deadline each say why, and that a missing program refuses the start.
;;;; One test runs a real /bin/sh program, for the argv a stub cannot prove.
;;;; Nothing here needs pulldown installed or a network.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "pulldown"
  (:help :pulldown)
  (:command "pulldown")
  (:running (is (= 60 (nodecode-pulldown::setting :timeout-seconds)) "a minute by default")
            (is (plusp (length (nodecode-pulldown::setting :program))) "and a program is named"))
  (:refused ("program" "definitely-no-such-program-xyz") ("timeout_seconds" 0) ("timeout_seconds" 600))
  (:idle pulldown:pulldown-error (pulldown:fetch +address+)))

;;; --- the tool ---------------------------------------------------------------------

(deftest pulldown-cell-the-tool-answers-with-the-page-and-its-routing ()
  (with-cell-stop ((pulldown-start))
    (with-pulldown-stub ()
      (is-present (entry (nle::find-tool "pulldown_fetch"))
        "pulldown_fetch is registered"
        (is (hash-table-p (nlk:contract-schema (second entry))) "against a declared contract")
        (let* ((call (nlk:contract-decode (second entry) (nlk:make-json-object "url" +address+)))
               (answer (funcall (third entry) call)))
          (is (search "# A Bridge in the Fog" answer) "the page's markdown is the answer")
          (is (search +address+ answer) "with the page's address")
          (is (search "[HTTP 200 · readable detail" answer) "and how it was read")
          (is (search "read by readability" answer) "the routing record is named")
          (is (search "> How one town rebuilt it" answer) "the description is quoted")
          (is (search "812ms" answer) "the time the program reported")
          (is (equal "get" (first (last-args))) "the CLI's own get is what ran")
          (is (equal +address+ (car (last (last-args)))) "with the address last")
          (is (search "pulldown" (first (last-run))) "and the configured program is what ran"))))))

(deftest pulldown-cell-detail-render-and-the-clip-reach-the-program ()
  (with-cell-stop ((pulldown-start))
    (with-pulldown-stub ()
      (let ((answer (pulldown:fetch +address+ :detail "structured" :render t :max-chars 3000)))
        (is (equal "structured" (nth 4 (last-args))) "the detail level rides --detail")
        (is (member "--render" (last-args) :test #'equal) "render asks the program for chromium")
        (is (search "· rendered]" answer) "and the answer says the page was rendered")))))

(deftest pulldown-cell-a-clip-cuts-the-page-and-says-how-much-is-left ()
  (with-cell-stop ((pulldown-start))
    (with-pulldown-stub (:stdout (pulldown-json
                                   :content (concatenate 'string +article+
                                                           (make-string 1500
                                                                        :initial-element #\a))))
      (let ((answer (pulldown:fetch +address+ :max-chars 1000)))
        (is (search "clipped at 1,000 of" answer) "the cut is said out loud")
        (is (search "larger max_chars" answer) "with the way to read the rest")
        (is (eql 0 (search "# A Bridge" answer)) "and the answer opens with the page")))))

(deftest pulldown-cell-a-page-that-wants-a-browser-says-so ()
  (with-cell-stop ((pulldown-start))
    (with-pulldown-stub (:stdout (pulldown-json :render-recommended t))
      (is (search "render true" (pulldown:fetch +address+)) "a JavaScript-looking page suggests the renderer")
      (is (null (search "render true" (pulldown:fetch +address+ :render t))) "and stops suggesting it once the page was rendered"))))

(deftest pulldown-cell-the-cache-is-a-folder-and-a-cached-page-says-so ()
  (let ((directory (uiop:native-namestring
                    (merge-pathnames "pulldown-cache/" (uiop:temporary-directory)))))
    (with-cell-stop ((pulldown-start "cache" t "cache_dir" directory))
      (with-pulldown-stub (:stdout (pulldown-json :from-cache t))
        (let ((answer (pulldown:fetch +address+)))
          (is (search "· cached" answer) "a page a cached copy answered says so")
          (is (member "--cache-dir" (last-args) :test #'equal) "the folder rides --cache-dir")
          (is (member directory (last-args) :test #'equal) "and it is the configured one"))))))

(deftest pulldown-cell-with-no-folder-the-cell-keeps-its-own-under-the-nodecode-home ()
  (with-cell-stop ((pulldown-start "cache" t))
    (with-pulldown-stub ()
      (pulldown:fetch +address+)
      (is (member "--cache-dir" (last-args) :test #'equal) "caching is on by default")
      (is (search "cache/pulldown" (nodecode-pulldown::default-cache-dir)) "in a folder of the cell's own, never another program's"))))

;;; --- the command ------------------------------------------------------------------

(deftest pulldown-cell-the-command-reads-a-page-and-asks-for-one-with-none ()
  (with-cell-stop ((pulldown-start))
    (with-pulldown-stub ()
      (let ((answer (cell-entry "nodecode-pulldown" "pulldown" +address+)))
        (is (search "A Bridge in the Fog" answer) "the operator reads the page")
        (is (equal "get" (first (last-args))) "through the same argv"))
      (let ((before *runs*))
        (is (search "Read a page here with /pulldown" (cell-entry "nodecode-pulldown" "pulldown" "")) "no argument says what the cell reads with")
        (is (equal before *runs*) "and reads nothing")))))

;;; --- every refusal ----------------------------------------------------------------

(deftest pulldown-cell-every-way-a-page-fails-says-why ()
  (with-cell-stop ((pulldown-start))
    (with-pulldown-stub (:stdout (pulldown-json :ok nil :status 404 :content "" :error "HTTP 404")
                          :exit 1)
      (is (search "HTTP 404" (pulldown-refusal +address+)) "the program's own reason rides it"))
    (with-pulldown-stub (:stdout "" :stderr "could not resolve example.org" :exit 2)
      (is (search "could not resolve" (pulldown-refusal +address+)) "a program that died says why"))
    (with-pulldown-stub (:stdout "")
      (is (search "printed nothing" (pulldown-refusal +address+)) "silence is named"))
    (with-pulldown-stub (:stdout "not json at all")
      (is (search "not JSON" (pulldown-refusal +address+)) "an answer that is no JSON says so"))
    (with-pulldown-stub (:stdout (pulldown-json :omit '("ok")))
      (is (search "no reason" (pulldown-refusal +address+)) "an answer that claims no page says so"))
    (with-pulldown-stub (:timed-out t)
      (is (search "did not finish" (pulldown-refusal +address+)) "a read past its deadline says so"))))

(deftest pulldown-cell-an-argument-the-program-cannot-take-is-refused ()
  (with-cell-stop ((pulldown-start))
    (with-pulldown-stub ()
      (flet ((refused (&rest arguments)
               (handler-case (apply #'pulldown:fetch +address+ arguments)
                 (error (condition) (princ-to-string condition)))))
        (is (search "minimal, readable, structured, full" (refused :detail "raw")) "an unknown detail names the four")
        (is (search "1000 to 40000" (refused :max-chars 10)) "a clip below the floor names the range")
        (is (search "http://" (pulldown-refusal "ftp://example.org/a-bridge")) "a scheme the program cannot read is refused")
        (is (null *runs*) "nothing was run for any of them")))))

;;; --- the real program -------------------------------------------------------------

(deftest pulldown-cell-the-program-is-really-run-and-its-argv-carries-the-address ()
  (let ((directory (merge-pathnames "pulldown-running/" (uiop:temporary-directory))))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree directory :validate t)))
      (with-cell-stop ((pulldown-start "program" (pulldown-recording-program directory)))
        (let ((answer (pulldown:fetch +address+)))
          (is (search "A Bridge in the Fog" answer) "the program's answer is the page"))
        (let ((argv (uiop:read-file-string (merge-pathnames "argv" directory))))
          (is (search "get" argv) "the CLI's get was asked")
          (is (search "--json-output" argv) "for the JSON answer")
          (is (search +address+ argv) "the address as an argument"))))))
