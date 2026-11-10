;;;; cell-test.lisp --- the request reader, the two templates, the fence, the verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches the network: a generated poller names a URL only in
;;;; the text it writes, and every write lands under a temporary root of the
;;;; test's own. Nothing generated is executed here either — the check reads
;;;; the text and runs the reader over it with evaluation off.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "foundry"
  (:help :foundry)
  (:command "foundry")
  (:running (is (nle::find-tool "foundry_plan") "the plan tool is registered")
            (is (nle::find-tool "foundry_build") "the build tool is registered")
            (is (nle::find-tool "foundry_list") "the list tool is registered")
            (is (search "one sentence in" (nle:help :foundry)) "the manual is up"))
  (:stopped (is (null (nle::find-tool "foundry_build")) "stop unregisters the tools"))
  (:refused ("max_items" 0)))

;;; --- reading the request ---------------------------------------------------------

(deftest foundry-cell-reads-a-request-into-one-archetype ()
  (with-cell-stop ((foundry-start))
    (let ((watcher (nodecode-foundry:spec-for
                    "Watch ~/Invoices for new PDF files every 30 minutes")))
      (is (equal "sqlite_folder_watcher" (getf watcher :archetype)) "a folder is a watcher")
      (is (equal "invoices-digest" (getf watcher :slug)) "the folder names the cell")
      (is (equal "invoices_digest_scan" (getf watcher :tool)) "and its tool")
      (is (equal "pdf" (getf watcher :ext)) "the one type asked for")
      (is (equal "~/Invoices" (getf watcher :source)) "the folder as written")
      (is (= 30 (getf watcher :interval)) "every 30 minutes"))
    (let ((poller (nodecode-foundry:spec-for
                   "poll https://api.github.com/repos/example/thing/issues every 2 hours")))
      (is (equal "sqlite_api_poller" (getf poller :archetype)) "a URL is a poller")
      (is (equal "github-poller" (getf poller :slug)) "a known host names itself")
      (is (equal "github_poller_poll" (getf poller :tool)))
      (is (= 120 (getf poller :interval)) "hours read as minutes")
      (is (equal "api.github.com" (getf poller :source-name)) "the host is the source")
      (is (equal "https://api.github.com/repos/example/thing/issues"
                 (getf poller :source)) "and the URL the endpoint"))
    (let ((plain (nodecode-foundry:spec-for "watch ~/Notes for new markdown files")))
      (is (= 15 (getf plain :interval)) "no interval read is the default")
      (is (equal "md" (getf plain :ext)) "a word for a type is the type"))))

(deftest foundry-cell-a-request-that-names-no-one-type-refuses ()
  (with-cell-stop ((foundry-start))
    (let ((none (foundry-refusal
                 (lambda () (nodecode-foundry:spec-for "watch ~/Notes for new things")))))
      (is (search "no file type" none) "a watcher with no type is refused, not guessed"))
    (let ((two (foundry-refusal
                (lambda ()
                  (nodecode-foundry:spec-for "watch ~/Notes for pdf and csv files")))))
      (is (search "more than one file type" two) "two types are refused too"))
    (let ((empty (foundry-refusal (lambda () (nodecode-foundry:spec-for " ")))))
      (is (search "a request is required" empty) "an empty request is refused"))
    (let ((boundary
            (handler-case
                (progn (nlk:contract-decode
                        (nlk:contract 'nodecode-foundry::foundry-request)
                        (nlk:json-object "output" "x"))
                       nil)
              (nlk:contract-refusal (condition) (princ-to-string condition)))))
      (is (search "request" boundary) "a call without the required request is refused"))))

;;; --- writing it ------------------------------------------------------------------

(deftest foundry-cell-writes-a-cell-and-checks-it ()
  (with-cell-stop ((foundry-start))
    (with-foundry-root (root)
      (let* ((result (nodecode-foundry:build
                      "watch ~/Invoices for new PDF files every 20 minutes"))
             (files (getf result :files))
             (directory (pathname (getf result :directory)))
             (checked (getf result :checked))
             (cell (uiop:read-file-string (merge-pathnames "cell.lisp" directory)))
             (draft (uiop:read-file-string (merge-pathnames "CATALOG.md" directory)))
             (readme (uiop:read-file-string (merge-pathnames "README.md" directory))))
        (is (= 6 (length files)) "the six files of a cell are written")
        (let ((expected '("CATALOG.md" "README.md" "cell.lisp" "config.example.jsonc"
                          "nodecode-invoices-digest.asd" "test/cell-test.lisp"))
              (forms (getf checked :forms)))
          (is (equal expected (sort (copy-list files) #'string<)) "the files of a cell are written")
          (is (null (getf checked :findings)) "the scan finds no marker")
          (is (getf checked :ok) (format nil "every file reads: ~s" forms)))
        (is (search "(nle:define-cell invoices-digest" cell) "the declaration is there")
        (is (search "(nlk:worker-start \"invoices-digest\"" cell) "and the worker it runs")
        (is (search ":default \"~/Invoices\"" cell) "the folder asked for is the default")
        (is (search "interval-minutes" cell) "and the interval is a setting")
        (is (search "**Slug:** invoices-digest  " draft) "the catalog draft names it")
        (is (search "Invoices Digest" readme) "the README says what it is")
        (let ((record (first (nodecode-foundry:listing))))
          (is (equal "sqlite_folder_watcher" (getf record :archetype)) "list reads the draft back"))))))

(deftest foundry-cell-a-poller-is-written-from-a-url ()
  (with-cell-stop ((foundry-start))
    (with-foundry-root (root)
      (let* ((result (nodecode-foundry:build
                      "poll https://example.com/items.json every 5 minutes"))
             (directory (pathname (getf result :directory)))
             (cell (uiop:read-file-string (merge-pathnames "cell.lisp" directory))))
        (let ((folder (car (last (pathname-directory directory)))))
          (is (equal "nodecode-example-poller" folder) "an unknown host names the folder"))
        (is (search "(nlk:http :get (setting :url)" cell) "the generated tool fetches its URL")
        (is (search "(defun new-items ()" cell) "and reads what is new")
        (is (null (getf (getf result :checked) :findings)) "the poller's text is clean")))))

(deftest foundry-cell-refuses-what-is-outside-its-root ()
  (with-cell-stop ((foundry-start))
    (with-foundry-root (root)
      (let ((outside (foundry-refusal
                      (lambda ()
                        (nodecode-foundry:build
                         "watch ~/Invoices for new PDF files"
                         :output (namestring (uiop:temporary-directory)))))))
        (is (search "outside the foundry root" outside) "a path outside the root refuses"))
      (nodecode-foundry:build "watch ~/Invoices for new PDF files")
      (let ((second (foundry-refusal
                     (lambda ()
                       (nodecode-foundry:build "watch ~/Invoices for new PDF files")))))
        (is (search "already holds a generated cell" second) "a second build refuses rather than overwrite")))))

;;; --- the tools and the command ----------------------------------------------------

(deftest foundry-cell-the-verbs-answer-through-the-root ()
  (with-cell-stop ((foundry-start))
    (with-foundry-root (root)
      (let ((planned (nodecode-foundry::plan-tool
                      (foundry-request-record "request"
                                              "poll https://example.com/feed.json"))))
        (is (search "archetype: sqlite_api_poller" planned) "foundry_plan answers the spec")
        (is (null (uiop:subdirectories (nodecode-foundry:root))) "and writes nothing"))
      (let ((built (nodecode-foundry::build-tool
                    (foundry-request-record "request" "poll https://example.com/feed.json"))))
        (is (search "generated example-poller" built) "foundry_build answers what it wrote"))
      (let ((listed (nodecode-foundry::list-tool
                     (nlk:contract-decode (nlk:contract 'nodecode-foundry::foundry-listing)
                                          (nlk:json-object)))))
        (is (search "example-poller" listed) "foundry_list answers what is under the root"))
      (let ((tested (nodecode-foundry::test-tool
                     (foundry-path-record "nodecode-example-poller/")))
            (catalogued (nodecode-foundry::catalog-tool
                         (foundry-path-record "nodecode-example-poller/"))))
        (is (search "static scan: clean" tested) "foundry_test checks a generated folder again")
        (is (search "**Slug:** example-poller " catalogued) "foundry_catalog reads the draft"))
      (let ((listed (nodecode-foundry:run-command "list" "s1"))
            (built (nodecode-foundry:run-command "build watch ~/Invoices for new PDF files" "s1")))
        (is (search "example-poller" listed) "/foundry list is the operator's line")
        (is (search "generated invoices-digest" built) "/foundry build writes one"))
      (is (search "Unknown /foundry subcommand" (nodecode-foundry:run-command "publish" "s1")) "an unknown verb is refused")
      (is (search "ERROR" (nodecode-foundry:run-command "test nowhere/" "s1")) "a bad path answers a line"))))
