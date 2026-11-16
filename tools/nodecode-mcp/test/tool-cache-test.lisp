;;;; tool-cache-test.lisp --- a kept catalog stands a server idle until its first call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The first start connects the fixture and keeps its catalog in the log
;;;; directory; the next start publishes that catalog with no process, and
;;;; the first call connects. Anything that is not the same server reached
;;;; the same way -- another command line, a torn file, `lazy': false --
;;;; connects at boot as before.

(in-package #:nodecode.test)

(defun mcp-keep-catalog (config-forms)
  "Start the cell over CONFIG-FORMS' servers, wait until each is ready (its
catalog kept), stop it."
  (let ((stop (mcp:start-cell (apply #'mcp-config config-forms))))
    (loop for (name) on config-forms by #'cddr
          do (is (eq :ready (mcp-wait-ready name)) "each server connects the first time"))
    (funcall stop)))

(deftest mcp-cell-tool-cache-stands-a-kept-server-idle (with-mcp-log-dir)
  (mcp-keep-catalog (list "probe" (mcp-fixture-entry)))
  (is (probe-file (mcp::tool-cache-path "probe")) "its connect keeps the catalog")
  (let ((stop (mcp:start-cell (mcp-config "probe" (mcp-fixture-entry)))))
    (unwind-protect
         (let ((server (mcp-server "probe")))
           (is (eq :idle (mcp::server-state server)) "the next start stands it idle at once")
           (is (null (mcp::server-client server)) "with no process and no connection")
           (is (= 10 (getf (mcp::server-snapshot server) :tool-count)) "on the kept catalog")
           (is (fboundp (find-symbol "PROBE/ECHO" :nodecode-mcp)) "its functions are defined")
           (is (search "echo" (mcp::primer-text)) "and the manual lists them")
           (is (search "probe idle 10 tools" (mcp::do-status)) "/mcp says idle, with the tools")
           (let ((answer (funcall (find-symbol "PROBE/ECHO" :nodecode-mcp) :message "x" :per-page 5)))
             (is (search "echo: x" answer) "the first call connects and answers")
             (is (search "\"per_page\": 5" answer) "its arguments resolved against the kept schema"))
           (is (eq :ready (mcp::server-state server)) "the server is ready from then on")
           (is (mcp-probe-process) "with its process"))
      (funcall stop))))

(deftest mcp-cell-tool-cache-other-way-of-reaching-connects (with-mcp-log-dir)
  (mcp-keep-catalog (list "probe" (mcp-fixture-entry)))
  ;; Another command line is another server: its kept catalog is no catalog.
  (let ((stop (mcp:start-cell (mcp-config "probe" (mcp-fixture-entry "--pages")))))
    (unwind-protect
         (progn
           (is (not (eq :idle (mcp::server-state (mcp-server "probe")))) "another args list connects at boot")
           (is (eq :ready (mcp-wait-ready "probe"))))
      (funcall stop))))

(deftest mcp-cell-tool-cache-lazy-false-connects-at-boot (with-mcp-log-dir)
  (mcp-keep-catalog (list "probe" (mcp-fixture-entry)))
  (let ((entry (mcp-fixture-entry)))
    (setf (gethash "lazy" entry) nil)
    (let ((stop (mcp:start-cell (mcp-config "probe" entry))))
      (unwind-protect
           (progn
             (is (not (eq :idle (mcp::server-state (mcp-server "probe")))) "an entry's lazy: false connects")
             (is (eq :ready (mcp-wait-ready "probe"))))
        (funcall stop))))
  (let ((config (mcp-config "probe" (mcp-fixture-entry))))
    (setf (gethash "lazy" (gethash "mcp" config)) nil)
    (let ((stop (mcp:start-cell config)))
      (unwind-protect
           (progn
             (is (not (eq :idle (mcp::server-state (mcp-server "probe")))) "so does the section's")
             (is (eq :ready (mcp-wait-ready "probe"))))
        (funcall stop)))))

(deftest mcp-cell-tool-cache-torn-file-connects (with-mcp-log-dir)
  (mcp-keep-catalog (list "probe" (mcp-fixture-entry)))
  (with-open-file (out (mcp::tool-cache-path "probe") :direction :output :if-exists :supersede)
    (write-string "{\"version\": 1, \"reached" out))
  (let ((stop (handler-bind ((warning #'muffle-warning))
                (mcp:start-cell (mcp-config "probe" (mcp-fixture-entry))))))
    (unwind-protect
         (progn
           (is (not (eq :idle (mcp::server-state (mcp-server "probe")))) "a torn catalog is none")
           (is (eq :ready (mcp-wait-ready "probe")))
           (is (= 10 (length (mcp::saved-tools (mcp::server-spec (mcp-server "probe")))))
               "and the connect keeps a whole one again"))
      (funcall stop))))

(deftest mcp-cell-tool-cache-holds-no-secret (with-mcp-log-dir)
  (let ((entry (mcp-fixture-entry)))
    (setf (gethash "env" entry) (nlk:json-object "FIXTURE_TOKEN" "s3cret-value"))
    (mcp-keep-catalog (list "probe" entry)))
  (is (not (search "s3cret-value" (uiop:read-file-string (mcp::tool-cache-path "probe"))))
      "the kept file names how a server is reached, never an env value"))
