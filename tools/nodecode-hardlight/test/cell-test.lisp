;;;; cell-test.lisp --- the guides, the five tools and one walk, against the seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing is stubbed, because nothing here reaches out: the guides are
;;;; files in the cell's own folder, the checks read text and answer JSON, and
;;;; the walk reads a scratch tree the test writes and deletes. What is proved
;;;; is the original's own behaviour: the topics and exact style IDs, a
;;;; measured contrast ratio with its role threshold, shortest paths in link
;;;; order with the graph's defects named, a direction packet that preserves an
;;;; incumbent, a bounded read-only sweep with its findings and its skips, and
;;;; the refusals of the wire.

(in-package #:nodecode.test)

(define-test-slice "hardlight" "HARDLIGHT-" :start nodecode-hardlight:start-cell)

(define-cell-lifecycle-tests "hardlight"
  (:help :hardlight)
  (:command "hardlight")
  (:running (is (functionp (third (nle::find-tool "hardlight_reference"))) "the reference tool is registered")
            (is (functionp (third (nle::find-tool "hardlight_direction"))) "the direction tool is registered")
            (is (functionp (third (nle::find-tool "hardlight_contrast"))) "the contrast tool is registered")
            (is (functionp (third (nle::find-tool "hardlight_navigation"))) "the navigation tool is registered")
            (is (functionp (third (nle::find-tool "hardlight_inspect"))) "the inspection tool is registered")
            (is (= 120 (nodecode-hardlight::setting :max-files)) "one inspection reads 120 files by default"))
  (:refused ("max_files" 0) ("max_files" "many")))

;;; --- helpers --------------------------------------------------------------------

(defun hardlight-call (name &rest pairs)
  "Call the contract tool NAME as a model's call reaches it: the arguments
decoded through the declaration, then the registered body."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-hardlight ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, or on the section's own defaults."
  `(with-cell-stop ((hardlight-start ,@pairs))
     ,@body))

(defun hardlight-scratch-tree ()
  "A scratch project the walk may read, under this run's own home."
  (let ((root (merge-pathnames (format nil "hardlight-~a/" (random 1000000))
                                (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "node_modules/" root))
    root))

(defun hardlight-write (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text out)))

;;; --- the guides ------------------------------------------------------------------

(deftest hardlight-cell-reference-answers-topics-and-grammars ()
  (with-hardlight ()
    (let ((index (hardlight-call "hardlight_reference" "topic" "index")))
      (is (search "swiss-editorial" index) "the index names every exact style ID")
      (is (search "workflow" index) "and every topic"))
    (let ((style (hardlight-call "hardlight_reference" "topic" "style" "style" "swiss-editorial")))
      (is (search "thesis:" style) "a style renders its thesis")
      (is (search "composition:" style) "and its composition")
      (is (search "sources" style) "and the sources it was derived from"))
    (is (search "unknown style ID"
                (hardlight-call "hardlight_reference" "topic" "style" "style" "not-a-style")) "unknown ID refused")
    (let ((color (hardlight-call "hardlight_reference" "topic" "color")))
      (is (search "# color" color) "a guide is its shipped markdown")
      (is (> (length color) 1000) "whole"))
    (let ((atlas (hardlight-call "hardlight_reference" "topic" "atlas")))
      (is (search "\"styles\"" atlas) "the atlas is the shipped library, as data")
      (is (search "\"swiss-editorial\"" atlas)))))

(deftest hardlight-cell-command-answers-help-styles-and-version ()
  (with-hardlight ()
    (is (search "workflow" (cell-entry "nodecode-hardlight" "hardlight" "" "s1")) "no argument says what it offers")
    (is (search "Hardlight UI 0.2.0" (cell-entry "nodecode-hardlight" "hardlight" "version" "s1")) "and its version")
    (is (search "swiss-editorial" (cell-entry "nodecode-hardlight" "hardlight" "styles" "s1")) "styles lists the atlas")
    (is (search "# motion" (cell-entry "nodecode-hardlight" "hardlight" "motion" "s1")) "TOPIC reads that guide")
    (is (search "Resolve navigation before surface polish" (nle:help :hardlight)) "the manual carries the workflow")))

;;; --- contrast --------------------------------------------------------------------

(deftest hardlight-cell-contrast-measures-each-pair-against-its-role ()
  (with-hardlight ()
    (let ((answer (hardlight-call
                   "hardlight_contrast" "pairs"
                   (vector (nlk:json-object "foreground" "#000000" "background" "#FFFFFF" "role" "text")
                           (nlk:json-object "foreground" "#777777" "background" "#FFFFFF" "role" "text")
                           (nlk:json-object "foreground" "#777777" "background" "#FFFFFF" "role" "ui")))))
      (is (search "\"ok\":true" answer) "three supported pairs")
      (is (search "\"ratio\":\"21.00\"" answer) "black on white is 21:1")
      (is (search "\"status\":\"fail\"" answer) "grey on white fails body text")
      (is (search "\"status\":\"pass\"" answer) "and passes a user-interface edge")
      (is (search "Not a WCAG conformance audit" answer) "the answer says what it is not"))
    (let ((answer (hardlight-call
                   "hardlight_contrast" "pairs"
                   (vector (nlk:json-object "foreground" "#12345" "background" "#FFFFFF" "role" "text")))))
      (is (search "\"ok\":false" answer) "a malformed colour is refused")
      (is (search "pairs[0]" answer) "naming the pair"))
    (signals-error nlk:contract-refusal
      (nlk:contract-decode (second (nle::find-tool "hardlight_contrast"))
                            (nlk:make-json-object)))
    (signals-error nlk:contract-refusal
      (nlk:contract-decode (second (nle::find-tool "hardlight_contrast"))
                            (nlk:make-json-object
                             "pairs" (vector (nlk:json-object "foreground" "#000000"
                                                               "background" "#FFFFFF" "role" "banner")))))))

;;; --- navigation ------------------------------------------------------------------

(deftest hardlight-cell-navigation-finds-paths-and-names-defects ()
  (with-hardlight ()
    (flet ((page (id label &rest links)
             (nlk:json-object "id" id "label" label
                               "links" (coerce (loop for (to text) on links by #'cddr
                                                      collect (nlk:json-object "to" to "label" text))
                                                'vector))))
      (let* ((pages (vector (page "a" "Start" "b" "Next" "c" "Next")
                            (page "b" "Second" "a" "Home" "d" "Detail" "zzz" "Missing")
                            (page "c" "Third" "a" "Home")
                            (page "d" "Detail")
                            (page "e" "Orphan" "a" "Home")))
             (tasks (vector (nlk:json-object "name" "reach third" "start" "a" "goal" "c")
                            (nlk:json-object "name" "reach detail" "start" "a" "goal" "d" "max_steps" 1)))
             (answer (hardlight-call "hardlight_navigation" "pages" pages "tasks" tasks)))
        (is (search "\"ok\":true" answer) "a well-formed graph is analyzed")
        (is (search "\"reachable\":true" answer) "the task path is found")
        (is (search "\"steps\":1" answer) "in one step")
        (is (search "\"over_budget\":true" answer) "a task over its own budget says so")
        (is (search "\"to\":\"zzz\"" answer) "an undeclared destination is named")
        (is (search "\"label\":\"Next\"" answer) "a label that reaches two pages is named")
        (is (search "\"dead_ends\"" answer) "and the dead ends")
        (is (search "\"d\"" answer))
        (is (search "\"unreachable_pages\":[\"e\"]" answer) "an orphan from the entry page is named")
        (is (search "not usability certification" answer) "the answer says what it is not"))
      (let ((answer (hardlight-call "hardlight_navigation" "pages" (vector (page "a" "Start"))
                                    "tasks" (vector (nlk:json-object "name" "nowhere" "start" "a" "goal" "q")))))
        (is (search "\"ok\":false" answer) "a task naming an undeclared page is refused")
        (is (search "must reference declared page IDs" answer) "with the reason"))
      (let ((answer (hardlight-call "hardlight_navigation"
                                    "pages" (vector (page "a" "Start") (page "a" "Again")))))
        (is (search "duplicate page ID" answer) "a duplicate ID is refused"))
      (let ((answer (hardlight-call "hardlight_navigation" "pages" (vector (page "a" "Start")) "home" "b")))
        (is (search "home must name a declared page ID" answer) "an undeclared home is refused")))))

;;; --- direction --------------------------------------------------------------------

(deftest hardlight-cell-direction-builds-a-packet-per-surface ()
  (with-hardlight ()
    (let ((packet (hardlight-call "hardlight_direction" "brief" "A field journal for surveyors"
                                   "surface" "reading")))
      (is (search "brief: A field journal for surveyors" packet) "the brief is carried whole")
      (is (search "swiss-editorial" packet) "reading offers the editorial grammar")
      (is (search "preserve incumbent: no" packet) "greenfield preserves nothing")
      (is (search "missing context:" packet) "the gaps are named")
      (is (search "verification matrix (nothing is run yet)" packet) "and nothing is claimed as verified"))
    (let ((packet (hardlight-call "hardlight_direction" "brief" "Refresh the shop"
                                   "authority" "refinement" "styles" (vector "quiet-craft")
                                   "context" (nlk:json-object
                                              "audience" "people buying tools once"
                                              "identity" "keep the existing tokens"))))
      (is (search "preserve incumbent: yes" packet) "a refinement preserves the incumbent")
      (is (search "quiet-craft" packet) "an explicit grammar is used")
      (is (search "  audience: people buying tools once" packet) "supplied facts are echoed")
      (is (search "structure: Preserve the incumbent composition" packet) "and structure is scoped to it")
      (is (search "tasks: List concrete jobs" packet) "a fact left out is a named gap")
      (is (not (search "identity: Record incumbent tokens" packet)) "a supplied field is not a gap"))
    (is (search "unknown style ID"
                (hardlight-call "hardlight_direction" "brief" "x" "styles" (vector "not-a-style"))) "an unknown grammar is refused")
    (signals-error nlk:contract-refusal
      (nlk:contract-decode (second (nle::find-tool "hardlight_direction"))
                            (nlk:make-json-object "surface" "product")))))

;;; --- inspection --------------------------------------------------------------------

(deftest hardlight-cell-inspect-reads-a-bounded-tree ()
  (with-hardlight ()
    (let ((root (hardlight-scratch-tree)))
      (unwind-protect
           (progn
             (hardlight-write (merge-pathnames "index.html" root)
                              "<html><body><img src=\"a.png\"><a href=\"#\">here</a><img src=\"b.png\" alt=\"ok\"></body></html>")
             (hardlight-write (merge-pathnames "site.css" root)
                              ":root { --ink: #111111; --paper: #ffffff; }\n.x { outline: none; }\n")
             (hardlight-write (merge-pathnames "node_modules/vendor.css" root) ".y { outline: none; }\n")
             (hardlight-write (merge-pathnames "private-key.css" root) ".z { --secret: #000000; }\n")
             (let ((answer (hardlight-call "hardlight_inspect"
                                            "root" (sb-ext:native-namestring root))))
               (is (search "\"ok\":true" answer) "the tree is read")
               (is (search "\"rule\":\"image-alt\"" answer) "an image with no alt is a finding")
               (is (search "\"rule\":\"placeholder-link\"" answer) "a placeholder link is a finding")
               (is (search "\"rule\":\"focus-outline\"" answer) "a removed outline is a finding")
               (is (search "\"name\":\"--ink\"" answer) "a custom property is reported")
               (is (search "\"value\":\"#111111\"" answer) "with its value")
               (is (search "\"path\":\"node_modules\"" answer) "a vendor folder is skipped")
               (is (search "private-key.css" answer) "a secret-looking name is skipped before any read")
               (is (search "\"files_read\":2" answer) "only the two frontend files were read")
               (is (search "WCAG conformance is established" answer) "the answer says what it is not"))
             (let ((answer (hardlight-call "hardlight_inspect"
                                            "root" (sb-ext:native-namestring root) "max_files" 1)))
               (is (search "\"files_read\":1" answer) "the file budget truncates rather than fails")
               (is (search "\"budget_hit\":true" answer) "and says it truncated"))
             (let ((answer (hardlight-call "hardlight_inspect" "root" "/nonexistent-hardlight-root")))
               (is (search "\"ok\":false" answer) "a root that is not there is refused")))
        (ignore-errors (uiop:delete-directory-tree root :validate t))))
    (let ((answer (hardlight-call "hardlight_inspect")))
      (is (search "\"ok\":false" answer) "with no root and no section root, inspection refuses"))))
