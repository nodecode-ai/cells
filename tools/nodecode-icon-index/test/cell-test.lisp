;;;; cell-test.lisp --- the icon-index cell, its catalog and a stubbed CDN.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every fetch is a stubbed NLK:HTTP and every folder a scratch one, so
;;;; nothing reaches the network or the operator's files. Each tool is called
;;;; the way a model's call reaches it - through the real dispatch, contract
;;;; and all - so the schema the provider is shown and the body that runs are
;;;; one shape.

(in-package #:nodecode.test)

(define-test-slice "icon-index" "ICON-INDEX-CELL-"
  :start nodecode-icon-index:start-cell
  :defaults (("asset_base" "https://cdn.test/3d-icons@aee7c0ab")))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-icons-directory ((directory) &body body)
  "BODY with DIRECTORY a fresh empty folder, removed afterwards."
  `(let ((,directory (merge-pathnames
                      (format nil "icon-index-test-~d-~d/" (get-universal-time) (random 1000000))
                      (uiop:temporary-directory))))
     (ensure-directories-exist (merge-pathnames "keep" ,directory))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,directory :validate t)))))

(defmacro with-cdn ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values BODY
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL OPTIONS) once BODY
reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url options) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected fetch of ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun answer-of (call tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it."
  (nle::message-content (execute-wire-call call tool arguments)))

(defun object-of (call tool arguments)
  (nlk:decode-json (answer-of call tool arguments)))

(defparameter +glb+ (make-array 12 :element-type '(unsigned-byte 8) :initial-element 103)
  "Twelve bytes standing in for a model file: the cell writes what it is given.")

(defun answer-png-file (url)
  (if (search "rocket/icon.png" url)
      (values +glb+ 200)
      (error "unexpected file ~a" url)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "icon-index"
  (:config "asset_base" "https://cdn.test/3d-icons@aee7c0ab")
  (:help :icon-index)
  (:command "icon-index")
  (:refused ("asset_base" "ftp://cdn.test/3d-icons")
             ("asset_base" "http://cdn.test/3d-icons")
             ("timeout" 0)))

(deftest icon-index-cell-refuses-an-unusable-asset-base ()
  ;; The one setting the files are read from: empty installs nothing, and
  ;; plain HTTP is refused because the library's own files are https.
  (let ((condition (signals-error nlk:config-refusal
                                  (icon-index-start "asset_base" "ftp://cdn.test/3d-icons"))))
    (is (search "https" (princ-to-string condition)) "a base that is not https is refused")
    (is (null (nle::find-tool "icon_search")) "and installs nothing"))
  (let ((condition (signals-error nlk:config-refusal
                                  (icon-index-start "asset_base" "http://cdn.test/3d-icons"))))
    (is (search "https" (princ-to-string condition)) "a plain-HTTP base is refused too")))

;;; --- icon_search --------------------------------------------------------------------

(deftest icon-index-cell-searches-without-a-network ()
  (with-cell-stop ((icon-index-start))
    (with-cdn (asked)
      (let* ((object (object-of "s1" "icon_search" "{\"query\":\"launch\"}"))
             (first-icon (aref (nlk:json-array object "icons") 0)))
        (is (plusp (nlk:json-value object :integer "count")) "a launch icon is found")
        (is (equal "rocket" (nlk:json-value first-icon :string "id")) "the rocket, which means launches")
        (is (equal "https://cdn.test/3d-icons@aee7c0ab/icons/rocket/icon.png" (nlk:json-value first-icon :string "png")) "with its preview PNG on the asset base"))
      (is (null asked) "and nothing was fetched: search reads the bundled catalog"))))

(deftest icon-index-cell-filters-by-category ()
  (with-cell-stop ((icon-index-start))
    (let* ((object (object-of "s2" "icon_search" "{\"category\":\"Money\",\"limit\":40}"))
           (icons (nlk:json-array object "icons")))
      (is (plusp (length icons)) "the money icons are listed")
      (is (every (lambda (icon) (equal "Money" (nlk:json-value icon :string "category"))) icons) "and every one of them is money"))
    (is (= 1 (nlk:json-value (object-of "s3" "icon_search" "{\"category\":\"Tech\",\"limit\":1}") :integer "count")) "a limit caps the answer")
    (is (search "is no category" (answer-of "s4" "icon_search" "{\"category\":\"Nope\"}")) "an unknown category is refused with the ones that exist")))

;;; --- icon_get -----------------------------------------------------------------------

(deftest icon-index-cell-answers-one-icon ()
  (with-cell-stop ((icon-index-start))
    (let* ((object (object-of "g1" "icon_get" "{\"id\":\"rocket\"}"))
           (files (gethash "files" object))
           (embed (gethash "embed" object)))
      (is (equal "Rocket" (nlk:json-value object :string "title")))
      (is (= 4 (hash-table-count files)) "orange has every format")
      (is (equal "https://cdn.test/3d-icons@aee7c0ab/icons/rocket/model.glb" (nlk:json-value (gethash "glb" files) :string "url")))
      (is (search "#id=rocket" (nlk:json-value embed :string "iframe")) "the iframe points at the viewer")
      (is (search "model-viewer" (nlk:json-value embed :string "html")))
      (is (search "MeshoptDecoder" (nlk:json-value embed :string "threejs")) "the three.js snippet decodes the compressed model")
      (is (search "rocket/icon.png" (nlk:json-value embed :string "markdown")) "and the markdown field shows the PNG")
      (is (search "CC0" (nlk:json-value (gethash "license" object) :string "name")) "the icons' licence rides along")))
  (with-cell-stop ((icon-index-start))
    (let* ((object (object-of "g2" "icon_get" "{\"id\":\"rocket\",\"color\":\"blue\"}")))
      (is (equal "blue" (nlk:json-value (gethash "color" object) :string "name")))
      (is (= 2 (hash-table-count (gethash "files" object))) "a non-orange colour has the GLB and the PNG")
      (is (search "colors/blue/model.glb" (nlk:json-value (gethash "glb" (gethash "files" object)) :string "url")) "and its files come from the colour's folder"))
    (let* ((object (object-of "g3" "icon_get" "{\"id\":\"rocket\",\"color\":\"#0ea5e9\"}"))
           (embed (gethash "embed" object)))
      (is (search "color=0ea5e9" (nlk:json-value embed :string "iframe")) "a custom hex rides the viewer")
      (is (stringp (nlk:json-value embed :string "note")) "and the answer says why there are no files"))))

(deftest icon-index-cell-suggests-an-icon-for-a-typo ()
  (with-cell-stop ((icon-index-start))
    (let ((text (answer-of "g4" "icon_get" "{\"id\":\"rockt\"}")))
      (is (search "no icon with id" text))
      (is (search "rocket" text) "the near spelling is suggested"))))

;;; --- icon_save ----------------------------------------------------------------------

(deftest icon-index-cell-saves-files-and-never-overwrites ()
  (with-icons-directory (directory)
    (with-cell-stop ((icon-index-start))
      (flet ((save (call)
               (object-of call "icon_save"
                          (format nil "{\"id\":\"rocket\",\"folder\":\"~a\",\"formats\":[\"png\"]}"
                                  (namestring directory)))))
        (with-cdn (asked ("/icons/rocket/" (answer-png-file url)))
          (let ((row (aref (nlk:json-array (save "v1") "files") 0)))
            (is (equal 12 (nlk:json-value row :integer "bytes")) "the file was written")
            (is (probe-file (nlk:json-value row :string "path")) "and it is there"))
          (is (eq :get (first (first asked))) "one GET for the one format"))
        (with-cdn (asked)
          (let ((row (aref (nlk:json-array (save "v2") "files") 0)))
            (is (search "already exists" (nlk:json-value row :string "skipped")) "a second save says it left the file alone"))
          (is (null asked) "and fetches nothing"))))))

(deftest icon-index-cell-refuses-what-it-cannot-save ()
  (with-icons-directory (directory)
    (with-cell-stop ((icon-index-start))
      (flet ((save (call arguments)
               (answer-of call "icon_save"
                          (format nil "{\"id\":\"rocket\",\"folder\":\"~a\"~a}"
                                  (namestring directory) arguments))))
        (with-cdn (asked)
          (is (search "is no format" (answer-of "v3" "icon_save" (format nil "{\"id\":\"rocket\",\"folder\":\"~a\",\"formats\":[\"obj\"]}" (namestring directory)))) "an unknown format is refused")
          (is (search "preset colours only" (save "v4" ",\"color\":\"#0ea5e9\"")) "a custom hex has no files")
          (is (search "only these formats" (save "v5" ",\"color\":\"teal\",\"formats\":[\"mp4\"]")) "and a teal icon has no turntable")
          (is (null asked) "none of them fetched anything"))))))

;;; --- the command and the eval verbs --------------------------------------------------

(deftest icon-index-cell-answers-the-command ()
  (with-cell-stop ((icon-index-start))
    (let ((text (nodecode-icon-index:icon-index-command "security" "s1")))
      (is (search "icon index" text))
      (is (search "rocket" text) "the icons that mean security are listed"))
    (is (search "all icons" (nodecode-icon-index:icon-index-command "" "s1")) "typed alone it lists the library")
    (is (search "no icon matches" (nodecode-icon-index:icon-index-command "zzzz" "s1")) "a word nothing answers says so")))

(deftest icon-index-cell-answers-eval-verbs ()
  (with-cell-stop ((icon-index-start))
    (is (search "\"rocket\"" (nodecode-icon-index:icon-search "launch")))
    (is (search "rocket/icon.png" (nodecode-icon-index:icon-get "rocket"))))
  (is (search "not running" (nodecode-icon-index:icon-search "launch")) "with nothing started the verb says so"))
