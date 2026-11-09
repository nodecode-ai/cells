;;;; cell-test.lisp --- the lifecycle, the two tools, the verdicts, the refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and the original's answers:
;;;; the section (a limit of zero refuses the start), both tools through the
;;;; real dispatch seam, the lookup's own scale and the markup it hands back,
;;;; every check the validator fails a page for and every warning it only
;;;; reads, a page read off disk, and a folder whose census snapshot is gone
;;;; refusing to start instead of answering nothing.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "aurora"
  (:help :aurora)
  (:command "aurora")
  (:running
   (is (nle::find-tool "aurora_search") "aurora_search is registered")
   (is (nle::find-tool "aurora_validate") "aurora_validate is registered")
   (is (plusp (getf (nodecode-aurora:ensure-census) :families)) "the census is indexed at start"))
  (:stopped
   (is (null (nle::find-tool "aurora_search")) "stop unregisters both tools")
   (is (null (nle::find-tool "aurora_validate"))))
  (:refused
   ("limit" 0)
   ("limit" "many")))

;;; --- the lookup -----------------------------------------------------------------

(deftest aurora-cell-the-lookup-finds-the-object ()
  (with-cell-stop ((aurora-start))
    (let ((text (aurora:lookup "tabla que se apila en movil")))
      (is (cl:search "nz-table" text) "the table family, in Spanish and in English")
      (is (cl:search "packs/p8-data.css" text) "the pack its classes live in")
      (is (cl:search "markup: <table" text) "and the real markup from the catalogue")
      (is (cl:search "channel v10" text) "the channel the snapshot names"))))

(deftest aurora-cell-a-query-naming-nothing-says-so ()
  (with-cell-stop ((aurora-start))
    (is (cl:search "ERROR:" (aurora:lookup "  ")) "an empty need is refused")
    (is (cl:search "Nothing in the Aurora census matched" (aurora:lookup "que de la")) "a query the census knows nothing about says so")))

(deftest aurora-cell-a-lookup-answers-through-the-wire ()
  (with-cell-stop ((aurora-start))
    (let ((text (aurora-call-tool "aurora_search"
                                   "{\"need\":\"alerta de error\",\"limit\":2,\"with_markup\":false}")))
      (is (cl:search "nz-alert" text) "the alert family")
      (is (null (cl:search "markup:" text)) "a call that asks for no markup gets none"))
    (let ((text (aurora-call-tool "aurora_search" "{\"need\":\"kpi\"}")))
      (is (cl:search "markup:" text) "markup rides by default, as it did in the original")
      (is (cl:search "nz-kpi" text)))))

(deftest aurora-cell-the-section-sets-how-many-families ()
  (with-cell-stop ((aurora-start "limit" 2))
    (let ((text (aurora:lookup "panel tabla chat")))
      (is (<= (count #\[ text) 2) "a lookup that names no limit answers with the section's two"))))

;;; --- the check ------------------------------------------------------------------

(deftest aurora-cell-a-clean-page-passes ()
  (let ((page (aurora-page)))
    (multiple-value-bind (ok failures warnings measured)
        (aurora:check page :footer (nodecode-aurora::default-footer))
      (is ok "no failures")
      (is (null failures))
      (is (null warnings) "and nothing for a person to read")
      (is (= 3 (getf measured :nz-classes)) "three nz classes")
      (is (equal '("p1-layout.css" "p3-typography.css") (getf measured :packs)) "the packs it measured"))))

(deftest aurora-cell-every-rule-broken-is-named ()
  (let* ((page (aurora-page
                :classes (format nil "<article class=\"nz-article nz-article--flush nz-card\">~
<p class=\"nz-text--muted\">Hola ~a</p></article>"
                                  (aurora-emoji))
                :packs '("packs/p1-layout.css")
                :style "h1{color:#2563eb;background:linear-gradient(red,blue);backdrop-filter:blur(2px);margin:0 !important}"
                :heads "<h1>Uno</h1><h1>Dos</h1>"
                :footer nil))
         (findings nil))
    (multiple-value-bind (ok failures warnings)
        (aurora:check page :footer (nodecode-aurora::default-footer))
      (declare (ignore warnings))
      (setf findings failures)
      (is (not ok) "a page that breaks the rules does not pass"))
    (flet ((names (needle) (some (lambda (finding) (cl:search needle finding)) findings)))
      (is (names "nz-card") "the invented class")
      (is (names "packs/p3-typography.css") "a pack a used class lives in, not linked")
      (is (names "gradient") "the gradient")
      (is (names "backdrop-filter") "the backdrop-filter")
      (is (names "!important") "the !important")
      (is (names "hand-written colour") "the colour written by hand")
      (is (names "emoji in the UI") "the emoji")
      (is (names "missing the exact footer") "the missing attribution")
      (is (names "more than one <h1>") "the second heading"))))

(deftest aurora-cell-the-warnings-are-read-not-blocking ()
  (let ((page (aurora-page :lang nil :theme nil :body-class nil
                            :style "@media (max-width: 500px){h1{letter-spacing:0}}"))
        (nodecode-aurora::+footer+ (nodecode-aurora::default-footer)))
    (multiple-value-bind (ok failures warnings)
        (aurora:check page :footer nodecode-aurora::+footer+)
      (is ok "warnings alone do not fail a page")
      (is (null failures))
      (is (= 4 (length warnings)) "the four the original warned about")
      (is (some (lambda (warning) (cl:search "missing lang" warning)) warnings))
      (is (some (lambda (warning) (cl:search "data-nz-theme" warning)) warnings))
      (is (some (lambda (warning) (cl:search "class=\"nz\"" warning)) warnings))
      (is (some (lambda (warning) (cl:search "mobile-first" warning)) warnings)))))

(deftest aurora-cell-the-attribution-can-be-turned-off ()
  (let ((page (aurora-page :footer nil)))
    (multiple-value-bind (ok) (aurora:check page :footer (nodecode-aurora::default-footer)
                                                  :require-footer nil)
      (is ok "a page without the footer passes when the section says the footer is optional"))))

;;; --- the page a model hands over ---------------------------------------------------

(deftest aurora-cell-validate-reads-a-file ()
  (let ((path (merge-pathnames (format nil "aurora-~d.html" (random 1000000))
                                (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (with-open-file (out path :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
             (write-string (aurora-page) out))
           (with-cell-stop ((aurora-start))
             (let ((text (aurora-call-tool "aurora_validate"
                                            (format nil "{\"path\":~s}" (namestring path)))))
               (is (cl:search "Aurora check: no failures" text) "the file was read, and it passes"))
             (let ((text (aurora-call-tool "aurora_validate" "{\"path\":\"/nonexistent/aurora.html\"}")))
               (is (cl:search "no such file" text) "a path that is not there says so"))
             (let ((text (aurora-call-tool "aurora_validate" "{}")))
               (is (cl:search "ERROR:" text) "a call naming no page at all says so"))))
      (when (probe-file path) (delete-file path)))))

(defparameter +aurora-inline-page+
  (format nil "<html lang=\"es\"><body class=\"nz\"><h1>Hola</h1><p class=\"nz-prose\">Texto</p>~
<footer>~a</footer></body></html>"
          (nodecode-aurora::default-footer))
  "A page small enough to ride a call, with no style block and one pack to link.")

(deftest aurora-cell-validate-takes-the-html-itself ()
  (with-cell-stop ((aurora-start))
    (let ((text (aurora-call-tool "aurora_validate"
                                   (format nil "{\"html\":~s}" +aurora-inline-page+))))
      (is (cl:search "Aurora check: FAILED" text) "a page that links no pack fails")
      (is (cl:search "packs/p3-typography.css" text) "and the failure names the pack it misses"))
    (let ((text (aurora-call-tool "aurora_validate"
                                   (format nil "{\"html\":~s}"
                                           (format nil "~a<link rel=\"stylesheet\" href=\"~a\">"
                                                   +aurora-inline-page+
                                                   (aurora-pack-link "packs/p3-typography.css"))))))
      (is (cl:search "Aurora check: no failures" text) "link the pack and the same page passes"))))

;;; --- the census the cell ships ------------------------------------------------------

(deftest aurora-cell-without-its-census-refuses ()
  (with-stubbed-fdefinition (nodecode-aurora::censo-path ()
                              (pathname "/nonexistent/aurora/censo.json"))
    (let ((nodecode-aurora::*meta* nil)
          (nodecode-aurora::*families* nil)
          (nodecode-aurora::*known* nil))
      (let ((condition (signals-error nlk:config-refusal (aurora-start))))
        (is (cl:search "census snapshot" (princ-to-string condition)) "the refusal names the file")
        (is (null (nle::find-tool "aurora_search")) "and installs nothing"))))
  (is (plusp (getf (aurora:ensure-census) :families)) "the shipped snapshot indexes again"))
