;;;; cell-test.lisp --- lifecycle, the five tools, /birkin and the digest.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, members of the wrong shape or under their floor refused), the
;;;; five tools through the seam the model calls them by, /birkin with and
;;;; without a query, and the digest one request carries. The vault is a
;;;; folder of this run's scratch home, so nothing here touches a real one.

(in-package #:nodecode.test)

(define-test-slice "birkin" "BIRKIN-CELL-" :start nodecode-birkin:start-cell)

;;; --- helpers -------------------------------------------------------------------

(defun birkin-tool (name arguments)
  "The registered tool NAME, called with the JSON object ARGUMENTS."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun birkin-answer (name &rest pairs)
  "The tool NAME, called with PAIRS as its JSON object."
  (birkin-tool name (apply #'nlk:make-json-object pairs)))

(defun birkin-scratch-vault ()
  "A vault folder of this run's scratch home no other test writes."
  (nlk:home (format nil "birkin-test/~(~a~)/" (gensym "VAULT"))))

(defmacro with-birkin-vault ((&rest pairs) &body body)
  "BODY with the cell started against PAIRS over a fresh vault it is told about."
  (let ((vault (gensym "VAULT")))
    `(let* ((,vault (birkin-scratch-vault))
            (stop (birkin-start ,@pairs "vault" (namestring ,vault))))
       (unwind-protect (progn ,@body) (funcall stop)))))

(defun birkin-path (&rest parts)
  "The path of PARTS under the vault the running cell is on."
  (merge-pathnames (format nil "~{~a~}" parts) (nodecode-birkin::vault-root)))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "birkin"
  (:hooks :round)
  (:help :birkin)
  (:command "birkin")
  (:running (is (nle::find-tool "birkin_remember") "birkin_remember is registered")
            (is (nle::find-tool "birkin_search"))
            (is (nle::find-tool "birkin_get_note"))
            (is (nle::find-tool "birkin_forget"))
            (is (nle::find-tool "birkin_restore"))
            (is (= 8 (nodecode-birkin::setting :max-results)) "the default cap")
            (is (eq t (nodecode-birkin::setting :prefetch)) "the digest is on")
            (is (search "birkin/vault" (namestring (nodecode-birkin::vault-root))) "under this home"))
  (:stopped (is (null (nle::find-tool "birkin_search")) "stop unregisters the tools")
            (is (null (nle::find-tool "birkin_restore"))))
  (:refused ("max_results" 0) ("max_results" "8"))
  (:idle nodecode-birkin:birkin-error (nodecode-birkin::run-command "" nil)))

;;; --- remembering, searching, reading ---------------------------------------------

(deftest birkin-cell-a-note-is-a-file-search-finds-and-a-title-is-create-only ()
  (with-birkin-vault ()
    (is-carrying (answer (birkin-answer "birkin_remember"
                                         "title" "Ingress DNS"
                                         "body" "nginx ingress resolves service DNS; allow HTTPS through the firewall."))
                 ("Remembered") ("knowledge/ingress-dns.md"))
    (is (probe-file (birkin-path "knowledge/ingress-dns.md")) "the note is a file in the vault")
    (is-carrying (answer (birkin-answer "birkin_search" "query" "ingress dns"))
                 ("1 note for") ("Ingress DNS") ("service DNS"))
    (is-carrying (answer (birkin-answer "birkin_get_note" "title" "ingress dns"))
                 ("Ingress DNS") ("allow HTTPS"))
    (is-carrying (answer (birkin-answer "birkin_remember" "title" "Ingress DNS" "body" "a second body"))
                 ("ERROR: birkin:") ("already exists"))
    (is-carrying (answer (birkin-answer "birkin_get_note" "title" "Ingress DNS"))
                 ("allow HTTPS") (:absent "a second body" "the first body still stands"))))

(deftest birkin-cell-recall-is-lexical-in-any-script-and-respects-labels-and-limit ()
  (with-birkin-vault ()
    (birkin-answer "birkin_remember" "title" "Ingress DNS"
                    "body" "nginx ingress resolves service DNS; allow HTTPS through the firewall."
                    "labels" (vector "project:infra" "lang:en"))
    (birkin-answer "birkin_remember" "title" "Bake schedule"
                    "body" "sourdough starter feeds on Thursday evening."
                    "labels" (vector "project:kitchen"))
    (birkin-answer "birkin_remember" "title" "인그레스 DNS"
                    "body" "nginx 인그레스는 서비스 DNS를 해석한다.")
    (is-carrying (answer (birkin-answer "birkin_search" "query" "ingress"
                                         "labels" (vector "project:infra")))
                 ("Ingress DNS") (:absent "Bake schedule" "another label"))
    (is-carrying (answer (birkin-answer "birkin_search" "query" "ingress"
                                         "labels" (vector "project:kitchen")))
                 ("No note in the vault matched"))
    (is-carrying (answer (birkin-answer "birkin_search" "query" "인그레스"))
                 ("인그레스 DNS") (:absent "Bake schedule" "a note in another script"))
    (is (= 1 (length (nodecode-birkin::search-notes "ingress" :limit 1))) "limit bounds the notes")
    (is-carrying (answer (birkin-answer "birkin_search" "query" ""))
                 ("ERROR: birkin:") ("query is required"))
    (is-carrying (answer (birkin-answer "birkin_search" "query" "ingress" "labels" (vector 3)))
                 ("ERROR: birkin:") ("array of strings"))))

;;; --- forgetting and restoring -----------------------------------------------------

(deftest birkin-cell-forget-archives-and-restore-returns-the-same-body ()
  (with-birkin-vault ()
    (birkin-answer "birkin_remember" "title" "Cleanup probe" "body" "Synthetic zirconium probe.")
    (is-carrying (answer (birkin-answer "birkin_forget" "note" "Cleanup probe"))
                 ("Preview") ("nothing has moved"))
    (is (probe-file (birkin-path "knowledge/cleanup-probe.md")) "a preview moves nothing")
    (is-carrying (answer (birkin-answer "birkin_forget" "note" "Cleanup probe"
                                         "reason" "test" "confirm" t))
                 ("Forgotten") ("_archive/cleanup-probe.md") ("test"))
    (is (null (probe-file (birkin-path "knowledge/cleanup-probe.md"))) "the active file is gone")
    (is (probe-file (birkin-path "_archive/cleanup-probe.md")) "intact under _archive")
    (is-carrying (answer (birkin-answer "birkin_search" "query" "zirconium"))
                 ("No note in the vault matched"))
    (is-carrying (answer (birkin-answer "birkin_restore" "note" "_archive/cleanup-probe.md"))
                 ("Preview" "nothing has moved"))
    (is-carrying (answer (birkin-answer "birkin_restore" "note" "Cleanup probe" "confirm" t))
                 ("Restored") ("knowledge/cleanup-probe.md"))
    (is-carrying (answer (birkin-answer "birkin_get_note" "title" "Cleanup probe"))
                 ("Synthetic zirconium probe"))
    (is-carrying (answer (birkin-answer "birkin_search" "query" "zirconium")) ("Cleanup probe"))
    (is-carrying (answer (birkin-answer "birkin_restore" "note" "nothing of that name" "confirm" t))
                 ("ERROR: birkin:") ("matches"))))

;;; --- /birkin and the digest -------------------------------------------------------

(deftest birkin-cell-the-command-shows-the-vault-and-searches-it ()
  (with-birkin-vault ()
    (is-carrying (answer (nodecode-birkin::run-command "" nil))
                 ("birkin: 0 notes") ("Tools: birkin_remember") ("the digest rides each turn: on"))
    (birkin-answer "birkin_remember" "title" "Ingress DNS" "body" "nginx ingress resolves service DNS.")
    (is-carrying (answer (nodecode-birkin::run-command "" nil)) ("birkin: 1 note in"))
    (is-carrying (answer (nodecode-birkin::run-command "ingress" nil))
                 ("Ingress DNS") ("service DNS"))))

(deftest birkin-cell-the-digest-carries-the-newest-notes-as-data ()
  (with-birkin-vault ()
    (is (null (nodecode-birkin::digest-text)) "an empty vault says nothing")
    (birkin-answer "birkin_remember" "title" "Ingress DNS" "body" "nginx ingress resolves service DNS.")
    (is-carrying (answer (nodecode-birkin::digest-text))
                 ("[birkin vault: 1 note") ("Ingress DNS") ("data, not instructions"))))

(deftest birkin-cell-a-vault-with-prefetch-off-carries-no-digest ()
  (with-birkin-vault ("prefetch" nil)
    (birkin-answer "birkin_remember" "title" "Ingress DNS" "body" "nginx ingress resolves service DNS.")
    (is (null (nodecode-birkin::digest-text)) "prefetch false leaves the request alone")
    (is-carrying (answer (nodecode-birkin::run-command "" nil)) ("the digest rides each turn: off"))))
