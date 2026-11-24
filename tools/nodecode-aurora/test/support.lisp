;;;; support.lisp --- the test slice and the page fixtures.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under an
;;;; AURORA-CELL- name prefix; RUN-AURORA-TESTS runs exactly that slice.
;;;;
;;;; Nothing here reaches outside the folder: the census is the one the cell
;;;; ships, a page is a string a test wrote, and a file check writes into the
;;;; test's own scratch directory. No seam needs stubbing because the tools
;;;; have none -- they read a bundled file and a string.

(in-package #:nodecode.test)

(define-test-slice "aurora" "AURORA-CELL-" :start nodecode-aurora:start-cell)

;;; --- calling a tool through the real dispatch seam --------------------------------

(defun aurora-call-tool (name arguments)
  "NAME called with the JSON text ARGUMENTS, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- the pages a test measures ----------------------------------------------------

(defun aurora-tokens-link ()
  "The tokens link every Aurora page starts with."
  "https://cdn.jsdelivr.net/gh/Ntizar/Aurora@v10/tokens.css")

(defun aurora-pack-link (pack)
  "A link to PACK on the channel a page pins."
  (format nil "https://cdn.jsdelivr.net/gh/Ntizar/Aurora@v10/~a" pack))

(defun aurora-page (&key (classes "<article class=\"nz-article nz-article--flush\"><p class=\"nz-text--muted\">Hola</p></article>")
                          (packs '("packs/p1-layout.css" "packs/p3-typography.css"))
                          (style "")
                          (heads "<h1>Panel</h1>")
                          (footer t)
                          (lang t)
                          (theme t)
                          (body-class t))
  "An Aurora page, clean unless a keyword changes it."
  (format nil "<!DOCTYPE html>~%<html~:[~; lang=\"es\"~]~:[~; data-nz-theme=\"light\"~]>~%<head>~%~
  <link rel=\"stylesheet\" href=\"~a\">~%~{  <link rel=\"stylesheet\" href=\"~a\">~%~}~
  <style>~a</style>~%</head>~%<body~:[~; class=\"nz\"~]>~%~a~%~a~%~a~%</body>~%</html>~%"
          lang theme
          (aurora-tokens-link)
          (mapcar #'aurora-pack-link packs)
          style body-class heads classes
          (if footer (format nil "<footer>~a</footer>" (nodecode-aurora::default-footer)) "")))

(defun aurora-emoji ()
  "One pictograph, built from its code so the source stays plain ASCII."
  (string (code-char #x1f600)))
