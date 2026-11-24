;;;; cell-test.lisp --- lifecycle, the ledger, the audit, a canary, the report.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a stubbed config: that a
;;;; start installs the tool, /xray and the manual and a stop takes every one
;;;; of them back, that a config of the wrong shape refuses, that a :ROUND hook
;;;; writes one metadata line and status counts it, that the audit names a
;;;; second vendor and the fix form for it, that a canary is invented, kept and
;;;; found again in this home's stores, that find without a subject says so, and
;;;; that the report is one self-contained file. Nothing dials: this cell has
;;;; no network seam at all.

(in-package #:nodecode.test)

(define-test-slice "privacy-xray" "PRIVACY-XRAY-CELL-"
  :start nodecode-privacy-xray:start-cell)

;;; --- the stubbed config -------------------------------------------------------

(defvar *stub-config* '()
  "The config object the cell's reader answers, as NLE:CONFIG-GET builds it:
keyword plists, vectors for arrays.")

(defvar *writes* '()
  "Every (PATH VALUE) the cell wrote through CONFIG-SET, newest first.")

(defun key-of (name) (intern (string-upcase name) :keyword))

(defun stub-config (&optional path)
  (labels ((walk (node steps)
             (cond ((null steps) node)
                   ((not (consp node)) nil)
                   (t (walk (getf node (key-of (first steps))) (rest steps))))))
    (walk *stub-config* path)))

(defun stub-config-set (path value)
  (push (list path value) *writes*)
  (format nil "~a: set" path))

(defun reset-files ()
  "The ledger and the canaries of this scratch home, gone: a test starts clean."
  (dolist (path (list (nodecode-privacy-xray:ledger-path)
                       (nlk:home "privacy-xray/canaries.tsv")))
    (when (probe-file path) (ignore-errors (delete-file path)))))

(defmacro with-xray (&body body)
  "Run BODY with the config stubbed, nothing written to the real one, and an
empty ledger."
  `(let ((nodecode-privacy-xray::*config* #'stub-config)
         (nodecode-privacy-xray::*config-set* #'stub-config-set)
         (*stub-config* *stub-config*)
         (*writes* '()))
     (reset-files)
     ,@body))

(defun ask (url body)
  "The privacy_xray tool's answer for BODY."
  (nle::message-content (execute-wire-call url "privacy_xray" body)))

(defun fire-round (session turn round)
  "One :ROUND op through the registered hook, as the turn loop runs it."
  (funcall (cell-hook "nodecode-privacy-xray" :round)
           (list :session-id session :turn turn :round round)
           (lambda (op) (declare (ignore op)) t)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "privacy-xray"
  (:fixture with-xray)
  (:hooks :round :credential)
  (:help :privacy-xray)
  (:command "xray")
  (:running (is (nle::find-tool "privacy_xray") "the tool is registered")
            (is (plusp (length (nodecode-privacy-xray::check-findings))) "the audit answers findings"))
  (:stopped (is (null (nle::find-tool "privacy_xray")) "stop unregisters the tool")
            (is (zerop (hash-table-count nodecode-privacy-xray::*destinations-seen*)) "and forgets the destinations it had written"))
  (:refused ("ledger_max_mb" 0)))

;;; --- the ledger ----------------------------------------------------------------

(deftest privacy-xray-cell-a-round-is-one-metadata-line ()
  (with-xray
    (with-cell-stop ((privacy-xray-start))
      (fire-round "s1" "t1" 1)
      (fire-round "s1" "t1" 2)
      (let ((entries (nodecode-privacy-xray::request-entries)))
        (is (= 2 (length entries)) "one line per round")
        (is (equal "s1" (nodecode-privacy-xray::value-of (first entries) "session")))
        (is (equal "2" (nodecode-privacy-xray::value-of (second entries) "round")))
        (let ((text (ask "u1" "{\"action\":\"status\"}")))
          (is (search "2 model request(s)" text) "status counts them")
          (is (search "Canary: none yet" text)))))))

(deftest privacy-xray-cell-a-credential-writes-its-host-once ()
  (with-xray
    (with-cell-stop ((privacy-xray-start))
      (labels ((resolve ()
                 (funcall (cell-hook "nodecode-privacy-xray" :credential)
                          (list :provider "anthropic" :family :anthropic
                                :endpoint "https://api.anthropic.com/v1/messages")
                          (lambda (op) (declare (ignore op)) :credential))))
        (is (eq :credential (resolve)) "the hook answers what next answers")
        (resolve)
        (let ((rows (nodecode-privacy-xray::destination-rows)))
          (is (= 1 (length rows)) "resolved twice, written once")
          (is (equal "api.anthropic.com" (first (first rows))))
          (is (search "Anthropic" (nodecode-privacy-xray::vendor-text "api.anthropic.com"))))))))

;;; --- the audit -----------------------------------------------------------------

(deftest privacy-xray-cell-the-audit-names-a-second-vendor ()
  (with-xray
    (let ((*stub-config*
            (list :general (list :default_model (list :provider "anthropic" :model "claude")
                                 :auxiliary_model (list :provider "openrouter" :model "deepseek/flash")))))
      (with-cell-stop ((privacy-xray-start))
        (let ((text (ask "u2" "{\"action\":\"check\"}")))
          (is (search "aux" text) "the side task is named")
          (is (search "openrouter.ai" text) "and its host")
          (is (search "api.anthropic.com" text) "the main route reads too")
          (is (search "config-set" text) "the fix form is printed"))))))

(deftest privacy-xray-cell-a-green-side-task-stays-quiet-until-asked ()
  (with-xray
    (let ((*stub-config*
            (list :general (list :default_model (list :provider "anthropic" :model "claude")
                                 :auxiliary_model (list :provider "anthropic" :model "claude")))))
      (with-cell-stop ((privacy-xray-start))
        (let ((quiet (nodecode-privacy-xray::findings-text))
              (all (ask "u3" "{\"action\":\"check\"}")))
          (is (not (search "aux:" quiet)) "a green finding is not in the short list")
          (is (search "green  aux" all) "but the full one carries it"))))))

(deftest privacy-xray-cell-a-fix-writes-only-when-asked ()
  (with-xray
    (let ((*stub-config*
            (list :general (list :default_model (list :provider "anthropic" :model "claude")
                                 :auxiliary_model (list :provider "openrouter" :model "deepseek/flash")))))
      (with-cell-stop ((privacy-xray-start))
        (let ((shown (ask "u4" "{\"action\":\"fix\",\"subject\":\"aux\"}")))
          (is (search "config-set" shown))
          (is (null *writes*) "nothing written by a look"))
        (let ((text (multiple-value-list (cell-entry "nodecode-privacy-xray" "xray" "fix aux yes"))))
          (is (search "auxiliary model is now" (first text)))
          (is (equal '("general" "auxiliary_model") (coerce (first (first *writes*)) 'list)) "the fix wrote the auxiliary model"))))))

;;; --- the canary ----------------------------------------------------------------

(deftest privacy-xray-cell-a-canary-is-invented-kept-and-found ()
  (with-xray
    (with-cell-stop ((privacy-xray-start))
      (let* ((text (ask "u5" "{\"action\":\"canary\"}"))
             (canary (nodecode-privacy-xray::active-canary))
             (email (nodecode-privacy-xray::field-of canary "email")))
        (is (search "example.org" text) "the e-mail domain is the reserved one")
        (is (search email text))
        (is (plusp (length (nodecode-privacy-xray::canary-rows))) "it is kept")
        (ensure-directories-exist (nlk:home "logs/"))
        (with-open-file (out (nlk:home "logs/xray-test.log") :direction :output
                                                         :if-exists :supersede
                                                         :if-does-not-exist :create)
          (format out "a message about ~a~%" email))
        (let ((found (ask "u6" (format nil "{\"action\":\"find\",\"subject\":\"~a\"}" email))))
          (is (search "1 hit(s)" found) "the store that carries it is named")
          (is (search "xray-test.log" found)))
        (when (probe-file (nlk:home "logs/xray-test.log"))
          (delete-file (nlk:home "logs/xray-test.log")))))))

(deftest privacy-xray-cell-find-without-a-subject-says-what-to-do ()
  (with-xray
    (with-cell-stop ((privacy-xray-start))
      (let ((text (ask "u7" "{\"action\":\"find\"}")))
        (is (search "ERROR: PRIVACY-XRAY-ERROR" text))
        (is (search "what to look for" text))))))

;;; --- the report -----------------------------------------------------------------

(deftest privacy-xray-cell-the-report-is-one-self-contained-file ()
  (with-xray
    (with-cell-stop ((privacy-xray-start))
      (fire-round "s1" "t1" 1)
      (let ((text (ask "u8" "{\"action\":\"report\"}")))
        (is (search "the leak map is at" text))
        (is (probe-file (nodecode-privacy-xray:report-path)) "the file is there")
        (let ((html (uiop:read-file-string (nodecode-privacy-xray:report-path))))
          (is (search "<!doctype html>" html))
          (is (search "Privacy x-ray" html))
          (is (not (search "<script" html)) "no scripts in it")
          (is (not (search "http://" html)) "and no remote asset"))))))
