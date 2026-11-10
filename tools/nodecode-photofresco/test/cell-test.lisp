;;;; cell-test.lisp --- starting and stopping, the three tools, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(deftest photofresco-cell-starts-with-its-tools-command-and-manual ()
  (with-cell-stop ((photofresco-start))
    (is (nle::find-tool "photofresco_connect") "the connect tool is registered")
    (is (nle::find-tool "photofresco_ops") "the operations tool is registered")
    (is (nle::find-tool "photofresco_file") "the file tool is registered")
    (is (equal "Nodecode" (nodecode-photofresco::setting :client)) "client defaults to the agent name")
    (is (= 180 (nodecode-photofresco::setting :wait-seconds)) "wait_seconds defaults to 180")
    (is (search "photofresco:execute" (nle:help :photofresco)) "the manual names the calls"))
  (is (null (nle::find-tool "photofresco_ops")) "the tool is gone after stop")
  )

(deftest photofresco-cell-a-setting-outside-its-shape-refuses ()
  (is (handler-case (progn (photofresco-start "wait_seconds" 5) nil)
        (nlk:config-refusal () t)) "a wait under 30 s refuses at start")
  (is (handler-case (progn (photofresco-start "read_roots" "not-an-array") nil)
        (nlk:config-refusal () t)) "a list setting given a bare string refuses too"))

(deftest photofresco-cell-a-call-with-no-session-says-so ()
  (with-cell-stop ((photofresco-start))
    (is (search "photofresco_connect"
                (handler-case (photofresco:status)
                  (photofresco:photofresco-error (condition) (princ-to-string condition)))) "no session yet names the tool that opens one")))

;;; --- the tools over a scripted browser --------------------------------------------

(deftest photofresco-cell-the-tools-answer-through-the-channel ()
  (with-scripted-link ((scripted-link
                        :answers (list (cons "status" (list :ok (args "session_id" "s-1")))
                                       (cons "documents" (list :ok (args "documents" 2))))))
    (is (search "s-1" (nodecode-photofresco::tool-ops (args "action" "status"))) "status answers the editor's own session")
    (is (search "2" (nodecode-photofresco::tool-ops (args "action" "documents"))) "documents answers the editor's own list")
    (is (search "layer.add"
                (nodecode-photofresco::tool-ops
                 (args "action" "execute" "op" "layer.add"
                       "params" (args "name" "Sky")))) "execute passes the operation's own name through")
    (is (search "layer.add"
                (nodecode-photofresco::tool-ops
                 (args "action" "execute" "op" "layer.add" "request_id" "req-1"))) "with a request id of the model's own")))

(deftest photofresco-cell-the-editors-refusal-is-an-error-with-its-code ()
  (with-scripted-link ((scripted-link
                        :answers '(("execute" :refuse "grant_missing" "the session lacks the Edit grant"))))
    (let ((said (handler-case
                    (progn (nodecode-photofresco::tool-ops
                            (args "action" "execute" "op" "layer.add"))
                           nil)
                  (photofresco:photofresco-error (condition) (princ-to-string condition)))))
      (is (search "grant_missing" said) "the editor's code is carried through")
      (is (search "Edit grant" said) "and its message"))))

(deftest photofresco-cell-a-file-outside-the-granted-folders-refuses ()
  (with-scripted-link ((scripted-link))
    (is (search "read_roots"
                (handler-case (progn (photofresco:open-file "/nowhere/at/all.png") nil)
                  (photofresco:photofresco-error (condition) (princ-to-string condition)))) "a path in no granted folder names the setting")
    (is (search "write_roots"
                (handler-case (progn (photofresco:export-file "/nowhere/at/all.png") nil)
                  (photofresco:photofresco-error (condition) (princ-to-string condition)))) "and so does an export")))

;;; --- the command ------------------------------------------------------------------

(deftest photofresco-cell-the-command-reads-the-session ()
  (with-scripted-link ((scripted-link
                        :answers (list (cons "status" (list :ok (args "session_id" "s-9"))))))
    (is (search "s-9" (nodecode-photofresco::run-slash "status" nil)) "/photofresco status answers the session")
    (is (search "status, documents, operations" (nodecode-photofresco::run-slash "" nil)) "and alone it says what it takes")))
