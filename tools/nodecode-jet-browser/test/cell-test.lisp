;;;; cell-test.lisp --- the lifecycle, the tool, the command, a failed check.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here: that the cell installs its tool, its command and its
;;;; manual and takes every one of them back on stop; that a section naming no
;;;; image, or a number out of range, refuses at start; that the tool drives
;;;; the container's protocol in order and answers the evidence; that the
;;;; command shows what it would run and then runs it; that a container that
;;;; answers the wrong title is an error the model reads rather than a crash;
;;;; and that a screenshot file, when the section names one, is the PNG.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "jet-browser"
  (:help :jet-browser)
  (:command "jetbrowser")
  (:running
   (is (equal "docker" (nodecode-jet-browser::setting :docker)) "the docker CLI defaults to the one on PATH")
   (is (equal 180 (nodecode-jet-browser::setting :timeout-seconds)))
   (is (search "sha256:" (nodecode-jet-browser::image)) "the section's image is the pinned one the test starts with")
   (is (probe-file (nodecode-jet-browser::seccomp-profile)) "the profile shipped beside the cell is the one the container runs under")
   (let ((argv (nodecode-jet-browser::docker-argv "probe")))
     (is (equal (nodecode-jet-browser::image) (car (last argv))) "the command ends in the section's image")
     (is (subsetp '("run" "--network=none" "--cap-drop=ALL" "--cap-add=SETUID"
                    "--cap-add=SETGID" "--security-opt=no-new-privileges"
                    "--rm" "-i")
                  argv :test #'equal) "and runs with its network off, its capabilities dropped")
     (is (find-if (lambda (arg) (and (stringp arg) (search "seccomp=" arg))) argv) "under the profile shipped beside the cell")))
  (:refused ("image" nil) ("timeout_seconds" 0))
  (:idle nodecode-jet-browser::jet-browser-error (nodecode-jet-browser::image)))

;;; --- the tool ---------------------------------------------------------------------

(deftest jet-browser-cell-the-tool-drives-the-protocol-and-answers-the-evidence ()
  (with-cell-stop ((jet-browser-start))
    (with-scripted (jet-browser-pass)
      (let ((evidence (nlk:decode-json (jet-browser-tool "jet_browser_verify" (nlk:json-object)))))
        (is (eq t (nlk:json-value evidence :boolean "success")))
        (is (equal "disabled" (nlk:json-value evidence :string "network")))
        (is (equal nodecode-jet-browser::+title+ (nlk:json-value evidence :string "title")))
        (is (equal nodecode-jet-browser::+typed+ (nlk:json-value evidence :string "input")))
        (is (> (nlk:json-value evidence :integer "screenshot_bytes") 1000))
        (is (= 64 (length (nlk:json-value evidence :string "screenshot_sha256"))) "the PNG's sha256, as the original answered it")
        (is (integerp (nlk:json-value evidence :integer "duration_ms"))))
      (is (equal '("create" "navigate" "document_state" "snapshot" "input"
                   "evaluate" "screenshot" "close")
                 (jet-browser-sent)) "one request per step, in the protocol's order")
      (is *closed* "and the client is always closed")
      (let ((create (car (last *calls*))))
        (is (equal '("op" "page_load_strategy" "profile_dir" "proxy")
                   (sort (loop for key being the hash-keys of create collect key)
                         #'string<)) "the first request carries nothing but the fixture's own fields")))))

(deftest jet-browser-cell-a-check-that-sees-the-wrong-page-is-an-error-to-read ()
  (with-cell-stop ((jet-browser-start))
    (with-scripted (jet-browser-pass :title "Some Other Page")
      (let ((evidence (nlk:decode-json (jet-browser-tool "jet_browser_verify" (nlk:json-object)))))
        (is (null (nlk:json-value evidence :boolean "success")))
        (is (search "Some Other Page" (nlk:json-value evidence :string "error")) "the title it read is in the error")
        (is (equal '("create" "navigate" "document_state" "snapshot")
                   (jet-browser-sent)) "and the check stopped there")
        (is *closed* "the container closed all the same")))))

(deftest jet-browser-cell-a-container-that-refuses-says-why ()
  (with-cell-stop ((jet-browser-start))
    (with-scripted (list (refusal "no such image"))
      (let ((evidence (nlk:decode-json (jet-browser-tool "jet_browser_verify" (nlk:json-object)))))
        (is (null (nlk:json-value evidence :boolean "success")))
        (is (search "no such image" (nlk:json-value evidence :string "error")))))))

;;; --- the command ------------------------------------------------------------------

(deftest jet-browser-cell-the-command-shows-what-it-would-run-then-runs-it ()
  (with-cell-stop ((jet-browser-start))
    (multiple-value-bind (text dialog)
        (cell-entry "nodecode-jet-browser" "jetbrowser" "")
      (is (search "sha256:" text) "the panel's line names the image")
      (is (plusp (length (princ-to-string dialog))) "and it is a panel"))
    (with-scripted (jet-browser-pass)
      (let ((line (cell-entry "nodecode-jet-browser" "jetbrowser" "run")))
        (is (search "Jet Browser verified" line))
        (is (search nodecode-jet-browser::+title+ line))
        (is (equal 8 (length (jet-browser-sent))) "the whole check ran")))))

;;; --- the section's own file, and the PNG -----------------------------------------

(deftest jet-browser-cell-a-screenshot-file-is-the-png-the-container-captured ()
  (let ((path (merge-pathnames "nodecode-jet-browser-test.png" (uiop:temporary-directory))))
    (ignore-errors (delete-file path))
    (with-cell-stop ((jet-browser-start "screenshot_file" (uiop:native-namestring path)))
      (with-scripted (jet-browser-pass)
        (let ((evidence (nlk:decode-json (jet-browser-tool "jet_browser_verify" (nlk:json-object)))))
          (is (equal (uiop:native-namestring path)
                     (nlk:json-value evidence :string "screenshot_file")))
          (is (probe-file path) "the PNG is written where the section said")
          (is (> (with-open-file (in path :element-type '(unsigned-byte 8))
                   (file-length in))
                 1000))))
      (ignore-errors (delete-file path)))))

(deftest jet-browser-cell-the-transport-reads-exactly-one-line ()
  ;; A pipe at end of file is what LISTEN hears as readable -- the docker
  ;; client dying is that case; a string stream is not, so only the line a
  ;; real answer carries is asserted here.
  (multiple-value-bind (line ended)
      (nodecode-jet-browser::read-line-within
       (make-string-input-stream (format nil "one~%two")) 1)
    (is (equal "one" line) "one line, and nothing after it")
    (is (null ended)))
  (multiple-value-bind (line ended)
      (nodecode-jet-browser::read-line-within
       (make-string-input-stream (format nil "~a~%rest" (make-string 5000 :initial-element #\x))) 1)
    (is (= 5000 (length line)) "a long line is one line")
    (is (null ended))))

(deftest jet-browser-cell-the-decoder-reads-base64-and-refuses-the-rest ()
  (let ((octets (jet-browser-png)))
    (is (nodecode-jet-browser::png-p
         (nodecode-jet-browser::base64-decode (base64-encode octets))) "a PNG survives the round trip")
    (is (signals-error nodecode-jet-browser::jet-browser-error
          (nodecode-jet-browser::base64-decode "not*base64")) "and anything else is a refusal, not a quiet guess")))
