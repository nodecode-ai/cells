;;;; cell-test.lisp --- the wincred cell against its own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The vault is stubbed -- a plist of two functions, the same seam the real
;;;; advapi32 binding sits behind -- so these tests need no Windows, no
;;;; credential and no network. The bytes are real: the decoding is tested on
;;;; what cmdkey and the Credential UI actually write.

(in-package #:nodecode.test)

(define-test-slice "wincred" "WINCRED-CELL-" :start nodecode-wincred:start-cell)

;;; --- the fixture ---------------------------------------------------------------

(defvar *asked* '()
  "Every target the stub vault was asked for this test, newest first.")

(defun octets (text)
  "TEXT as the bytes a CLI tool writes into a credential."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun utf16 (text &key (bom t))
  "TEXT as Credential Manager's own UI writes it: UTF-16 little-endian, a
byte-order mark first."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (when bom
      (vector-push-extend #xFF bytes)
      (vector-push-extend #xFE bytes))
    (loop for character across text
          for code = (char-code character)
          do (vector-push-extend (logand code #xFF) bytes)
             (vector-push-extend (logand (ash code -8) #xFF) bytes))
    (coerce bytes '(vector (unsigned-byte 8)))))

(defparameter +held+
  (list (cons "hermes/openai" (octets "sk-test-123"))
        (cons "hermes/zai" (utf16 "zai-test-456"))
        (cons "hermes/plain" (utf16 "utf16-value" :bom nil))
        (cons "hermes/garbled" (vector 115 0 107))
        (cons "hermes/empty" (vector)))
  "What the stub vault holds: UTF-8, two spellings of UTF-16, one blob that
decodes to neither, and one with nothing in it.")

(defparameter +map+
  (vector "OPENAI_API_KEY=hermes/openai"
          "ZAI_API_KEY=wincred://hermes/zai"
          "GARBLED_TOKEN=hermes/garbled")
  "The section every test starts the cell with.")

(defun stub-vault (&key (available t))
  "The vault as a test sees it: the same (:AVAILABLE :READ) seam the platform
binding sits behind, answered from +HELD+."
  (list :available (lambda () available)
        :read (lambda (target)
                (push target *asked*)
                (let ((held (assoc target +held+ :test #'equal)))
                  (unless held
                    (error 'nodecode-wincred:wincred-error
                           :target target :code 1168
                           :detail (nodecode-wincred::win32-detail target 1168)))
                  (copy-seq (cdr held))))))

(defmacro with-vault ((&key (available t)) &body body)
  "BODY with the stub vault in place of the real one, asking nothing of
Windows."
  `(let ((nodecode-wincred::*vault* (stub-vault :available ,available))
         (*asked* '()))
     ,@body))

(defmacro with-stub-vault (&body body)
  "BODY with the stub vault and the section's own map started."
  `(with-vault () ,@body))

(defmacro with-cell (&body body)
  "BODY inside the cell started on +MAP+ against the stub vault."
  `(with-stub-vault
     (with-cell-stop ((wincred-start "map" +map+))
       ,@body)))

(defun wire (tool &rest pairs)
  "TOOL called the way the model calls it, as the text the model reads back."
  (nle::message-content
   (execute-wire-call "wincred-call" tool (apply #'nlk:make-json-object pairs))))

(defun ran (target)
  "Assert that the newest target the stub was asked for is TARGET."
  (is (equal target (first *asked*)) (format nil "the vault was asked for ~s" target)))

;;; --- the cell's own life ---------------------------------------------------------

(define-cell-lifecycle-tests "wincred"
  (:fixture with-stub-vault)
  (:config "map" +map+)
  (:help :wincred)
  (:command "wincred")
  (:running
   (is (nle::find-tool "wincred_status") "the status is registered")
   (is (nle::find-tool "wincred_read") "so is the one read")
   (is (nle::find-tool "wincred_apply") "and the apply")
   (is (equal "hermes/openai"
              (cdr (assoc "OPENAI_API_KEY" (nodecode-wincred:mapping) :test #'string=))) "the map is read off the section's own list"))
  (:stopped
   (is (null (nle::find-tool "wincred_read")) "and stop takes every one back"))
  (:refused ("max_chars" 1)))

;;; --- wincred_status ----------------------------------------------------------------

(deftest wincred-cell-status-names-the-map-and-no-value ()
  (with-cell
    (let ((answer (wire "wincred_status")))
      (is (search "map: 3 entries" answer) "every entry the section maps")
      (is (search "OPENAI_API_KEY <- hermes/openai: found (11 bytes)" answer) "the credential behind it and its size")
      (is (search "ZAI_API_KEY <- hermes/zai: found" answer) "a wincred:// target is the same target")
      (is (search "GARBLED_TOKEN <- hermes/garbled: the credential is not UTF-8 or UTF-16 text" answer) "a blob that does not decode is reported, not injected")
      (is (null (search "sk-test-123" answer)) "and not one value"))))

;;; --- wincred_read -------------------------------------------------------------------

(deftest wincred-cell-read-answers-the-value-alone ()
  (with-cell
    (is (equal "sk-test-123" (wire "wincred_read" "name" "OPENAI_API_KEY")) "the value, and nothing else")
    (ran "hermes/openai")
    (is (equal "zai-test-456" (wire "wincred_read" "name" "ZAI_API_KEY")) "a UTF-16 credential reads as the text it holds")
    (is (equal "utf16-value" (nodecode-wincred:credential-value "hermes/plain")) "and one with no byte-order mark, by the NUL count")))

(deftest wincred-cell-read-takes-a-target-with-its-scheme ()
  (with-cell
    (is (equal "sk-test-123" (wire "wincred_read" "name" "hermes/openai")) "a bare target")
    (is (equal "sk-test-123" (wire "wincred_read" "name" "wincred://hermes/openai")) "and the original's scheme, which the map also takes")))

(deftest wincred-cell-a-missing-credential-says-what-to-do ()
  (with-cell
    (let ((answer (wire "wincred_read" "name" "hermes/nothing")))
      (is (search "no generic credential named" answer) "what the vault says")
      (is (search "Credential Manager" answer) "and where to store it")
      (is (search "cmdkey /list" answer) "and how to see the names there are"))))

(deftest wincred-cell-a-blob-that-does-not-decode-fails-closed ()
  (with-cell
    (let ((answer (wire "wincred_read" "name" "hermes/garbled")))
      (is (search "not UTF-8 or UTF-16 text" answer) "the value is refused, not mangled"))
    (is (null (uiop:getenv "GARBLED_TOKEN")) "and no environment variable was made from it")))

(deftest wincred-cell-a-read-without-a-name-refuses ()
  (is (signals-error nlk:contract-refusal
                     (nodecode-wincred:decode-credential-read (nlk:make-json-object))) "the one required member of the boundary"))

;;; --- wincred_apply, and the environment ----------------------------------------------

(deftest wincred-cell-apply-sets-the-environment-without-showing-values ()
  (with-cell
    (is (equal "sk-test-123" (uiop:getenv "OPENAI_API_KEY")) "the start applied the map")
    (nodecode-wincred:unset-environment-variable "OPENAI_API_KEY")
    (let ((answer (wire "wincred_apply")))
      (is (search "OPENAI_API_KEY: set from hermes/openai (11 bytes), not shown" answer) "the name and the size, never the value")
      (is (null (search "sk-test-123" answer)) "no value in the answer")
      (is (equal "sk-test-123" (uiop:getenv "OPENAI_API_KEY")) "and the process has it"))
    (let ((answer (wire "wincred_apply")))
      (is (search "OPENAI_API_KEY: left alone" answer) "a variable already set wins, as .env does"))))

(deftest wincred-cell-stop-puts-the-environment-back ()
  (with-stub-vault
    (nodecode-wincred:unset-environment-variable "OPENAI_API_KEY")
    (with-cell-stop ((wincred-start "map" +map+))
      (is (equal "sk-test-123" (uiop:getenv "OPENAI_API_KEY")) "the start applied the map")
      (funcall stop)
      (is (null (uiop:getenv "OPENAI_API_KEY")) "and the stop took it back out"))))

;;; --- off Windows -------------------------------------------------------------------

(deftest wincred-cell-off-windows-the-tools-still-answer ()
  (with-vault (:available nil)
    (with-cell-stop ((wincred-start "map" +map+))
      (let ((answer (wire "wincred_status")))
        (is (search "unavailable: this is not Windows" answer) "the platform is said, not guessed")
        (is (search "OPENAI_API_KEY <- hermes/openai: not read here" answer) "and nothing is read"))
      (is (search "nothing was applied here" (wire "wincred_apply")) "apply says so too")
      (is (signals-error nodecode-wincred:wincred-error
                         (nodecode-wincred:credential-value "hermes/openai")) "a read refuses instead of answering an empty string")
      (is (search "only available on Windows"
                  (second (cell-notice "nodecode-wincred"))) "and the board carries the reason"))))

;;; --- /wincred ------------------------------------------------------------------------

(deftest wincred-cell-the-command-answers-the-status-and-one-entry ()
  (with-cell
    (is (search "map: 3 entries" (cell-entry "nodecode-wincred" "wincred" "")) "no argument is the status")
    (is (search "OPENAI_API_KEY <- hermes/openai"
                (cell-entry "nodecode-wincred" "wincred" "OPENAI_API_KEY")) "an environment variable's name describes that one")
    (is (search "applied to this process's environment"
                (cell-entry "nodecode-wincred" "wincred" "apply")) "and apply applies the map")))

;;; --- the map and the bytes -----------------------------------------------------------

(deftest wincred-cell-the-map-skips-what-is-malformed ()
  (multiple-value-bind (pairs warnings)
      (nodecode-wincred:parse-mapping
       '("no-equals" "9LIVES=hermes/openai" "EMPTY="
         "OPENAI_API_KEY=hermes/openai" "OPENAI_API_KEY=hermes/zai"))
    (is (equal 1 (length pairs)) "one usable entry")
    (is (equal (cons "OPENAI_API_KEY" "hermes/openai") (first pairs)) "and the first claim of a name is the one kept")
    (is (equal 4 (length warnings)) "every malformed entry is a warning, not a failure")))

(deftest wincred-cell-decoding-takes-utf8-and-both-utf16-spellings ()
  (is (equal "plain-value" (nodecode-wincred:decode-credential (octets "plain-value"))) "UTF-8, as a CLI tool writes it")
  (is (equal "utf16-value" (nodecode-wincred:decode-credential (utf16 "utf16-value"))) "UTF-16LE with a byte-order mark, as the Credential UI writes it")
  (is (equal "utf16-value" (nodecode-wincred:decode-credential (utf16 "utf16-value" :bom nil))) "and one with no byte-order mark, by the NUL count")
  (is (equal "sk-tail"
             (nodecode-wincred:decode-credential
              (concatenate '(vector (unsigned-byte 8)) (octets "sk-tail") (vector 0)))) "a trailing NUL is padding, not a mangled value")
  (is (equal "" (nodecode-wincred:decode-credential (vector))) "an empty blob is empty")
  (is (signals-error nodecode-wincred:credential-decode-error
                     (nodecode-wincred:decode-credential (vector 115 0 107))) "an interior NUL with an odd length decodes to neither and fails closed"))
