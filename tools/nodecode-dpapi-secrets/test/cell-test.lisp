;;;; cell-test.lisp --- the dpapi-secrets cell against its own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The store is real -- a scratch file under the temporary directory, written
;;;; by the cell's own writer -- so the format, the atomic replace and the
;;;; file mode are under test. Nothing is stubbed that could be real: there is
;;;; no process, no network and no key, and the DPAPI path itself is exercised
;;;; where the platform has it and refused in words where it does not.

(in-package #:nodecode.test)

(define-test-slice "dpapi-secrets" "DPAPI-SECRETS-CELL-" :start nodecode-dpapi-secrets:start-cell)

(defparameter +root+
  (merge-pathnames "nodecode-dpapi-secrets-test/" (uiop:temporary-directory)) "The scratch home every test writes its store in.")

(defparameter +entries+
  '(("openai" . "sk-test-123") ("zai" . "zai-test-456") ("empty-one" . "")) "What the scratch store holds: two keys and one empty entry.")

(defun scratch-store ()
  "The scratch store's pathname."
  (merge-pathnames "secrets.dpapi" +root+))

(defun write-scratch-store (&optional (entries +entries+))
  "The scratch store, written with ENTRIES through the cell's own writer."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (pair entries)
      (setf (gethash (car pair) map) (cdr pair)))
    (nodecode-dpapi-secrets:write-store (scratch-store) map :file)
    (scratch-store)))

(defun write-bytes (path text)
  "PATH written with TEXT's own bytes, header and all."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :element-type '(unsigned-byte 8))
    (write-sequence (map '(vector (unsigned-byte 8)) #'char-code text) out))
  path)

(defmacro with-store (&body body) "BODY with a scratch home, and the directory gone afterwards."
  `(unwind-protect
        (progn (ensure-directories-exist +root+) ,@body)
     (uiop:delete-directory-tree +root+ :validate t :if-does-not-exist :ignore)))

(defmacro with-cell ((&rest pairs) &body body) "BODY inside the cell started on the scratch store and PAIRS."
  `(with-cell-stop ((apply #'dpapi-secrets-start (append (list ,@pairs) (list "file" (namestring (scratch-store))))))
     ,@body))

(defun wire (tool &rest pairs)
  "TOOL called the way the model calls it, as the text the model reads back."
  (nle::message-content
   (execute-wire-call "dpapi-secrets-call" tool (apply #'nlk:make-json-object pairs))))

;;; --- the cell's own life ---------------------------------------------------

(define-cell-lifecycle-tests "dpapi-secrets"
  (:fixture with-store)
  (:config "file" (namestring (scratch-store)))
  (:help :dpapi-secrets)
  (:command "dpapi-secrets")
  (:running
   (is (nle::find-tool "dpapi_list") "the listing is registered")
   (is (nle::find-tool "dpapi_show") "so is the one read")
   (is (nle::find-tool "dpapi_set") "and the write")
   (is (nle::find-tool "dpapi_unset") "and the removal")
   (is (nle::find-tool "dpapi_env") "and the bulk read")
   (is (nle::find-tool "dpapi_status") "and the status")
   (is (equal "auto" (nodecode-dpapi-secrets::setting :protection)) "the documented default protection")
   (is (equal (namestring (scratch-store))
              (namestring (nodecode-dpapi-secrets:store-path))) "the store the section names"))
  (:stopped
   (is (null (nle::find-tool "dpapi_show")) "and stop takes every one back")
   (is (null (nle::find-tool "dpapi_env")) "the bulk read too"))
  (:refused ("protection" "nonsense")))

;;; --- the store on disk -------------------------------------------------------

(deftest dpapi-secrets-cell-store-round-trips ()
  (with-store
    (write-scratch-store)
    (multiple-value-bind (map protection)
        (nodecode-dpapi-secrets:read-store (scratch-store))
      (is (eq :file protection) "a platform without DPAPI writes the file header")
      (is (equal "sk-test-123" (gethash "openai" map)) "the value comes back")
      (is (equal '("empty-one" "openai" "zai")
                 (nodecode-dpapi-secrets:store-entries map)) "the names, sorted, and the empty entry is an entry")))
  (with-store
    (write-scratch-store)
    (let* ((path (scratch-store))
           (text (uiop:read-file-string path :external-format :latin-1)))
      (is (uiop:string-prefix-p "NODECODE-DPAPI-FILE-1" text) "the header says what wrote it")
      (is (search "sk-test-123" text) "a file store is not encrypted, and says so"))))

(deftest dpapi-secrets-cell-a-foreign-file-refuses ()
  (with-store
    (write-bytes (scratch-store) "GARBAGE\n{}")
    (is (signals-error nodecode-dpapi-secrets:dpapi-secrets-error
                       (nodecode-dpapi-secrets:read-store (scratch-store))) "a file that is not a store is refused, not guessed at")
    (write-bytes (scratch-store) "HERMES-DPAPI-1\nnot-a-blob")
    (is (signals-error nodecode-dpapi-secrets:dpapi-secrets-error
                       (nodecode-dpapi-secrets:read-store (scratch-store))) "the original's header is read as a DPAPI blob, and a bad one is an error")))

;;; --- the tools ---------------------------------------------------------------

(deftest dpapi-secrets-cell-status-names-the-store-and-no-value ()
  (with-store
    (write-scratch-store)
    (with-cell ("map" (vector "OPENAI_API_KEY=openai"))
      (let ((answer (wire "dpapi_status")))
        (is (search "store:" answer) "where the store is")
        (is (search "entries: 3 (empty-one, openai, zai)" answer) "how many entries it holds, by name and never by value")
        (is (search "OPENAI_API_KEY=openai" answer) "the mapping as written")
        (is (null (search "sk-test" answer)) "and not one value")))))

(deftest dpapi-secrets-cell-a-missing-store-is-a-refusal ()
  (with-store
    (with-cell ("map" (vector "OPENAI_API_KEY=openai"))
      (let ((answer (wire "dpapi_show" "name" "openai")))
        (is (search "no DPAPI secret store" answer) "the model is told there is no store yet")
        (is (search "/dpapi-secrets set" answer) "and the verb that makes one")))))

(deftest dpapi-secrets-cell-set-list-and-show ()
  (with-store
    (with-cell ("map" (vector "OPENAI_API_KEY=openai"))
      (is (search "stored openai" (wire "dpapi_set" "name" "openai" "value" "sk-test-123")) "the write answers what it did")
      (let ((answer (wire "dpapi_list")))
        (is (search "openai  env: OPENAI_API_KEY" answer) "the entry, and the variable mapped to it")
        (is (null (search "sk-test-123" answer)) "a listing holds no value"))
      (is (equal "sk-test-123" (wire "dpapi_show" "name" "openai")) "the one read answers the value alone")
      (is (equal "sk-test-123" (nodecode-dpapi-secrets:secret "openai")) "and a form reads the same entry without a tool call")
      (is (search "removed openai" (wire "dpapi_unset" "name" "openai")) "the removal answers what it did")
      (is (search "no entry named" (wire "dpapi_show" "name" "openai")) "and the entry is gone, in the store's own words"))))

(deftest dpapi-secrets-cell-an-empty-value-is-refused ()
  (with-store
    (with-cell ()
      (is (signals-error nlk:contract-refusal
                         (nodecode-dpapi-secrets:decode-entry-write
                          (nlk:make-json-object "name" "openai" "value" ""))) "the boundary refuses an empty value before the store is touched")
      (is (signals-error nlk:contract-refusal
                         (nodecode-dpapi-secrets:decode-entry (nlk:make-json-object))) "and a read with no name"))))

;;; --- the mapping the original injected -----------------------------------------

(deftest dpapi-secrets-cell-env-is-the-mapping-resolved ()
  (with-store
    (write-scratch-store)
    (with-cell ("map" (vector "OPENAI_API_KEY=openai" "ZAI_API_KEY=dpapi://zai"
                               "GONE_API_KEY=absent" "EMPTY_API_KEY=empty-one"))
      (let ((answer (wire "dpapi_env")))
        (is (search "OPENAI_API_KEY=sk-test-123" answer) "an entry, resolved")
        (is (search "ZAI_API_KEY=zai-test-456" answer) "and a dpapi:// reference read the same way")
        (is (search "GONE_API_KEY: no entry named \"absent\"" answer) "a missing entry is a warning, not silence")
        (is (search "EMPTY_API_KEY: entry \"empty-one\" is empty" answer) "so is an empty one")
        (is (equal '(("OPENAI_API_KEY" . "sk-test-123") ("ZAI_API_KEY" . "zai-test-456"))
                   (nodecode-dpapi-secrets:resolve)) "and a form gets the same pairs as an alist")))))

(deftest dpapi-secrets-cell-a-bad-map-line-warns-and-resolves-the-rest ()
  (with-store
    (write-scratch-store)
    (with-cell ("map" (vector "lower-case=openai" "NO_EQUALS" "OPENAI_API_KEY=openai"))
      (multiple-value-bind (mapping warnings) (nodecode-dpapi-secrets:parse-map
                                                (nodecode-dpapi-secrets::setting :map))
        (is (equal '(("OPENAI_API_KEY" . "openai")) mapping) "the one good line resolves")
        (is (= 2 (length warnings)) "and each bad line is named")
        (is (find-if (lambda (warning) (search "not a valid environment variable name" warning)) warnings) "the name rule is the original's")))))

(deftest dpapi-secrets-cell-nothing-mapped-is-a-refusal ()
  (with-store
    (write-scratch-store)
    (with-cell ()
      (let ((answer (wire "dpapi_env")))
        (is (search "nothing is mapped" answer) "the map is the one thing a resolution needs")))))

;;; --- /dpapi-secrets -----------------------------------------------------------

(deftest dpapi-secrets-cell-command-answers-every-verb ()
  (with-store
    (with-cell ("map" (vector "OPENAI_API_KEY=openai"))
      (is (search "dpapi-secrets:" (nodecode-dpapi-secrets::run-command "" "s1")) "no arguments is the status")
      (nodecode-dpapi-secrets::run-command "set openai sk-test-123" "s1")
      (is (search "OPENAI_API_KEY=sk-test-123" (nodecode-dpapi-secrets::run-command "env" "s1")) "env is the mapping resolved")
      (is (search "openai  env: OPENAI_API_KEY" (nodecode-dpapi-secrets::run-command "list" "s1")) "list is the names")
      (is (equal "sk-test-123" (nodecode-dpapi-secrets::run-command "show openai" "s1")) "show is the value")
      (is (search "secrets.dpapi" (nodecode-dpapi-secrets::run-command "path" "s1")) "path is the file")
      (multiple-value-bind (text dialog)
          (nodecode-dpapi-secrets::run-command "set openai" "s1")
        (is (search "the value is asked next" text) "set with no value asks for it")
        (is (getf dialog :rows) "in a dialog")))))
