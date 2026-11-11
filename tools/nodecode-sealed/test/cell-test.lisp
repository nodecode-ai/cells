;;;; cell-test.lisp --- lifecycle, the four tools, the command, the format, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a real store in a
;;;; scratch folder and a stubbed environment: that a start installs the four
;;;; tools, /sealed and the manual and a stop takes every one of them back;
;;;; that a wrong-typed setting refuses; that a value put in comes back out
;;;; while the file on disk holds no plaintext; that the store the original
;;;; writes opens here, byte for byte; that AES-256-GCM matches the published
;;;; vector; that a start loads the store into the environment and the stop
;;;; puts back exactly what each variable held; that a missing store, a wrong
;;;; key and a foreign format are refusals naming the way back; and that
;;;; /sealed answers every verb.

(in-package #:nodecode.test)

(define-test-slice "sealed" "SEALED-CELL-" :start nodecode-sealed:start-cell)

;;; --- the environment, a hash table --------------------------------------------------

(defvar *sealed-test-env* nil
  "The variables the cell sees in one test: what it reads, sets and unsets.")

(defmacro with-stub-env (&body body)
  "BODY with the cell's three environment seams bound to one hash table, so no
test touches the environment of the image it runs in."
  `(let ((*sealed-test-env* (make-hash-table :test #'equal))
         (nodecode-sealed::*loaded* nil)
         (nodecode-sealed::*live* nil)
         (nodecode-sealed::*getenv* (lambda (name) (gethash name *sealed-test-env*)))
         (nodecode-sealed::*setenv* (lambda (name value)
                                       (setf (gethash name *sealed-test-env*) value)))
         (nodecode-sealed::*unsetenv* (lambda (name) (remhash name *sealed-test-env*))))
     ,@body))

(defmacro with-sealed ((&rest pairs) &body body)
  "BODY with the cell started on a fresh store and key file under this run's
home, and any PAIRS added to the section."
  `(let* ((base (format nil "sealed-cell-~d" (incf *sealed-cell-stores*)))
          (store (namestring (nlk:home (format nil "~a/sealed.store" base))))
          (key (namestring (nlk:home (format nil "~a/sealed.key" base)))))
     (with-cell-stop ((sealed-start "store" store "key_file" key ,@pairs))
       ,@body)))

(defvar *sealed-cell-stores* 0 "How many scratch stores this run made.")

(defun sealed-call (tool value)
  "Run the registered TOOL on VALUE, as a model's call would."
  (funcall (third (nle::find-tool tool)) value))

(defun sealed-refusal (thunk)
  "THUNK's refusal as text, or NIL when it answered."
  (handler-case (progn (funcall thunk) nil)
    (nodecode-sealed:sealed-error (condition) (format nil "~a" condition))))

(defun sealed-store-text ()
  (uiop:read-file-string (nodecode-sealed::store-path)))

;;; --- the original's own store ------------------------------------------------------

(defparameter +original-store+
  (format nil "SEALED1~%~a~%"
          "EBESExQVFhcYGRobXt7rcyilX9fqEGZrLwodPKU1REpejxjus7apIhBpdqg7yTNmmMlD3kwFpnOAyF9aWvJ6Qun6GU3HOyuKaWO2-GC9wVki")
  "A store the original wrote: key 00..1f, nonce 10..1b, DEMO_TOKEN=\"sk demo-1234\", PLAIN=1.")

(defun octets-of (text)
  (sb-ext:string-to-octets text :external-format :ascii))

(defun original-key ()
  "The 32-octet key the store above was sealed under: 00, 01, ... 1f."
  (let ((key (make-array 32 :element-type '(unsigned-byte 8))))
    (loop for index from 0 below 32 do (setf (aref key index) index))
    key))

;;; --- lifecycle ----------------------------------------------------------------------

(define-cell-lifecycle-tests "sealed"
  (:help :sealed)
  (:command "sealed")
  (:fixture with-stub-env)
  (:running
   (is (nle::find-tool "sealed_status") "the status tool is registered")
   (is (nle::find-tool "sealed_list") "the list tool is registered")
   (is (nle::find-tool "sealed_set") "the set tool is registered")
   (is (nle::find-tool "sealed_unset") "the unset tool is registered")
   (is (null (nodecode-sealed::setting :store)) "the store path is unset until the operator names one")
   (is (eq t (nodecode-sealed::setting :override-existing)) "a stored variable wins by default"))
  (:stopped
   (is (null (nle::find-tool "sealed_set")) "stop unregisters the tools")
   (is (null (nle::find-tool "sealed_list"))))
  (:refused ("store" 5) ("key_file" 3)))

;;; --- the format -----------------------------------------------------------------------

(deftest sealed-cell-the-originals-store-opens-here ()
  (let ((values (nodecode-sealed::open-store-text (original-key) +original-store+)))
    (is (equal 2 (length values)) "both variables of the original's store")
    (is (equal "sk demo-1234" (cdr (assoc "DEMO_TOKEN" values :test #'string=))) "a quoted value reads back without its quotes")
    (is (equal "1" (cdr (assoc "PLAIN" values :test #'string=)))))
  (is (search "did not open"
              (sealed-refusal (lambda ()
                                (nodecode-sealed::open-store-text
                                 (make-array 32 :element-type '(unsigned-byte 8) :initial-element 9)
                                 +original-store+)))) "another key fails the tag rather than answering half a store")
  (is (search "not a sealed store"
              (sealed-refusal (lambda ()
                                (nodecode-sealed::open-store-text (original-key)
                                                                  (format nil "SEALED2~%AAAA~%"))))) "a store of another format version is refused, not decrypted into garbage"))

(deftest sealed-cell-a-value-survives-the-dotenv-body ()
  (let* ((values (list (cons "PLAIN" "abc") (cons "SPACED" "two words")
                        (cons "QUOTED" "say \"hi\"") (cons "MULTI" (format nil "one~%two"))
                        (cons "EMPTY" "")))
         (text (nodecode-sealed::dump-dotenv values)))
    (is (equal (sort (copy-list values) #'string< :key #'car)
               (nodecode-sealed::parse-dotenv text)) "spaces, quotes, a newline and an empty value all read back as written")))

(deftest sealed-cell-aes-256-gcm-matches-the-published-vector ()
  (let* ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0))
         (plain (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
         (sealed (nodecode-sealed::aes-gcm-seal key nonce #() plain)))
    (is (equal "cea7403d4d606b6e074ec5d3baf39d18d0d1c8a799996bf0265b98b5d48ab919"
               (nodecode-sealed::octets->hex sealed)) "the GCM specification's AES-256 case: ciphertext then tag")
    (is (equalp plain (nodecode-sealed::aes-gcm-open key nonce #() sealed)) "and it opens back")
    (is (null (nodecode-sealed::aes-gcm-open key nonce #() (subseq sealed 0 (1- (length sealed))))) "a body one octet short is refused")))

;;; --- the verbs and the store on disk ---------------------------------------------------

(deftest sealed-cell-a-value-put-in-comes-back-out-and-the-file-is-ciphertext ()
  (with-sealed ()
    (is (search "sealed: store" (sealed:init)) "init makes the pair")
    (is (search "set DEMO_TOKEN" (sealed:set-value "DEMO_TOKEN" "sk-test-1234")))
    (is (equal "sk-test-1234" (sealed:value "DEMO_TOKEN")) "the value comes back through the seal")
    (let ((text (sealed-store-text)))
      (is (uiop:string-prefix-p "SEALED1" text) "the store starts with the original's magic")
      (is (not (search "sk-test-1234" text)) "the file holds no plaintext")
      (is (not (search "DEMO_TOKEN" text)) "not even the variable's name"))
    (is (= 32 (length (nodecode-sealed::read-bytes (nodecode-sealed::key-path)))) "the key is 32 raw octets")
    (is (= #o600 (nodecode-sealed::file-mode (nodecode-sealed::key-path))) "the key is 0600")
    (is (= #o600 (nodecode-sealed::file-mode (nodecode-sealed::store-path))) "the store is 0600")
    (is (equal '("DEMO_TOKEN") (sealed:names)))
    (is (search "store: mode 600 ok" (sealed:status)) "status reports the store's mode")
    (is (search "entries: 1" (sealed:status)) "and how many variables it holds")
    (is (search "rotated the key" (sealed:rotate)) "a rotation keeps the entries")
    (is (equal "sk-test-1234" (sealed:value "DEMO_TOKEN")) "under the new key")
    (is (probe-file (nodecode-sealed::backup-of (nodecode-sealed::key-path))) "the previous key is kept")
    (is (search "removed DEMO_TOKEN" (sealed:unset "DEMO_TOKEN")))
    (is (equal '() (sealed:names)) "an empty store answers, it does not fail")))

(deftest sealed-cell-a-missing-store-and-a-bad-name-refuse ()
  (with-sealed ()
    (is (search "run /sealed init" (sealed-refusal (lambda () (sealed:names)))) "a read before init names the way back")
    (is (null (probe-file (nodecode-sealed::key-path))) "a read creates no key")
    (sealed:init)
    (is (search "must be letters" (sealed-refusal (lambda () (sealed:set-value "1BAD" "x")))))
    (is (search "the value is empty" (sealed-refusal (lambda () (sealed:set-value "GOOD" "")))))
    (is (search "already exists" (sealed-refusal (lambda () (sealed:init)))) "a second init does not overwrite the key")
    (is (search "sealed: store" (sealed:init :force t)) "force makes a new pair and keeps the entries")))

;;; --- the environment --------------------------------------------------------------------

(deftest sealed-cell-a-start-loads-and-a-stop-puts-it-back ()
  (with-stub-env
    (let* ((base (format nil "sealed-cell-~d" (incf *sealed-cell-stores*)))
           (store (namestring (nlk:home (format nil "~a/sealed.store" base))))
           (key (namestring (nlk:home (format nil "~a/sealed.key" base)))))
      (with-cell-stop ((sealed-start "store" store "key_file" key))
        (sealed:init)
        (sealed:set-value "SEALED_CELL_DEMO" "sk-live-9"))
      (is (null (gethash "SEALED_CELL_DEMO" *sealed-test-env*)) "a stop put the variable back")
      (setf (gethash "SEALED_CELL_DEMO" *sealed-test-env*) "from-the-shell")
      (with-cell-stop ((sealed-start "store" store "key_file" key))
        (is (equal "sk-live-9" (gethash "SEALED_CELL_DEMO" *sealed-test-env*)) "a start puts the store's variables in the environment")
        (sealed:set-value "SEALED_CELL_LATE" "sk-late-1")
        (is (equal "sk-late-1" (gethash "SEALED_CELL_LATE" *sealed-test-env*)) "a variable set while the cell runs goes live at once"))
      (is (equal "from-the-shell" (gethash "SEALED_CELL_DEMO" *sealed-test-env*)) "and a stop restores exactly what was there")
      (is (null (gethash "SEALED_CELL_LATE" *sealed-test-env*)) "a variable the shell did not have is removed")
      (with-cell-stop ((sealed-start "store" store "key_file" key "override_existing" nil))
        (is (equal "from-the-shell" (gethash "SEALED_CELL_DEMO" *sealed-test-env*)) "override_existing false leaves the shell's value standing")))))

;;; --- the tools --------------------------------------------------------------------------

(deftest sealed-cell-the-tools-answer-through-the-registry ()
  (with-sealed ()
    (sealed:init)
    (is (search "set DEMO_TOKEN"
                (sealed-call "sealed_set" (nodecode-sealed::make-set-call
                                            :name "DEMO_TOKEN" :value "sk-test-1234"))) "the set tool stores one variable")
    (let ((answer (sealed-call "sealed_list" (nodecode-sealed::make-list-call))))
      (is (search "DEMO_TOKEN  (12 chars)" answer) "the list names it and the length of its value")
      (is (not (search "sk-test-1234" answer)) "never the value"))
    (is (search "no variable starts with AWS_"
                (sealed-call "sealed_list" (nodecode-sealed::make-list-call :prefix "AWS_"))) "a prefix narrows the list")
    (let ((answer (sealed-call "sealed_status" (nodecode-sealed::make-status-call :store-open t))))
      (is (search "entries: 1" answer) "status counts the entries")
      (is (search "loaded: 0" answer) "and says what is in the environment"))
    (is (search "entries: (not read)"
                (sealed-call "sealed_status" (nodecode-sealed::make-status-call :store-open nil))) "open false reports without the key")
    (is (search "ERROR: SEALED-ERROR"
                (sealed-call "sealed_unset" (nodecode-sealed::make-unset-call :name "NOPE"))) "a refusal is the tool's answer, not a broken call")
    (is (search "removed DEMO_TOKEN"
                (sealed-call "sealed_unset" (nodecode-sealed::make-unset-call :name "DEMO_TOKEN"))))))

;;; --- the command --------------------------------------------------------------------------

(deftest sealed-cell-the-command-answers-every-verb ()
  (with-stub-env
   (with-sealed ()
    (is (search "sealed init" (cell-entry "nodecode-sealed" "sealed" "")) "no argument answers the manual")
    (is (search "sealed: store" (cell-entry "nodecode-sealed" "sealed" "init")))
    (is (search "set DEMO_TOKEN" (cell-entry "nodecode-sealed" "sealed" "set DEMO_TOKEN sk-1")))
    (is (equal "sk-1" (cell-entry "nodecode-sealed" "sealed" "value DEMO_TOKEN")))
    (is (search "DEMO_TOKEN  (4 chars)" (cell-entry "nodecode-sealed" "sealed" "list")))
    (is (search "entries: 1" (cell-entry "nodecode-sealed" "sealed" "status")))
    (is (search "rotated the key" (cell-entry "nodecode-sealed" "sealed" "rotate")))
    (is (equal "sk-1" (cell-entry "nodecode-sealed" "sealed" "value DEMO_TOKEN")) "the value survives a rotation")
    (is (search "loaded 1 variables" (cell-entry "nodecode-sealed" "sealed" "load")))
    (is (search "put 1 variables back" (cell-entry "nodecode-sealed" "sealed" "unload")))
    (is (search "removed DEMO_TOKEN" (cell-entry "nodecode-sealed" "sealed" "unset DEMO_TOKEN")))
    (is (search "unknown /sealed verb" (cell-entry "nodecode-sealed" "sealed" "nope")) "a verb it does not know is said so, not signalled"))))
