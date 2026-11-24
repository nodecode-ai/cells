;;;; cell-test.lisp --- the slots, the pairing, the removal, the words each answer uses.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs a GOWA server, a phone or the network: every request goes
;;;; through NODECODE-GOWA::*HTTP*, a stub that records the call it was handed
;;;; and answers what the test chose, and a pairing QR lands in the run's own
;;;; home. What is under test is what this cell owns -- which route each tool
;;;; calls, what it refuses before calling, and the words it answers with.

(in-package #:nodecode.test)

(define-test-slice "gowa" "GOWA-CELL-" :start nodecode-gowa:start-cell)

(define-cell-lifecycle-tests "gowa"
  (:config "base_url" "http://127.0.0.1:3000")
  (:help :gowa)
  (:command "gowa")
  (:running
   (is (nle::find-tool "gowa_devices") "the slots are a tool the model can call")
   (is (nle::find-tool "gowa_device_login") "so is pairing one")
   (is (nle::find-tool "gowa_device_remove") "and removing one"))
  (:stopped
   (is (null (nle::find-tool "gowa_devices")) "and stop takes all three back"))
  (:refused ("timeout_seconds" 0) ("base_url" "ftp://gowa.example"))
  (:idle nodecode-gowa:gowa-error (nodecode-gowa::devices-text)))

;;; --- the fixture ------------------------------------------------------------------

(defvar *calls* '()
  "Every stubbed request of the test, (METHOD URL . OPTIONS), newest first.")

(defvar *answers* nil
  "What the stubbed *HTTP* answers: a function of METHOD and URL.")

(defmacro with-gowa ((&rest pairs) &body body)
  "BODY with the cell started on its section plus PAIRS, and a stubbed GOWA."
  `(let* ((*calls* '())
          (*answers* (lambda (method url)
                       (declare (ignore method url))
                       (error "the cell made a call no test asked for")))
          (nodecode-gowa::*http*
            (lambda (method url &rest options)
              (push (list* method url options) *calls*)
              (funcall *answers* method url))))
     (with-cell-stop ((gowa-start "base_url" "http://127.0.0.1:3000" ,@pairs))
       ,@body)))

(defun serve (entries)
  "The stub answers the first ENTRY whose needle the url holds, with its values."
  (setf *answers*
        (lambda (method url)
          (declare (ignore method))
          (let ((entry (find-if (lambda (entry) (search (car entry) url)) entries)))
            (unless entry (error "no stub for ~a" url))
            (values-list (second entry))))))

(defun asked ()
  "The stubbed requests, oldest first."
  (reverse *calls*))

(defun path-of (call)
  "The path CALL asked for, without the section's origin."
  (let ((url (second call)))
    (subseq url (length "http://127.0.0.1:3000"))))

(defun sent (call name)
  "CALL's JSON body member NAME, or NIL."
  (let ((content (getf (cddr call) :content)))
    (and content (nlk:json-value (nlk:decode-json content) :string name))))

(defun header (call name)
  (cdr (assoc name (getf (cddr call) :headers) :test #'string-equal)))

(defun wire (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content
   (execute-wire-call "gowa-call" name (nlk:encode-json-object arguments))))

(defmacro with-auth ((value) &body body)
  "BODY with the section's auth_env holding VALUE, and the environment put back."
  (let ((saved (gensym "SAVED"))
        (name "NODECODE_GOWA_TEST_AUTH"))
    `(let ((,saved (uiop:getenv ,name)))
       (unwind-protect (progn (sb-posix:setenv ,name ,value 1) ,@body)
         (if ,saved (sb-posix:setenv ,name ,saved 1) (sb-posix:unsetenv ,name))))))

(defun png-bytes ()
  "The eight bytes of a PNG signature and three more: a QR no real scanner reads."
  #(137 80 78 71 13 10 26 10 1 2 3))

;;; --- what the server holds ---------------------------------------------------------

(deftest gowa-cell-lists-the-slots-it-holds ()
  (with-gowa ("auth_env" "NODECODE_GOWA_TEST_AUTH")
    (with-auth ("Basic dGVzdA==")
      (serve '(("/devices"
                ("{\"results\":[{\"id\":\"one\",\"state\":\"logged_in\",\"phone_number\":\"+15550100001\"},{\"id\":\"two\",\"state\":\"disconnected\"}]}"
                 200))))
      (let ((answer (wire "gowa_devices" (nlk:make-json-object "action" "list"))))
        (is (search "2 device slots" answer) "the answer counts them")
        (is (search "one  logged_in  +15550100001" answer) "a linked slot shows its state and number")
        (is (search "two  disconnected" answer) "an unlinked one just its state")
        (let ((authorization (header (first (asked)) "authorization")))
          (is (equal "Basic dGVzdA==" authorization) "the section's value goes with every call"))))))

(deftest gowa-cell-shows-one-slot-by-its-exact-id ()
  (with-gowa ()
    (serve '(("/devices/one" ("{\"results\":{\"id\":\"one\",\"state\":\"connecting\"}}" 200))))
    (let ((answer (wire "gowa_devices" (nlk:make-json-object "action" "get" "device_id" "one"))))
      (is (search "GOWA slot one: one  connecting" answer) "one slot, named by its id and its state")
      (is (equal "/devices/one" (path-of (first (asked)))) "read from the slot's own route"))))

(deftest gowa-cell-without-a-base-url-refuses-naming-it ()
  (let ((condition (signals-error nlk:config-refusal (gowa-start "timeout_seconds" 15))))
    (is (search "base_url" (princ-to-string condition)) "the refusal names the member to set")
    (is (search "/settings" (princ-to-string condition)) "and says where to set it")))

;;; --- pairing --------------------------------------------------------------------

(deftest gowa-cell-pairs-a-new-slot-with-a-qr ()
  (with-gowa ()
    (serve '(("/devices/demo/login"
              ("{\"results\":{\"device_id\":\"demo\",\"qr_link\":\"http://127.0.0.1:3000/statics/qrcode/demo.png\",\"qr_duration\":30}}"
               200))
             ("/statics/qrcode/demo.png" (#(137 80 78 71 13 10 26 10 1 2 3) 200))
             ("/devices/demo" ("{\"message\":\"device demo not found\"}" 500))
             ("/devices" ("{\"results\":{\"id\":\"demo\"}}" 200))))
    (let ((answer (wire "gowa_device_login"
                        (nlk:make-json-object "action" "qr" "device_id" "demo"
                                               "create_if_missing" t))))
      (is (search "was made, and is pairing" answer) "a slot the server did not hold is created first")
      (is (search "30 seconds" answer) "with the lifetime GOWA gave it")
      (let ((file (nodecode-gowa::qr-path "demo")))
        (is (probe-file file) "the QR image is written where the answer says")
        (is (search file answer) "and named in the answer, so (look ...) finds it")
        (let ((octets (make-array 11 :element-type '(unsigned-byte 8))))
          (with-open-file (in file :element-type '(unsigned-byte 8))
            (read-sequence octets in))
          (is (equalp (png-bytes) octets) "the bytes are GOWA's own, unread")))
      (let ((methods (mapcar #'first (asked))))
        (is (equal '(:get :post :get :get) methods) "the original's four calls, in its order"))
      (is (equal "demo" (sent (second (asked)) "device_id")) "the creation posts the slot's id"))))

(deftest gowa-cell-refuses-to-pair-a-linked-slot-again ()
  (with-gowa ()
    (serve '(("/devices/demo"
              ("{\"results\":{\"id\":\"demo\",\"state\":\"connected\",\"jid\":\"15550100001@s.whatsapp.net\"}}"
               200))))
    (let ((answer (wire "gowa_device_login" (nlk:make-json-object "action" "qr" "device_id" "demo"))))
      (is (search "already linked or pairing" answer) "a linked slot is not paired again")
      (is (= 1 (length (asked))) "and no login was started"))))

(deftest gowa-cell-pairs-with-a-code-for-a-number ()
  (with-gowa ()
    (serve '(("/devices/demo/login/code" ("{\"results\":{\"device_id\":\"demo\",\"pair_code\":\"ABCD-1234\"}}" 200))
             ("/devices/demo" ("{\"results\":{\"id\":\"demo\",\"state\":\"disconnected\"}}" 200))
             ("/devices" ("{\"results\":[]}" 200))))
    (let ((answer (wire "gowa_device_login"
                        (nlk:make-json-object "action" "code" "device_id" "demo"
                                               "phone" "+15550100001"))))
      (is (search "ABCD-1234" answer) "the code the operator types is in the answer")
      (is (search "temporary credential" answer) "with the warning the original gave")
      (let ((route (path-of (third (asked)))))
        (is (equal "/devices/demo/login/code?phone=15550100001" route) "the slot's route, the number as digits")))))

(deftest gowa-cell-refuses-a-number-another-slot-holds ()
  (with-gowa ()
    (serve '(("/devices/demo" ("{\"message\":\"device demo not found\"}" 500))
             ("/devices"
              ("{\"results\":[{\"id\":\"other\",\"jid\":\"15550100001@s.whatsapp.net\"}]}" 200))))
    (let ((answer (wire "gowa_device_login"
                        (nlk:make-json-object "action" "code" "device_id" "demo"
                                               "phone" "+15550100001" "create_if_missing" t))))
      (is (search "already linked to device other" answer) "the number's own slot is named")
      (is (= 2 (length (asked))) "so nothing was created and no code was asked for"))))

(deftest gowa-cell-refuses-a-qr-from-another-origin ()
  (with-gowa ()
    (serve '(("/devices/demo/login" ("{\"results\":{\"qr_link\":\"https://elsewhere.example/qr.png\"}}" 200))
             ("/devices/demo" ("{\"results\":{\"id\":\"demo\",\"state\":\"disconnected\"}}" 200))
             ("/devices" ("{\"results\":[]}" 200))))
    (let ((answer (wire "gowa_device_login" (nlk:make-json-object "action" "qr" "device_id" "demo"))))
      (is (search "different origin" answer) "a link off the server's origin is refused")
      (is (= 2 (length (asked))) "so nothing was downloaded from it"))))

;;; --- removal, and the operator's command -------------------------------------------

(deftest gowa-cell-removes-a-slot-only-when-confirmed ()
  (with-gowa ()
    (serve '(("/devices/demo" ("{\"message\":\"Device removed\"}" 200))))
    (let ((refused (wire "gowa_device_remove" (nlk:make-json-object "device_id" "demo"))))
      (is (search "confirm" refused) "a call without confirm is refused, and names the key")
      (is (null *calls*) "and nothing was sent"))
    (let ((done (wire "gowa_device_remove"
                      (nlk:make-json-object "device_id" "demo" "confirm" t))))
      (is (search "removed slot demo" done) "confirmed, the slot goes")
      (is (eq :delete (first (first (asked)))) "and the server is asked to purge it")
      (is (equal "/devices/demo" (path-of (first (asked)))) "addressed by its exact id"))))

(deftest gowa-cell-command-lists-and-shows-a-slot ()
  (with-gowa ()
    (serve '(("/devices/one" ("{\"results\":{\"id\":\"one\",\"state\":\"logged_in\"}}" 200))
             ("/devices" ("{\"results\":[{\"id\":\"one\",\"state\":\"logged_in\"}]}" 200))))
    (is (search "1 device slot" (nodecode-gowa::run-command "" nil)) "bare, /gowa lists the slots")
    (is (search "GOWA slot one" (nodecode-gowa::run-command "one" nil)) "an argument names one slot")
    (serve '(("/devices/two" ("{\"message\":\"device two not found\"}" 500))))
    (let ((answer (nodecode-gowa::run-command "two" nil)))
      (is (search "HTTP 500" answer) "a slot the server does not hold answers in its words"))))
