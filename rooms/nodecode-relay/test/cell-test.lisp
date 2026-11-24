;;;; cell-test.lisp --- the gate, the pairing codes, and the page behind it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every socket here is on loopback: a stub page stands in for the harness,
;;;; a second listener is the relay itself, and the store is fresh per test.
;;;; Nothing dials out.

(in-package #:nodecode.test)

(define-test-slice "relay" "RELAY-CELL-" :start nodecode-relay:start-cell)

(defun relay-free-port ()
  (let ((socket (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect (usocket:get-local-port socket)
      (usocket:socket-close socket))))

(defun relay-body (text)
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun relay-http (port method path &key headers body)
  "One request to the relay on PORT => (VALUES STATUS HEADERS BODY-TEXT)."
  (let ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let* ((stream (usocket:socket-stream socket))
                (payload (and body (relay-body body)))
                (head (with-output-to-string (out)
                        (format out "~a ~a HTTP/1.1~c~chost: 127.0.0.1~c~c"
                                method path #\Return #\Linefeed #\Return #\Linefeed)
                        (dolist (header headers)
                          (format out "~a: ~a~c~c" (car header) (cdr header)
                                  #\Return #\Linefeed))
                        (when payload
                          (format out "content-length: ~d~c~c" (length payload)
                                  #\Return #\Linefeed))
                        (format out "~c~c" #\Return #\Linefeed))))
           (write-sequence (relay-body head) stream)
           (when payload (write-sequence payload stream))
           (finish-output stream)
           (let* ((line (nodecode-relay::read-line-octets stream))
                  (parts (and line (uiop:split-string line :separator " ")))
                  (status (if parts (parse-integer (second parts) :junk-allowed t) 0))
                  (headers (loop for header = (nodecode-relay::read-line-octets stream)
                                 while (and header (plusp (length header)))
                                 for colon = (position #\: header)
                                 when colon
                                   collect (cons (string-downcase (subseq header 0 colon))
                                                 (string-trim " " (subseq header (1+ colon))))))
                  (length (parse-integer
                           (or (cdr (assoc "content-length" headers :test #'string-equal)) "0")
                           :junk-allowed t))
                  (payload (make-array (or length 0) :element-type '(unsigned-byte 8))))
             (when (and length (plusp length)) (read-sequence payload stream))
             (values status headers
                     (sb-ext:octets-to-string payload :external-format :utf-8))))
      (usocket:socket-close socket))))

(defun relay-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-relay-pair ((relay origin store port) &body body)
  "RELAY in front of a stub page ORIGIN, both on loopback, with a fresh STORE."
  (let ((o (gensym)) (r (gensym)))
    `(let* ((,o (nodecode-relay::start-listener
                 "127.0.0.1" 0
                 (lambda (request)
                   (declare (ignore request))
                   (values 200 '(("content-type" . "text/plain"))
                           "the page behind the relay"))))
            (,port (relay-free-port))
            (,store (nodecode-relay::new-store))
            (,r (nodecode-relay::start-listener
                 "127.0.0.1" ,port
                 (lambda (request)
                   (nodecode-relay::relay-response
                    ,store request
                    :harness-host "127.0.0.1"
                    :harness-port (nodecode-relay::listener-port ,o)
                    :trusted-p nil)))))
       (declare (ignorable ,port))
       (unwind-protect (let ((,relay ,r)) ,@body)
         (nodecode-relay::stop-listener ,r)
         (nodecode-relay::stop-listener ,o)))))

(deftest relay-cell-sends-a-stranger-to-the-sign-in-and-takes-the-password (with-relay-pair (relay origin store port)
  (declare (ignore relay origin))
  (multiple-value-bind (status headers) (relay-http port "GET" "/")
    (is (= 302 status) "a stranger reaches no page")
    (is (equal "/relay/login" (relay-header headers "location")) "and is sent to the sign-in"))
  (multiple-value-bind (status headers body) (relay-http port "GET" "/relay/health")
    (declare (ignore headers))
    (is (= 200 status) "health answers without a session")
    (is (search "\"ok\":true" body) "and says the relay is up")
    (is (search "\"password\":false" body) "with no password yet"))
  (nodecode-relay::set-password store "swordfish")
  (is (nodecode-relay::password-set-p store) "the password is on the relay")
  (multiple-value-bind (status)
      (relay-http port "POST" "/relay/login" :body "password=nope"
                  :headers '(("content-type" . "application/x-www-form-urlencoded")))
    (is (= 401 status) "a wrong password is refused"))
  (setf (nodecode-relay::store-attempts store) nil)
  (let* ((store-2 store)
         (device (nodecode-relay::mint-device store-2 "a browser"))
         (cookie (format nil "~a=~a" nodecode-relay::+session-cookie+
                         (nodecode-relay::session-cookie store-2 device))))
    (multiple-value-bind (status headers body)
        (relay-http port "GET" "/" :headers (list (cons "cookie" cookie)))
      (declare (ignore headers))
      (is (= 200 status) "a session cookie reaches the page")
      (is (equal "the page behind the relay" body) "which is the page behind the relay"))
    (multiple-value-bind (status)
        (relay-http port "GET" "/" :headers (list (cons "cookie" (format nil "~a=00.dead.beef" nodecode-relay::+session-cookie+))))
      (is (= 302 status) "a cookie the store did not sign is a stranger's")))))

(deftest relay-cell-signs-in-with-the-password (with-relay-pair (relay origin store port)
  (declare (ignore relay origin))
  (nodecode-relay::set-password store "swordfish")
  (multiple-value-bind (status headers)
      (relay-http port "POST" "/relay/login" :body "password=swordfish"
                  :headers '(("content-type" . "application/x-www-form-urlencoded")))
    (is (= 303 status) "the password opens a session")
    (let ((cookie (relay-header headers "set-cookie")))
      (is (search nodecode-relay::+session-cookie+ cookie) "the session is the relay's own cookie")
      (is (search "HttpOnly" cookie) "which the page's scripts cannot read")
      (multiple-value-bind (status) (relay-http port "GET" "/" :headers (list (cons "cookie" cookie)))
        (is (= 200 status) "and carries a browser through to the page"))))))

(deftest relay-cell-pairs-a-device-once-and-revokes-it ()
  (let* ((store (nodecode-relay::new-store))
         (code (nodecode-relay::mint-code store)))
    (is (= 8 (length code)) "a pairing code is eight digits")
    (multiple-value-bind (device token) (nodecode-relay::redeem-code store code "phone")
      (is (and device token) "the code buys one device")
      (is (equal "phone" (nodecode-relay::plist device :name)) "named as the operator named it")
      (is (null (nodecode-relay::redeem-code store code "again")) "and a spent code buys nothing")
      (is (nodecode-relay::device-for-token store token) "the token is the device's")
      (is (null (nodecode-relay::device-for-token
                 store (concatenate 'string token "x"))) "and only its exact value is")
      (let* ((request (nodecode-relay::make-request
                        :method "GET" :path "/relay/devices"
                        :headers (list (cons "authorization" (format nil "Bearer ~a" token)))))
             (status (nodecode-relay::relay-response store request)))
        (is (= 200 status) "a paired device reads the device list"))
      (is (equal "phone" (nodecode-relay::revoke-device
                             store (nodecode-relay::plist device :id))) "revoking names what it takes out")
      (is (null (nodecode-relay::device-for-token store token)) "and the token dies with it"))))

(deftest relay-cell-locks-an-address-after-five-wrong-guesses ()
  (let ((store (nodecode-relay::new-store)))
    (loop repeat 5 do (nodecode-relay::note-failure store "10.0.0.7"))
    (is (not (nodecode-relay::locked-p store "10.0.0.7")) "five wrong guesses are still allowed")
    (is (= 5 (nodecode-relay::plist
              (cdr (assoc "10.0.0.7" (nodecode-relay::store-attempts store) :test #'equal))
              :failures 0)) "and are counted")
    (nodecode-relay::note-failure store "10.0.0.7")
    (is (nodecode-relay::locked-p store "10.0.0.7") "the sixth locks that address")
    (is (not (nodecode-relay::locked-p store "10.0.0.8")) "and no other address")
    (nodecode-relay::note-success store "10.0.0.7")
    (is (not (nodecode-relay::locked-p store "10.0.0.7")) "signing in clears the lock")))

(deftest relay-cell-signs-every-device-out-at-once ()
  (let* ((store (nodecode-relay::new-store))
         (code (nodecode-relay::mint-code store)))
    (multiple-value-bind (device token) (nodecode-relay::redeem-code store code "laptop")
      (declare (ignore device))
      (is (nodecode-relay::device-for-token store token) "the device is in")
      (nodecode-relay::sign-out-everywhere store)
      (is (null (nodecode-relay::device-for-token store token)) "signing out everywhere takes it out")
      (is (null (nodecode-relay::store-devices store)) "and leaves no device at all"))))

(deftest relay-cell-commands-the-machine-side ()
  (let ((nodecode-relay::*store* (nodecode-relay::new-store)))
    (is (search "code " (nodecode-relay::run-slash "pair phone" nil)) "the command makes a code")
    (is (search "no device" (nodecode-relay::run-slash "devices" nil)) "and nothing is paired yet")
    (is (search "password set" (nodecode-relay::run-slash "password swordfish" nil)) "the password is set from here")
    (is (nodecode-relay::password-ok-p nodecode-relay::*store* "swordfish") "which the store now holds")
    (is (search "code " (nodecode-relay::pair-tool
                          (nodecode-relay::decode-relay-pair
                           (nlk:json-object "name" "phone")))) "the tool makes one too")
    (is (search "every session" (nodecode-relay::run-slash "signout" nil)) "signing out is one verb")
    (is (search "unknown verb" (nodecode-relay::run-slash "melt" nil)) "and an unknown verb says so")))

(define-cell-lifecycle-tests "relay"
  (:config "bind" "127.0.0.1" "port" (relay-free-port)
           "harness_port" (relay-free-port))
  (:help :relay)
  (:command "relay")
  (:running (is (nle::find-tool "relay_pair") "relay_pair is registered")
            (is (nle::find-tool "relay_devices") "relay_devices is registered")
            (is (nodecode-relay::running-p) "the relay's own socket is open")
            (is (search "listening" (nodecode-relay::run-slash "status" nil)) "the command says where it stands")
            (is (search "code " (nodecode-relay::run-slash "pair phone" nil)) "and hands out a code"))
  (:stopped (is (not (nodecode-relay::running-p)) "stop closes the socket"))
  (:refused ("port" 0) ("harness_port" 0))
  (:idle nodecode-relay::relay-error (nodecode-relay::run-slash "pair phone" nil)))
