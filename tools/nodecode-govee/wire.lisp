;;;; wire.lisp --- the one network seam, the key, and what Govee's answers mean.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every byte this cell moves goes through GOVEE-REQUEST, so a test stubs the
;;;; one name and never touches the network. Govee answers HTTP 200 with a
;;;; "code" member of its own as well, so both are read: a status that is not
;;;; 2xx and a code that is not 200 are the same refusal to the model, worded
;;;; with what to do about it. Refusals carry no response body and no key.

(in-package #:nodecode-govee)

(defparameter +base-url+ "https://openapi.api.govee.com/router/api/v1"
  "Govee's developer API: the one host a call leaves for.")

(defparameter +endpoints+ '("/user/devices" "/device/state" "/device/scenes" "/device/control")
  "The four endpoints the original used, and the only ones this cell posts to.")

(defparameter +max-bytes+ 2000000
  "How large one Govee answer may be before it is refused: a device list is
the biggest thing the API returns and it is far under this.")

(defparameter +read-back-delay+ 2.1
  "Seconds before each state read-back. The cloud reports a change shortly
after it lands, and Govee allows 30 state reads a minute per device, so two
reads two seconds apart stay inside the limit.")

(defvar *sleeper* #'sleep
  "How a read-back waits. A test binds this to something that does not, so the
one thing a command spends is the two requests it makes.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun encode (object)
  "OBJECT as the JSON text a tool result carries: the model reads the same
members the original's tool answered with."
  (nlk:encode-json-object object))

(defun api-key ()
  "The key the section holds inline or names by variable, else GOVEE_API_KEY,
which is where the original read it."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "GOVEE_API_KEY"))))

(defun check-config ()
  "Refuse a section that could reach Govee with no key, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "govee: this cell needs a Govee developer API key, and none is set: export "
                                 "GOVEE_API_KEY, or paste the key in \"api_key\" (or name its variable in "
                                 "\"api_key_env\") in the govee section. A key is made in the Govee Home app "
                                 "under Profile > Settings > Apply for API Key."))))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +base-url+)))

(defun request-id ()
  "A fresh request id: sixty-four random bits in the shape Govee's own
examples send, because every device call carries one."
  (format nil "~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x"
          (random (expt 2 32)) (random (expt 2 16)) (random (expt 2 16))
          (random (expt 2 16)) (random (expt 2 48))))

(defun request (endpoint &optional payload)
  "ENDPOINT on the configured root, with PAYLOAD as its JSON body when given
=> the decoded answer object. A transport failure, a status Govee refused, or
an answer whose own code is not 200 signals GOVEE-ERROR with the one line the
model can act on."
  (unless (member endpoint +endpoints+ :test #'string=)
    (fail "govee: ~a is not one of the endpoints this cell uses" endpoint))
  (let* ((url (concatenate 'string (root) endpoint))
         (timeout (setting :timeout-seconds))
         (headers (list (cons "govee-api-key" (or (api-key) ""))
                        (cons "accept" "application/json")))
         (text nil)
         (status nil)
         (truncated nil))
    (handler-case
        (multiple-value-setq (text status truncated)
          (if payload
              (nlk:http :post url
                        :headers (cons (cons "content-type" "application/json") headers)
                        :content (nlk:encode-json-object payload)
                        :timeout timeout
                        :connect-timeout (min 10 timeout)
                        :max-bytes +max-bytes+)
              (nlk:http :get url
                        :headers headers
                        :timeout timeout
                        :connect-timeout (min 10 timeout)
                        :max-bytes +max-bytes+)))
      (error (condition)
        (fail "govee ~a: ~a. A command may have been delivered: read the state before sending it again"
              endpoint (nle:transport-failure-label condition url))))
    (when truncated
      (fail "govee ~a: the answer passed ~d bytes and was cut" endpoint +max-bytes+))
    (cond ((and (integerp status) (<= 200 status 299)) nil)
          ((member status '(401 403))
           (fail "govee ~a: the API key was refused (HTTP ~a); put a usable key in GOVEE_API_KEY, or in the govee section's api_key in /settings"
                 endpoint status))
          ((eql status 429)
           (fail "govee ~a: rate limit reached (HTTP 429); Govee allows 30 state and scene reads a minute per device, so wait before trying again and do not resend the command on your own"
                 endpoint))
          (t (fail "govee ~a: HTTP ~a" endpoint status)))
    (let ((answer (ignore-errors (nlk:decode-json text))))
      (unless (hash-table-p answer)
        (fail "govee ~a: the answer was not JSON" endpoint))
      (let ((code (nlk:json-value answer :any "code")))
        (unless (equal "200" (princ-to-string (or code "")))
          (fail "govee ~a: Govee answered code ~a~@[ (~a)~]"
                endpoint (or code "none")
                (or (nlk:json-value answer :string "msg")
                    (nlk:json-value answer :string "message"))))
        answer))))
