;;;; wire.lisp --- the two networks, and what comes back from them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two peers: Colab's own assignment API on colab.research.google.com (the
;;;; /tun/m endpoints the Colab web app uses) and the runtime proxy Colab
;;;; hands back with an assignment, a Jupyter server behind one short-lived
;;;; token. The token for Google is the operator's own OAuth grant, refreshed
;;;; here from a refresh token and kept in memory; the runtime token comes
;;;; with the assignment and is never written anywhere. Every byte goes
;;;; through NLK:HTTP or KERNEL-EXECUTE, so a test stubs those two names.

(in-package #:nodecode-colab)

(defparameter +colab-url+ "https://colab.research.google.com"
  "Colab's own address, where runtimes are assigned, kept alive and released.")
(defparameter +token-url+ "https://oauth2.googleapis.com/token"
  "Google's token endpoint: where the refresh token becomes an access token.")
(defparameter +xssi+ ")]}'"
  "The prefix Colab puts on its JSON so a script cannot read it from a page.")
(defparameter +client-agent+ "nodecode-colab"
  "What this cell calls itself to Colab; the original plugin's own name.")
(defparameter +accelerators+ '("T4" "L4" "A100" "H100" "NONE")
  "The accelerators a run may ask for; T4 is on the free tier.")
(defparameter +tpu+ '("V5E1" "V6E1")
  "The accelerators Colab calls TPUs.")
(defparameter +high-mem-only+ '("L4" "V5E1" "V6E1")
  "The accelerators that are always the high-RAM shape.")

(define-condition runtime-gone (error)
  ((detail :initarg :detail :reader runtime-gone-detail))
  (:report (lambda (condition stream) (write-string (runtime-gone-detail condition) stream)))
  (:documentation "The Colab VM behind a run was reclaimed or reassigned."))

(defun gone (format-control &rest arguments)
  "Signal RUNTIME-GONE: the run's VM is no longer there."
  (error 'runtime-gone :detail (apply #'format nil format-control arguments)))

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun root ()
  "Colab's address this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +colab-url+)))

(defun url-encode (text)
  "TEXT with everything but an unquoted character percent-encoded."
  (with-output-to-string (out)
    (loop for char across (princ-to-string text)
          do (if (or (alphanumericp char) (find char "-._~/"))
                 (write-char char out)
                 (format out "%~2,'0X" (char-code char))))))

(defun b64 (text)
  "TEXT, UTF-8 octets, as standard base64."
  (cl-base64:usb8-array-to-base64-string
   (babel:string-to-octets text :encoding :utf-8)))

(defun unb64 (text)
  "Base64 TEXT back as a string."
  (babel:octets-to-string (cl-base64:base64-string-to-usb8-array text) :encoding :utf-8))

;;; --- the Google grant --------------------------------------------------------

(defvar *token* nil
  "The access token this cell last got from Google, as (TOKEN . EXPIRES-AT).")
(defvar *token-lock* (bt2:make-lock :name "colab token")
  "One refresh at a time: a hundred turns of a session share the one token.")

(defun refresh-access-token ()
  "The access token a refresh token buys, cached until a minute before it lapses."
  (let ((client (nonempty (setting :client-id)))
        (secret (nonempty (setting :client-secret)))
        (refresh (nonempty (setting :refresh-token))))
    (unless (and client secret refresh)
      (fail "colab: a refresh_token needs client_id and client_secret in the colab section"))
    (let ((body (format nil "grant_type=refresh_token&client_id=~a&client_secret=~a&refresh_token=~a"
                        (url-encode client) (url-encode secret) (url-encode refresh))))
      (multiple-value-bind (text status)
          (nlk:http :post +token-url+
                    :headers '(("content-type" . "application/x-www-form-urlencoded"))
                    :content body :timeout 30)
        (let ((data (and (stringp text) (ignore-errors (nlk:decode-json text)))))
          (unless (and (integerp status) (< status 300))
            (fail "colab: Google refused the refresh token: HTTP ~d ~a" status
                  (or (nlk:json-value data :text "error_description") "")))
          (let ((token (nlk:json-value data :text "access_token")))
            (unless token (fail "colab: Google answered no access_token"))
            (setf *token* (cons token (+ (get-universal-time)
                                         (max 60 (- (or (nlk:json-value data :integer "expires_in") 3600)
                                                     60)))))
            token))))))

(defun google-token ()
  "A Google access token with the Colaboratory scope."
  (bt2:with-lock-held (*token-lock*)
    (or (nonempty (setting :access-token))
        (let ((cached *token*))
          (if (and cached (> (cdr cached) (get-universal-time)))
              (car cached)
              (refresh-access-token))))))

;;; --- the Colab tunnel --------------------------------------------------------

(defun decode-tunnel (text)
  "TEXT answered by Colab as the value it means, the XSSI prefix taken off."
  (when (stringp text)
    (let ((trimmed (string-left-trim '(#\Newline #\Return #\Space #\Tab) text)))
      (when (and (>= (length trimmed) (length +xssi+))
                 (string= +xssi+ trimmed :end2 (length +xssi+)))
        (setf trimmed (string-left-trim '(#\Newline #\Return) (subseq trimmed (length +xssi+)))))
      (when (plusp (length trimmed)) (nlk:decode-json trimmed)))))

(defun error-text (text)
  "What a refused answer says, in the one line the model reads."
  (let ((data (and (stringp text) (ignore-errors (nlk:decode-json text)))))
    (or (nlk:json-value (nlk:json-value data :object "error") :text "message")
        (nlk:json-value data :text "error")
        (and (stringp text) (subseq text 0 (min 200 (length text)))))))

(defun tunnel (method path &key query headers content (timeout 60))
  "One call to Colab's assignment API on PATH => the decoded answer, or NIL.
A refused status is COLAB-ERROR; the message is what the operator repairs."
  (let* ((url (concatenate 'string (root) path
                            (if query (concatenate 'string "?" query) "")
                            (if query "&" "?") "authuser=0"))
         (all (cons (cons "authorization" (format nil "Bearer ~a" (google-token)))
                    (cons (cons "accept" "application/json")
                          (cons (cons "x-colab-client-agent" +client-agent+) headers)))))
    (multiple-value-bind (text status)
        (nlk:http method url :headers all :content content :timeout timeout)
      (cond ((and (integerp status) (< status 300)) (decode-tunnel text))
            ((eql status 412)
             (fail "colab: this account already holds its maximum number of runtimes; release one (colab_runtimes, colab_release_runtime) or wait for one to end"))
            ((eql status 401)
             (fail "colab: HTTP 401, Google refused the token; set access_token (or refresh_token with client_id and client_secret) again in the colab section"))
            ((eql status 403)
             (fail "colab: HTTP 403, this account may not use Colab: ~a" (error-text text)))
            (t (fail "colab: HTTP ~d on ~a~@[: ~a~]" status path
                     (let ((said (error-text text))) (and said (plusp (length said)) said))))))))

(defun notebook-hash ()
  "A random 44-character name of the shape Colab gives a notebook."
  (let ((chars "abcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat 44 collect (char chars (random (length chars)))) 'string)))

;;; --- an assigned runtime, and the proxy it hands back -------------------------

(defstruct (runtime (:constructor make-runtime (&key endpoint accelerator url token)))
  endpoint accelerator url token)

(defun runtime-of (data accelerator)
  "The runtime an assignment DATA describes."
  (let ((info (nlk:json-value data :object "runtimeProxyInfo")))
    (make-runtime :endpoint (nlk:json-value data :text "endpoint")
                  :accelerator (or (nlk:json-value data :text "accelerator") accelerator)
                  :url (string-right-trim "/" (or (nlk:json-value info :text "url") ""))
                  :token (nlk:json-value info :text "token"))))

(defun list-runtimes ()
  "The runtimes assigned to the account, as plists."
  (let ((data (tunnel :get "/tun/m/assignments")))
    (loop for item across (or (nlk:json-value data :array "assignments") #())
          collect (list :endpoint (nlk:json-value item :text "endpoint")
                        :accelerator (nlk:json-value item :text "accelerator")))))

(defun assign-runtime (accelerator &key high-mem)
  "Ask Colab for a runtime, doing the two-call dance its token needs."
  (let* ((accelerator (string-upcase (or accelerator "T4")))
         (query (with-output-to-string (out)
                  (format out "nbh=~a" (notebook-hash))
                  (unless (string= accelerator "NONE")
                    (format out "&variant=~a&accelerator=~a"
                            (if (member accelerator +tpu+ :test #'string=) "TPU" "GPU") accelerator))
                  (when (and high-mem (not (member accelerator +high-mem-only+ :test #'string=)))
                    (format out "&shape=hm"))))
         (first (tunnel :get "/tun/m/assign" :query query :timeout 120)))
    (if (nlk:json-value first :text "endpoint")
        (runtime-of first accelerator)
        (let ((token (nlk:json-value first :text "token")))
          (unless token (fail "colab: Colab did not return an assignment token"))
          (runtime-of (tunnel :post "/tun/m/assign" :query query
                              :headers (list (cons "x-goog-colab-token" token))
                              :timeout 240)
                      accelerator)))))

(defun keep-alive-p (runtime)
  "Refresh RUNTIME's idle timer. NIL when the VM is gone."
  (handler-case
      (progn (tunnel :get (format nil "/tun/m/~a/keep-alive/" (url-encode (runtime-endpoint runtime)))
                     :headers '(("x-colab-tunnel" . "Google")) :timeout 15)
             t)
    (runtime-gone () nil)
    (error () nil)))

(defun release-runtime (endpoint)
  "Unassign ENDPOINT, freeing its compute."
  (let* ((path (format nil "/tun/m/unassign/~a" (url-encode endpoint)))
         (first (tunnel :get path))
         (token (nlk:json-value first :text "token")))
    (unless token (fail "colab: Colab did not return an unassign token"))
    (tunnel :post path :headers (list (cons "x-goog-colab-token" token)))
    endpoint))

(defun proxy-url (runtime path &key extra)
  "PATH on RUNTIME's proxy, with the runtime token and the account it belongs to."
  (format nil "~a~a?authuser=0&colab-runtime-proxy-token=~a~@[&~a~]"
          (runtime-url runtime) path (runtime-token runtime) extra))

(defun proxy (runtime method path &key content headers extra (timeout 120))
  "One call to the runtime's Jupyter server => its body as text. A refused
token or a missing server is RUNTIME-GONE: the VM was taken away."
  (multiple-value-bind (body status)
      (nlk:http method (proxy-url runtime path :extra extra)
                :headers (cons (cons "x-colab-runtime-proxy-token" (runtime-token runtime))
                               (cons (cons "x-colab-client-agent" +client-agent+) headers))
                :content content :timeout timeout)
    (cond ((and (integerp status) (< status 300)) body)
          ((member status '(401 403 404 410)) (gone "colab: the runtime is gone (HTTP ~d)" status))
          (t (fail "colab: runtime proxy HTTP ~d on ~a~@[: ~a~]" status path
                   (let ((said (error-text body))) (and said (plusp (length said)) said)))))))

(defun decode-body (text)
  "An HTTP body as the value it encodes, or NIL."
  (and (stringp text) (plusp (length (nlk:trimmed text)))
       (ignore-errors (nlk:decode-json text))))

(defun kernel-id (runtime)
  "The kernel of RUNTIME's Jupyter server, started if there is none."
  (let ((kernels (nlk:json-value (decode-body (proxy runtime :get "/api/kernels")) :array)))
    (if (and kernels (plusp (length kernels)))
        (nlk:json-value (aref kernels 0) :text "id")
        (nlk:json-value (decode-body (proxy runtime :post "/api/kernels"
                                            :headers '(("content-type" . "application/json"))
                                            :content (nlk:encode-json-object
                                                      (nlk:make-json-object "name" "python3"))))
                        :text "id"))))

(defun put-file (runtime path text)
  "Write TEXT to PATH on the runtime, through its Jupyter contents API."
  (proxy runtime :put (format nil "/api/contents/~a" (url-encode path))
         :headers '(("content-type" . "application/json"))
         :content (nlk:encode-json-object
                   (nlk:make-json-object "type" "file" "format" "base64" "content" (b64 text)))))

(defun entries (runtime path)
  "PATH on the runtime as a list of (:NAME :TYPE :SIZE) plists; NIL when it is not there."
  (handler-case
      (let ((data (decode-body (proxy runtime :get (format nil "/api/contents/~a" (url-encode path))
                                      :extra "content=1"))))
        (loop for entry across (or (nlk:json-value data :array "content") #())
              collect (list :name (nlk:json-value entry :text "name")
                            :type (nlk:json-value entry :text "type")
                            :size (or (nlk:json-value entry :integer "size") 0))))
    (runtime-gone () (signal-runtime-gone))))

(defun signal-runtime-gone ()
  (gone "colab: the runtime is gone"))

(defun get-file (runtime path)
  "PATH on the runtime, as text."
  (proxy runtime :get (format nil "/api/contents/~a" (url-encode path))
         :extra "content=1&format=base64")
  (let ((data (decode-body (proxy runtime :get (format nil "/api/contents/~a" (url-encode path))
                                  :extra "content=1&format=base64"))))
    (unb64 (or (nlk:json-value data :text "content") ""))))

(defun interrupt-kernel (runtime kernel)
  "Ask the runtime's Jupyter server to interrupt KERNEL."
  (handler-case
      (proxy runtime :post (format nil "/api/kernels/~a/interrupt" (url-encode kernel)))
    (error () nil)))

;;; --- running code in the kernel ----------------------------------------------

(defun ws-url (runtime)
  "RUNTIME's proxy as a websocket address."
  (let ((url (runtime-url runtime)))
    (cond ((eql 0 (search "https://" url)) (concatenate 'string "wss://" (subseq url 8)))
          ((eql 0 (search "http://" url)) (concatenate 'string "ws://" (subseq url 7)))
          (t url))))

(defun kernel-execute (runtime kernel code &key (timeout 3600) (on-output (lambda (text) (declare (ignore text)))))
  "Run CODE in KERNEL, handing every stdout chunk to ON-OUTPUT. => the
error text a failing cell raised, or NIL. The one websocket this cell opens:
Jupyter speaks execute_request over it and nothing else here does."
  (let* ((session (notebook-hash))
         (id (notebook-hash))
         (done (bt2:make-semaphore :name "colab kernel"))
         (failure nil)
         (url (format nil "~a/api/kernels/~a/channels?session_id=~a&authuser=0&colab-runtime-proxy-token=~a"
                      (ws-url runtime) kernel session (runtime-token runtime)))
         (ws (wsd:make-client url
                               :additional-headers
                               (list (cons "X-Colab-Runtime-Proxy-Token" (runtime-token runtime))
                                     (cons "X-Colab-Client-Agent" +client-agent+))
                               :max-length (* 64 1024 1024))))
    (unwind-protect
         (progn
           (wsd:on :message ws
                   (lambda (message)
                     (let* ((text (if (stringp message) message (princ-to-string message)))
                            (msg (ignore-errors (nlk:decode-json text)))
                            (parent (nlk:json-value msg :object "parent_header")))
                       (when (and msg (equal id (nlk:json-value parent :text "msg_id")))
                         (let ((kind (nlk:json-value (nlk:json-value msg :object "header") :text "msg_type"))
                               (content (nlk:json-value msg :object "content")))
                           (cond ((equal kind "stream")
                                  (ignore-errors
                                    (funcall on-output (or (nlk:json-value content :text "text") ""))))
                                 ((equal kind "error")
                                  (setf failure (format nil "~a: ~a"
                                                        (nlk:json-value content :text "ename")
                                                        (nlk:json-value content :text "evalue"))))
                                 ((and (equal kind "status")
                                       (equal (nlk:json-value content :text "execution_state") "idle"))
                                  (ignore-errors (bt2:signal-semaphore done)))))))))
           (wsd:start-connection ws)
           (wsd:send ws
                     (nlk:encode-json-object
                      (nlk:make-json-object
                       "header" (nlk:make-json-object "msg_id" id "username" "nodecode"
                                                       "session" session "msg_type" "execute_request"
                                                       "version" "5.3")
                       "parent_header" (nlk:make-json-object)
                       "metadata" (nlk:make-json-object)
                       "channel" "shell"
                       "buffers" (vector)
                       "content" (nlk:make-json-object "code" code "silent" :false
                                                        "store_history" :false
                                                        "user_expressions" (nlk:make-json-object)
                                                        "allow_stdin" :false "stop_on_error" t))))
           (unless (bt2:wait-on-semaphore done :timeout timeout)
             (fail "colab: the kernel did not finish within ~ds" timeout))
           failure)
      (ignore-errors (wsd:close-connection ws)))))
