;;;; wire.lisp --- the local browser link: a WebSocket to Chrome's DevTools port,
;;;; and the page channel the editor opens on itself.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here is Node-specific: Chrome, Edge and Chromium all speak the same
;;;; DevTools protocol over a WebSocket on a loopback port. A LINK is the whole
;;;; transport, and every part of it that touches the machine or a socket is a
;;;; slot holding a function, so a test drives the cell with a link of its own.
;;;;
;;;;   (link-send LINK METHOD PARAMS SESSION-ID) => the request's result value
;;;;   (link-pump LINK)                            => T when it delivered something
;;;;
;;;; The page side is the connector channel the editor boots with: the connector
;;;; adds one CDP binding, the editor removes it from its global scope and calls
;;;; it back with a random key; every message after that goes through one fixed
;;;; page function, DELIVER, called with data and never code.

(in-package #:nodecode-photofresco)

;;; --- base64, for the handshake's nonce -------------------------------------------

(defparameter *base64-alphabet*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64-encode (octets)
  "OCTETS as standard base64, padded. Only what a handshake key needs."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (with-output-to-string (out)
    (loop for i from 0 below (length octets) by 3
          for a = (aref octets i)
          for b = (if (< (+ i 1) (length octets)) (aref octets (+ i 1)) 0)
          for c = (if (< (+ i 2) (length octets)) (aref octets (+ i 2)) 0)
          for n = (logior (ash a 16) (ash b 8) c)
          do (write-char (char *base64-alphabet* (ldb (byte 6 18) n)) out)
             (write-char (char *base64-alphabet* (ldb (byte 6 12) n)) out)
             (write-char (if (< (+ i 1) (length octets))
                             (char *base64-alphabet* (ldb (byte 6 6) n)) #\=)
                         out)
             (write-char (if (< (+ i 2) (length octets))
                             (char *base64-alphabet* (ldb (byte 6 0) n)) #\=)
                         out))))

;;; --- one WebSocket connection ---------------------------------------------------

(defstruct (ws (:conc-name ws-))
  socket stream)

(defun ws-handshake-key ()
  (let ((octets (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16) (setf (aref octets i) (random 256)))
    (base64-encode octets)))

(defun ws-open (host port path &key (timeout 10))
  "A client WebSocket to HOST:PORT at PATH, its handshake done."
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp))
         (stream nil))
    (sb-bsd-sockets:socket-connect socket
                                    (sb-bsd-sockets:make-inet-address host) port)
    (setf stream (sb-bsd-sockets:socket-make-stream
                  socket :input t :output t :element-type '(unsigned-byte 8)
                  :buffering :none))
    (format stream "GET ~a HTTP/1.1~C~CHost: ~a:~d~C~CUpgrade: websocket~C~CConnection: Upgrade~C~CSec-WebSocket-Key: ~a~C~CSec-WebSocket-Version: 13~C~C~C~C"
            path #\Return #\Linefeed host port #\Return #\Linefeed
            #\Return #\Linefeed #\Return #\Linefeed (ws-handshake-key)
            #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
    (finish-output stream)
    (let ((status (ws-read-line stream timeout)))
      (unless (and status (search " 101 " status))
        (fail "the DevTools port refused the WebSocket handshake (~a)" (or status "no answer"))))
    ;; the response headers, to the blank line
    (loop repeat 32
          do (let ((line (ws-read-line stream timeout)))
               (when (or (null line) (zerop (length line))) (return))))
    (make-ws :socket socket :stream stream)))

(defun ws-read-line (stream timeout)
  (declare (ignore timeout))
  (let ((line (make-string-output-stream)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte) (return (get-output-stream-string line)))
                   ((= byte 10) (return (string-right-trim '(#\Return)
                                                           (get-output-stream-string line))))
                   (t (write-char (code-char byte) line))))))

(defun ws-write (ws octets)
  "One client frame: final, masked, binary-or-text as given by OPCODE."
  (let* ((length (length octets))
         (mask (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4) (setf (aref mask i) (random 256)))
    (let ((header (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (flet ((push1 (n) (vector-push-extend n header)))
        (push1 #x81)
        (cond ((< length 126) (push1 (logior #x80 length)))
              ((< length 65536) (push1 (logior #x80 126)) (push1 (ldb (byte 8 8) length)) (push1 (ldb (byte 8 0) length)))
              (t (push1 (logior #x80 127))
                 (loop for shift from 56 downto 0 by 8 do (push1 (ldb (byte 8 shift) length)))))
        (dotimes (i 4) (push1 (aref mask i))))
      (write-sequence header (ws-stream ws))
      (let ((body (make-array length :element-type '(unsigned-byte 8))))
        (dotimes (i length)
          (setf (aref body i) (logand #xff (logxor (aref octets i) (aref mask (mod i 4))))))
        (write-sequence body (ws-stream ws))
        (finish-output (ws-stream ws))))))

(defun ws-send-text (ws text)
  (ws-write ws (sb-ext:string-to-octets text :external-format :utf-8)))

(defun ws-read-frame (ws)
  "=> (VALUES OPCODE TEXT). OPCODE 1 carries TEXT; 8 is the peer closing."
  (let* ((first (read-byte (ws-stream ws) nil nil))
         (second nil) (length 0))
    (unless first (return-from ws-read-frame (values 8 nil)))
    (setf second (read-byte (ws-stream ws) nil nil))
    (unless second (return-from ws-read-frame (values 8 nil)))
    (let ((masked (logtest #x80 second))
          (declared (logand #x7f second)))
      (setf length (cond ((< declared 126) declared)
                         ((= declared 126) (logior (ash (read-byte (ws-stream ws)) 8)
                                                   (read-byte (ws-stream ws))))
                         (t (let ((n 0))
                              (dotimes (i 8) (setf n (logior (ash n 8) (read-byte (ws-stream ws)))))
                              n))))
      (let ((mask (and masked (let ((m (make-array 4 :element-type '(unsigned-byte 8))))
                                (dotimes (i 4) (setf (aref m i) (read-byte (ws-stream ws))))
                                m)))
            (body (make-array length :element-type '(unsigned-byte 8))))
        (loop for i below length
              for byte = (read-byte (ws-stream ws) nil 0)
              do (setf (aref body i) (if mask (logand #xff (logxor byte (aref mask (mod i 4)))) byte)))
        (let ((opcode (logand #x0f first)))
          (values opcode (unless (= opcode 8)
                           (sb-ext:octets-to-string body :external-format :utf-8))))))))

(defun ws-close (ws)
  (ignore-errors (sb-bsd-sockets:socket-close (ws-socket ws))))

;;; --- a browser of its own --------------------------------------------------------

(defparameter *browser-names*
  '("google-chrome" "google-chrome-stable" "chromium" "chromium-browser" "microsoft-edge"))

(defun executable-path (name)
  "NAME's path on the PATH, or NIL."
  (loop for folder in (uiop:split-string (or (uiop:getenv "PATH") "")
                                          :separator '(#\:))
        for candidate = (merge-pathnames name (uiop:ensure-directory-pathname
                                               (if (plusp (length folder)) folder "/")))
        when (probe-file candidate) return (namestring candidate)))

(defun find-browser ()
  "The browser to drive: the `browser' setting, else one installed for the user."
  (let ((named (and (fboundp 'setting) (setting :browser))))
    (cond ((and (stringp named) (plusp (length named)) (probe-file named)) (namestring named))
          ((and (stringp named) (plusp (length named)) (executable-path named)))
          ((find-if #'executable-path (mapcar #'string *browser-names*)))
          (t (fail "no Chrome, Edge or Chromium found; name one in the browser setting")))))

(defun browser-args (profile)
  (list "--user-data-dir=" (namestring profile)
        "--remote-debugging-port=0" "--no-first-run"
        "--no-default-browser-check" "about:blank"))

(defun devtools-port (profile limit)
  "The port the browser wrote to its profile, once it has."
  (let ((file (merge-pathnames "DevToolsActivePort" profile)))
    (loop repeat (* 4 limit)
          for text = (and (probe-file file) (uiop:read-file-string file))
          do (when (and text (plusp (length text)))
               (let ((line (first (uiop:split-string text :separator '(#\Newline)))))
                 (when (and line (every #'digit-char-p line)) (return (parse-integer line)))))
             (sleep 0.25)
          finally (fail "the browser did not open its DevTools port within ~d s" limit))))

(defun http-get-text (host port path)
  "A plain GET of a loopback DevTools endpoint, as text."
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
         (stream nil))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-connect socket (sb-bsd-sockets:make-inet-address host) port)
           (setf stream (sb-bsd-sockets:socket-make-stream
                         socket :input t :output t :element-type '(unsigned-byte 8) :buffering :none))
           (format stream "GET ~a HTTP/1.0~C~CHost: ~a:~d~C~C~C~C"
                   path #\Return #\Linefeed host port #\Return #\Linefeed #\Return #\Linefeed)
           (finish-output stream)
           (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
             (loop for byte = (read-byte stream nil nil)
                   while byte do (vector-push-extend byte octets))
             (let ((text (sb-ext:octets-to-string octets :external-format :utf-8)))
               (or (second (uiop:split-string text :separator '("\r\n\r\n")))
                   (second (uiop:split-string text :separator '(#\Newline #\Newline)))))))
      (when stream (ignore-errors (close stream)))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun devtools-websocket-url (port)
  (let* ((text (http-get-text "127.0.0.1" port "/json/version"))
         (json (and text (nlk:decode-json text))))
    (or (nlk:json-value json :string "webSocketDebuggerUrl")
        (fail "the browser named no DevTools WebSocket"))))

;;; --- CDP over that socket -------------------------------------------------------

(defstruct (link (:conc-name link-))
  (send #'error :type function)     ; (link method params session-id) => result value
  (pump #'error :type function)     ; (link) => T when it delivered a message
  (close (lambda (link) (declare (ignore link))) :type function)
  (queue '())                       ; page messages not yet read by a call
  (pending (make-hash-table))       ; CDP request id => result
  (key nil)                         ; the page channel's random key
  (context-id nil)                  ; the execution context that announced it
  (target-id nil)
  (cdp-session nil)
  (session-key nil)                 ; the editor's per-pairing session key
  (seq 0)
  (origin nil)
  (ws nil))

(defun cdp-message (link text)
  "One DevTools message: a response is filed by id, an event dispatched."
  (let ((message (nlk:decode-json text)))
    (cond
      ((nlk:json-value message :string "method")
       (page-event link (nlk:json-value message :string "method")
                   (nlk:json-value message "params")
                   (nlk:json-value message :string "sessionId")))
      ((nlk:json-value message :integer "id")
       (setf (gethash (nlk:json-value message :integer "id") (link-pending link))
             (cons (nlk:json-value message "result") (nlk:json-value message "error"))))
      (t nil))
    message))

(defun page-event (link method params session-id)
  "The editor's own events: a channel announcement, a message, a closed tab."
  (when (and (link-cdp-session link) session-id
             (not (equal session-id (link-cdp-session link))))
    (return-from page-event nil))
  (cond
    ((and method (string= method "Runtime.bindingCalled"))
     (let ((payload (nlk:json-value params :string "payload")))
       (when payload
         (let ((message (nlk:decode-json payload)))
           (when (nlk:json-value message :integer "v")
             (page-message link message (nlk:json-value params :integer "executionContextId")))))))
    ((and method (string= method "Target.detachedFromTarget"))
     (setf (link-cdp-session link) nil))
    (t nil)))

(defun page-message (link message context-id)
  "A message from the editor channel: its announcement, a response, an event."
  (let ((type (nlk:json-value message :string "type")))
    (cond
      ((equal type "ready")
       (let ((key (nlk:json-value message :string "key")))
         (when (and key (= 64 (length key)))
           (setf (link-key link) key)
           (setf (link-context-id link) context-id))))
      ((and (member type '("response" "event") :test #'equal)
            (or (link-context-id link) (null context-id))
            (or (null context-id) (= context-id (link-context-id link))))
       (setf (link-queue link) (append (link-queue link) (list message))))
      (t nil))))

(defun real-send (link method params session-id)
  (let* ((id (incf (link-seq link)))
         (text (nlk:encode-json-object
                (let ((object (nlk:json-object "id" id "method" method)))
                  (when params (setf (gethash "params" object) params))
                  (when session-id (setf (gethash "sessionId" object) session-id))
                  object))))
    (ws-send-text (ws-of link) text)
    (loop repeat 20000
          for answer = (gethash id (link-pending link))
          do (cond (answer (remhash id (link-pending link))
                            (let ((result (car answer)) (error (cdr answer)))
                              (when error
                                (fail "the browser refused ~a: ~a" method
                                      (or (nlk:json-value error :string "message") "no reason")))
                              (return result)))
                   ((funcall (link-pump link) link))
                   (t (fail "the browser did not answer ~a within its time" method))))))

(defun real-pump (link)
  "Read one DevTools frame; T when it carried a message."
  (multiple-value-bind (opcode text) (ws-read-frame (ws-of link))
    (cond ((= opcode 8) (setf (link-key link) nil) t)
          ((and text (plusp (length text))) (cdp-message link text) t)
          (t nil))))

(defun ws-of (link) (link-ws link))

(defun (setf ws-of) (ws link) (setf (link-ws link) ws))

;;; --- opening the editor's tab -----------------------------------------------------

(defparameter *binding-name* "__photofrescoConnector")

(defparameter *deliver*
  (concatenate 'string
               "function (envelope) { const port = globalThis.photofrescoConnector; "
               "return port && typeof port.receive === 'function' ? port.receive(envelope) : 'no_channel'; }")
  "The only function ever run in the editor page; its argument is data, never code.")

(defun json-encode (object) (nlk:encode-json-object object))

(defun cdp-call (link method &key params)
  (funcall (link-send link) link method params (link-cdp-session link)))

(defun open-editor (&key (origin nil))
  "Launch the connector's own browser, open the editor in a tab of its own,
wait for the page channel, and answer the LINK. Nothing of the user's own
browser or profile is touched: the profile is this cell's."
  (let* ((root (nlk:home "photofresco/"))
         (profile (merge-pathnames "browser-profile/" root))
         (exe (find-browser)))
    (ensure-directories-exist (merge-pathnames "DevToolsActivePort" profile))
    (ignore-errors (delete-file (merge-pathnames "DevToolsActivePort" profile)))
    (uiop:run-program (cons exe (browser-args profile))
                      :ignore-error-status t :output nil :error-output nil :wait nil)
    (let* ((port (devtools-port profile 60))
           (ws (ws-open "127.0.0.1" port
                        (devtools-path (devtools-websocket-url port))))
           (link (make-link :send #'real-send :pump #'real-pump :close #'ignore
                            :origin (or origin (setting :editor-url)))))
      (setf (ws-of link) ws)
      (let* ((created (cdp-call link "Target.createTarget" :params (json-object "url" "about:blank")))
             (target (nlk:json-value created :string "targetId"))
             (attached (cdp-call link "Target.attachToTarget"
                                  :params (json-object "targetId" target "flatten" t)))
             (session (nlk:json-value attached :string "sessionId")))
        (setf (link-target-id link) target)
        (setf (link-cdp-session link) session)
        (cdp-call link "Runtime.enable")
        (cdp-call link "Page.enable")
        (cdp-call link "Runtime.addBinding" :params (json-object "name" *binding-name*))
        (cdp-call link "Page.navigate" :params (json-object "url" (launch-url link)))
        (unless (wait-for link (lambda () (and (link-key link) (link-context-id link))) 600)
          (fail "the editor did not open its connector channel"))
        link))))

(defun devtools-path (url)
  (let ((start (or (search "://" url) (return-from devtools-path "/"))))
    (let ((slash (position #\/ url :start (+ start 3))))
      (if slash (subseq url slash) "/"))))

(defun json-object (&rest keys-and-values)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (gethash key object) value))
    object))

(defun launch-url (link)
  (format nil "~a~a~:[?~;&~]pf_ref=mcp" (link-origin link)
          (if (and (plusp (length (link-origin link)))
                   (char= #\/ (char (link-origin link) (1- (length (link-origin link))))))
              "" "/")
          (search "?" (link-origin link))))

(defun wait-for (link predicate tries)
  (loop repeat tries
        do (when (funcall predicate) (return t))
           (unless (funcall (link-pump link) link) (sleep 0.05))
        finally (return (funcall predicate))))
