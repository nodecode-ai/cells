;;;; cell.lisp --- /relay, its two tools, and the listener it starts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The page stays where it is, on loopback; this cell binds a second socket
;;;; on the address the operator named and answers every request through it
;;;; only for a password session or a paired device. /relay is the machine's
;;;; own side of that: the address to open, a code to read out, the devices
;;;; let in, the password, and signing everyone out.

(in-package #:nodecode-relay)

(defvar *store* nil "The gate's state while the cell runs.")
(defvar *listener* nil "The socket in front of the page while the cell runs.")

(defun state-file ()
  (nlk:home "relay/state.sexp"))

(defun running-p ()
  (and *listener* (listener-open *listener*)))

(defun addresses ()
  "The addresses this machine offers, loopback left out."
  (let ((text (ignore-errors
                (uiop:run-program '("hostname" "-I") :output :string :error-output nil))))
    (if text
        (remove-if (lambda (a) (or (uiop:string-prefix-p "127." a)
                                    (find #\: a)
                                    (string= a (setting :bind))))
                   (uiop:split-string (string-trim '(#\Space #\Newline #\Tab) text) :separator " "))
        nil)))

(defun address-text ()
  (let ((port (if (running-p) (listener-port *listener*) 0))
        (hosts (addresses)))
    (format nil "port ~d on ~a~@[ · ~{~a~^, ~}~]~@[ · no password yet, set one at /relay/login on this machine~]"
            port (setting :bind) hosts
            (and *store* (not (password-set-p *store*))))))

(defun answer (request)
  "One request through the relay's socket."
  (let ((peer (request-header request "x-relay-peer")))
    (relay-response *store* request
                    :harness-host (setting :harness-host)
                    :harness-port (setting :harness-port)
                    :trusted-p (and (setting :trust-loopback)
                                    (or (null peer) (uiop:string-prefix-p "127." peer))))))

(defun install ()
  (when (zerop (or (setting :harness-port) 0))
    (fail "harness_port is the port of the page behind the relay; `nodecode web' prints it"))
  (setf *store* (load-store (state-file)))
  (setf *listener* (start-listener (setting :bind) (setting :port) #'answer))
  (nle:keep-running "the relay in front of this machine's page")
  (nle:notice (format nil "relay: listening — ~a" (address-text)) :key "nodecode-relay")
  (nle:on-stop (lambda ()
                 (stop-listener *listener*)
                 (setf *listener* nil)
                 (when *store* (save-store *store* (state-file)))
                 (setf *store* nil)
                 (nle:notice nil :key "nodecode-relay"))))

(defun gate ()
  (or *store* (fail "the relay is not running (/relay start)")))

;;; --- the machine's side ------------------------------------------------------

(defun run-slash (args session-id)
  "One /relay: the address, or a pairing code, the devices, the password, or
signing everyone out."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (verb (if space (subseq text 0 space) text))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (cond
      ((or (string= verb "") (string= verb "status"))
       (format nil "relay: ~a~%~a"
               (if (running-p) "listening" "stopped")
               (if (running-p) (address-text) "start it: set the section, then /relay start")))
      ((string= verb "start")
       (gate)
       (unless (running-p)
         (stop-listener *listener*)
         (setf *listener* (start-listener (setting :bind) (setting :port) #'answer)))
       (format nil "relay: listening — ~a" (address-text)))
      ((string= verb "stop")
       (stop-listener *listener*)
       (setf *listener* nil)
       "relay: stopped · nothing reaches this machine through the relay")
      ((string= verb "pair")
       (format nil "relay: code ~a for ~a — good for five minutes, once; scan or type it at /relay/pair"
               (mint-code (gate))
               (if (plusp (length rest)) rest "a device")))
      ((string= verb "devices")
       (let ((devices (store-devices (gate))))
         (if devices
             (format nil "relay: ~{~{~a ~a (added ~a)~}~^~%~}"
                     (mapcar (lambda (d) (list (plist d :id) (plist d :name)
                                                (plist d :added 0)))
                             devices))
             "relay: no device is paired yet · /relay pair phone")))
      ((string= verb "revoke")
       (if (plusp (length rest))
           (if (revoke-device (gate) rest)
               (format nil "relay: ~a is out" rest)
               (format nil "relay: no device ~a" rest))
           "relay: which device? /relay devices"))
      ((string= verb "password")
       (if (plusp (length rest))
           (progn (set-password (gate) rest)
                  (save-store *store* (state-file))
                  "relay: password set")
           "relay: /relay password SECRET"))
      ((string= verb "signout")
       (sign-out-everywhere (gate))
       (save-store *store* (state-file))
       "relay: every session and every device is out")
      ((string= verb "help")
       "relay: status · start · stop · pair NAME · devices · revoke ID · password SECRET · signout")
      (t (format nil "relay: unknown verb ~s · /relay help" verb)))))

;;; --- what the model may do ---------------------------------------------------

(nlk:define-contract relay-pair
  (:doc "Make a single-use pairing code, good for five minutes, and read it out to
the person at the machine. A paired device gets a token and a session of its own.")
  ("name" :text :default "a device" :doc "what the device is called in the list"))

(nlk:define-contract relay-devices
  (:doc "The devices paired with this machine's relay, each with its id, so one
can be revoked by id.")
  ("filter" :text :default "" :doc "only devices whose name contains this"))

(defun pair-tool (pair)
  (format nil "code ~a for ~a — good for five minutes, once"
          (mint-code (gate)) (relay-pair-name pair)))

(defun devices-tool (devices &aux (only (string-downcase (relay-devices-filter devices))))
  (let ((paired (remove-if-not (lambda (device)
                                 (or (zerop (length only))
                                     (search only (string-downcase (plist device :name)))))
                               (store-devices (gate)))))
    (if paired
        (format nil "~{~a ~a~^; ~}"
                (loop for d in paired append (list (plist d :id) (plist d :name))))
        "no device paired")))

(nle:define-cell relay
  (:section ("relay")
    (:guide "harness_port is the page's own port; port is the relay's; boot opens only the machine's own browser")
    ("bind" :string :default "0.0.0.0" :doc "the address the relay listens on")
    ("port" :integer :default 3443 :min 1 :doc "the relay's own port")
    ("harness_host" :string :default "127.0.0.1" :doc "where the page behind the relay is")
    ("harness_port" :integer :default 3080 :min 1 :doc "the page's own port, which `nodecode web` prints")
    ("trust_loopback" :boolean :default t
                      :doc "a browser on this machine is already you, so it needs no password"))
  (:help :relay
   "relay: reach this machine's page from a phone, behind a password and paired devices"
   "The relay binds a second socket on `bind`:`port` and forwards to the page at
`harness_host`:`harness_port`, which stays on loopback. A request through the
relay is let in only as a password session or a paired device: /relay password
SECRET sets the password from this machine; /relay pair phone makes a
single-use five-minute code; /relay devices lists what is paired and /relay
revoke ID takes one out; /relay signout ends every session and device at once.
A browser on this machine needs none of that while trust_loopback is on.")
  (:tool "relay_pair" (nlk:contract 'relay-pair) 'pair-tool)
  (:tool "relay_devices" (nlk:contract 'relay-devices) 'devices-tool)
  (:command "relay" 'run-slash
            :description "Reach this machine's page from a phone: the address, a pairing code, the devices, the password"
            :session nil)
  (:start #'install))
