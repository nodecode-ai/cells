;;;; ipc.lisp --- Discord's local IPC: framing, the handshake, SET_ACTIVITY.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Discord desktop client listens on a Unix socket of its own (a named
;;;; pipe on Windows): discord-ipc-0 upward, under $XDG_RUNTIME_DIR, $TMPDIR
;;;; or /tmp. A frame is a 32-bit little-endian opcode, a 32-bit little-endian
;;;; body length, then that many bytes of UTF-8 JSON. Opcode 0 is the
;;;; handshake ({"v": 1, "client_id": ...}), opcode 1 a command
;;;; ({"cmd": "SET_ACTIVITY", "args": {...}, "nonce": ...}), opcode 2 a close.
;;;;
;;;; Nothing here dials anything but that socket: the traffic is the card the
;;;; operator's own Discord client shows, and it goes no further than the
;;;; client does. Discord's app id names the card's name and art; the user's
;;;; own application id is the one setting the cell cannot invent.
;;;;
;;;; The transport is four functions in a struct, so a test hands the cell a
;;;; pipe of its own instead of a socket: *OPEN-TRANSPORT* is the seam.

(in-package #:nodecode-discord-presence)

(defun op-code (name)
  "The IPC opcode NAME stands for: the handshake, a command, a close."
  (ecase name (:handshake 0) (:frame 1) (:close 2)))

(defun max-payload ()
  "The largest body an incoming frame may claim, so a hostile length cannot
make the cell allocate more."
  (* 4 1024 1024))

(defun ipc-slots ()
  "Discord's own socket names: discord-ipc-0 through discord-ipc-9."
  10)

(defstruct (transport (:conc-name tp-) (:constructor make-transport))
  "One open IPC pipe: LABEL what it is, SEND a function of octets, RECV a
function of a count and a timeout answering octets or NIL, CLOSE its inverse."
  (label "" :read-only t)
  (send (lambda (octets) (declare (ignore octets))) :read-only t)
  (recv (lambda (count timeout) (declare (ignore count timeout)) nil) :read-only t)
  (close (lambda ()) :read-only t))

;;; --- octets --------------------------------------------------------------------

(defun u32le-octets (number)
  "NUMBER as four little-endian bytes."
  (let ((out (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4 out)
      (setf (aref out i) (ldb (byte 8 (* 8 i)) number)))))

(defun octets-u32le (octets offset)
  "The little-endian 32-bit number at OFFSET."
  (logior (aref octets offset)
          (ash (aref octets (+ offset 1)) 8)
          (ash (aref octets (+ offset 2)) 16)
          (ash (aref octets (+ offset 3)) 24)))

(defun utf8-octets (text)
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun utf8-string (octets)
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defun frame-octets (opcode payload)
  "OPCODE and PAYLOAD, a JSON string, as the eight-byte header and its body."
  (let* ((body (utf8-octets payload))
         (out (make-array (+ 8 (length body)) :element-type '(unsigned-byte 8))))
    (replace out (u32le-octets opcode) :start1 0)
    (replace out (u32le-octets (length body)) :start1 4)
    (replace out body :start1 8)
    out))

(defun read-exactly (stream count &optional timeout)
  "COUNT octets of STREAM, or NIL on end of file or a timeout's silence."
  (let ((buffer (make-array count :element-type '(unsigned-byte 8)))
        (got 0))
    (handler-case
        (loop while (< got count)
              for next = (read-sequence buffer stream :start got)
              do (if (= next got) (return nil) (setf got next)))
      (sb-sys:io-timeout () (return-from read-exactly nil))
      (error () (return-from read-exactly nil)))
    (and (= got count) buffer)))

;;; --- the real socket -------------------------------------------------------------

(defun runtime-dirs ()
  "Where Discord's socket may sit: the profile's runtime dir, its own
/run/user/<uid>, $TMPDIR on macOS, and /tmp."
  (remove nil
          (list (let ((dir (uiop:getenv "XDG_RUNTIME_DIR")))
                  (and dir (plusp (length dir)) dir))
                (format nil "/run/user/~d" (sb-posix:getuid))
                (let ((dir (uiop:getenv "TMPDIR")))
                  (and dir (plusp (length dir)) (string-right-trim "/" dir)))
                "/tmp")))

(defun socket-candidates ()
  "Every socket name the client may be listening on, the operator's own
setting first, then slot 0 upward in each runtime dir."
  (let ((explicit (ignore-errors (nonblank (setting :socket-path)))))
    (append (and explicit (list explicit))
            (loop for slot below (ipc-slots)
                  append (loop for dir in (runtime-dirs)
                               collect (format nil "~a/discord-ipc-~d" dir slot))))))

(defun open-socket-transport (name)
  "NAME as an open transport, or NIL when nothing listens there."
  (handler-case
      (let* ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
             (stream nil)
             (closed nil))
        (sb-bsd-sockets:socket-connect socket name)
        (setf stream (sb-bsd-sockets:socket-make-stream
                      socket :input t :output t :element-type '(unsigned-byte 8)
                      :buffering :full :timeout 1))
        (make-transport
         :label name
         :send (lambda (octets)
                 (write-sequence octets stream)
                 (force-output stream))
         :recv (lambda (count timeout)
                 (declare (ignore timeout))
                 (read-exactly stream count))
         :close (lambda ()
                  (unless closed
                    (setf closed t)
                    (ignore-errors (close stream))
                    (ignore-errors (sb-bsd-sockets:socket-close socket))))))
    (error () nil)))

(defvar *open-transport* #'open-socket-transport
  "How one socket name is opened; a test binds a pipe of its own here.")

;;; --- frames ---------------------------------------------------------------------

(defun send-frame (transport opcode payload)
  "OPCODE with PAYLOAD, one JSON string, on TRANSPORT."
  (funcall (tp-send transport) (frame-octets opcode payload))
  transport)

(defun receive-frame (transport &optional (timeout 1))
  "(values OPCODE TEXT) of one frame, or NIL when none arrives."
  (let ((header (funcall (tp-recv transport) 8 timeout)))
    (when header
      (let ((opcode (octets-u32le header 0))
            (length (octets-u32le header 4)))
        (when (and (<= 0 length) (<= length (max-payload)))
          (let ((body (if (zerop length)
                          (make-array 0 :element-type '(unsigned-byte 8))
                          (funcall (tp-recv transport) length timeout))))
            (when body
              (values opcode (utf8-string body)))))))))

(defun send-handshake (transport client-id)
  "Opcode 0, the one frame Discord wants before any command."
  (send-frame transport (op-code :handshake)
               (nlk:encode-json-object
                (nlk:json-object "v" 1 "client_id" client-id))))

(defun new-nonce ()
  "A per-command correlation string; Discord answers it in the reply."
  (format nil "~36r" (random (expt 2 48))))

(defun send-command (transport cmd args)
  "One opcode 1 command with ARGS."
  (send-frame transport (op-code :frame)
               (nlk:encode-json-object
                (nlk:json-object "cmd" cmd
                                  "args" (or args (nlk:json-object))
                                  "nonce" (new-nonce)))))

(defun activity-object (card)
  "CARD as Discord's activity object: only the members it has."
  (let ((activity (nlk:json-object)))
    (dolist (pair (list (list "details" (getf card :details))
                         (list "state" (getf card :state))
                         (list "large_image" (getf card :large-image))
                         (list "large_text" (getf card :large-text))))
      (destructuring-bind (key value) pair
        (when (and (stringp value) (plusp (length value)))
          (setf (gethash key activity) value))))
    (setf (gethash "timestamps" activity)
          (nlk:json-object "start" (getf card :started)))
    activity))

(defun getpid ()
  "This process' pid, which Discord asks for in SET_ACTIVITY."
  (ignore-errors (sb-posix:getpid)))
