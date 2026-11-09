;;;; table.lisp --- the port table: one row shape, one backend a platform.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A row is a plist and nothing more:
;;;;
;;;;   (:protocol "tcp" :address "0.0.0.0" :port 3000 :state "listen"
;;;;    :pid 4711 :process "node.exe" :path "C:/.../node.exe"
;;;;    :remote-address "0.0.0.0" :remote-port 0)
;;;;
;;;; Three backends answer the same rows, read-only, each the cheapest table
;;;; its platform offers:
;;;;
;;;;   :windows  GetExtendedTcpTable / GetExtendedUdpTable through
;;;;             iphlpapi.dll, the owning PID in the row and
;;;;             QueryFullProcessImageName for the image: what netstat -ano
;;;;             prints, without parsing console text, and no child process.
;;;;   :linux    /proc/net/{tcp,tcp6,udp,udp6}, joined on the socket inode
;;;;             against /proc/<pid>/fd, so the PID and image come from
;;;;             files, again with no child process.
;;;;   :darwin   lsof's field output (-F), the one table macOS offers
;;;;             without a kext: process, protocol, endpoint and state in a
;;;;             shape made to be parsed.
;;;;
;;;; Nothing here opens, binds or connects a socket, and nothing writes.
;;;; The tests rebind *ROW-SOURCE* (any platform), *PROC-ROOT* (a fixture
;;;; tree) and *RUN* (lsof's output) instead of running on a Windows box.

(in-package #:nodecode-ports)

;;; --- the seams -------------------------------------------------------------------

(defvar *platform* nil
  "NIL detects the platform; :WINDOWS, :LINUX or :DARWIN forces one.")

(defvar *row-source* nil
  "When bound, a function of (PROTOCOL INCLUDE-IPV6) answering rows in place
of the platform's own table: what a test or a layer substitutes.")

(defvar *proc-root* (uiop:ensure-directory-pathname #p"/proc/")
  "Where the Linux backend reads its tables and its PIDs from.")

(defvar *timeout-seconds* 10
  "How long a command this cell runs may take before it is killed.")

(defvar *run* nil
  "NIL reads a command's output with RUN-OUTPUT; a function of ARGV
answering text is what a test or a layer substitutes.")

(defvar *process-name-cache* (make-hash-table)
  "PID -> image path, so a table of two hundred rows opens no PID twice.")

(defun detect-platform ()
  "The backend this process gets, or NIL when there is none."
  (cond ((uiop:os-windows-p) :windows)
        ((uiop:os-macosx-p) :darwin)
        ((uiop:os-unix-p) :linux)
        (t nil)))

(defun platform ()
  (or *platform* (detect-platform)))

(defun run-output (argv)
  "ARGV's standard output and error, or a refusal when it cannot be run.
A command that outlives *TIMEOUT-SECONDS* is killed and refuses."
  (handler-case
      (let* ((process (uiop:launch-program argv :output :stream :error-output :output))
             (stream (uiop:process-info-output process))
             (deadline (+ (get-internal-real-time)
                          (* *timeout-seconds* internal-time-units-per-second)))
             (collected '())
             (done nil))
        (unwind-protect
             (loop
               (loop for char = (read-char-no-hang stream)
                     while char do (push char collected))
               (cond ((sb-ext:process-alive-p process)
                      (if (> (get-internal-real-time) deadline)
                          (return)
                          (sleep 0.01)))
                     (t ;; the process is gone; one more read drains the pipe
                      (loop for char = (read-char-no-hang stream)
                            while char do (push char collected))
                      (setf done t)
                      (return))))
          (unless done (ignore-errors (sb-ext:process-kill process 9)))
          (ignore-errors (sb-ext:process-close process)))
        (if done
            (coerce (nreverse collected) 'string)
            (fail "~a did not answer within ~a second~:p" (first argv) *timeout-seconds*)))
    (error (condition)
      (fail "~a could not be run: ~a" (first argv) condition))))

;;; --- the entry point ---------------------------------------------------------------

(defun rows (&key (protocol :both) (include-ipv6 t))
  "Every row this machine's table holds for PROTOCOL (:TCP, :UDP or :BOTH),
IPv6 tables too when INCLUDE-IPV6."
  (cond (*row-source* (funcall *row-source* protocol include-ipv6))
        (t (case (platform)
             (:windows (windows-rows protocol include-ipv6))
             (:linux (linux-rows protocol include-ipv6))
             (:darwin (lsof-rows protocol include-ipv6))
             (t (fail "no port table on this platform: this cell reads iphlpapi on
Windows, /proc on Linux and lsof on macOS"))))))

(defun protocol-name (protocol ipv6)
  (format nil "~(~a~)~:[~;6~]" protocol ipv6))

;;; --- Linux: /proc/net, and the PIDs behind the inodes ------------------------------

#-win32
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun entry-names (directory)
  "The file names under DIRECTORY, or NIL when it cannot be read."
  (handler-case
      (loop for path in (directory (merge-pathnames "*" directory))
            for name = (file-namestring path)
            when (and name (plusp (length name))) collect name)
    (error () nil)))

(defun subdirectory-names (directory)
  "The names of DIRECTORY's subdirectories, or NIL when it cannot be read."
  (handler-case
      (loop for path in (uiop:subdirectories directory)
            for name = (first (last (pathname-directory path)))
            when name collect (princ-to-string name))
    (error () nil)))

#-win32
(defun read-link (path)
  "PATH's target, or NIL."
  (ignore-errors (sb-posix:readlink (uiop:native-namestring path))))

#+win32
(defun read-link (path)
  (declare (ignore path))
  nil)

(defun read-text (path)
  "PATH's contents without its trailing whitespace, or NIL."
  (ignore-errors (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (uiop:read-file-string path))))

(defun digits-p (text)
  (and (plusp (length text)) (every #'digit-char-p text)))

(defun hex-p (text)
  (and (plusp (length text)) (every (lambda (char) (digit-char-p char 16)) text)))

(defun proc-line-p (fields)
  "Whether FIELDS is a /proc/net row rather than the table's header."
  (and (>= (length fields) 10)
       (hex-p (fourth fields))
       (let ((colon (position #\: (second fields))))
         (and colon (hex-p (subseq (second fields) 0 colon))))))

#-win32
(defun socket-owners ()
  "INODE -> (PID NAME PATH) over every process whose file descriptors this
user may read: the join /proc/net/tcp's inode column needs."
  (let ((owners (make-hash-table :test #'equal)))
    (dolist (entry (subdirectory-names *proc-root*))
      (when (digits-p entry)
        (let* ((directory (merge-pathnames (format nil "~a/" entry) *proc-root*))
               (fd-directory (merge-pathnames "fd/" directory))
               (name (read-text (merge-pathnames "comm" directory)))
               (path (read-link (merge-pathnames "exe" directory)))
               (pid (parse-integer entry)))
          (dolist (fd (entry-names fd-directory))
            (let* ((target (read-link (merge-pathnames fd fd-directory)))
                   (inode (and target
                               (uiop:string-prefix-p "socket:[" target)
                               (subseq target 8 (max 8 (1- (length target)))))))
              (when (and inode (plusp (length inode)) (digits-p inode))
                (setf (gethash inode owners) (list pid name path))))))))
    owners))

(defparameter +proc-states+
  '(("01" . "established") ("02" . "syn_sent") ("03" . "syn_received")
    ("04" . "fin_wait_1") ("05" . "fin_wait_2") ("06" . "time_wait")
    ("07" . "closed") ("08" . "close_wait") ("09" . "last_ack")
    ("0A" . "listen") ("0B" . "closing") ("0C" . "delete_tcb"))
  "The state codes of /proc/net/tcp, named the way netstat names them.")

(defun proc-state (code)
  (or (cdr (assoc (string-upcase code) +proc-states+ :test #'string=)) "unknown"))

(defun hex-bytes (text)
  "TEXT as a list of two-hex-digit values."
  (loop for index from 0 below (- (length text) 1) by 2
        collect (parse-integer text :start index :end (+ 2 index) :radix 16)))

(defun ipv4-text (hex)
  "The address /proc writes as eight hex digits, little-endian by word."
  (format nil "~{~d~^.~}" (reverse (hex-bytes hex))))

(defun ipv6-text (hex)
  "The address /proc writes as thirty-two hex digits, four words of four
bytes each, each word reversed. Uncompressed, as the kernel stores it."
  (let ((bytes (loop for word from 0 below 4
                     append (reverse (hex-bytes (subseq hex (* 8 word) (* 8 (1+ word))))))))
    (format nil "~{~(~4,'0x~)~^:~}"
            (loop for index from 0 below 16 by 2
                  collect (logior (ash (nth index bytes) 8) (nth (1+ index) bytes))))))

(defun split-hex-endpoint (text)
  "TEXT's address and port, as /proc writes \"ADDR:PORT\"."
  (let ((colon (position #\: text)))
    (if colon
        (values (subseq text 0 colon)
                (ignore-errors (parse-integer (subseq text (1+ colon)) :radix 16)))
        (values text nil))))

#-win32
(defun proc-table-rows (file protocol ipv6 owners)
  "The rows of one /proc/net table, joined to OWNERS on the socket inode."
  (let ((text (handler-case (uiop:read-file-string file) (error () nil))))
    (when text
      (loop for line in (uiop:split-string text :separator '(#\Newline))
            for fields = (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
                                 :test #'string=)
            when (proc-line-p fields)
              collect (multiple-value-bind (local port) (split-hex-endpoint (second fields))
                        (multiple-value-bind (remote remote-port)
                            (split-hex-endpoint (third fields))
                          (let* ((owner (gethash (nth 9 fields) owners))
                                 (state (if (eq protocol :udp)
                                            "bound"
                                            (proc-state (fourth fields)))))
                            (list :protocol (protocol-name protocol ipv6)
                                  :address (if ipv6 (ipv6-text local) (ipv4-text local))
                                  :port port
                                  :state state
                                  :pid (first owner)
                                  ;; the image's own name reads better than
                                  ;; comm, which a kernel truncates to fifteen
                                  :process (or (and (third owner)
                                                    (file-namestring (third owner)))
                                               (second owner))
                                  :path (third owner)
                                  :remote-address (if ipv6 (ipv6-text remote)
                                                       (ipv4-text remote))
                                  :remote-port remote-port))))))))

#-win32
(defun linux-rows (protocol include-ipv6)
  "The rows of /proc/net/{tcp,tcp6,udp,udp6} under *PROC-ROOT*."
  (let ((owners (socket-owners))
        (out '()))
    (flet ((read-one (name for ipv6)
             (setf out (nconc out (proc-table-rows
                                   (merge-pathnames (format nil "net/~a" name) *proc-root*)
                                   for ipv6 owners)))))
      (when (member protocol '(:tcp :both))
        (read-one "tcp" :tcp nil)
        (when include-ipv6 (read-one "tcp6" :tcp t)))
      (when (member protocol '(:udp :both))
        (read-one "udp" :udp nil)
        (when include-ipv6 (read-one "udp6" :udp t))))
    out))

#+win32
(defun linux-rows (protocol include-ipv6)
  (declare (ignore protocol include-ipv6))
  (fail "/proc is the Linux table: this process is on ~(~a~)" (platform)))

;;; --- macOS: lsof's field output ---------------------------------------------------

(defun lsof-split-endpoint (text)
  "TEXT as lsof writes one endpoint, \"ADDR:PORT\", its address's brackets
off and its wildcard as the unspecified address. => ADDRESS, PORT."
  (let* ((colon (position #\: text :from-end t))
         (address (if colon (subseq text 0 colon) text))
         (port (and colon (ignore-errors (parse-integer text :start (1+ colon))))))
    (when (and (plusp (length address))
               (char= (char address 0) #\[)
               (char= (char address (1- (length address))) #\]))
      (setf address (subseq address 1 (1- (length address)))))
    (values (if (string= address "*") "0.0.0.0" address) (or port 0))))

(defun lsof-state (text)
  "The state a lsof T field names, \"ST=LISTEN\", as netstat says it."
  (let ((state (string-downcase (or (second (uiop:split-string text :separator "=")) ""))))
    (if (string= state "") "unknown" state)))

(defun lsof-row (name pid process protocol)
  "One lsof NAME line, \"LOCAL->REMOTE\" or \"LOCAL\", as a row."
  (let* ((arrow (search "->" name))
         (local (if arrow (subseq name 0 arrow) name))
         (remote (and arrow (subseq name (+ 2 arrow)))))
    (multiple-value-bind (address port) (lsof-split-endpoint local)
      (multiple-value-bind (remote-address remote-port)
          (if remote (lsof-split-endpoint remote) (values nil nil))
        (list :protocol protocol :address address :port port :state "unknown"
              :pid pid :process process :path nil
              :remote-address remote-address :remote-port remote-port)))))

(defun lsof-parse (text)
  "TEXT, lsof's -FpcnPT field output, as rows. A name line opens a row and
the T line that follows it settles its state."
  (let ((rows '()) (pid nil) (process nil) (protocol "tcp") (last nil))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
      (when (> (length line) 1)
        (let ((tag (char line 0)) (value (subseq line 1)))
          (case tag
            (#\p (setf pid (ignore-errors (parse-integer value :junk-allowed t)))
                 (setf process nil protocol "tcp" last nil))
            (#\c (setf process value))
            (#\P (setf protocol (string-downcase value)))
            (#\n (setf last (lsof-row value pid process protocol))
                 (when pid (setf rows (nconc rows (list last)))))
            (#\T (when last (setf (getf last :state) (lsof-state value))))))))
    rows))

(defun lsof-rows (protocol include-ipv6)
  "The rows lsof prints for PROTOCOL on this Mac."
  (let ((text (funcall (or *run* #'run-output)
                        '("lsof" "-nP" "-FpcnPT" "-iTCP" "-iUDP"))))
    (loop for row in (lsof-parse text)
          when (and (or (eq protocol :both)
                        (string-equal (getf row :protocol) (string protocol)))
                    (or include-ipv6
                        (not (find #\: (getf row :address) :test #'char=))))
            collect row)))

;;; --- Windows: iphlpapi through sb-alien --------------------------------------------

(defconstant +af-inet+ 2)
(defconstant +af-inet6+ 23)
(defconstant +tcp-table-owner-pid-all+ 5)
(defconstant +udp-table-owner-pid+ 1)
(defconstant +error-insufficient-buffer+ 122)
(defconstant +process-query-limited-information+ #x1000)

#+win32
(progn
  (sb-alien:load-shared-object "iphlpapi.dll")
  (sb-alien:load-shared-object "kernel32.dll")

  (sb-alien:define-alien-routine ("GetExtendedTcpTable" extended-tcp-table)
      sb-alien:unsigned-int
    (table sb-alien:system-area-pointer)
    (size (* sb-alien:unsigned-int))
    (order sb-alien:int)
    (family sb-alien:unsigned-int)
    (class sb-alien:unsigned-int)
    (reserved sb-alien:unsigned-int))

  (sb-alien:define-alien-routine ("GetExtendedUdpTable" extended-udp-table)
      sb-alien:unsigned-int
    (table sb-alien:system-area-pointer)
    (size (* sb-alien:unsigned-int))
    (order sb-alien:int)
    (family sb-alien:unsigned-int)
    (class sb-alien:unsigned-int)
    (reserved sb-alien:unsigned-int))

  (sb-alien:define-alien-routine ("OpenProcess" open-process) sb-alien:system-area-pointer
    (access sb-alien:unsigned-int)
    (inherit sb-alien:int)
    (pid sb-alien:unsigned-int))

  (sb-alien:define-alien-routine ("QueryFullProcessImageNameW" query-process-name)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (flags sb-alien:unsigned-int)
    (buffer sb-alien:system-area-pointer)
    (size (* sb-alien:unsigned-int)))

  (sb-alien:define-alien-routine ("CloseHandle" close-handle) sb-alien:int
    (handle sb-alien:system-area-pointer)))

#+win32
(defun wide-text (sap units)
  "The text UNITS UTF-16 code units at SAP hold, surrogate pairs joined."
  (let ((out (make-string-output-stream)))
    (loop with index = 0
          while (< index units)
          for unit = (sb-sys:sap-ref-16 sap (* 2 index))
          do (cond ((zerop unit) (return))
                   ((<= #xD800 unit #xDBFF)
                    (let ((low (and (< (1+ index) units)
                                    (let ((next (sb-sys:sap-ref-16 sap (* 2 (1+ index)))))
                                      (and (<= #xDC00 next #xDFFF) next)))))
                      (if low
                          (progn (write-char (code-char (+ #x10000
                                                           (ash (- unit #xD800) 10)
                                                           (- low #xDC00)))
                                             out)
                                 (incf index 2))
                          (progn (write-char (code-char unit) out) (incf index)))))
                   (t (write-char (code-char unit) out) (incf index))))
    (get-output-stream-string out)))

#+win32
(defun windows-process-name (pid)
  "PID's image path, through QueryFullProcessImageName with the least right
that answers it, PROCESS_QUERY_LIMITED_INFORMATION, or NIL."
  (multiple-value-bind (cached present) (gethash pid *process-name-cache*)
    (if present
        cached
        (setf (gethash pid *process-name-cache*)
              (cond ((= pid 0) "System Idle Process")
                    ((= pid 4) "System")
                    (t (let ((handle (open-process +process-query-limited-information+ 0 pid)))
                         (unless (sb-sys:sap= handle (sb-sys:int-sap 0))
                           (unwind-protect
                                (sb-alien:with-alien ((size sb-alien:unsigned-int 32767)
                                                      (buffer (array sb-alien:unsigned-short 32768)))
                                  (when (plusp (query-process-name
                                                handle 0 (sb-alien:addr buffer)
                                                (sb-alien:addr size)))
                                    (wide-text (sb-alien:alien-sap (sb-alien:addr buffer))
                                               (sb-alien:slot size 0))))
                             (close-handle handle))))))))))

#+win32
(defun windows-table (udp-p family class row-size)
  "One iphlpapi table. => (VALUES SAP COUNT ROW-SIZE BUFFER), the last to be
freed, or (VALUES NIL 0 ROW-SIZE NIL) when the table is empty."
  (sb-alien:with-alien ((size sb-alien:unsigned-int 0))
    (let ((getter (if udp-p #'extended-udp-table #'extended-tcp-table)))
      (let ((code (sb-alien:alien-funcall getter (sb-sys:int-sap 0) (sb-alien:addr size)
                                          0 family class 0)))
        (unless (member code (list 0 +error-insufficient-buffer+))
          (fail "the ~a table could not be sized (win32 ~d)" (if udp-p "UDP" "TCP") code))
        (if (zerop (sb-alien:slot size 0))
            (values nil 0 row-size nil)
            (let ((buffer (sb-alien:make-alien sb-alien:unsigned-char
                                               (sb-alien:slot size 0))))
              (setf code (sb-alien:alien-funcall getter (sb-alien:alien-sap buffer)
                                                 (sb-alien:addr size) 0 family class 0))
              (unless (zerop code)
                (sb-alien:free-alien buffer)
                (fail "the ~a table could not be read (win32 ~d)"
                      (if udp-p "UDP" "TCP") code))
              (let ((sap (sb-alien:alien-sap buffer)))
                (values sap (sb-sys:sap-ref-32 sap 0) row-size buffer))))))))

#+win32
(defun ipv6-alien-text (sap base)
  "The address sixteen bytes at SAP and BASE hold, uncompressed."
  (format nil "~{~(~4,'0x~)~^:~}"
          (loop for index from 0 below 16 by 2
                collect (logior (ash (sb-sys:sap-ref-8 sap (+ base index)) 8)
                                (sb-sys:sap-ref-8 sap (+ base index 1))))))

#+win32
(defun ipv4-alien-text (sap base)
  "The address four bytes at SAP and BASE hold, in network order."
  (format nil "~d.~d.~d.~d"
          (sb-sys:sap-ref-8 sap base) (sb-sys:sap-ref-8 sap (+ base 1))
          (sb-sys:sap-ref-8 sap (+ base 2)) (sb-sys:sap-ref-8 sap (+ base 3))))

#+win32
(defparameter +tcp-states+
  '("closed" "listen" "syn_sent" "syn_received" "established" "fin_wait_1"
    "fin_wait_2" "close_wait" "closing" "last_ack" "time_wait" "delete_tcb")
  "The MIB_TCP_STATE values, named the way netstat names them.")

#+win32
(defun windows-row (sap base &key family ipv6-p udp-p local port state remote remote-port pid)
  "One iphlpapi row at BASE as the row plist every backend answers, its
offsets named by the caller. Ports are stored in network byte order in the
low sixteen bits of a DWORD."
  (let* ((owner (sb-sys:sap-ref-32 sap (+ base pid)))
         (image (windows-process-name owner))
         (port-value (logand (logior (ash (sb-sys:sap-ref-8 sap (+ base port)) 8)
                                     (sb-sys:sap-ref-8 sap (+ base port 1)))
                              #xFFFF)))
    (declare (ignore family))
    (list :protocol (protocol-name (if udp-p :udp :tcp) ipv6-p)
          :address (if ipv6-p (ipv6-alien-text sap (+ base local))
                        (ipv4-alien-text sap (+ base local)))
          :port port-value
          :state (if udp-p "bound"
                     (let ((value (sb-sys:sap-ref-32 sap (+ base state))))
                       (or (nth (1- value) +tcp-states+) (princ-to-string value))))
          :pid owner
          :process (and image (first (last (uiop:split-string image :separator '(#\\ #\/)))))
          :path image
          :remote-address (unless udp-p
                            (if ipv6-p (ipv6-alien-text sap (+ base remote))
                                (ipv4-alien-text sap (+ base remote))))
          :remote-port (unless udp-p
                         (logior (ash (sb-sys:sap-ref-8 sap (+ base remote-port)) 8)
                                 (sb-sys:sap-ref-8 sap (+ base remote-port 1)))))))

#+win32
(defun windows-rows (protocol include-ipv6)
  "The rows of the TCP and UDP owner tables, IPv4 and IPv6."
  (let ((out '()))
    (labels ((collect-table (udp-p family class row-size make-row)
               (multiple-value-bind (sap count size buffer)
                   (windows-table udp-p family class row-size)
                 (unwind-protect
                      (when sap
                        (dotimes (index count)
                          (push (funcall make-row sap (+ 4 (* index size))) out)))
                   (when buffer (sb-alien:free-alien buffer))))))
      (when (member protocol '(:tcp :both))
        (collect-table nil +af-inet+ +tcp-table-owner-pid-all+ 24
                       (lambda (sap base)
                         (windows-row sap base :family +af-inet+ :ipv6-p nil :udp-p nil
                                      :local 4 :port 8 :state 0 :remote 12
                                      :remote-port 16 :pid 20)))
        (when include-ipv6
          (collect-table nil +af-inet6+ +tcp-table-owner-pid-all+ 56
                         (lambda (sap base)
                           (windows-row sap base :family +af-inet6+ :ipv6-p t :udp-p nil
                                        :local 0 :port 16 :state 48 :remote 24
                                        :remote-port 44 :pid 52)))))
      (when (member protocol '(:udp :both))
        (collect-table t +af-inet+ +udp-table-owner-pid+ 12
                       (lambda (sap base)
                         (windows-row sap base :family +af-inet+ :ipv6-p nil :udp-p t
                                      :local 0 :port 4 :remote 0 :remote-port 4 :pid 8)))
        (when include-ipv6
          (collect-table t +af-inet6+ +udp-table-owner-pid+ 28
                         (lambda (sap base)
                           (windows-row sap base :family +af-inet6+ :ipv6-p t :udp-p t
                                        :local 0 :port 20 :remote 0 :remote-port 20 :pid 24)))))
    out)))

#-win32
(defun windows-rows (protocol include-ipv6)
  (declare (ignore protocol include-ipv6))
  (fail "iphlpapi is Windows' table: this process is on ~(~a~)" (platform)))
