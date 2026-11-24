;;;; store.lisp --- the store on disk: one file, one header, one flat map.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's format, kept byte for byte so a store the Hermes plugin
;;;; wrote keeps working where this cell runs:
;;;;
;;;;     HERMES-DPAPI-1 \n <DPAPI blob of the JSON object {name: value}>
;;;;
;;;; The header is not security -- DPAPI already refuses a blob it did not
;;;; produce -- it turns "this is some other file" into a clear error instead
;;;; of an opaque decryption failure.
;;;;
;;;; A platform without DPAPI writes the same map under a header of its own,
;;;; NODECODE-DPAPI-FILE-1, with the JSON in the clear and the file mode set
;;;; to 0600. It says so wherever it is asked (see PROTECTION in cell.lisp),
;;;; because that is a weaker promise than the original's and the operator
;;;; should read it rather than infer it. A header is what decides how a file
;;;; is read, so a store carried between platforms still says what wrote it.
;;;;
;;;; Writes go to a temporary file in the same directory and are renamed into
;;;; place, so an interrupted write cannot leave a truncated store behind.

(in-package #:nodecode-dpapi-secrets)

(defparameter +dpapi-header+ "HERMES-DPAPI-1"
  "The original plugin's magic, so its store is read here unchanged.")

(defparameter +file-header+ "NODECODE-DPAPI-FILE-1"
  "A store this cell wrote where DPAPI does not exist: the same map, in the clear.")

(defun octets (text)
  "TEXT as UTF-8 octets."
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun text (octets)
  "OCTETS as text, UTF-8."
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defun file-octets (path)
  "Every byte of PATH."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buffer in)
      buffer)))

(defun split-header (bytes)
  "BYTES split at its first newline => (values HEADER PAYLOAD), or NIL when
 there is no header line at all."
  (let ((break (position 10 bytes)))
    (when break
      (values (map 'string #'code-char (subseq bytes 0 break))
              (subseq bytes (1+ break))))))

(defun parse-store-json (bytes path)
  "BYTES -- a decrypted store payload -- as the name -> value map it holds."
  (let ((parsed (handler-case (shasht:read-json (text bytes))
                  (error () (fail "~a decrypted to something that is not JSON" path)))))
    (unless (hash-table-p parsed)
      (fail "~a does not hold a name -> value mapping" path))
    (let ((values (make-hash-table :test #'equal)))
      (maphash (lambda (name value)
                 (unless (stringp value)
                   (fail "~a holds a non-string entry (~s)" path name))
                 (setf (gethash name values) value))
               parsed)
      values)))

(defun read-store (path)
  "The store at PATH => (values MAP PROTECTION), PROTECTION :DPAPI or :FILE.
 Signals DPAPI-SECRETS-ERROR for a file that is not a store this reads and
 for a blob this account cannot decrypt: nothing is guessed and nothing is
 resolved out of a file that only nearly parses."
  (multiple-value-bind (header payload) (split-header (file-octets path))
    (cond
      ((null header)
       (fail "~a is not a DPAPI secret store (no header line)" path))
      ((equal header +dpapi-header+)
       (when (zerop (length payload))
         (fail "~a has a header but no encrypted payload" path))
       (values (parse-store-json (dpapi-unprotect payload) path) :dpapi))
      ((equal header +file-header+)
       (values (parse-store-json payload path) :file))
      (t
       (fail "~a is not a DPAPI secret store (header ~s)" path header)))))

(defun write-store (path map protection)
  "MAP written to PATH under PROTECTION (:DPAPI or :FILE), atomically."
  (let* ((json (shasht:write-json map nil))
         (payload (ecase protection
                    (:dpapi (dpapi-protect (octets json)))
                    (:file (octets json))))
         (head (octets (if (eq protection :dpapi) +dpapi-header+ +file-header+)))
         (break (make-array 1 :element-type '(unsigned-byte 8) :initial-element 10))
         (bytes (concatenate '(vector (unsigned-byte 8)) head break payload)))
    (ensure-directories-exist path)
    (let ((temp (merge-pathnames (format nil "~a.~d.tmp" (file-namestring path) (random 1000000))
                                  path)))
      (unwind-protect
           (progn
             (with-open-file (out temp :direction :output :if-exists :supersede
                                        :element-type '(unsigned-byte 8))
               (write-sequence bytes out)
               (finish-output out))
             ;; The mode is the whole of a file store's protection, so it is
             ;; set before the file is given its name.
             #+unix (sb-posix:chmod temp #o600)
             (uiop:rename-file-overwriting-target temp path))
        (when (probe-file temp)
          (ignore-errors (delete-file temp)))))))

(defun store-entries (map)
  "MAP's entry names, sorted, and never a value."
  (sort (loop for name being the hash-keys of map collect name) #'string<))

(defun store-entry (map name)
  "=> (values VALUE FOUND) for NAME in MAP."
  (gethash name map))
