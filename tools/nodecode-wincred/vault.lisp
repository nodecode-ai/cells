;;;; vault.lisp --- the Windows Credential Manager, and the one seam to it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original bound advapi32 through ctypes: CredReadW for a generic
;;;; credential named in the map, the blob decoded as UTF-8 or UTF-16, and a
;;;; value that decodes to neither skipped with a warning rather than injected
;;;; as a mangled key. The same read is here, on SBCL's alien interface, and
;;;; nothing else: no enumeration of the vault, no write to it, no process, no
;;;; network. *VAULT* is the one seam, so the whole cell runs, and is tested,
;;;; off Windows.

(in-package #:nodecode-wincred)

;;; --- what a failed read says ----------------------------------------------

(define-condition wincred-error (error)
  ((target :initarg :target :initform nil :reader wincred-error-target)
   (code :initarg :code :initform nil :reader wincred-error-code)
   (detail :initarg :detail :initform "the credential could not be read"
           :reader wincred-error-detail))
  (:report (lambda (condition stream)
             (write-string (wincred-error-detail condition) stream))))

(define-condition credential-decode-error (wincred-error) ()
  (:default-initargs
   :detail "the credential is not UTF-8 or UTF-16 text, so it was not injected"))

(defparameter +error-access-denied+ 5)
(defparameter +error-not-found+ 1168)
(defparameter +error-no-such-logon-session+ 1312)

(defun win32-detail (target code)
  "What win32 CODE means for TARGET, in the words that name the fix."
  (cond
    ((eql code +error-not-found+)
     (format nil "no generic credential named ~s in this user's vault; store it in Credential Manager (Windows Credentials, Add a generic credential) -- cmdkey /list lists the ones there are"
             target))
    ((eql code +error-access-denied+)
     (format nil "access denied reading ~s from this user's vault" target))
    ((eql code +error-no-such-logon-session+)
     (format nil "this logon session has no credential vault, so ~s cannot be read" target))
    (t (format nil "CredReadW failed for ~s (win32 error ~a)" target code))))

;;; --- the one seam ------------------------------------------------------------

(defvar *vault* nil
  "How the vault is reached, as (:AVAILABLE FUNCTION :READ FUNCTION) or NIL
for the platform's own. READ takes a generic credential's target name and
answers its blob as octets, signalling WINCRED-ERROR when it cannot. A test
binds this, so nothing touches a real vault and no platform is needed.")

(defparameter *windows-backend*
  (list :available 'windows-available-p :read 'windows-read)
  "The real vault: advapi32's CredReadW, read-only, in the current user's vault.")

(defun vault () (or *vault* *windows-backend*))

(defun vault-available-p ()
  "Whether this machine has the vault the cell reads."
  (funcall (getf (vault) :available)))

(defun vault-read (target)
  "TARGET's blob as octets, read through the seam."
  (funcall (getf (vault) :read) target))

(defun windows-available-p () #+win32 t #-win32 nil)

(defun windows-read (target)
  "TARGET's blob from the current user's generic credentials."
  #+win32 (%cred-read target)
  #-win32 (error 'wincred-error :target target
                 :detail "Windows Credential Manager is only available on Windows, and this is not Windows"))

;;; --- the platform binding ----------------------------------------------------
;;
;; Everything below is compiled on Windows only; off Windows the seam answers
;; :UNAVAILABLE above and none of it is read. Credential Manager's own calls
;; take LPWSTR, and this SBCL has no wide-string alien type, so the buffers are
;; built by hand: unsigned-short is one UTF-16 code unit.

#+win32
(progn
  (sb-alien:define-alien-routine ("CredReadW" %cred-read-w) sb-alien:int
    (target (* sb-alien:unsigned-short))
    (credential-type sb-alien:unsigned-int)
    (flags sb-alien:unsigned-int)
    (credential (* t)))

  (sb-alien:define-alien-routine ("CredFree" %cred-free) sb-alien:void
    (buffer (* t)))

  (sb-alien:define-alien-routine ("SetEnvironmentVariableW" %set-environment-w) sb-alien:int
    (name (* sb-alien:unsigned-short))
    (value (* sb-alien:unsigned-short)))

  (defparameter +cred-type-generic+ 1)

  ;; CREDENTIALW is read by offset rather than by a declared struct: two
  ;; DWORDs, then the pointers, and the blob pair this reads sits at 32/40 on
  ;; x86-64 and 24/28 on x86.
  (defparameter +blob-size-offset+ #+64-bit 32 #-64-bit 24)
  (defparameter +blob-offset+ #+64-bit 40 #-64-bit 28)

  (defun %wide (text)
    "TEXT as the NUL-terminated UTF-16 buffer the Credential Manager calls
want => (values BUFFER UNITS), BUFFER to be freed by the caller."
    (let* ((units (loop for character across text
                        for code = (char-code character)
                        if (> code #xFFFF)
                          collect (logior #xD800 (ash (- code #x10000) -10))
                          and collect (logior #xDC00 (logand (- code #x10000) #x3FF))
                        else collect code))
           (buffer (sb-alien:make-alien sb-alien:unsigned-short (1+ (length units)))))
      (loop for code in units
            for index from 0
            do (setf (sb-alien:deref buffer index) code))
      (setf (sb-alien:deref buffer (length units)) 0)
      (values buffer (length units))))

  (defun %last-error () (sb-win32:get-last-error))

  (defun %copy-blob (credential)
    "The blob of CREDENTIAL, an alien (* T) to a CREDENTIALW, as octets."
    (let* ((struct (sb-alien:alien-sap credential))
           (size (sb-sys:sap-ref-32 struct +blob-size-offset+))
           (blob (sb-sys:sap-ref-sap struct +blob-offset+))
           (octets (make-array size :element-type '(unsigned-byte 8))))
      (dotimes (index size octets)
        (setf (aref octets index) (sb-sys:sap-ref-8 blob index)))))

  (defun %cred-read (target)
    "TARGET's blob from the current user's vault, read with CredReadW."
    (multiple-value-bind (name units) (%wide target)
      (declare (ignore units))
      (unwind-protect
           (let ((out (sb-alien:make-alien (* t))))
             (unwind-protect
                  (progn
                    (unless (plusp (%cred-read-w name +cred-type-generic+ 0 out))
                      (let ((code (%last-error)))
                        (error 'wincred-error :target target :code code
                                               :detail (win32-detail target code))))
                    (let* ((credential (sb-alien:deref out))
                           (blob (%copy-blob credential)))
                      (%cred-free (sb-alien:alien-sap credential))
                      blob))
               (sb-alien:free-alien out)))
        (sb-alien:free-alien name)))))

;;; --- the process environment -------------------------------------------------

(defun environment-value (name)
  "NAME's value in this process's environment, or NIL."
  (uiop:getenv name))

(defun c-text (text)
  "TEXT as the plain string the C boundary takes: trimming answers a
subsequence, which an alien argument cannot be."
  (coerce text 'simple-string))

(defun %put-environment (name value)
  #+win32 (multiple-value-bind (wide units) (%wide name)
            (declare (ignore units))
            (unwind-protect
                 (multiple-value-bind (text text-units) (%wide value)
                   (declare (ignore text-units))
                   (unwind-protect (%set-environment-w wide text)
                     (sb-alien:free-alien text)))
              (sb-alien:free-alien wide)))
  #-win32 (%libc-setenv (c-text name) (c-text value) 1))

(defun %drop-environment (name)
  #+win32 (multiple-value-bind (wide units) (%wide name)
            (declare (ignore units))
            (unwind-protect (%set-environment-w wide nil)
              (sb-alien:free-alien wide)))
  #-win32 (%libc-unsetenv (c-text name)))

#-win32
(progn
  (sb-alien:define-alien-routine ("setenv" %libc-setenv) sb-alien:int
    (name sb-alien:c-string)
    (value sb-alien:c-string)
    (overwrite sb-alien:int))

  (sb-alien:define-alien-routine ("unsetenv" %libc-unsetenv) sb-alien:int
    (name sb-alien:c-string)))

(defun set-environment-variable (name value)
  "NAME => VALUE in this process, so a child started from here on sees it."
  (%put-environment name value)
  value)

(defun unset-environment-variable (name)
  "Take NAME out of this process's environment."
  (%drop-environment name)
  nil)

;;; --- decoding a blob ---------------------------------------------------------

(defun plausible-text-p (text)
  "Whether TEXT reads as text rather than as binary: no C0 control but tab,
newline and return."
  (every (lambda (character)
           (or (char>= character #\Space) (find character "\t\n\r")))
         text))

(defun %utf8 (raw)
  "RAW decoded as UTF-8, or NIL when it is not."
  (handler-case (sb-ext:octets-to-string raw :external-format :utf-8)
    (error () nil)))

(defun %utf16 (raw start end little-endian-p)
  "RAW between START and END decoded as UTF-16 => a string, or a refusal for
an odd length and for a surrogate with no partner."
  (let ((length (- end start)))
    (when (oddp length)
      (error 'credential-decode-error
             :detail "the credential is an odd number of bytes of UTF-16"))
    (let ((units (make-array (floor length 2)))
          (out (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
      (dotimes (index (length units))
        (let ((at (+ start (* 2 index))))
          (setf (aref units index)
                (if little-endian-p
                    (logior (aref raw at) (ash (aref raw (1+ at)) 8))
                    (logior (aref raw (1+ at)) (ash (aref raw at) 8))))))
      (loop with index = 0
            while (< index (length units))
            for unit = (aref units index)
            do (cond
                 ((<= #xD800 unit #xDBFF)
                  (let ((low (and (< (1+ index) (length units))
                                  (aref units (1+ index)))))
                    (unless (and low (<= #xDC00 low #xDFFF))
                      (error 'credential-decode-error
                             :detail "the credential has half of a UTF-16 surrogate pair"))
                    (vector-push-extend
                     (code-char (+ #x10000 (ash (- unit #xD800) 10) (- low #xDC00)))
                     out)
                    (incf index 2)))
                 ((<= #xDC00 unit #xDFFF)
                  (error 'credential-decode-error
                         :detail "the credential has half of a UTF-16 surrogate pair"))
                 (t (vector-push-extend (code-char unit) out)
                    (incf index)))
            finally (return (coerce out 'string))))))

(defun trim-nuls (text)
  "TEXT without the NUL padding Windows leaves at the end."
  (string-right-trim (string (code-char 0)) text))

(defun decode-credential (raw)
  "RAW, the bytes of a generic credential, as the text it holds.

Windows stores the blob as bytes with no declared encoding: cmdkey, the
Credential UI and PowerShell write UTF-16LE, most CLI tools write UTF-8. Both
are accepted and anything else is refused, because a mangled API key is worse
than no key."
  (let ((length (length raw)))
    (when (zerop length) (return-from decode-credential ""))
    (when (and (>= length 2) (= (aref raw 0) #xFF) (= (aref raw 1) #xFE))
      (return-from decode-credential (trim-nuls (%utf16 raw 2 length t))))
    (when (and (>= length 2) (= (aref raw 0) #xFE) (= (aref raw 1) #xFF))
      (return-from decode-credential (trim-nuls (%utf16 raw 2 length nil))))
    (let ((utf8 (%utf8 raw)))
      (when utf8
        (let ((clean (trim-nuls utf8)))
          (unless (find (code-char 0) clean)
            (return-from decode-credential clean))))
      ;; A UTF-16LE blob of ASCII also decodes as UTF-8 -- it just carries a NUL
      ;; between every character, which no environment variable can hold. Prefer
      ;; the UTF-16 reading when the bytes look like UTF-16 (about half of them
      ;; NUL), not for the lone trailing NUL of a UTF-8 value.
      (when (and (evenp length)
                 (or (null utf8)
                     (>= (count 0 raw) (max 2 (floor (* length 2) 5)))))
        (let ((text (handler-case (%utf16 raw 0 length t) (wincred-error () nil))))
          (when text
            (let ((clean (trim-nuls text)))
              (when (and (not (find (code-char 0) clean)) (plausible-text-p clean))
                (return-from decode-credential clean)))))))
    (error 'credential-decode-error)))
