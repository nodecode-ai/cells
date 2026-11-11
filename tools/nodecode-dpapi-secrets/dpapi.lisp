;;;; dpapi.lisp --- the Windows Data Protection API, through SBCL's own alien
;;;; interface.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original called crypt32 through Python's ctypes and declared every
;;;; prototype with explicit argtypes, because an undeclared pointer argument
;;;; is truncated to 32 bits on a 64-bit host and the call fails silently.
;;;; SBCL's alien types say the same thing: every argument below has a type,
;;;; and the two who-is-null pointers are system-area-pointers, not integers.
;;;;
;;;; KEYING. CryptProtectData derives its key from the current Windows user
;;;; profile, with no password, no key file and no prompt: a blob written here
;;;; is readable by that user on that machine and by nobody else -- not by
;;;; another local account, not after the file is copied to another machine,
;;;; and not by the same user after their master keys are reset.
;;;;
;;;; CRYPTPROTECT_UI_FORBIDDEN is the original's choice kept as it was: no
;;;; dialog on a startup path, and a locked or changed profile fails with an
;;;; error instead of popping a prompt nothing can answer. The machine-wide
;;;; flag is not passed, so another account on the box can never read it.
;;;;
;;;; Outside Windows there is nothing to bind: DPAPI-AVAILABLE-P is NIL and
;;;; both calls refuse in words, which is what the original's BINARY_MISSING
;;;; error kind said.

(in-package #:nodecode-dpapi-secrets)

(defconstant +cryptprotect-ui-forbidden+ #x1
  "CRYPTPROTECT_UI_FORBIDDEN: never pop a dialog, never prompt.")

(defun dpapi-available-p ()
  "Whether this platform has the Windows Data Protection API."
  #+win32 t
  #-win32 nil)

#+win32
(progn
  ;; The two libraries the calls live in. Loaded here, at load time, so a
  ;; call never pays for a lookup and a missing DLL is a load failure of this
  ;; folder rather than a surprise mid-turn.
  (sb-alien:load-shared-object "crypt32.dll")
  (sb-alien:load-shared-object "kernel32.dll")

  ;; DATA_BLOB: a length and a pointer, the layout crypt32 reads and writes.
  ;; PB-DATA is a pointer (not the C header's one-byte array) because only
  ;; the two calls below ever touch one.
  (sb-alien:define-alien-type data-blob
    (sb-alien:struct data-blob
      (cb-data sb-alien:unsigned-int)
      (pb-data (* sb-alien:unsigned-char))))

  (defun octets->alien (octets)
    "OCTETS in heap an alien call may read, freed by the caller."
    (let* ((size (length octets))
           (alien (sb-alien:make-alien sb-alien:unsigned-char (max 1 size))))
      (loop for index from 0 below size
            do (setf (sb-alien:deref alien index) (aref octets index)))
      alien))

  (defun alien->octets (blob)
    "The bytes BLOB names, as a fresh vector."
    (let* ((size (sb-alien:slot blob 'cb-data))
           (pointer (sb-alien:slot blob 'pb-data))
           (out (make-array size :element-type '(unsigned-byte 8))))
      (dotimes (index size out)
        (setf (aref out index) (sb-alien:deref pointer index)))))

  (defun call-dpapi (operation source answer)
    "OPERATION -- CryptProtectData or CryptUnprotectData -- on SOURCE into
 ANSWER: 1 when it worked, else 0 and the Win32 error is left to the caller."
    (let ((function (sb-alien:extern-alien operation
                      (sb-alien:function sb-alien:int
                                         (* data-blob)          ; pDataIn
                                         sb-alien:system-area-pointer ; szDataDescr
                                         sb-alien:system-area-pointer ; pOptionalEntropy
                                         sb-alien:system-area-pointer ; pvReserved
                                         sb-alien:system-area-pointer ; pPromptStruct
                                         sb-alien:unsigned-int  ; dwFlags
                                         (* data-blob)))))      ; pDataOut
      (sb-alien:alien-funcall function source
                               (sb-sys:int-sap 0) (sb-sys:int-sap 0)
                               (sb-sys:int-sap 0) (sb-sys:int-sap 0)
                               +cryptprotect-ui-forbidden+ answer)))

  (defun free-dpapi-block (blob)
    "Give BLOB's buffer back to the local allocator crypt32 took it from."
    (let ((pointer (sb-alien:slot blob 'pb-data)))
      (when pointer
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "LocalFree"
           (sb-alien:function sb-alien:system-area-pointer
                              sb-alien:system-area-pointer))
         (sb-alien:alien-sap pointer))
        (setf (sb-alien:slot blob 'pb-data) nil))))

  (defun transform (operation octets)
    "OCTETS through OPERATION: the bytes it answered."
    (let ((in (octets->alien octets)))
      (unwind-protect
           (sb-alien:with-alien ((source data-blob)
                                 (answer data-blob))
             (setf (sb-alien:slot source 'cb-data) (length octets)
                   (sb-alien:slot source 'pb-data)
                   (sb-alien:sap-alien (sb-alien:alien-sap in) (* sb-alien:unsigned-char)))
             (unless (plusp (call-dpapi operation (sb-alien:addr source) (sb-alien:addr answer)))
               (fail "~a failed on this Windows profile (winerror ~a); a store another account wrote, or one written before this profile's keys were reset, cannot be read here"
                     operation (sb-alien:get-errno)))
             (unwind-protect (alien->octets answer)
               (free-dpapi-block answer)))
        (sb-alien:free-alien in))))

  (defun dpapi-protect (octets)
    "OCTETS encrypted for the current Windows user."
    (transform "CryptProtectData" octets))

  (defun dpapi-unprotect (octets)
    "OCTETS, a blob this Windows user's profile wrote, decrypted."
    (transform "CryptUnprotectData" octets)))

#-win32
(defun dpapi-protect (octets)
  "OCTETS encrypted for the current Windows user; nowhere else is there one."
  (declare (ignore octets))
  (fail "Windows DPAPI is only available on Windows: this store is a DPAPI blob, which only the Windows account that wrote it can read"))

#-win32
(defun dpapi-unprotect (octets)
  "OCTETS, a blob this Windows user's profile wrote, decrypted."
  (declare (ignore octets))
  (fail "Windows DPAPI is only available on Windows: this store is a DPAPI blob, which only the Windows account that wrote it can read"))
