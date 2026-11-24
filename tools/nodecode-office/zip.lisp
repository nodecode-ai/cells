;;;; zip.lisp --- the ZIP container Office documents are: read it, repack it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A .docx, .pptx and .xlsx is one ZIP holding XML parts. This file reads
;;;; one into memory as entries and writes it back, so an edit is a string
;;;; change on one part and a repack, never a new format. Nothing here is
;;;; specific to Office.
;;;;
;;;; Reading inflates a deflated entry (CHIPZ, which rides with nodecode);
;;;; writing copies an untouched entry's compressed bytes through unchanged
;;;; and stores an edited one uncompressed, which every reader accepts and
;;;; no compressor is needed for. ZIP64, encryption and multi-disk archives
;;;; are refused: an Office file never is one.

(in-package #:nodecode-office)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defconstant +local-header+ #x04034b50)
(defconstant +central-header+ #x02014b50)
(defconstant +end-of-directory+ #x06054b50)

(defun u16 (vector index)
  (logior (aref vector index) (ash (aref vector (1+ index)) 8)))

(defun u32 (vector index)
  (logior (aref vector index)
          (ash (aref vector (+ index 1)) 8)
          (ash (aref vector (+ index 2)) 16)
          (ash (aref vector (+ index 3)) 24)))

(defun put-u16 (vector index value)
  (setf (aref vector index) (ldb (byte 8 0) value)
        (aref vector (1+ index)) (ldb (byte 8 8) value))
  vector)

(defun put-u32 (vector index value)
  (loop for byte from 0 below 4
        do (setf (aref vector (+ index byte)) (ldb (byte 8 (* 8 byte)) value)))
  vector)

(defun read-octets (path)
  "PATH's whole contents as one OCTETS vector."
  (with-open-file (stream path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (unless stream (return-from read-octets nil))
    (let ((vector (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence vector stream)
      vector)))

(defun write-octets (path vector)
  "Write VECTOR to PATH, through a temporary file in the same directory and
one rename, so an interrupted write leaves the old file whole."
  (let* ((path (pathname path))
         (temporary (make-pathname :name (format nil ".~a.tmp" (or (pathname-name path) "office"))
                                    :type nil :defaults path)))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (with-open-file (stream temporary :element-type '(unsigned-byte 8)
                                             :direction :output :if-exists :supersede
                                             :if-does-not-exist :create)
             (write-sequence vector stream))
           (rename-file temporary path))
      (when (probe-file temporary) (ignore-errors (delete-file temporary))))))

;;; --- text in and out of bytes ------------------------------------------------

(defun octets-of (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun string-of (octets &key (start 0) (end (length octets)))
  (sb-ext:octets-to-string octets :start start :end end :external-format :utf-8))

;;; --- CRC-32 -------------------------------------------------------------------

(defparameter *crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (loop for index from 0 below 256
          do (let ((value index))
               (loop repeat 8
                     do (setf value (if (logbitp 0 value)
                                        (logxor (ash value -1) #xEDB88320)
                                        (ash value -1))))
               (setf (aref table index) value)))
    table))

(defun crc32 (octets)
  (let ((crc #xFFFFFFFF))
    (loop for byte across octets
          do (setf crc (logxor (aref *crc-table* (logand (logxor crc byte) #xFF))
                               (ash crc -8))))
    (logand (logxor crc #xFFFFFFFF) #xFFFFFFFF)))

;;; --- an entry ----------------------------------------------------------------

(defstruct (zip-entry (:constructor %make-zip-entry))
  name            ; the part's path inside the archive, e.g. "word/document.xml"
  (method 0)
  (crc 0)
  (compressed-size 0)
  (size 0)
  raw             ; the compressed bytes as the archive holds them
  contents        ; the uncompressed bytes, once computed or once edited
  (edited nil))

(defun zip-entry-data (entry)
  "ENTRY's uncompressed bytes, inflating them the first time."
  (or (zip-entry-contents entry)
      (setf (zip-entry-contents entry)
            (let ((raw (zip-entry-raw entry)))
              (case (zip-entry-method entry)
                (0 raw)
                (8 (chipz:decompress nil 'chipz:deflate raw))
                (t (error "entry ~a uses compression method ~a, which this reader does not know"
                          (zip-entry-name entry) (zip-entry-method entry))))))))

(defun make-zip-entry (name &key raw data method crc compressed-size size)
  (%make-zip-entry :name name :raw raw :contents data :method (or method 0) :crc (or crc 0)
                   :compressed-size (or compressed-size 0) :size (or size 0)))

(defun set-entry-text (entry text)
  "ENTRY's bytes become TEXT's, and it is stored, not deflated."
  (let ((octets (octets-of text)))
    (%make-zip-entry :name (zip-entry-name entry)
                     :method 0
                     :crc (crc32 octets)
                     :compressed-size (length octets)
                     :size (length octets)
                     :raw octets
                     :contents octets
                     :edited t)))

(defun zip-entry-text (entry)
  (string-of (zip-entry-data entry)))

;;; --- reading ------------------------------------------------------------------

(defun find-end-of-directory (octets)
  "The offset of the end-of-central-directory record, reading backwards
through the at-most 64KiB comment it may carry."
  (loop for index downfrom (- (length octets) 22) to (max 0 (- (length octets) 65558))
        do (when (= (u32 octets index) +end-of-directory+) (return index))
        finally (error "this is not a ZIP archive: no end-of-central-directory record")))

(defun zip-entries (octets)
  "OCTETS as a list of ZIP-ENTRY, in the order the central directory holds."
  (let* ((end (find-end-of-directory octets))
         (count (u16 octets (+ end 10)))
         (cursor (u32 octets (+ end 16)))
         (entries '()))
    (dotimes (index count)
      (unless (= (u32 octets cursor) +central-header+)
        (error "ZIP central directory is malformed at entry ~d" index))
      (let* ((method (u16 octets (+ cursor 10)))
             (crc (u32 octets (+ cursor 16)))
             (compressed-size (u32 octets (+ cursor 20)))
             (size (u32 octets (+ cursor 24)))
             (name-length (u16 octets (+ cursor 28)))
             (extra-length (u16 octets (+ cursor 30)))
             (comment-length (u16 octets (+ cursor 32)))
             (local (u32 octets (+ cursor 42)))
             (name (string-of octets :start (+ cursor 46) :end (+ cursor 46 name-length)))
             (data-start (+ local 30 (u16 octets (+ local 26)) (u16 octets (+ local 28))))
             (raw (subseq octets data-start (min (length octets) (+ data-start compressed-size)))))
        (push (make-zip-entry name :raw raw :method method :crc crc
                              :compressed-size compressed-size :size size)
              entries)
        (setf cursor (+ cursor 46 name-length extra-length comment-length))))
    (nreverse entries)))

;;; --- writing ------------------------------------------------------------------

(defun entry-bytes (entry)
  "ENTRY as it is written: its stored bytes and its method."
  (if (or (zip-entry-edited entry) (null (zip-entry-raw entry)))
      (let ((data (zip-entry-data entry)))
        (values data 0 (crc32 data) (length data) (length data)))
      (values (zip-entry-raw entry) (zip-entry-method entry) (zip-entry-crc entry)
              (zip-entry-compressed-size entry) (zip-entry-size entry))))

(defun repack (entries)
  "The archive ENTRIES describe, as one OCTETS vector."
  (let ((chunks '())
        (central '())
        (offset 0))
    (dolist (entry entries)
      (multiple-value-bind (bytes method crc compressed size) (entry-bytes entry)
        (let* ((name (octets-of (zip-entry-name entry)))
               (header (make-array 30 :element-type '(unsigned-byte 8) :initial-element 0)))
          (put-u32 header 0 +local-header+)
          (put-u16 header 4 20)
          (put-u16 header 8 method)
          (put-u32 header 14 crc)
          (put-u32 header 18 compressed)
          (put-u32 header 22 size)
          (put-u16 header 26 (length name))
          (push header chunks)
          (push name chunks)
          (push bytes chunks)
          (push (let ((record (make-array 46 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (put-u32 record 0 +central-header+)
                  (put-u16 record 4 20)
                  (put-u16 record 6 20)
                  (put-u16 record 10 method)
                  (put-u32 record 16 crc)
                  (put-u32 record 20 compressed)
                  (put-u32 record 24 size)
                  (put-u16 record 28 (length name))
                  (put-u32 record 42 offset)
                  record)
                central)
          (push name central)
          (incf offset (+ 30 (length name) (length bytes))))))
    (let* ((central-start offset)
           (central-length (loop for chunk in (reverse central) sum (length chunk)))
           (end (make-array 22 :element-type '(unsigned-byte 8) :initial-element 0))
           (total (length entries)))
      (put-u32 end 0 +end-of-directory+)
      (put-u16 end 8 total)
      (put-u16 end 10 total)
      (put-u32 end 12 central-length)
      (put-u32 end 16 central-start)
      (let ((all (append (nreverse chunks) (nreverse central) (list end))))
        (let ((out (make-array (loop for chunk in all sum (length chunk))
                                :element-type '(unsigned-byte 8)))
              (position 0))
          (dolist (chunk all out)
            (replace out chunk :start1 position)
            (incf position (length chunk))))))))
