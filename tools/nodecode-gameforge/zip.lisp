;;;; zip.lisp --- a ZIP archive, stored entries: what an HTML game upload
;;;; wants, written without a deflater.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A game build is pixels and samples already; deflating them buys a few
;;;; percent and costs a compressor, so every entry is stored. The archive is
;;;; the format's three records and nothing else: each entry's local header
;;;; and bytes, the central directory, and the end record that names it.

(in-package #:nodecode-gameforge)

(defun file-octets (path)
  "PATH's bytes, as a simple octet vector."
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun name-octets (name)
  "NAME as the bytes a ZIP stores: ASCII, which is all a slug can be."
  (let ((octets (make-octets (length name))))
    (push-ascii octets name)
    octets))

(defun write-zip (path entries)
  "Write ENTRIES, a list of (NAME . OCTETS), to PATH as a ZIP archive with
nothing compressed, and answer the namestring. A stored entry's two size
fields are equal, and its CRC is the one the PNG encoder already carries."
  (ensure-directories-exist path)
  (let ((body (make-octets 8192))
        (central (make-octets 1024))
        (count 0))
    (dolist (entry entries)
      (destructuring-bind (name . octets) entry
        (let ((name-bytes (name-octets name))
              (crc (crc32 octets))
              (size (length octets))
              (offset (fill-pointer body)))
          (push-le32 body #x04034b50)   ; local file header
          (push-le16 body 20)           ; version needed
          (push-le16 body 0)            ; flags
          (push-le16 body 0)            ; method: stored
          (push-le16 body 0)            ; time
          (push-le16 body #x21)         ; date: 1980-01-01
          (push-le32 body crc)
          (push-le32 body size)
          (push-le32 body size)
          (push-le16 body (fill-pointer name-bytes))
          (push-le16 body 0)            ; extra
          (push-octets body name-bytes)
          (push-octets body octets)
          (push-le32 central #x02014b50) ; central directory header
          (push-le16 central 20)
          (push-le16 central 20)
          (push-le16 central 0)
          (push-le16 central 0)
          (push-le16 central 0)
          (push-le16 central #x21)
          (push-le32 central crc)
          (push-le32 central size)
          (push-le32 central size)
          (push-le16 central (fill-pointer name-bytes))
          (push-le16 central 0)          ; extra
          (push-le16 central 0)          ; comment
          (push-le16 central 0)          ; disk
          (push-le16 central 0)          ; internal attributes
          (push-le32 central 0)          ; external attributes
          (push-le32 central offset)
          (push-octets central name-bytes)
          (incf count))))
    (let ((offset (fill-pointer body))
          (central-size (fill-pointer central)))
      (push-octets body central)
      (push-le32 body #x06054b50)        ; end of central directory
      (push-le16 body 0)
      (push-le16 body 0)
      (push-le16 body count)
      (push-le16 body count)
      (push-le32 body central-size)
      (push-le32 body offset)
      (push-le16 body 0))                ; comment length
    (with-open-file (stream path :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
      (write-sequence body stream))
    (namestring path)))

(defun zip-names (path)
  "The names a ZIP carries, read back from its end record: what a test can
check without an unzipper on the machine."
  (let* ((octets (file-octets path))
         (tail (max 0 (- (length octets) 22))))
    (loop for start from tail downto 0
          when (and (<= (+ start 4) (length octets))
                    (= (logior (aref octets start)
                               (ash (aref octets (+ start 1)) 8)
                               (ash (aref octets (+ start 2)) 16)
                               (ash (aref octets (+ start 3)) 24))
                       #x06054b50))
            return (loop with count = (logior (aref octets (+ start 10))
                                              (ash (aref octets (+ start 11)) 8))
                         with at = (logior (aref octets (+ start 16))
                                           (ash (aref octets (+ start 17)) 8)
                                           (ash (aref octets (+ start 18)) 16)
                                           (ash (aref octets (+ start 19)) 24))
                         repeat count
                         collect (let* ((name-length (logior (aref octets (+ at 28))
                                                             (ash (aref octets (+ at 29)) 8)))
                                        (extra-length (logior (aref octets (+ at 30))
                                                              (ash (aref octets (+ at 31)) 8)))
                                        (comment-length (logior (aref octets (+ at 32))
                                                                (ash (aref octets (+ at 33)) 8)))
                                        (name (map 'string #'code-char
                                                   (subseq octets (+ at 46) (+ at 46 name-length)))))
                                   (setf at (+ at 46 name-length extra-length comment-length))
                                   name)))))
