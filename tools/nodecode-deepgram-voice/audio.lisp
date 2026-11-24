;;;; audio.lisp --- the bytes of one call: octets in and out, the container
;;;; they are, the file a spoken answer is saved as, and the recording a
;;;; transcribe call reads.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-deepgram-voice)

;;; --- an argument, or the section's answer ----------------------------------------

(defun filled (text)
  "TEXT trimmed, or NIL when it holds nothing: the test the section's default
hangs on, since an empty string is true in Lisp."
  (let ((text (nlk:trimmed (or text ""))))
    (and (plusp (length text)) text)))

;;; --- octets ----------------------------------------------------------------------

(defun octets-of (data)
  "DATA as octets: NLK:HTTP answers a string when the peer sent text."
  (etypecase data
    (string (map '(vector (unsigned-byte 8)) #'char-code data))
    ((array (unsigned-byte 8) (*)) data)
    ((array t (*)) data)
    (null (make-array 0 :element-type '(unsigned-byte 8)))))

(defun octets-text (data)
  "DATA, text or octets, as a string."
  (if (stringp data) data (map 'string #'code-char data)))

(defun ascii (text)
  "TEXT's characters as the octets a container header stores them as."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun starts-with (octets prefix)
  "Whether OCTETS opens with the ASCII PREFIX."
  (let ((prefix (ascii prefix)))
    (and (>= (length octets) (length prefix))
         (equalp (subseq octets 0 (length prefix)) prefix))))

;;; --- is it really audio ------------------------------------------------------------

(defun audio-p (octets extension)
  "Whether OCTETS are a file of the container EXTENSION names, by its signature.
A 200 that is a JSON error object, an HTML page or nothing at all is caught
here rather than saved as a broken sound."
  (let ((n (length octets)))
    (when (< n 4) (return-from audio-p nil))
    (cond
      ((string= extension "wav") (and (starts-with octets "RIFF")
                                       (>= n 12)
                                       (equalp (subseq octets 8 12) (ascii "WAVE"))))
      ((string= extension "ogg") (and (starts-with octets "OggS")
                                       (search (ascii "OpusHead") octets :end2 (min n 512))))
      ((string= extension "flac") (starts-with octets "fLaC"))
      (t (or (starts-with octets "ID3")
             (and (= (aref octets 0) #xFF)
                  (= (logand (aref octets 1) #xE0) #xE0)))))))

;;; --- a name ---------------------------------------------------------------------

(defun slug (text &optional (limit 40))
  "TEXT as a file name's stem: lowercase letters, digits and single dashes, at
most LIMIT of them, never empty."
  (let ((out (make-string-output-stream))
        (last-dash t)
        (count 0))
    (block walk
      (loop for char across (string-downcase text)
            do (cond ((or (alphanumericp char) (> (char-code char) 127))
                      (when (< count limit)
                        (write-char char out)
                        (incf count)
                        (setf last-dash nil)))
                     ((not last-dash)
                      (when (< count limit)
                        (write-char #\- out)
                        (incf count)
                        (setf last-dash t)))
                     (t nil))
            when (>= count limit) do (return-from walk)))
    (let ((text (string-right-trim "-" (get-output-stream-string out))))
      (if (plusp (length text)) text "voice"))))

(defun datestamp ()
  "The local time as YYYYMMDD_HHMMSS, the stamp a saved file's name carries."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0d~2,'0d~2,'0d_~2,'0d~2,'0d~2,'0d"
            year month day hour minute second)))

(defun voice-id ()
  "A fresh 12-hex-digit id, so two calls in one second never collide."
  (format nil "~12,'0x" (random (expt 16 12))))

;;; --- the recording a transcribe call reads -----------------------------------------

(defun resolve-recording (text)
  "TEXT as a pathname: ~/... against the operator's home, an absolute path as
it is, anything else under the Nodecode home."
  (let ((path (nlk:expand-home (nlk:trimmed text))))
    (if (uiop:absolute-pathname-p path) path (nlk:home path))))

(defun content-type-of (path)
  "The content type a recording at PATH is uploaded as."
  (or (cdr (assoc (string-downcase (or (pathname-type path) "")) +content-types+
                  :test #'string=))
      "application/octet-stream"))

(defun read-recording (path)
  "The whole recording at PATH as octets, under +MAX-UPLOAD-BYTES+. A missing
file, a folder, or one too large is a refusal that names what to do."
  (unless (probe-file path)
    (fail "there is no file at ~a: give the recording's path (~/... or absolute), or copy it into the home first"
          (namestring path)))
  (when (uiop:directory-pathname-p (probe-file path))
    (fail "~a is a folder, not a recording: name one file inside it" (namestring path)))
  (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (unless in (fail "~a could not be opened for reading" (namestring path)))
    (let* ((size (file-length in))
           (octets (make-array size :element-type '(unsigned-byte 8))))
      (when (> size +max-upload-bytes+)
        (fail "~a is ~a and one call uploads at most ~a: split it into shorter recordings"
              (file-name path) (nlk:size-text size) (nlk:size-text +max-upload-bytes+)))
      (when (zerop size)
        (fail "~a is empty: there is nothing to transcribe" (file-name path)))
      (read-sequence octets in)
      octets)))

(defun file-name (path)
  "PATH as a name to say out loud: the file's own name and type."
  (format nil "~@[~a~]~@[.~a~]" (pathname-name path) (pathname-type path)))
