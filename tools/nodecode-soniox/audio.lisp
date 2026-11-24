;;;; audio.lisp --- the bytes of one call: octets in and out, the container
;;;; they are, the file a spoken answer is saved as, and the recording a
;;;; transcribe call reads.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-soniox)

;;; --- an argument, or the section's answer ------------------------------------

(defun filled (text)
  "TEXT trimmed, or NIL when it holds nothing: the test a section's default
hangs on, since an empty string is true in Lisp."
  (let ((text (nlk:trimmed (or text ""))))
    (and (plusp (length text)) text)))

;;; --- octets -------------------------------------------------------------------

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

(defun compact-text (data &optional (limit 300))
  "DATA as one line of at most LIMIT characters, for an error's own words."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (octets-text data)))
         (flat (with-output-to-string (out)
                 (loop for char across text
                       do (write-char (if (member char '(#\Newline #\Return #\Tab)) #\Space char)
                                      out)))))
    (if (plusp (length flat)) (subseq flat 0 (min limit (length flat))) "no body")))

(defun ascii (text)
  "TEXT's characters as the octets a container header stores them as."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun starts-with (octets prefix)
  "Whether OCTETS opens with the ASCII PREFIX."
  (let ((prefix (ascii prefix)))
    (and (>= (length octets) (length prefix))
         (equalp (subseq octets 0 (length prefix)) prefix))))

;;; --- is it really audio ---------------------------------------------------------

(defun trim-start (octets)
  "OCTETS past any leading whitespace, so a JSON error body is recognised
whatever the peer padded it with."
  (let ((index 0))
    (loop while (and (< index (length octets))
                     (member (aref octets index) '(9 10 13 32)))
          do (incf index))
    (subseq octets index)))

(defun error-document-p (octets)
  "Whether OCTETS open as a JSON object or an HTML page: a 200 that is one is
an error the endpoint wrapped, not sound."
  (let ((start (trim-start octets)))
    (and (plusp (length start)) (member (aref start 0) '(35 123 60)))))   ; #{ #<

(defun opus-extension (octets)
  "The extension opus bytes are saved as: .ogg when they are an Ogg container,
.opus when they are raw packets."
  (if (starts-with octets "OggS") "ogg" "opus"))

(defun audio-extension (octets format-name)
  "The extension OCTETS are saved as, or NIL when they are no audio at all.
A 200 carrying a JSON error object, an HTML page or nothing is caught here
rather than saved as a broken sound."
  (let ((n (length octets)))
    (when (and (>= n 4) (not (error-document-p octets)))
      (cond
        ((string= format-name "wav")
         (and (starts-with octets "RIFF") (>= n 12)
              (equalp (subseq octets 8 12) (ascii "WAVE"))
              "wav"))
        ((string= format-name "flac") (and (starts-with octets "fLaC") "flac"))
        ((member format-name '("opus" "ogg") :test #'string=) (opus-extension octets))
        (t (and (or (starts-with octets "ID3")
                    (and (= (aref octets 0) #xFF) (= (logand (aref octets 1) #xE0) #xE0)))
                "mp3"))))))

;;; --- a name --------------------------------------------------------------------

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

(defun call-id ()
  "A fresh 12-hex-digit id, so two calls in one second never collide."
  (format nil "~12,'0x" (random (expt 16 12))))

;;; --- the recording a transcribe call reads ---------------------------------------

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
file, a folder, or one too large is a refusal that names what to do, raised
before anything is uploaded."
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
