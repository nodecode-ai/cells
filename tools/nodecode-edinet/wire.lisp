;;;; wire.lisp --- the one network seam: the EDINET API v2 over HTTPS.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's client.py in one file: two GETs under /api/v2 -- one file
;;;; date's document list, and one document's bytes -- with the API key in
;;;; the Subscription-Key query parameter, the way EDINET spells it. Nothing
;;;; else is dialled: the base is one host, named in the section. Every byte
;;;; goes through NLK:HTTP, so a test stubs it and nothing reaches the
;;;; network.

(in-package #:nodecode-edinet)

(defparameter +base+ "https://api.edinet-fsa.go.jp/api/v2"
  "EDINET API v2: the only service this cell dials. The section may name another base.")

(defparameter +portal+ "https://disclosure2.edinet-fsa.go.jp/"
  "The viewing site every answer cites, as EDINET's terms (WZEK0030) ask.")

(defparameter +pdl+ "https://www.digital.go.jp/resources/open_data/public_data_license_v1.0"
  "PDL 1.0, the other half of the citation EDINET's terms want.")

(defparameter +key-guide+ "https://disclosure2dl.edinet-fsa.go.jp/guide/static/disclosure/WEEK0060.html"
  "FSA's EDINET API materials page: a key is free, and issued from there.")

(defparameter +user-agent+ "nodecode-edinet/0.1.0")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun settings ()
  "The running settings, or the refusal the model reads while the cell is stopped."
  (running-settings))

(defun option (key &optional default)
  "KEY's value in the running section, or DEFAULT."
  (or (getf (settings) key) default))

(defun api-key ()
  "The key the section names inline, else EDINET_API_KEY from the environment."
  (or (nonempty (getf (settings) :api-key))
      (nonempty (uiop:getenv "EDINET_API_KEY"))))

(defun base-url ()
  "The API base this cell talks to, without a trailing slash."
  (string-right-trim "/" (or (nonempty (getf (settings) :base-url)) +base+)))

(defun check-config ()
  "Refuse a section that would call EDINET with no key, naming where one comes from."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (format nil "edinet: this cell needs an EDINET API key: set \"api_key\" ~
in the edinet section (or name a variable in \"api_key_env\"), or export EDINET_API_KEY. ~
The key is free and issued from FSA's EDINET API materials page (~a)." +key-guide+))))

(defun url-encode (text)
  "TEXT with everything outside an unreserved character percent-encoded."
  (with-output-to-string (out)
    (loop for char across text
          for code = (char-code char)
          do (if (or (alphanumericp char) (find char "-._~" :test #'char=))
                 (write-char char out)
                 (format out "%~2,'0X" code)))))

(defun json-member (object key)
  "KEY's member of OBJECT, a decoded JSON object, or NIL."
  (and (hash-table-p object) (multiple-value-bind (value present) (gethash key object)
                                (and present (not (eq value :null)) value))))

(defun replace-all (text old new)
  "TEXT with every OLD replaced by NEW."
  (if (or (null old) (zerop (length old)))
      text
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search old text :start2 start)
              do (write-string text out :start start :end (or at (length text)))
              while at
              do (write-string new out)
              do (setf start (+ at (length old)))))))

(defun scrub (text)
  "TEXT with the key replaced by ***, so no message carries it."
  (let ((key (ignore-errors (api-key))))
    (if key (replace-all (princ-to-string text) key "***") (princ-to-string text))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters."
  (let ((text (or text "")))
    (if (<= (length text) limit) text (subseq text 0 limit))))

(defun request (path params &key binary)
  "GET PATH under the API base with PARAMS (an alist of strings) and the key.
Answers (values BODY STATUS): BODY is text, or octets with BINARY."
  (let* ((key (api-key))
         (query (format nil "~{~a~^&~}"
                        (append (loop for (name . value) in params
                                      collect (format nil "~a=~a" name (url-encode value)))
                                (list (format nil "Subscription-Key=~a" (url-encode key))))))
         (url (format nil "~a~a?~a" (base-url) path query)))
    (multiple-value-bind (body status)
        (nlk:http :get url
                  :headers (list (cons "user-agent" +user-agent+)
                                 (cons "accept" "*/*"))
                  :timeout (option :timeout-seconds 30)
                  :binary binary)
      (values body (or status 0)))))

(defun body-octets (body)
  "BODY — what NLK:HTTP answered — as octets."
  (typecase body
    (string (sb-ext:string-to-octets body :external-format :utf-8))
    ((vector t) (if (subtypep (array-element-type body) '(unsigned-byte 8))
                    body
                    (coerce body '(vector (unsigned-byte 8)))))
    (t (coerce (or body "") '(vector (unsigned-byte 8))))))

(defun body-text (body)
  "BODY — what NLK:HTTP answered — as text, for a message."
  (typecase body
    (string body)
    ((vector t) (handler-case (sb-ext:octets-to-string (body-octets body) :external-format :utf-8)
                  (error () "")))
    (t "")))

(defun http-refusal (status body)
  "What a refused HTTP STATUS with BODY means, in one line the model can act on."
  (let ((detail (nlk:trimmed (clip (body-text body) 200))))
    (format nil "EDINET answered HTTP ~a~@[ (~a)~]~a"
            status (and (plusp (length detail)) (scrub detail))
            (case status
              ((401 403) ": the API key was rejected. Check \"api_key\" in the edinet section.")
              (404 ": no such document, or a file date outside EDINET's retention.")
              (429 ": too many requests. Wait a few seconds and call again.")
              (t ".")))))

(defun envelope-status (payload)
  "PAYLOAD's metadata.status as an integer, or NIL."
  (let ((status (json-member (json-member payload "metadata") "status")))
    (typecase status
      (string (parse-integer status :junk-allowed t))
      (integer status)
      (t nil))))

(defun envelope-message (payload)
  "PAYLOAD's metadata.message, when it carries one."
  (let ((message (json-member (json-member payload "metadata") "message")))
    (and (stringp message) (nlk:trimmed message))))

(defun decode-payload (text)
  "TEXT decoded as JSON, or NIL when it is not JSON."
  (handler-case (nlk:decode-json text) (error () nil)))

(defun fetch-day (date)
  "The document list JSON for one file date DATE, a hash table, or :NOT-FOUND."
  (multiple-value-bind (body status)
      (request "/documents.json" (list (cons "date" date) (cons "type" "2")))
    (cond
      ((eql status 404) :not-found)
      ((not (eql status 200)) (fail "~a" (http-refusal status body)))
      (t (let ((payload (decode-payload (body-text body))))
           (unless (hash-table-p payload)
             (fail "EDINET answered a file-date list that is not a JSON object: ~a"
                   (scrub (clip (body-text body) 200))))
           (let ((envelope (envelope-status payload)))
             (cond ((eql envelope 404) :not-found)
                   ((or (eql envelope 200) (null envelope)) payload)
                   (t (fail "EDINET answered metadata.status ~a~@[ (~a)~] for ~a."
                            envelope (envelope-message payload) date)))))))))

(defun magic-kind (octets)
  "What OCTETS start with: :pdf, :zip or NIL."
  (cond ((and (>= (length octets) 4)
               (= (aref octets 0) #x25) (= (aref octets 1) #x50)
               (= (aref octets 2) #x44) (= (aref octets 3) #x46)) :pdf)
        ((and (>= (length octets) 2)
               (= (aref octets 0) #x50) (= (aref octets 1) #x4B)) :zip)
        (t nil)))

(defun fetch-document (doc-id type)
  "DOC-ID's bytes for TYPE (1 the submission ZIP, 2 the PDF, 5 the CSV ZIP), as octets."
  (multiple-value-bind (body status)
      (request (format nil "/documents/~a" (url-encode doc-id))
               (list (cons "type" (princ-to-string type)))
               :binary t)
    (unless (eql status 200) (fail "~a" (http-refusal status body)))
    (let ((octets (body-octets body)))
      (unless (plusp (length octets))
        (fail "EDINET answered an empty body for ~a." doc-id))
      ;; HTTP 200 carrying a JSON envelope is EDINET's other failure shape.
      (when (= (aref octets 0) (char-code #\{))
        (let ((payload (decode-payload (body-text octets))))
          (when (envelope-status payload)
            (fail "EDINET answered metadata.status ~a~@[ (~a)~] for ~a."
                  (envelope-status payload) (envelope-message payload) doc-id))))
      (let ((kind (magic-kind octets)))
        (case type
          (2 (unless (eq kind :pdf)
               (fail "EDINET answered something that is not a PDF for ~a; check the doc id and the type."
                     doc-id)))
          ((1 5) (unless (eq kind :zip)
                   (fail "EDINET answered something that is not a ZIP for ~a; check the doc id and the type."
                         doc-id)))))
      octets)))

(defun filing-page (doc-id)
  "The viewing site's page for DOC-ID, the URL EDINET's terms ask a citation to name."
  (format nil "~aWZEK0040.aspx?~a" +portal+ doc-id))

(defun citation (&optional doc-id)
  "The source line every answer ends with: the filing page when there is one, else the portal."
  (format nil "source: EDINET (FSA, Japan) ~a, PDL 1.0 (~a)"
          (if (nonempty doc-id) (filing-page doc-id) +portal+) +pdl+))
