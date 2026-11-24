;;;; cell-test.lisp --- lifecycle, the tool, /tls and the decoder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here reads a scratch tree of certificates this file builds
;;;; byte by byte: no network, no clock, no file of the operator's. The tool
;;;; is called the way a model's call reaches it, through the real dispatch
;;;; and its contract, so the schema the provider is shown and the body that
;;;; runs are one shape.

(in-package #:nodecode.test)

;;; --- a certificate, built from the bytes a real one has --------------------

(defun tls-octets (&rest vectors)
  (apply #'concatenate '(vector (unsigned-byte 8)) vectors))

(defun tls-der-length (length)
  (if (< length 128)
      (vector length)
      (let ((bytes '()))
        (loop while (plusp length)
              do (push (mod length 256) bytes)
                 (setf length (floor length 256)))
        (coerce (cons (+ 128 (length bytes)) bytes) '(vector (unsigned-byte 8))))))

(defun tls-der (tag content)
  (tls-octets (vector tag) (tls-der-length (length content)) content))

(defun tls-seq (&rest parts)
  (tls-der #x30 (apply #'tls-octets parts)))

(defun tls-chars (text)
  (map 'vector #'char-code text))

(defun tls-oid (text)
  "TEXT (`1.2.840.113549.1.1.11') as a DER OBJECT IDENTIFIER."
  (let* ((arcs (mapcar #'parse-integer (uiop:split-string text :separator ".")))
         (bytes (list (+ (* 40 (first arcs)) (second arcs)))))
    (dolist (arc (cddr arcs))
      ;; The base-128 groups, most significant first, each with a
      ;; continuation bit but the last: push order already ends that way,
      ;; since the last group pushed sits at the head of the list.
      (let ((chunk '()))
        (loop do (push (logand arc #x7f) chunk)
              (setf arc (ash arc -7))
              until (zerop arc))
        (loop for tail on chunk
              do (setf bytes (append bytes
                                      (list (if (cdr tail)
                                                (logior (car tail) #x80)
                                                (car tail))))))))
    (tls-der #x06 (coerce bytes '(vector (unsigned-byte 8))))))

(defparameter +tls-attribute-oids+
  '(("CN" . "2.5.4.3") ("O" . "2.5.4.10") ("C" . "2.5.4.6") ("OU" . "2.5.4.11")) "The attributes these fixtures write into a name.")

(defun tls-name (text)
  "`CN=example.com,O=Example' as an X.501 Name."
  (apply #'tls-seq
         (loop for pair in (uiop:split-string text :separator ",")
               for parts = (uiop:split-string pair :separator "=")
               for oid = (or (cdr (assoc (first parts) +tls-attribute-oids+ :test #'string=)) "2.5.4.3")
               collect (tls-der #x31
                                 (tls-seq (tls-oid oid)
                                          (tls-der #x0c (tls-chars (second parts))))))))

(defun tls-integer (octets)
  (tls-der #x02 (if (plusp (logand (aref octets 0) #x80))
                     (tls-octets (vector 0) octets)
                     octets)))

(defun tls-rsa-spki (bits)
  "An RSA public key whose modulus is exactly BITS bits (a multiple of 8)."
  (let* ((count (ceiling bits 8))
         (modulus (make-array count :element-type '(unsigned-byte 8) :initial-element #xa5)))
    (setf (aref modulus 0) #xc3)
    (tls-seq (tls-seq (tls-oid "1.2.840.113549.1.1.1") (tls-der #x05 (vector)))
             (tls-der #x03 (tls-octets (vector 0)
                                        (tls-seq (tls-integer modulus)
                                                 (tls-integer (vector 1 0 1))))))))

(defun tls-ec-spki (curve)
  (tls-seq (tls-seq (tls-oid "1.2.840.10045.2.1") (tls-oid curve))
           (tls-der #x03 (vector 0 #x04 #x01 #x02 #x03 #x04))))

(defun tls-certificate (&key (subject "CN=example.com,O=Example")
                              (issuer "CN=Example CA,O=Example")
                              (not-before "250101000000Z") (not-after "350101000000Z")
                              (rsa-bits 2048) curve
                              (signature "1.2.840.113549.1.1.11"))
  "A certificate of the shape a TLS server presents, over the fields a
posture check reads."
  (let* ((algorithm (tls-seq (tls-oid signature) (tls-der #x05 (vector))))
         (tbs (tls-seq (tls-der #xa0 (tls-integer (vector 2)))
                       (tls-integer (vector 1))
                       algorithm
                       (tls-name issuer)
                       (tls-seq (tls-der #x17 (tls-chars not-before))
                                (tls-der #x17 (tls-chars not-after)))
                       (tls-name subject)
                       (if curve (tls-ec-spki curve) (tls-rsa-spki rsa-bits)))))
    (tls-seq tbs algorithm (tls-der #x03 (vector 0 #xde #xad)))))

(defun tls-base64 (octets)
  "OCTETS as base64, wrapped at 64 characters as PEM writes it."
  (let ((alphabet nodecode-tls-posture::+base64+)
        (out (make-string-output-stream))
        (column 0))
    (flet ((emit (value)
             (write-char (char alphabet value) out)
             (incf column)
             (when (= column 64) (write-char #\Newline out) (setf column 0))))
      (loop for index from 0 below (length octets) by 3
            for a = (aref octets index)
            for b = (if (< (1+ index) (length octets)) (aref octets (1+ index)) 0)
            for c = (if (< (+ index 2) (length octets)) (aref octets (+ index 2)) 0)
            for group = (logior (ash a 16) (ash b 8) c)
            for remaining = (- (length octets) index)
            do (emit (ldb (byte 6 18) group))
               (emit (ldb (byte 6 12) group))
               (if (>= remaining 2) (emit (ldb (byte 6 6) group)) (write-char #\= out))
               (if (>= remaining 3) (emit (ldb (byte 6 0) group)) (write-char #\= out))))
    (get-output-stream-string out)))

(defun tls-pem (&rest certificates)
  "CERTIFICATES as one PEM file: a chain when there is more than one."
  (with-output-to-string (out)
    (dolist (der certificates)
      (write-string "-----BEGIN CERTIFICATE-----" out)
      (terpri out)
      (write-string (string-right-trim '(#\Newline) (tls-base64 der)) out)
      (terpri out)
      (write-string "-----END CERTIFICATE-----" out)
      (terpri out))))

;;; --- the scratch tree -------------------------------------------------------

(defvar *tls-root* nil "The tree the current test audits, bound by WITH-TLS-FIXTURE.")

(defun tls-write (relative text)
  "RELATIVE under *TLS-ROOT*, its parents made."
  (let ((path (merge-pathnames relative *tls-root*)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :element-type 'base-char)
      (write-string text out))
    path))

(defun tls-stage ()
  "A host's TLS in one tree: an expired self-signed certificate, a 1024-bit
SHA-1 one, a healthy one, a Let's Encrypt pair, and the server block that
serves two of them."
  (tls-write "etc/ssl/certs/expired.pem"
             (tls-pem (tls-certificate :subject "CN=gone.example,O=Example"
                                        :issuer "CN=gone.example,O=Example"
                                        :not-before "180101000000Z"
                                        :not-after "210101000000Z")))
  (tls-write "etc/ssl/certs/sha1-1024.pem"
             (tls-pem (tls-certificate :signature "1.2.840.113549.1.1.5" :rsa-bits 1024)))
  (tls-write "etc/ssl/certs/healthy.pem"
             (tls-pem (tls-certificate :subject "CN=healthy.example,O=Example")))
  (tls-write "etc/letsencrypt/live/example.com/fullchain.pem"
             (tls-pem (tls-certificate :subject "CN=served.example,O=Example")
                      (tls-certificate :subject "CN=Example CA,O=Example"
                                       :issuer "CN=Example CA,O=Example")))
  (tls-write "etc/letsencrypt/live/example.com/cert.pem"
             (tls-pem (tls-certificate :subject "CN=served.example,O=Example")))
  (tls-write "etc/nginx/nginx.conf" "server {
  listen 443 ssl;
  ssl_certificate /etc/ssl/certs/expired.pem;
  ssl_protocols TLSv1 TLSv1.2;
  ssl_ciphers 'HIGH:!aNULL:!MD5:RC4-SHA';
}

server {
  listen 443 ssl;
  ssl_certificate /etc/letsencrypt/live/example.com/cert.pem;
}
"))

(defmacro with-tls-fixture (&body body)
  "BODY over a fresh tree holding the fixture host."
  `(with-temp-trees ((*tls-root* "tls-posture"))
     (tls-stage)
     ,@body))

;;; --- the slice, and the lifecycle every cell answers the same way ----------

(define-test-slice "tls-posture" "TLS-POSTURE-CELL-" :start nodecode-tls-posture:start-cell
  :defaults (("root" (namestring *tls-root*))))

(define-cell-lifecycle-tests "tls-posture"
  (:fixture with-tls-fixture)
  (:help :tls)
  (:command "tls")
  (:running (is (nle::find-tool "tls_posture") "the tool is registered")
            (is (= 30 (nodecode-tls-posture::setting :expiry-days)) "the default window")
            (is (uiop:directory-exists-p (nodecode-tls-posture::setting :root)) "root resolved once"))
  (:stopped (is (null (nle::find-tool "tls_posture")) "stop unregisters the tool"))
  (:refused ("expiry_days" 0)))

(deftest tls-posture-cell-a-section-without-a-root-is-refused ()
  (with-tls-fixture
    (let ((refusal (refusal-text nlk:config-refusal
                       (nodecode-tls-posture:start-cell
                        (nlk:json-object "tls-posture" (nlk:make-json-object))))))
      (is (search "root" refusal) "it names the member to set"))
    (is (null (nle::find-tool "tls_posture")) "a refused start installed nothing")
    (let ((refusal (refusal-text nlk:config-refusal
                       (tls-posture-start "root" "/nonexistent/tls-posture/root/"))))
      (is (search "not a directory" refusal) "and a root that is not there"))))

;;; --- the tool, through the dispatch a model's call takes --------------------

(defun tls-arguments (&rest pairs)
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun tls-answer (&rest pairs)
  "The tool's answer to PAIRS, as the model reads it."
  (nle::message-content
   (execute-wire-call "tls-posture-1" "tls_posture" (apply #'tls-arguments pairs))))

(defun tls-object (&rest pairs)
  (nlk:decode-json (apply #'tls-answer pairs)))

(defun tls-finding (object check)
  "The finding OBJECT carries for CHECK, or NIL."
  (find check (nlk:json-array object "findings")
        :key (lambda (finding) (nlk:json-value finding :string "check"))
        :test #'string=))

(defun tls-field (finding name)
  (nlk:json-value finding :string name))

(deftest tls-posture-cell-the-tool-answers-every-finding-the-tree-holds ()
  (with-tls-fixture
    (with-cell-stop ((tls-posture-start))
      (let* ((object (tls-object))
             (grade (nlk:json-value object :string "grade")))
        (is (equal "ACTION REQUIRED" grade) "a high finding is action required")
        (is (search "not a statement that the target is secure"
                    (nlk:json-value object :string "verdict")) "the verdict never claims safety")
        (is-present (expired (tls-finding object "TLS-001")) "the expired certificate"
          (is (search "etc/ssl/certs/expired.pem" (tls-field expired "assertion")) "the expired file is named")
          (is (equal "high" (tls-field expired "severity")))
          (is (search "Served by etc/nginx/nginx.conf:3" (tls-field expired "assertion")) "a certificate finding names the server block that serves it")
          (is (member "served" (coerce (nlk:json-array expired "tags") 'list)
                      :test #'string=) "and carries the served tag"))
        (is-present (weak (tls-finding object "TLS-004")) "the 1024-bit key"
          (is (search "RSA key is only 1024 bits" (tls-field weak "title")))
          (is (equal "high" (tls-field weak "severity")) "below 2048 bits is high"))
        (is-present (sha1 (tls-finding object "TLS-005")) "the SHA-1 signature"
          (is (search "SHA1" (string-upcase (tls-field sha1 "title")))))
        (is (search "self-signed" (tls-field (tls-finding object "TLS-006") "title")) "the self-signed certificate is reported, not graded as a defect")
        (is-present (chain (tls-finding object "TLS-007")) "the leaf served beside its chain"
          (is (search "fullchain.pem" (tls-field chain "assertion")) "the chain file beside the leaf is named"))
        (let ((protocol (tls-finding object "TLS-010")))
          (is (search "TLS 1.0" (tls-field protocol "title")) "the ENABLED version")
          (is (not (search "TLS 1.2" (tls-field protocol "title"))) "TLS 1.2 is not reported as deprecated"))
        (let ((cipher (tls-finding object "TLS-011")))
          (is (search "RC4-SHA" (tls-field cipher "title")))
          (is (not (search "MD5" (tls-field cipher "title"))) "an exclusion is the fix, not the finding"))
        (is (member "etc/ssl/certs/expired.pem"
                    (coerce (nlk:json-array (nlk:json-value object :object "coverage") "inputs_read")
                            'list)
                    :test #'string=) "coverage names every file the run read")))))

(deftest tls-posture-cell-an-empty-tree-is-inconclusive-not-clean ()
  (with-tls-fixture
    (with-temp-trees ((empty "\"tls-empty\""))
      (ensure-directories-exist empty)
      (with-cell-stop ((tls-posture-start))
        (let* ((object (tls-object "root" (namestring empty)))
               (coverage (nlk:json-value object :object "coverage")))
          (is (equal "INCONCLUSIVE" (nlk:json-value object :string "grade")) "nothing read is not the same as nothing wrong")
          (is (plusp (length (nlk:json-array coverage "checks_skipped"))) "and the checks that could not run are named")
          (is (zerop (length (nlk:json-array object "findings")))))))))

(deftest tls-posture-cell-a-configured-certificate-that-is-missing-is-a-finding ()
  (with-tls-fixture
    (with-temp-trees ((other "tls-elsewhere"))
      (let ((root other))
        (setf *tls-root* root)
        (tls-write "etc/apache2/sites-enabled/ssl.conf" "SSLCertificateFile /etc/ssl/certs/absent.pem
SSLCertificateChainFile /etc/ssl/certs/chain.pem
")
        (with-cell-stop ((tls-posture-start))
          (let* ((object (tls-object))
                 (missing (tls-finding object "TLS-008")))
            (is-present missing "the missing certificate"
              (is (search "absent.pem" (tls-field missing "assertion")))
              (is (equal "high" (tls-field missing "severity"))))
            (is (equal "ACTION REQUIRED" (nlk:json-value object :string "grade")) "a configured certificate that is not there is action required")
            (is (find-if (lambda (line)
                           (search "No certificate material" line))
                         (coerce (nlk:json-array (nlk:json-value object :object "coverage") "limitations")
                                 'list)) "and says so in the limitations")))))))

(deftest tls-posture-cell-a-relative-reference-is-a-limitation-not-a-guess ()
  (with-tls-fixture
    (with-temp-trees ((other "tls-relative"))
      (setf *tls-root* other)
      (tls-write "etc/nginx/conf.d/app.conf" "server {
  listen 443 ssl;
  ssl_certificate certs/app.crt;
  ssl_protocols TLSv1.2 TLSv1.3;
}
")
      (with-cell-stop ((tls-posture-start))
        (let* ((object (tls-object))
               (limitations (coerce (nlk:json-array (nlk:json-value object :object "coverage") "limitations")
                                    'list)))
          (is (find-if (lambda (line) (search "RELATIVE" line)) limitations) "a relative path is recorded, never resolved")
          (is (null (tls-finding object "TLS-010")) "TLS 1.2 and 1.3 only is clean")
          (is (null (tls-finding object "TLS-008")) "and no finding was invented"))))))

(deftest tls-posture-cell-the-decoder-reads-key-size-and-digest ()
  (let ((rsa (nodecode-tls-posture:decode-certificate (tls-certificate :rsa-bits 1024
                                                                        :signature "1.2.840.113549.1.1.5")))
        (ec (nodecode-tls-posture:decode-certificate (tls-certificate :curve "1.2.840.10045.3.1.7"))))
    (is (equal "RSA" (nodecode-tls-posture:cert-key-type rsa)))
    (is (= 1024 (nodecode-tls-posture:cert-key-bits rsa)))
    (is (equal "sha1" (nodecode-tls-posture:cert-sig-algo rsa)))
    (is (equal "CN=example.com, O=Example" (nodecode-tls-posture:cert-subject rsa)) "the subject is reported as text")
    (is (equal "EC" (nodecode-tls-posture:cert-key-type ec)))
    (is (= 256 (nodecode-tls-posture:cert-key-bits ec)))
    (is (null (nodecode-tls-posture:decode-certificate (vector 1 2 3))) "a blob that is not a certificate decodes to nothing, never to a pass")))

(deftest tls-posture-cell-a-cipher-and-a-protocol-are-read-as-offers ()
  (is (equal '("RC4-SHA") (nodecode-tls-posture::offered-weak-ciphers "HIGH:!aNULL:!MD5:RC4-SHA")))
  (is (null (nodecode-tls-posture::offered-weak-ciphers "HIGH:!aNULL:!MD5:!3DES")) "a string that names a weak suite to disable it offers nothing")
  (is (equal '("TLS 1.0") (nodecode-tls-posture::weak-protocol-labels '("TLSv1" "TLSv1.2"))))
  (is (null (nodecode-tls-posture::weak-protocol-labels '("TLSv1.2" "TLSv1.3"))) "TLSv1 is not a substring match for TLSv1.2")
  (is (null (nodecode-tls-posture::weak-protocol-labels '("all" "-SSLv3" "-TLSv1" "-TLSv1.1"))) "Apache writes the versions it removes"))

(deftest tls-posture-cell-the-command-answers-the-summary ()
  (with-tls-fixture
    (with-cell-stop ((tls-posture-start))
      (let ((text (nodecode-tls-posture::run-command "" "tls-1")))
        (is (search "tls-posture" text))
        (is (search "ACTION REQUIRED" text))
        (is (search "TLS-001" text) "the worst finding is named first")
        (is (search "checks run" text) "and what ran is said")))))
