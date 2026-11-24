;;;; shape.lisp --- Shodan's firehose, projected for a context window.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One /shodan/host answer for a busy host is hundreds of kilobytes: every
;;;; HTTP body in full, base64 favicons, screenshots, whole certificate
;;;; chains. Handed to a model it spends the window on markup. So the three
;;;; verbosities project it here: summary (identity, location, tags,
;;;; vulnerabilities worst-first, one line per service), detail (adds the
;;;; certificate and the requested headers), raw (Shodan's own JSON, fitted).

(in-package #:nodecode-shodan)

;;; --- reading values out of decoded JSON -------------------------------------------

(defun field (object key &rest more)
  "KEY's value in OBJECT, a hash table; with MORE, the same walk on down."
  (let ((value (and (hash-table-p object) (gethash key object))))
    (if more (apply #'field value more) value)))

(defun text-of (object key &rest more)
  "KEY's string value in OBJECT, with MORE walked on down; NIL when empty."
  (let ((value (apply #'field object key more)))
    (and (stringp value) (plusp (length (nlk:trimmed value))) value)))

(defun num-of (object key)
  (let ((value (field object key)))
    (and (numberp value) value)))

(defun array-of (object key)
  "KEY's value in OBJECT as a list: a JSON array, a single value, or nothing."
  (let ((value (field object key)))
    (cond ((null value) '())
          ((and (vectorp value) (not (stringp value))) (coerce value 'list))
          ((listp value) value)
          (t (list value)))))

(defun vec (list)
  "LIST as a JSON array."
  (coerce list 'vector))

(defun clip (text limit)
  (let ((text (and text (nlk:trimmed (princ-to-string text)))))
    (if (and text (> (length text) limit))
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

(defun one-line (text)
  (and text (substitute #\Space #\Newline (substitute #\Space #\Return text))))

(defun asn-text (value)
  "An ASN as Shodan sends it (\"AS13335\" or 13335), never doubled."
  (let ((text (and value (princ-to-string value))))
    (and text (plusp (length text))
         (if (uiop:string-prefix-p "AS" (string-upcase text)) text (format nil "AS~a" text)))))

(defun clean-object (alist)
  "An object from ALIST, dropping what carries nothing: NIL, the empty
string and the empty array. Zero and false are kept."
  (let ((object (nlk:json-object)))
    (dolist (pair alist object)
      (let ((value (cdr pair)))
        (unless (or (null value)
                    (equal value "")
                    (and (vectorp value) (not (stringp value)) (zerop (length value)))
                    (and (listp value) (null value)))
          (setf (gethash (car pair) object) value))))))

(defun put (object key value)
  "Set KEY on OBJECT when VALUE is worth saying."
  (unless (or (null value) (equal value "") (and (vectorp value) (not (stringp value)) (zerop (length value))))
    (setf (gethash key object) value))
  object)

;;; --- vulnerabilities ---------------------------------------------------------------

(defun shape-vulns (vulns &key (limit 40))
  "Shodan's two shapes into one list, worst first: an object keyed by CVE id
with cvss and summary, or a bare array of ids (what InternetDB answers)."
  (let ((rows
          (cond
            ((hash-table-p vulns)
             (loop for cve being the hash-keys of vulns using (hash-value row)
                   collect (list cve (num-of row "cvss") (clip (text-of row "summary") 200))))
            ((and (vectorp vulns) (not (stringp vulns)))
             (loop for row across vulns
                   collect (if (hash-table-p row)
                               (list (text-of row "cve") (num-of row "cvss") (clip (text-of row "summary") 200))
                               (list (princ-to-string row) nil nil))))
            ((listp vulns)
             (loop for row in vulns collect (list (princ-to-string row) nil nil)))
            (t '()))))
    (vec (loop for (cve cvss summary) in (subseq rows 0 (min limit (length rows)))
               when cve
                 collect (clean-object `(("cve" . ,cve)
                                         ("cvss" . ,cvss)
                                         ("summary" . ,summary)))))))

(defun vuln-count (vulns)
  (cond ((hash-table-p vulns) (hash-table-count vulns))
        ((and (vectorp vulns) (not (stringp vulns))) (length vulns))
        ((listp vulns) (length vulns))
        (t 0)))

;;; --- one service banner ------------------------------------------------------------

(defun service-line (banner verbosity)
  "BANNER, one entry of a host's data array, as one tight line of JSON."
  (let* ((http (field banner "http"))
         (ssl (field banner "ssl"))
         (cert (field ssl "cert"))
         (product (text-of banner "product"))
         (version (text-of banner "version"))
         (descriptor (and (or product version)
                          (nlk:trimmed (format nil "~@[~a~]~@[ ~a~]" product version))))
         (line (clean-object
                `(("port" . ,(num-of banner "port"))
                  ("transport" . ,(or (text-of banner "transport") "tcp"))
                  ("module" . ,(text-of banner "module"))
                  ("product" . ,descriptor)
                  ("title" . ,(clip (one-line (text-of http "title")) 120))
                  ("server" . ,(text-of http "server"))
                  ("os" . ,(text-of banner "os"))
                  ("timestamp" . ,(text-of banner "timestamp"))
                  ("cpe" . ,(vec (array-of banner "cpe")))
                  ("vulns" . ,(vec (array-of banner "vulns")))))))
    (when (string-equal verbosity "detail")
      (put line "ssl"
           (and (hash-table-p ssl)
                (clean-object `(("versions" . ,(vec (array-of ssl "versions")))
                                ("cipher" . ,(text-of ssl "cipher" "name"))
                                ("subject" . ,(text-of cert "subject" "CN"))
                                ("issuer" . ,(text-of cert "issuer" "CN"))
                                ("expired" . ,(field cert "expired"))))))
      (put line "headers" (text-of http "headers"))
      (put line "data" (clip (text-of banner "data") 400)))
    line))

;;; --- a host ------------------------------------------------------------------------

(defun shape-host (raw verbosity)
  "A /shodan/host/{ip} or InternetDB record, projected as VERBOSITY says."
  (if (string-equal verbosity "raw")
      raw
      (let* ((banners (array-of raw "data"))
             (services (sort (loop for banner in banners
                                   when (hash-table-p banner)
                                     collect (service-line banner verbosity))
                             #'< :key (lambda (line) (or (field line "port") 0))))
             (ports (if services
                        (remove-duplicates (loop for line in services
                                                 when (field line "port") collect (field line "port")))
                        (array-of raw "ports")))
             (vulns (field raw "vulns")))
        (clean-object
         `(("ip" . ,(or (text-of raw "ip_str") (text-of raw "ip")))
           ("hostnames" . ,(vec (array-of raw "hostnames")))
           ("domains" . ,(vec (array-of raw "domains")))
           ("org" . ,(text-of raw "org"))
           ("isp" . ,(text-of raw "isp"))
           ("asn" . ,(asn-text (field raw "asn")))
           ("os" . ,(text-of raw "os"))
           ("country" . ,(or (text-of raw "country_name") (text-of raw "country_code")))
           ("city" . ,(text-of raw "city"))
           ("tags" . ,(vec (array-of raw "tags")))
           ("last_update" . ,(text-of raw "last_update"))
           ("open_ports" . ,(vec (sort (remove nil ports) #'<)))
           ("service_count" . ,(if services (length services) (length (array-of raw "ports"))))
           ("services" . ,(vec services))
           ("cpes" . ,(vec (array-of raw "cpes")))
           ("vuln_count" . ,(vuln-count vulns))
           ("vulns" . ,(shape-vulns vulns)))))))

;;; --- facets, matches, the fits -------------------------------------------------------

(defun shape-facets (facets &key (limit 10))
  "Shodan's facets, each row value and count."
  (declare (ignore limit))
  (let ((object (nlk:json-object)))
    (when (hash-table-p facets)
      (loop for name being the hash-keys of facets using (hash-value rows)
            do (setf (gethash name object)
                     (vec (loop for row in (let ((value rows))
                                             (cond ((and (vectorp value) (not (stringp value))) (coerce value 'list))
                                                   ((listp value) value)
                                                   (t '())))
                                when (hash-table-p row)
                                  collect (clean-object `(("value" . ,(field row "value"))
                                                          ("count" . ,(num-of row "count")))))))))
    object))

(defun shape-match (match verbosity)
  "One search match, as a line the model can act on."
  (let* ((http (field match "http"))
         (product (text-of match "product"))
         (version (text-of match "version"))
         (line (clean-object
                `(("ip" . ,(or (text-of match "ip_str") (text-of match "ip")))
                  ("port" . ,(num-of match "port"))
                  ("transport" . ,(or (text-of match "transport") "tcp"))
                  ("org" . ,(text-of match "org"))
                  ("product" . ,(and (or product version)
                                     (nlk:trimmed (format nil "~@[~a~]~@[ ~a~]" product version))))
                  ("title" . ,(clip (one-line (text-of http "title")) 100))
                  ("hostnames" . ,(vec (array-of match "hostnames")))
                  ("domains" . ,(vec (array-of match "domains")))
                  ("country" . ,(or (text-of match "location" "country_name")
                                    (text-of match "country_code")))
                  ("city" . ,(or (text-of match "location" "city") (text-of match "city")))
                  ("timestamp" . ,(text-of match "timestamp"))
                  ("vulns" . ,(vec (array-of match "vulns")))))))
    (when (string-equal verbosity "detail")
      (put line "data" (clip (text-of match "data") 300)))
    line))

(defun shape-search (raw &key query page verbosity total)
  "A /shodan/host/search answer, projected."
  (if (string-equal verbosity "raw")
      raw
      (let ((matches (array-of raw "matches")))
        (clean-object
         `(("query" . ,query)
           ("page" . ,page)
           ("total" . ,(or (num-of raw "total") total))
           ("returned" . ,(length matches))
           ("matches" . ,(vec (loop for match in matches
                                     when (hash-table-p match)
                                       collect (shape-match match verbosity))))
           ("facets" . ,(shape-facets (field raw "facets"))))))))

(defun fit (object max-chars)
  "OBJECT's JSON, cut down to MAX-CHARS by dropping what a summary can lose,
and saying so, rather than truncating mid-JSON."
  (let ((text (nlk:encode-json-object object)))
    (if (<= (length text) max-chars)
        object
        (let ((small (nlk:json-object)))
          (loop for key being the hash-keys of object using (hash-value value)
                unless (member key '("services" "matches" "data" "cpes" "vulns") :test #'string=)
                  do (setf (gethash key small) value))
          (setf (gethash "note" small)
                (format nil "Cut to fit the context window (over ~d characters): the per-service and per-match rows were dropped. Ask again with verbosity \"summary\", a smaller limit, or one target at a time."
                        max-chars))
          (if (<= (length (nlk:encode-json-object small)) max-chars)
              small
              (let ((tiny (nlk:json-object)))
                (loop for key being the hash-keys of small using (hash-value value)
                      unless (member key '("hostnames" "domains" "tags" "note") :test #'string=)
                        do (setf (gethash key tiny) value))
                (setf (gethash "note" tiny)
                      (format nil "Cut to fit the context window: ~a" (gethash "note" small)))
                tiny))))))
