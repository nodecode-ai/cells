;;;; records.lisp --- what a probe leaves behind, and what two records say
;;;; side by side.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A saved record is the bundle's own JSON under the profile's folder, one
;;;; file per probe, named by the probe's id: a directory of evidence the
;;;; operator can copy to another machine by hand. The comparison is the
;;;; original's: stage statuses and the one detail that decides each stage,
;;;; a verdict from the two overall outcomes, hypotheses worded as
;;;; hypotheses, one manual follow-up and the limitations that keep a
;;;; difference from reading as a root cause.
;;;;
;;;; Field names are the original's -- verdict, first_divergence, headline,
;;;; findings, next_tests, limitations -- so an answer read here is the
;;;; answer the plugin gave.

(in-package #:nodecode-network-xray)

;;; --- the directory a profile's evidence sits in ------------------------------------

(defun records-directory ()
  "Where this profile's records live: the section's directory when it names
one, else the cell's own folder under the profile's home."
  (let ((named (nlk:trimmed (or (setting :directory) ""))))
    (uiop:ensure-directory-pathname
     (if (plusp (length named))
         (uiop:parse-native-namestring named)
         (nlk:home "network-xray/")))))

(defun record-path (id)
  "ID's record file."
  (merge-pathnames (format nil "~a.json" id) (records-directory)))

(defun valid-id-p (text)
  "Whether TEXT is a record id this cell wrote: hex and dashes, 1..36 characters."
  (and (stringp text) (<= 1 (length text) 36)
       (every (lambda (character)
                (or (digit-char-p character 16) (char= character #\-)))
              text)))

(defun save-record (bundle origin)
  "BUNDLE saved as ORIGIN's record ('local' or 'imported') => the record as
the answer carries it. The file is the cell's own: one directory, one file
per id, written whole and left private."
  (let* ((directory (records-directory))
         (id (nlk:json-value bundle :string "id"))
         (path (record-path id)))
    (ensure-directories-exist (merge-pathnames "record.json" directory))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
      (write-string (nlk:encode-json-object
                     (nlk:make-json-object "id" id "origin" origin "probe" bundle))
                    out))
    (ignore-errors (sb-posix:chmod (uiop:native-namestring path) #o600))
    (nlk:make-json-object "id" id "origin" origin
                           "path" (file-namestring path)
                           "probe" bundle)))

(defun read-record (id)
  "ID's record as it was saved => (values RECORD BUNDLE), refusing what the
cell did not write."
  (unless (valid-id-p id)
    (fail "~s is not a probe id: use the exact ids /network-xray history lists" id))
  (let ((path (record-path id)))
    (unless (probe-file path)
      (fail "no probe ~a is saved in this profile: list the history and use one of its ids" id))
    (let ((record (nlk:decode-json (uiop:read-file-string path) :whole t)))
      (unless (hash-table-p record)
        (fail "the record file for ~a is not a JSON object" id))
      (let ((bundle (nlk:json-value record :object "probe")))
        (unless bundle
          (fail "the record file for ~a carries no probe" id))
        (values record bundle)))))

(defun record-overall (bundle)
  "BUNDLE's outcome: healthy, failed or inconclusive -- the original's
reading of the ladder, never a guess made from a stage that did not run."
  (let ((capture (nlk:json-value bundle :string "capture" "status"))
        (stages (nlk:json-value bundle :array "stages")))
    (cond
      ((not (equal capture "complete")) "inconclusive")
      ((and stages (equal "ok" (nlk:json-value (aref stages 3) :string "status"))) "healthy")
      ((and stages (some (lambda (stage) (equal "error" (nlk:json-value stage :string "status")))
                         stages))
       "failed")
      (t "inconclusive"))))

(defun overall (bundle)
  "BUNDLE's outcome, as the record's summary names it."
  (record-overall bundle))

(defun history (&optional limit)
  "The newest records this profile holds, at most LIMIT => the object the
history answer carries. No network call: this reads the directory alone."
  (let ((directory (records-directory))
        (limit (max 1 (or limit (setting :max-records) 50)))
        (entries '()))
    (when (probe-file directory)
      (dolist (path (directory (merge-pathnames "*.json" directory)))
        (let ((record (ignore-errors (nlk:decode-json (uiop:read-file-string path)
                                                       :whole t))))
          (when (hash-table-p record)
            (let ((bundle (nlk:json-value record :object "probe")))
              (when bundle
                (push (nlk:make-json-object
                       "id" (or (nlk:json-value bundle :string "id") "")
                       "label" (or (nlk:json-value bundle :string "vantage" "label") "")
                       "url" (or (nlk:json-value bundle :string "target" "url") "")
                       "captured_at" (or (nlk:json-value bundle :string "captured_at") "")
                       "overall" (record-overall bundle)
                       "origin" (or (nlk:json-value record :string "origin") "local")
                       "digest" (if (digest-matches-p bundle) "verified" "mismatch"))
                      entries)))))))
    (setf entries (sort entries #'string>
                         :key (lambda (entry)
                                ;; the capture time, then the id, as the original
                                ;; ordered two records taken in the same second.
                                (format nil "~a ~a"
                                        (nlk:json-value entry :string "captured_at")
                                        (nlk:json-value entry :string "id")))))
    (nlk:make-json-object "probes" (coerce (subseq entries 0 (min limit (length entries)))
                                            'vector))))

;;; --- two records, side by side -----------------------------------------------------

(defparameter *base-limitations*
  '("Point-in-time direct-connection observations; proxies and browser trust stores may behave differently."
    "Imported results are not authenticated; hashes detect content changes, not origin."
    "Each probe attempts a single address; untested addresses and paths may behave differently.")
  "What any comparison says before it says anything about the two records.")

(defun finding (kind stage text)
  "One finding of a comparison."
  (nlk:make-json-object "kind" kind "stage" stage "text" text))

(defun stage-of (bundle index)
  "BUNDLE's stage INDEX, or an empty one when a bundle lacks it."
  (let ((stages (nlk:json-value bundle :array "stages")))
    (if (and stages (> (length stages) index))
        (aref stages index)
        (empty-stage "unknown"))))

(defun differences (left right)
  "The stage statuses and the one deciding detail that differ => (values
STATUS-FINDINGS DETAIL-FINDINGS)."
  (let ((statuses '()) (details '())
        (keys '(("dns" . "addresses") ("tcp" . "address")
                ("tls" . "protocol") ("http" . "status_code"))))
    (loop for index from 0 below 4
          for name = (aref *stage-names* index)
          for first = (stage-of left index)
          for second = (stage-of right index)
          do (let ((one (nlk:json-value first :string "status"))
                   (two (nlk:json-value second :string "status")))
               (unless (equal one two)
                 (push (finding "observation" name
                                (format nil "~a statuses differ: left ~a, right ~a."
                                        (string-upcase name) one two))
                       statuses)))
             (let* ((key (cdr (assoc name keys :test #'string=)))
                    (one (nlk:json-value first :any "details" key))
                    (two (nlk:json-value second :any "details" key)))
               (when (and one two (not (equal one two)))
                 (push (finding
                        "observation" name
                        (cond ((string= name "dns")
                               "Recorded DNS address sets differ; CDN or resolver variation is possible.")
                              ((string= name "tcp")
                               "The selected connection addresses differ; each probe tests only one address.")
                              ((string= name "tls")
                               "Recorded TLS protocols differ; the endpoints or their configuration may differ, not proof of attack.")
                              (t (format nil "HTTP status codes differ: left ~a, right ~a." one two))))
                       details))))
    (values (nreverse statuses) (nreverse details))))

(defun selected-address (bundle)
  "BUNDLE's connect_ip when it overrode DNS, else the address it connected to."
  (or (let ((override (nlk:json-value bundle :string "target" "connect_ip")))
        (unless (eq :null override) override))
      (nlk:json-value (stage-of bundle 1) :string "details" "address")
      (let ((addresses (nlk:json-value (stage-of bundle 0) :array "details" "addresses")))
        (and addresses (plusp (length addresses)) (aref addresses 0)))))

(defun loopback-text-p (text)
  "Whether TEXT is a loopback address, as the original read one."
  (and (stringp text)
       (or (uiop:string-prefix-p "127." text) (string= text "::1")
           (string= text "0:0:0:0:0:0:0:1"))))

(defun limitations (left right)
  "The comparison's limitations: the base three, when the two were taken,
and what each record's own policy hid."
  (let ((lines (copy-list *base-limitations*)))
    (push (format nil "Capture times: left ~a, right ~a. Recorded results can be stale and do not establish current health."
                  (nlk:json-value left :string "captured_at")
                  (nlk:json-value right :string "captured_at"))
          lines)
    (loop for (side . bundle) in (list (cons "Left" left) (cons "Right" right))
          do (let ((override (nlk:json-value bundle :string "target" "connect_ip")))
               (unless (eq :null override)
                 (push (format nil "~a uses an explicit connect_ip override (~a); the original Host, TLS server name and certificate verification are preserved. This is not normal-DNS success."
                               side override)
                       lines)))
             (when (nlk:json-value (stage-of bundle 3) :boolean "details" "redirect")
               (push (format nil "~a received a redirect. It was not followed; the destination remains untested." side)
                     lines))
             (when (nlk:json-value (stage-of bundle 0) :boolean "details" "truncated")
               (push (format nil "~a's DNS address list was truncated; unrecorded addresses cannot be compared." side)
                     lines)))
    (nreverse lines)))

(defun hypotheses (left right)
  "What the two records suggest, worded as hypotheses and never as a cause."
  (let ((found '())
        (addresses (list (selected-address left) (selected-address right))))
    (when (and (every #'loopback-text-p addresses))
      (push (finding "hypothesis" "tcp"
                     "The recorded destinations are loopback addresses: each machine reaches itself. A service listening on one machine need not be listening on the other; check the intended service host before changing firewall or DNS settings.")
            found))
    (loop for (side . bundle) in (list (cons "A" left) (cons "B" right))
          do (loop for index from 0 below 4
                   do (let* ((stage (stage-of bundle index))
                             (status (nlk:json-value stage :string "status"))
                             (code (nlk:json-value stage :string "error" "code"))
                             (http (nlk:json-value stage :integer "details" "status_code"))
                             (advice
                               (cond
                                 ((not (equal status "error")) nil)
                                 ((equal code "dns_failed")
                                  "Name resolution failed. Compare resolver configuration and whether this hostname exists on both networks; the service itself has not been tested from this vantage.")
                                 ((equal code "connection_refused")
                                  "A connection was refused at the selected address and port. Check whether the intended service is listening there; an active rejection is different from a timeout.")
                                 ((equal code "socket_timeout")
                                  "The operation timed out. Routing, filtering, an unresponsive endpoint or network delay could explain it; this capture does not distinguish them.")
                                 ((equal code "certificate_invalid")
                                  "TCP connected but certificate verification failed. Check the certificate hostname, trust chain, validity and local clock; do not bypass verification.")
                                 ((equal code "tls_failed")
                                  "TCP connected but TLS negotiation failed. Compare the endpoint and protocol configuration before investigating application paths.")
                                 ((member http '(401 403))
                                  "The server rejected an anonymous HEAD request. Authentication, access policy or method handling may differ from a signed-in browser; this collector sends no credentials.")
                                 ((eql http 405)
                                  "The server rejects HEAD. A GET request may behave differently; this tool has not tested GET. Check the endpoint in an approved browser instead of treating this as a TCP failure.")
                                 ((eql http 404)
                                  "The HTTP endpoint was reached but the path returned 404. Check the requested path and virtual-host routing; DNS and TCP completed for this capture.")
                                 ((and http (>= http 500))
                                  "The server returned an application or upstream error after the preceding network stages completed. Inspect service and upstream health rather than assuming DNS failed."))))
                     (when advice
                       (push (finding "hypothesis" (aref *stage-names* index)
                                      (format nil "Perspective ~a: ~a" side advice))
                             found)))))
    (nreverse found)))

(defun next-tests (left right observations)
  "One manual follow-up at most: the peer's address, dialed from the other
vantage with the original Host and TLS server name."
  (let ((suggestions '()) (seen '()))
    (loop for (side . current) in (list (cons "left" left) (cons "right" right))
          for peer = (if (string= side "left") right left)
          do (let ((address (nlk:json-value (stage-of peer 1) :string "details" "address")))
               (when (and (equal "ok" (nlk:json-value (stage-of peer 1) :string "status"))
                          address
                          (not (equal address (selected-address current)))
                          (not (member address seen :test #'string=)))
                 (push address seen)
                 (push (nlk:make-json-object
                        "title" (format nil "Manually test the peer's address from the ~a vantage" side)
                        "reason" "A confirmed follow-up can distinguish address selection from vantage effects. Preserve the original Host and TLS server name, and full certificate verification; this is an explicit override, not normal DNS."
                        "url" (nlk:json-value left :string "target" "url")
                        "connect_ip" address
                        "requires_confirmation" t)
                       suggestions))))
    (when (and suggestions (not observations)) (setf suggestions '()))
    (coerce (nreverse suggestions) 'vector)))

(defun compare-records (left right)
  "LEFT and RIGHT, two saved bundles, compared => the comparison object.
A difference is a difference: the verdict names the two outcomes, the
findings keep observation, hypothesis and limitation apart, and no
suggested test is run."
  (unless (equal (nlk:json-value left :string "id")
                 (nlk:json-value right :string "target" "url"))
    nil)
  (when (equal (nlk:json-value left :string "id") (nlk:json-value right :string "id"))
    (fail "choose two different probe ids"))
  (unless (equal (nlk:json-value left :string "target" "url")
                 (nlk:json-value right :string "target" "url"))
    (fail "a comparison needs the same canonical URL on both sides: ~a and ~a"
          (nlk:json-value left :string "target" "url")
          (nlk:json-value right :string "target" "url")))
  (multiple-value-bind (statuses details) (differences left right)
    (let* ((observations (append statuses details))
           (outcomes (list (record-overall left) (record-overall right)))
           (verdict (cond ((member "inconclusive" outcomes :test #'equal) "inconclusive")
                          ((equal outcomes '("healthy" "healthy")) "both_healthy")
                          ((equal outcomes '("failed" "failed")) "both_failed")
                          (t "diverged")))
           (headline
             (cond
               ((string= verdict "both_healthy")
                "Both recorded HEAD probes succeeded; observed differences are not failures.")
               ((string= verdict "both_failed")
                "Both recorded probes failed; this does not establish a common cause.")
               ((string= verdict "inconclusive")
                "Incomplete capture prevents a conclusive outcome comparison.")
               (t "The recorded outcomes differ between these vantage observations.")))
           (refusals '()))
      (when (string= verdict "diverged")
        (loop for (failed-side . failed) in (list (cons "A" left) (cons "B" right))
              for other = (if (string= failed-side "A") right left)
              do (when (and (equal "connection_refused"
                                    (nlk:json-value (stage-of failed 1) :string "error" "code"))
                            (equal "ok" (nlk:json-value (stage-of other 1) :string "status")))
                   (setf headline
                         (format nil "TCP connects from ~a; ~a records a refused connection."
                                 (if (string= failed-side "A") "B" "A") failed-side)))))
      (loop for (side . bundle) in (list (cons "Left" left) (cons "Right" right))
            do (loop for index from 0 below 4
                     do (let ((stage (stage-of bundle index)))
                          (when (equal "error" (nlk:json-value stage :string "status"))
                            (push (finding "observation" (aref *stage-names* index)
                                           (format nil "~a ~a recorded ~a: ~a"
                                                   side (string-upcase (aref *stage-names* index))
                                                   (nlk:json-value stage :string "error" "code")
                                                   (nlk:json-value stage :string "error" "message")))
                                  refusals)))))
      (let* ((limits (limitations left right))
             (findings (append observations (nreverse refusals)
                                (mapcar (lambda (text) (finding "limitation" "capture" text))
                                        limits)))
             (found (if (member verdict '("diverged" "both_failed") :test #'string=)
                        (hypotheses left right)
                        '())))
        (setf findings (append findings found))
        (when (and statuses (not (string= verdict "both_healthy")) (null found))
          (setf findings
                (append findings
                        (list (finding "hypothesis" (nlk:json-value (stage-of left 0) :string "name")
                                       "The earliest status difference narrows a follow-up test, but does not identify an absolute root cause.")))))
        (nlk:make-json-object
         "schema_version" 1
         "kind" "network_xray.comparison"
         "verdict" verdict
         "first_divergence" (if observations
                                (nlk:json-value (first observations) :string "stage")
                                :null)
         "headline" headline
         "findings" (coerce findings 'vector)
         "next_tests" (if (and observations (not (string= verdict "inconclusive")))
                          (next-tests left right observations)
                          #())
         "limitations" (coerce limits 'vector))))))
