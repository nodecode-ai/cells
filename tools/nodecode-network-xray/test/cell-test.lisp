;;;; cell-test.lisp --- the lifecycle; the ladder and what it records; the
;;;; refusals; the store; the comparison; the import; /network-xray.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own work: that a probe walks the four
;;;; stages in order and keeps each answer, that a refused connection stops
;;;; the ladder where it stopped instead of filling in the rest, that a URL
;;;; outside the rule refuses in words, that a saved record reads back and
;;;; its digest catches an edit, that a comparison keeps observations,
;;;; hypotheses and limitations apart, and that each tool and the command
;;;; answer through the wire.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "network-xray"
  (:config "vantage" "test-vantage")
  (:help :network-xray)
  (:command "network-xray")
  (:running (is (= 3 (nodecode-network-xray::setting :timeout)) "one stage may take three seconds")
            (is (equal "test-vantage" (nodecode-network-xray::setting :vantage)) "the section's label is the running vantage"))
  (:refused ("timeout" "soon"))
  (:idle network-xray:network-xray-error
         (network-xray:probe "https://example.test/")
         (network-xray:import-bundle "{}")
         (network-xray:compare-records-by-id "aaa" "bbb")))

;;; --- the ladder ------------------------------------------------------------------

(deftest network-xray-cell-the-ladder-is-recorded ()
  (with-xray ()
    (with-ladder (:dns '("93.184.216.34" "2606:2800:220:1:248:1893:25c8:1946"))
      (multiple-value-bind (answer record bundle) (xray-probe-record "label" "laptop at home")
        (is (eq t (nlk:json-value answer :boolean "ok")) "the call answers ok")
        (is (equal "local" (nlk:json-value record :string "origin")) "saved as this machine's own probe")
        (is (equal "https://example.test/a" (nlk:json-value bundle :string "target" "url")) "the target is the canonical URL, the path kept")
        (is (equal "laptop at home" (nlk:json-value bundle :string "vantage" "label")) "the call's label overrides the section's")
        (is (equal "HEAD" (nlk:json-value bundle :string "policy" "method")) "one HEAD")
        (is (null (nlk:json-value bundle :any "target" "connect_ip")) "DNS selected the address, not an override")
        (is (equal '("ok" "ok" "ok" "ok")
                   (loop for index from 0 below 4
                         collect (nlk:json-value (xray-stage bundle index) :string "status"))) "every stage answered")
        (is (equal "93.184.216.34"
                   (nlk:json-value (xray-stage bundle 1) :string "details" "address")) "the first address DNS answered is the one connected")
        (is (equal "dns" (nlk:json-value (xray-stage bundle 1) :string "details" "selection")) "and the record says DNS chose it")
        (is (equal "TLSv1.3" (nlk:json-value (xray-stage bundle 2) :string "details" "protocol")) "the TLS evidence carries the protocol it negotiated")
        (is (= 200 (nlk:json-value (xray-stage bundle 3) :integer "details" "status_code")) "and the HTTP evidence the status code")
        (is (equal "complete" (nlk:json-value bundle :string "capture" "status")) "the capture finished")
        (is (equal "healthy" (network-xray:overall bundle)) "and the outcome is healthy")
        (is (probe-file (nodecode-network-xray::record-path (nlk:json-value bundle :string "id"))) "the record is on disk under its own id")))))

(deftest network-xray-cell-a-refused-connection-stops-the-ladder ()
  (with-xray ()
    (with-ladder (:refuse t)
      (multiple-value-bind (answer record bundle) (xray-probe-record)
        (declare (ignore record))
        (is (eq t (nlk:json-value answer :boolean "ok")) "a refused connection is evidence, not a failed call")
        (is (equal "error" (nlk:json-value (xray-stage bundle 1) :string "status")) "the TCP stage records the refusal")
        (is (equal "connection_refused" (nlk:json-value (xray-stage bundle 1) :string "error" "code")) "with the code the original used")
        (is (equal '("skipped" "skipped")
                   (loop for index from 2 below 4
                         collect (nlk:json-value (xray-stage bundle index) :string "status"))) "TLS and HTTP are skipped, never guessed")
        (is (equal "failed" (network-xray:overall bundle)) "the outcome is failed")))))

(deftest network-xray-cell-an-explicit-address-needs-no-dns ()
  (with-xray ()
    (let ((nodecode-network-xray:*resolver*
            (lambda (target)
              (declare (ignore target))
              (error 'usocket:ns-error)))
          (nodecode-network-xray:*connector* (xray-connector))
          (nodecode-network-xray:*handshaker* (xray-tls))
          (nodecode-network-xray:*head* (xray-head 204))
          (nodecode-network-xray:*closer* (lambda (socket) (declare (ignore socket)) nil)))
      (multiple-value-bind (answer record bundle) (xray-probe-record "connect_ip" "10.1.2.3")
        (declare (ignore answer record))
        (is (equal "dns_failed" (nlk:json-value (xray-stage bundle 0) :string "error" "code")) "DNS failure stays on the record")
        (is (equal "10.1.2.3" (nlk:json-value (xray-stage bundle 1) :string "details" "address")) "the override is the address connected")
        (is (equal "override" (nlk:json-value (xray-stage bundle 1) :string "details" "selection")) "and the record says it overrode DNS")
        (is (equal "HTTP 204" (nlk:json-value (xray-stage bundle 3) :string "summary")) "the HEAD still ran")))))

(deftest network-xray-cell-a-url-outside-the-rule-refuses ()
  (with-xray ()
    (with-ladder ()
      (flet ((refusal (&rest pairs)
               (string-downcase
                (apply #'xray-tool-text "network_xray_probe"
                       (list (apply #'xray-arguments pairs))))))
        (is (search "query" (refusal "url" "https://example.test/a?next=1")) "a query string is refused by name")
        (is (search "http" (refusal "url" "ftp://example.test/")) "a scheme the probe does not speak is refused")
        (is (search "url" (refusal "url" "")) "an empty URL is refused, by the contract before a probe starts")
        (is (search "fragment" (refusal "url" "https://example.test/a#top")) "a fragment is refused")))))

(deftest network-xray-cell-a-probe-without-a-vantage-label-refuses ()
  (with-xray ("vantage" "")
    (with-ladder ()
      (is (search "vantage label"
                  (string-downcase
                   (xray-tool-text "network_xray_probe"
                                   (xray-arguments "url" "https://example.test/a")))) "the one required setting refuses a probe by name, and says what to set")
      (is (zerop (length (xray-items (nlk:json-value (xray-tool "network_xray_history" "{}")
                                                         :object "result")
                                         "probes"))) "and nothing was saved for the refused probe"))))

(deftest network-xray-cell-a-call-outside-the-contract-refuses ()
  (is (handler-case
           (progn (nlk:contract-decode (nlk:contract 'nodecode-network-xray::xray-probe)
                                        (nlk:make-json-object))
                  nil)
         (nlk:contract-refusal () t)) "a probe with no url refuses, never probes an empty string")
  (is (handler-case
           (progn (nlk:contract-decode (nlk:contract 'nodecode-network-xray::xray-compare)
                                        (nlk:make-json-object "left_id" "aaa"))
                  nil)
         (nlk:contract-refusal () t)) "a comparison with one id refuses"))

;;; --- the store, and what a digest is worth ---------------------------------------

(deftest network-xray-cell-the-store-lists-newest-first ()
  (with-xray ()
    (with-ladder ()
      (let ((first (nlk:json-value (xray-probe-record "label" "first") :object "result"))
            (second (nlk:json-value (xray-probe-record "label" "second") :object "result")))
        (let* ((answer (xray-tool "network_xray_history" "{}"))
               (probes (xray-items (nlk:json-value answer :object "result") "probes")))
          (is (eq t (nlk:json-value answer :boolean "ok")) "the history answers")
          (is (= 2 (length probes)) "both probes are listed")
          (is (equal (sort (list (nlk:json-value first :string "id")
                                 (nlk:json-value second :string "id"))
                           #'string<)
                     (sort (mapcar (lambda (probe) (nlk:json-value probe :string "id"))
                                   probes)
                           #'string<)) "the two probes it answered with, and no others")
          (is (equal (mapcar (lambda (probe)
                               (format nil "~a ~a" (nlk:json-value probe :string "captured_at")
                                       (nlk:json-value probe :string "id")))
                             probes)
                     (sort (mapcar (lambda (probe)
                                     (format nil "~a ~a"
                                             (nlk:json-value probe :string "captured_at")
                                             (nlk:json-value probe :string "id")))
                                   probes)
                           #'string>)) "newest first, the capture time and then the id")
          (is (equal "verified" (nlk:json-value (first probes) :string "digest")) "and a record's digest verifies against its own content"))))))

(deftest network-xray-cell-an-edited-record-shows-its-digest ()
  (with-xray ()
    (with-ladder ()
      (let* ((record (nlk:json-value (xray-probe-record) :object "result"))
             (id (nlk:json-value record :string "id"))
             (path (nodecode-network-xray::record-path id)))
        (let ((saved (nlk:decode-json (uiop:read-file-string path) :whole t)))
          (setf (gethash "status" (aref (gethash "stages" (gethash "probe" saved)) 0))
                "error")
          (with-open-file (out path :direction :output :if-exists :supersede
                                     :external-format :utf-8)
            (write-string (nlk:encode-json-object saved) out)))
        (let ((probes (xray-items (nlk:json-value (xray-tool "network_xray_history" "{}")
                                                    :object "result")
                                   "probes")))
          (is (equal "mismatch" (nlk:json-value (first probes) :string "digest")) "an edited record reports a digest that no longer matches"))))))

;;; --- two vantage points ----------------------------------------------------------

(deftest network-xray-cell-the-comparison-keeps-the-kinds-apart ()
  (with-xray ()
    (let ((healthy (with-ladder () (nlk:json-value (xray-probe-record "label" "here") :object "result")))
          (broken (with-ladder (:refuse t) (nlk:json-value (xray-probe-record "label" "there") :object "result"))))
      (let* ((answer (xray-tool "network_xray_compare"
                                 (xray-arguments "left_id" (nlk:json-value broken :string "id")
                                                 "right_id" (nlk:json-value healthy :string "id"))))
             (comparison (nlk:json-value answer :object "result"))
             (kinds (xray-finding-kinds comparison))
             (tests (xray-items comparison "next_tests")))
        (is (eq t (nlk:json-value answer :boolean "ok")) "the comparison answers")
        (is (equal "diverged" (nlk:json-value comparison :string "verdict")) "one probe connected, the other was refused")
        (is (equal "tcp" (nlk:json-value comparison :string "first_divergence")) "the ladder diverged at TCP")
        (is (search "refused" (nlk:json-value comparison :string "headline")) "the headline names the refusal")
        (is (member "observation" kinds :test #'equal) "observations are kept")
        (is (member "limitation" kinds :test #'equal) "and limitations with them")
        (is (member "hypothesis" kinds :test #'equal) "hypotheses are labelled as hypotheses")
        (is (>= (length (xray-items comparison "limitations")) 4) "the limitations include what any comparison carries")
        (is (every (lambda (test) (nlk:json-value test :boolean "requires_confirmation")) tests) "a suggested follow-up asks before it runs")))))

(deftest network-xray-cell-two-records-of-different-urls-do-not-compare ()
  (with-xray ()
    (with-ladder ()
      (let ((left (nlk:json-value (xray-probe-record) :object "result"))
            (other (with-ladder ()
                     (nlk:json-value (xray-tool "network_xray_probe"
                                                 (xray-arguments "url" "https://other.test/"))
                                     :object "result"))))
        (declare (ignore other))
        (let ((answer (xray-tool "network_xray_compare"
                                  (xray-arguments "left_id" (nlk:json-value left :string "id")
                                                  "right_id" (nlk:json-value left :string "id")))))
          (is (null (nlk:json-value answer :boolean "ok")) "the same id twice is refused")
          (is (search "different" (nlk:json-value answer :string "error")) "and says the two ids must differ"))))))

(deftest network-xray-cell-a-comparison-needs-the-same-url ()
  (with-xray ()
    (let ((one (with-ladder () (nlk:json-value (xray-probe-record) :object "result")))
          (two (with-ladder ()
                 (nlk:json-value (xray-tool "network_xray_probe"
                                             (xray-arguments "url" "https://other.test/a"))
                                 :object "result"))))
      (let ((answer (xray-tool "network_xray_compare"
                                (xray-arguments "left_id" (nlk:json-value one :string "id")
                                                "right_id" (nlk:json-value two :string "id")))))
        (is (null (nlk:json-value answer :boolean "ok")) "two URLs do not compare")
        (is (search "same canonical URL" (nlk:json-value answer :string "error")) "and the refusal names the rule")))))

;;; --- a peer's bundle --------------------------------------------------------------

(deftest network-xray-cell-an-imported-bundle-is-checked ()
  (with-xray ()
    (flet ((bundle-json (&rest pairs)
             (nlk:encode-json-object
              (apply #'nlk:copy-json-object
                     (network-xray:seal (network-xray::empty-bundle
                                         (network-xray:parse-target "https://peer.test/")
                                         "peer"))
                     pairs))))
      (let ((answer (xray-tool "network_xray_import" (xray-arguments "bundle_json" (bundle-json)))))
        (is (eq t (nlk:json-value answer :boolean "ok")) "a bundle the collector wrote imports")
        (is (equal "imported" (nlk:json-value (nlk:json-value answer :object "result") :string "origin")) "and is saved as imported, never as this machine's probe"))
      (let ((answer (xray-tool "network_xray_import"
                                (xray-arguments "bundle_json"
                                                (bundle-json "kind" "something.else")))))
        (is (null (nlk:json-value answer :boolean "ok")) "a bundle of another kind refuses"))
      (let ((answer (xray-tool "network_xray_import"
                                (xray-arguments "bundle_json" (bundle-json "extra" "member")))))
        (is (null (nlk:json-value answer :boolean "ok")) "a member no record carries refuses"))
      (let ((answer (xray-tool "network_xray_import" (xray-arguments "bundle_json" "not json"))))
        (is (search "not JSON" (nlk:json-value answer :string "error")) "and text that is not JSON says so")))))

;;; --- the operator's command ------------------------------------------------------

(deftest network-xray-cell-the-command-prints-the-ladder-and-the-history ()
  (with-xray ()
    (with-ladder ()
      (xray-probe-record "label" "laptop at home")
      (let ((text (nodecode-network-xray::slash-network-xray "https://example.test/a" nil)))
        (is (search "test-vantage" text) "the probe's ladder names the vantage it was taken under")
        (is (search "HTTP 200" text) "and what the HEAD answered")
        (is (search "outcome: healthy" text) "with the outcome on the last line"))
      (let ((text (nodecode-network-xray::slash-network-xray "history" nil)))
        (is (search "https://example.test/a" text) "the history lists the saved URL"))
      (let ((text (nodecode-network-xray::slash-network-xray "" nil)))
        (is (search "https://example.test/a" text) "no argument lists the history too"))
      (let ((text (nodecode-network-xray::slash-network-xray "nonsense" nil)))
        (is (search "history" text) "a word it does not know answers with the usage line")))))
