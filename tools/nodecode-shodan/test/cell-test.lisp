;;;; cell-test.lisp --- the shodan cell against a stubbed Shodan.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Shodan endpoint is a stubbed NLK:HTTP and the key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Shodan, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it
;;;; — through the real dispatch, contract and all — so the schema the
;;;; provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "shodan" "SHODAN-CELL-" :start nodecode-shodan:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-shodan-key (&body body)
  "BODY with SHODAN_API_KEY set to a fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "SHODAN_API_KEY") "test-key"))
     ,@body))

(defmacro with-shodan-keyless (&body body)
  "BODY with no key anywhere, as a fresh keyless install."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (eql 0 (length name)))
     ,@body))

(defmacro with-shodan-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Shodan request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun shodan-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun shodan-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (shodan-answer id tool arguments)))

(defparameter +host-json+
  "{\"ip_str\":\"1.1.1.1\",\"hostnames\":[\"one.one.one.one\"],\"org\":\"Cloudflare\",\"isp\":\"Cloudflare\",\"asn\":\"AS13335\",\"country_name\":\"Australia\",\"city\":\"Brisbane\",\"tags\":[\"cdn\"],\"last_update\":\"2026-01-01T00:00:00.000000\",\"ports\":[53,80,443],\"vulns\":{\"CVE-2020-1234\":{\"cvss\":9.8,\"summary\":\"A bad one\"}},\"data\":[{\"port\":443,\"transport\":\"tcp\",\"product\":\"CloudFlare\",\"http\":{\"title\":\"403 Forbidden\"}},{\"port\":53,\"transport\":\"udp\",\"module\":\"dns-udp\"}]}"
  "One host, as GET /shodan/host/1.1.1.1 answers it.")

(defparameter +internetdb-json+
  "{\"ip\":\"1.1.1.1\",\"ports\":[22,80],\"hostnames\":[\"one.one.one.one\"],\"cpes\":[\"cpe:/a:openbsd:openssh\"],\"tags\":[\"cdn\"],\"vulns\":[\"CVE-2020-1234\"]}"
  "The same host, as the keyless InternetDB answers it.")

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "shodan"
  (:config "api_key_env" "SHODAN_API_KEY")
  (:help :shodan)
  (:command "shodan")
  (:refused ("verbosity" 5) ("max_result_chars" 5))
  (:fixture with-shodan-key))

;;; --- shodan_host --------------------------------------------------------------------

(deftest shodan-cell-answers-a-host-and-shapes-it ()
  (with-shodan-key
    (with-cell-stop ((shodan-start))
      (with-shodan-wire (asked ("api.shodan.io/shodan/host/1.1.1.1"
                                 (values +host-json+ 200)))
        (let* ((object (shodan-object "sh-1" "shodan_host" "{\"ip\":\"1.1.1.1\"}"))
               (host (nlk:json-value object :object "host")))
          (is (equal "1.1.1.1" (nlk:json-value host :string "ip")))
          (is (equal "AS13335" (nlk:json-value host :string "asn")) "the ASN is not doubled")
          (is (= 2 (length (nlk:json-array host "services"))) "one line per banner")
          (is (= 2 (length (nlk:json-array host "open_ports"))) "the ports come from the banners, as the original derives them")
          (is (equal "CVE-2020-1234"
                     (nlk:json-value (aref (nlk:json-array host "vulns") 0) :string "cve")))
          (is (= 1 (length asked)) "one request")
          (is (search "key=test-key" (second (first asked))) "the key rides the query string"))))))

(deftest shodan-cell-answers-a-host-keyless-from-internetdb ()
  (with-shodan-keyless
    (with-cell-stop ((shodan-start))
      (with-shodan-wire (asked ("internetdb.shodan.io/1.1.1.1"
                                 (values +internetdb-json+ 200)))
        (let* ((object (shodan-object "sh-2" "shodan_host" "{\"ip\":\"1.1.1.1\"}"))
               (host (nlk:json-value object :object "host")))
          (is (equal "internetdb" (nlk:json-value host :string "source")))
          (is (= 2 (length (nlk:json-array host "open_ports"))))
          (is (search "InternetDB" (nlk:json-value object :string "degraded")) "the answer says what it is")
          (is (not (search "key=" (second (first asked)))) "a keyless endpoint takes no key"))))))

;;; --- shodan_count, free --------------------------------------------------------------

(deftest shodan-cell-counts-with-facets-for-free ()
  (with-shodan-key
    (with-cell-stop ((shodan-start))
      (with-shodan-wire (asked ("/shodan/host/count"
                                 (values "{\"total\":14773533,\"facets\":{\"version\":[{\"value\":\"9.6p1\",\"count\":2789193}]}}" 200)))
        (let ((object (shodan-object "sh-3" "shodan_count"
                                      "{\"query\":\"port:22 product:OpenSSH\",\"facets\":\"version:5\"}")))
          (is (= 14773533 (nlk:json-value object :integer "total")))
          (is (equal "9.6p1"
                     (nlk:json-value (aref (nlk:json-array
                                            (nlk:json-value object :object "facets") "version") 0)
                                     :string "value")))
          (is (search "free" (nlk:json-value object :string "cost")))
          (is (= 0 (gethash "query_credits_spent" (nodecode-shodan::ledger-object))) "a count spends no query credit"))))))

;;; --- shodan_cve, keyless ------------------------------------------------------------

(deftest shodan-cell-reads-a-cve-with-no-key ()
  (with-shodan-keyless
    (with-cell-stop ((shodan-start))
      (with-shodan-wire (asked ("cvedb.shodan.io/cve/CVE-2021-44228"
                                 (values "{\"cve_id\":\"CVE-2021-44228\",\"cvss\":10.0,\"summary\":\"Log4Shell\",\"kev\":true,\"epss\":0.97}" 200)))
        (let* ((object (shodan-object "sh-4" "shodan_cve" "{\"action\":\"get\",\"cve\":\"cve-2021-44228\"}"))
               (cve (nlk:json-value object :object "cve")))
          (is (equal "CVE-2021-44228" (nlk:json-value cve :string "cve")) "the id is upper-cased")
          (is (eq t (nlk:json-value cve :boolean "known_exploited")))
          (is (= 1 (length asked))))))))

;;; --- refusals -----------------------------------------------------------------------

(deftest shodan-cell-a-search-without-a-key-refuses ()
  (with-shodan-keyless
    (with-cell-stop ((shodan-start))
      (with-shodan-wire (asked)
        (let ((answer (shodan-answer "sh-5" "shodan_search" "{\"query\":\"apache country:DE\"}")))
          (is (search "no Shodan API key" answer))
          (is (search "api_key_env" answer) "the refusal says where a key goes")
          (is (null asked) "and no request went out"))))))

(deftest shodan-cell-a-scan-is-refused-until-the-operator-enables-it ()
  (with-shodan-key
    (with-cell-stop ((shodan-start))
      (with-shodan-wire (asked)
        (let ((answer (shodan-answer "sh-6" "shodan_scan" "{\"action\":\"submit\",\"targets\":\"1.1.1.1\"}")))
          (is (search "on-demand scanning is off" answer))
          (is (null asked) "and nothing was submitted"))))))

(deftest shodan-cell-a-call-outside-the-allowlist-is-refused ()
  (with-shodan-key
    (with-cell-stop ((shodan-start "scan_enabled" t "scan_allowlist" (vector "10.0.0.0/8")))
      (with-shodan-wire (asked)
        (let ((answer (shodan-answer "sh-7" "shodan_scan" "{\"action\":\"submit\",\"targets\":\"8.8.8.8\"}")))
          (is (search "outside the configured scan_allowlist" answer))
          (is (null asked)))))))

;;; --- /shodan, the command ------------------------------------------------------------

(deftest shodan-cell-command-answers-a-bare-host-as-markdown ()
  (with-shodan-key
    (with-cell-stop ((shodan-start))
      (with-shodan-wire (asked ("api.shodan.io/shodan/host/1.1.1.1"
                                 (values +host-json+ 200)))
        (let ((answer (cell-entry "nodecode-shodan" "shodan" "1.1.1.1")))
          (is (search "**1.1.1.1**" answer))
          (is (search "Cloudflare" answer))
          (is (search "exposed services" answer)))))))

(deftest shodan-cell-command-counts-a-query-without-spending ()
  (with-shodan-key
    (with-cell-stop ((shodan-start))
      (with-shodan-wire (asked ("/shodan/host/count"
                                 (values "{\"total\":2000000,\"facets\":{\"country\":[{\"value\":\"US\",\"count\":900000}]}}" 200)))
        (let ((answer (cell-entry "nodecode-shodan" "shodan" "port:3389")))
          (is (search "2,000,000 matches" answer))
          (is (search "US: 900,000" answer))
          (is (not (search "/shodan/host/search" (format nil "~s" asked))) "a count is not a search"))))))
