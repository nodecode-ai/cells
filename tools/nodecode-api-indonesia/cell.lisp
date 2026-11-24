;;;; cell.lisp --- the section, the manual, the three tools and the /command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The server is the product: API Indonesia holds Indonesian public data
;;;; from official sources and serves its own tool set at run time. So the
;;;; cell registers verbs that outlive any version of that set -- one that
;;;; lists it, one that calls into it -- and a third that reaches the REST
;;;; paths the original's skill documents, for the day the set and the need
;;;; disagree. The skill itself becomes the manual topic: the endpoint table,
;;;; what a format check means, what a 401 and a 402 cost.

(in-package #:nodecode-api-indonesia)

(nlk:define-peripheral api-indonesia :not-running t)

(defparameter +primer+
  "API Indonesia serves Indonesian public data from official sources -- Kemendagri, Kemendikdasmen,
Kemenkes, BMKG, Bank Indonesia, BPJPH, BPOM, DJKI, OJK -- through one hosted server. Three verbs
reach it, and all of them spend the same key's credits: a successful request costs 2 credits
(Rp2), a failed one is free.
  api_indonesia_tools     the server's own tools, one line each. refresh asks again instead of
                           answering from this process's copy. Call it first.
  api_indonesia_call      one of them: tool is its name exactly as listed, arguments its own
                           arguments as an object. The tool's description says what it wants.
  api_indonesia_get      one documented REST path with a query object, under /api/v1: the
                           fallback when the tool list lacks what you need, and the shortest
                           way to a plain lookup.
Paths worth knowing, all under /api/v1: /wilayah/provinsi, /wilayah/provinsi/{id}/kabupaten,
/wilayah/kabupaten/{id}/kecamatan, /wilayah/kecamatan/{id}/kelurahan, /wilayah/search?q=,
/wilayah-boundary/{code}, /kodepos/search?q=, /kodepos/lookup, /libur?tahun=, /libur/upcoming,
/sholat?kabupaten_id=&tanggal=, /sholat/list-kota, /hijriah/konversi, /sekolah/search?q=,
/kampus/search, /rumah-sakit/search, /gempa/terkini, /gempa/dirasakan, /cuaca?adm4=,
/peringatan-dini, /kualitas-udara/{city}, /kurs-bi/latest, /ump, /halal?q=, /bpom?q=,
/merek/search?q=, /ojk/products, /ojk/illegals, /harga-pangan, /kbbi/word/{word},
/quran/surah/{id}, /alkitab/verse/{book}/{chapter}/{verse}. api_indonesia_get only reads: the
validating and calculator paths that take a body (validasi/nik, validasi/npwp, util/rupiah/format,
util/rupiah/terbilang, util/qris/parse) go through api_indonesia_call.
Regions are Kemendagri codes, and a village's code is the adm4 a weather query wants. A NIK or
NPWP check only confirms the number is well formed, never that a person or taxpayer exists: say so
when you report one. Cite the data source the answer's metadata names. A 401 means the key is
missing or wrong and a 402 that the account is out of credits: tell the operator, who tops up at
https://dashboard.apiindonesia.id. A call that failed answers what the server said."
  "What (help :api-indonesia) answers while the cell runs.")

;;; --- the boundaries the model crosses ------------------------------------------------

(nlk:define-contract api-indonesia-tools
  (:doc "List the tools the API Indonesia server offers right now: Indonesian administrative regions, postal codes, holidays and cuti bersama, prayer times, BMKG earthquakes, weather and warnings, schools, hospitals, exchange rates, minimum wage, halal and BPOM lookups, OJK investment lists, KBBI, Al-Quran and more. Call this first, then api_indonesia_call with one tool's name.")
  ("refresh" :boolean :default nil
   :doc "ask the server again instead of answering from the copy this process already has"))

(nlk:define-contract api-indonesia-call
  (:doc "Call one tool of the API Indonesia server: a read-only lookup or calculator over Indonesian public data from official sources. Every successful call spends the credits of the account that owns the key.")
  ("tool" :text :required t
   :doc "the tool's name, exactly as api_indonesia_tools lists it")
  ("arguments" :object
   :doc "the tool's own arguments as a JSON object, as its description asks; leave it out when the tool takes none"))

(nlk:define-contract api-indonesia-get
  (:doc "GET one documented API Indonesia REST path: the same data, key and credits as the server's own tools, and the fallback when that tool list lacks what you need. Reads only.")
  ("path" :text :required t
   :doc "the path under /api/v1, such as /wilayah/provinsi or /libur?tahun=2026; a full API Indonesia URL also works, and only API Indonesia is ever addressed")
  ("query" :object
   :doc "the path's query parameters as a JSON object, such as {\"q\": \"bandung\"} or {\"tahun\": 2026}"))

;;; --- the three verbs -----------------------------------------------------------------

(defun list-tools (arguments)
  "api_indonesia_tools: the server's own tool list, as text."
  (multiple-value-bind (tools error) (tools-result (api-indonesia-tools-refresh arguments))
    (or error (format-tools tools))))

(defun call-tool (arguments)
  "api_indonesia_call: one tool of the server, its answer as text."
  (let ((name (api-indonesia-call-tool arguments))
        (given (api-indonesia-call-arguments arguments)))
    (multiple-value-bind (result error)
        (call "tools/call" (nlk:json-object "name" name :opt "arguments" given) t)
      (or error (format-result result)))))

(defun get-path (arguments)
  "api_indonesia_get: one documented REST path, its answer as text."
  (rest-get (api-indonesia-get-path arguments)
            (api-indonesia-get-query arguments)))

;;; --- the command -----------------------------------------------------------------

(defun run-command (args session-id)
  "The /api-indonesia command: whether a key is set, and the server's tools."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond
      ((member word '("" "status") :test #'string=)
       (format nil "api-indonesia: ~a~%key: ~:[not set~;set~] (the variable ~a names)~%~
tools: ~:[not asked yet~;~:*~d known~]~%~
/api-indonesia tools lists them; /api-indonesia refresh asks the server again"
               (setting :url)
               (and (api-key) t)
               (or (setting :api-key-env) "")
               (length *tools*)))
      ((string= word "tools") (list-tools (nlk:json-object "refresh" nil)))
      ((string= word "refresh") (list-tools (nlk:json-object "refresh" t)))
      (t "usage: /api-indonesia - whether a key is set; /api-indonesia tools - the server's tools; ~
/api-indonesia refresh - ask the server again"))))

;;; --- the entry -------------------------------------------------------------------------

(defun install ()
  "A refused section leaves nothing installed; a good one keeps no session
between starts, so a restart really is a new session."
  (check-config)
  (setf *initialized* nil *session-id* nil *tools* nil *counter* 0)
  (nle:on-stop (lambda () (setf *initialized* nil *session-id* nil *tools* nil))))

(nle:define-cell api-indonesia
  (:section ("api-indonesia")
    (:guide "set the environment variable the section's api_key_env names to the aip_live_... key from the API Indonesia dashboard (a new account starts with 3,000 credits and there is no monthly free tier); url and rest_url are already right; timeout_seconds bounds one exchange; a successful call costs the key's account 2 credits and a failed one is free")
    ("api_key_env" :env :default "API_INDONESIA_KEY"
     :doc "the environment variable holding the API Indonesia key (aip_live_...)")
    ("url" :string :default "https://mcp.apiindonesia.id/mcp"
     :doc "the MCP server's Streamable HTTP endpoint")
    ("rest_url" :string :default "https://use.apiindonesia.id/api/v1"
     :doc "the REST API's base, which api_indonesia_get appends a documented path to")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one exchange's ceiling, in seconds"))
  (:start #'install)
  (:help :api-indonesia
         "api-indonesia: Indonesian public data - regions, postal codes, holidays, prayer times, BMKG, wages - through the API Indonesia server"
         +primer+)
  (:tool "api_indonesia_tools" (nlk:contract 'api-indonesia-tools) #'list-tools)
  (:tool "api_indonesia_call" (nlk:contract 'api-indonesia-call) #'call-tool)
  (:tool "api_indonesia_get" (nlk:contract 'api-indonesia-get) #'get-path)
  (:command "api-indonesia" #'run-command
            :description "API Indonesia: whether a key is set, and the server's tools"
            :argument-hint "tools|refresh"))
