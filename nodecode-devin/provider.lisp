;;;; provider.lisp --- what Devin is: its address, its identity, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/devin.kdl and providers/devin.kdl, catalog/src/wire/devin.ts (the
;;;; client identities the backend gates on), catalog/src/discovery/devin.ts
;;;; (the roster GetCliModelConfigs serves, normalized), the Devin rows of
;;;; catalog/src/compat/collapse.ts's collapseWithTable and its reviewed
;;;; table, and catalog/src/model-thinking.ts's resolveWireModelId.
;;;; models.json in this folder carries omp's two bundled seed rows
;;;; (tools/omp-models.py wrote it), wire.json what the wire reads beside them
;;;; (tools/wire-rows.py wrote it): each seed's router and parallel-tool
;;;; flags, omp's reviewed Devin families, its selector aliases.
;;;;
;;;; Devin's models are served by Codeium's Cascade backend at
;;;; https://server.codeium.com. The roster is per account, so omp bundles only
;;;; the two SWE-1.6 seeds and replaces them with what GetCliModelConfigs
;;;; answers once a credential can ask. A Cascade roster names one wire uid
;;;; per reasoning effort (claude-opus-5-low ... claude-opus-5-max); omp folds
;;;; each family into one logical model whose effort picks the uid, from the
;;;; family metadata the server ships and from its own reviewed table, and
;;;; this file does the same.

(in-package #:nodecode-devin)

(defparameter +base+ "https://server.codeium.com"
  "Where Cascade is served (DEVIN_DEFAULT_BASE_URL).")

(defparameter +env+ '("DEVIN_API_KEY")
  "The environment variables a Devin key is read from.")

(defparameter +session-token-prefix+ "devin-session-token$"
  "The scheme a session token carries on the wire (normalizeDevinSessionToken).")

(defun devin-os ()
  "Metadata.os: darwin, windows or linux, as omp derives it from the platform."
  (case (uiop:operating-system)
    (:macosx "darwin")
    (:windows "windows")
    (t "linux")))

(defun session-token (key)
  "KEY with the session-token scheme in front, unless it already carries it."
  (cond ((or (null key) (zerop (length key))) "")
        ((uiop:string-prefix-p +session-token-prefix+ key) key)
        (t (concatenate 'string +session-token-prefix+ key))))

(defun wire-metadata (api-key &optional (user-jwt ""))
  "The released Devin CLI's Metadata with API-KEY as given (devinWireMetadata):
`ideType: chisel' is what unlocks AssignModel and the CLI model surface."
  (encode-metadata :ide-name "devin-cli" :ide-version "3000.11.3" :ide-type "chisel"
                   :extension-name "chisel" :extension-version "3000.11.3"
                   :api-key (or api-key "") :locale "en" :os (devin-os) :user-jwt user-jwt))

(defun cli-metadata (api-key &optional (user-jwt ""))
  "WIRE-METADATA with API-KEY as a session token (devinCliMetadata)."
  (wire-metadata (session-token api-key) user-jwt))

(defparameter +supported-model-displays+ '(3 4 6 7 8)
  "The display slots discovery asks for: MODEL_ROUTER, QUICK_REVIEW, the
internal default (6), unclassified (7) and normal (8). Asking for the internal
ones is what makes the server answer its full catalog.")

(defun discovery-metadata (api-key)
  "The dev-channel chisel identity GetCliModelConfigs answers the full native
config set to (devinDiscoveryMetadata), with the display slots."
  (encode-metadata :ide-name "chisel" :ide-version "0.0.0-dev"
                   :extension-name "chisel" :extension-version "0.0.0-dev"
                   :api-key (session-token api-key) :locale "en" :os (devin-os)
                   :supported-model-displays +supported-model-displays+))

(defun legacy-metadata (api-key)
  "The Windsurf editor identity a legacy Enterprise seat lists its whole roster to."
  (encode-metadata :ide-name "windsurf" :ide-version "3.2.23"
                   :extension-name "windsurf" :extension-version "1.48.2"
                   :api-key (or api-key "") :locale "en"))

;;; --- the bundled rows ------------------------------------------------------------

(defun bundled (name)
  "The JSON file NAME in this folder, decoded."
  (nlk:decode-json (uiop:read-file-string (asdf:system-relative-pathname "nodecode-devin" name))))

(defparameter +models+ (bundled "models.json")
  "omp's bundled Devin seed rows, read when this file loads: a vector of objects.")

(defparameter +wire+ (bundled "wire.json")
  "What the wire reads beside the seeds: their flags, omp's reviewed families, its aliases.")

(defparameter +efforts+ '("minimal" "low" "medium" "high" "xhigh" "max")
  "omp's THINKING_EFFORTS, weakest first.")

(defparameter +routing-keys+ (cons "off" +efforts+)
  "The keys a family's routing may carry, in omp's order.")

;;; A model is a plist (a spec): :id :name :reasoning :input :tools :cost
;;; (INPUT OUTPUT CACHE-READ CACHE-WRITE) :context :output :router :parallel,
;;; and for a collapsed family :wire-id (the default uid), :routing (an alist
;;; of effort or "off" to uid) and :efforts.

(defun seed-spec (row)
  "One models.json ROW as a spec, its flags from wire.json."
  (flet ((value (type &rest keys) (apply #'nlk:json-value row type keys)))
    (let* ((id (value :string "id"))
           (flags (nlk:json-value +wire+ :object "models" id)))
      (list :id id
            :name (value :string "name")
            :reasoning (value :boolean "reasoning")
            :input (coerce (or (value :array "input") #("text")) 'list)
            :tools t
            :cost (list (or (value :number "cost" "input") 0) (or (value :number "cost" "output") 0)
                        (or (value :number "cost" "cacheRead") 0) (or (value :number "cost" "cacheWrite") 0))
            :context (value :integer "context")
            :output (value :integer "output")
            :router (nlk:json-value flags :boolean "model_router")
            :parallel (nlk:json-value flags :boolean "parallel_tool_calls")))))

(defun seed-specs ()
  "The bundled seeds as specs."
  (map 'list #'seed-spec +models+))

;;; --- discovery's normalization (discovery/devin.ts) --------------------------------

(defparameter +default-context+ 200000)
(defparameter +default-max-tokens+ 64000)

(defparameter +image-blind-uids+ '("swe-1-6" "swe-1-6-fast")
  "Uids whose configs say they take images while their backend drops them
(DEVIN_IMAGE_BLIND_UIDS, verified by omp 2026-08-14).")

(defun supports-thinking-p (config)
  "The server's model features decide; a config with none is read from its label."
  (let ((features (getf (getf config :info) :features))
        (label (getf config :label)))
    (cond (features (getf features :thinking))
          ((ppcre:scan "(?i)\\bno thinking\\b" label) nil)
          (t (and (ppcre:scan "(?i)think|thinking|minimal|high|medium|low|xhigh|max|reasoning" label) t)))))

(defun decimal (text)
  "The decimal number TEXT (digits, an optional point and digits) as a double."
  (let* ((point (position #\. text))
         (whole (parse-integer text :end point))
         (fraction (if point (subseq text (1+ point)) "")))
    (+ whole (if (plusp (length fraction))
                 (/ (parse-integer fraction) (expt 10d0 (length fraction)))
                 0d0))))

(defun denominator-tokens (denominator)
  "The tokens one cost dimension covers (devinCostDenominatorTokens): `1M
tokens' is a million, a denominator naming no number a million too."
  (or (ppcre:register-groups-bind (number suffix) ("(?i)(\\d+(?:\\.\\d+)?)\\s*([kmb])?" denominator)
        (let ((tokens (* (decimal number)
                         (if suffix
                             (ecase (char-downcase (char suffix 0)) (#\k 1000) (#\m 1000000) (#\b 1000000000))
                             1))))
          (and (plusp tokens) tokens)))
      1000000))

(defun model-cost (config)
  "Per-million rates from the config's cost dimensions, read up to the
composite card's `Sidekick' marker (devinModelCost)."
  (let ((input 0) (output 0) (cache-read 0))
    (loop for dimension in (getf config :dimensions)
          for label = (string-downcase (nlk:trimmed (getf dimension :label)))
          do (when (equal label "sidekick") (return))
             (when (member (getf dimension :kind) '(1 2))
               (let ((per-million (/ (ffloor (+ 0.5d0 (* (/ (* (getf dimension :value) 1000000)
                                                             (denominator-tokens (getf dimension :denominator)))
                                                          1000000)))
                                     1000000)))
                 (cond ((equal label "input") (setf input per-million))
                       ((equal label "cached input") (setf cache-read per-million))
                       ((equal label "output") (setf output per-million))))))
    (list input output cache-read 0)))

(defun config-spec (config uid router)
  "One config as a raw spec keyed on its wire UID (devinModelSpec)."
  (let* ((info (getf config :info))
         (features (getf info :features))
         (images (and (if features (getf features :images) (getf config :supports-images))
                      (not (member uid +image-blind-uids+ :test #'equal))))
         (output (or (getf info :max-output-tokens) 0)))
    (list :id uid
          :name (let ((label (nlk:trimmed (getf config :label)))) (if (plusp (length label)) label uid))
          :reasoning (supports-thinking-p config)
          :input (if images '("text" "image") '("text"))
          ;; a router ships no features: Cascade serves only tool-calling models
          :tools (if features (getf features :tool-calls) t)
          :cost (model-cost config)
          :context (if (plusp (getf config :max-tokens)) (getf config :max-tokens) +default-context+)
          :output (if (plusp output) output +default-max-tokens+)
          :router router
          :parallel (and features (getf features :parallel-tool-calls)))))

(defun fusion-lead (uid live)
  "The lead uid of a fusion pairing UID, :NONE for a pairing whose lead is
not LIVE (a hash of uids), NIL for a uid that is no pairing (devinFusionLeadUid)."
  (when (uiop:string-prefix-p "fusion-" uid)
    (let ((cut (search "-sidekick-" uid)))
      (when (and cut (> cut (length "fusion-")))
        (let ((lead (subseq uid (length "fusion-") cut)))
          (cond ((gethash lead live) lead)
                ((uiop:string-suffix-p lead "-fast")
                 (let ((base (subseq lead 0 (- (length lead) (length "-fast")))))
                   (cond ((gethash (format nil "~a-priority" base) live) (format nil "~a-priority" base))
                         ((gethash base live) base)
                         (t :none))))
                (t :none)))))))

(defun route-to-lead (spec lead)
  "SPEC, a fusion pairing, sent as its LEAD's spec and budgeted as it (routeDevinFusionLead)."
  (append (list :wire-id (getf lead :id)
                :reasoning (getf lead :reasoning) :input (getf lead :input) :tools (getf lead :tools)
                :cost (getf lead :cost) :context (getf lead :context) :output (getf lead :output)
                :parallel (getf lead :parallel))
          spec))

(defun effort-of-name (name)
  "A family entry's effort name as an effort, or \"off\" (DEVIN_FAMILY_EFFORT_BY_NAME)."
  (let ((token (string-downcase (remove-if-not #'alphanumericp name))))
    (cond ((member token '("none" "nothinking") :test #'equal) "off")
          ((member token +efforts+ :test #'equal) token))))

(defun normalized-key (key)
  "A family entry key as omp compares it: punctuation to spaces, lowercase."
  (string-trim " " (ppcre:regex-replace-all "[^a-z0-9]+" (string-downcase key) " ")))

(defstruct (lane-family (:copier nil))
  id name (members '()) default-member (routing '()))

(defun collect-family-lane (lanes config uid)
  "File CONFIG under its server-declared family lane in LANES, an ordered
alist of id to LANE-FAMILY (collectDevinFamilyLane)."
  (let* ((metadata (getf config :family))
         (label (and metadata (nlk:trimmed (getf metadata :label)))))
    (when (and label (plusp (length label)))
      (let ((effort nil) (thinking :unset) (fast nil) (wide nil))
        (dolist (entry (getf metadata :entries))
          (let ((value (getf entry :value))
                (key (normalized-key (getf entry :key))))
            (when value
              (cond ((equal key "fast mode") (setf fast (= 1 (getf value :order))))
                    ((equal key "thinking") (setf thinking (= 1 (getf value :order))))
                    ((equal key "1m context") (setf wide (= 1 (getf value :order))))
                    ((member key '("effort" "reasoning effort") :test #'equal)
                     (setf effort (effort-of-name (getf value :name))))))))
        ;; Claude's paired configs share one effort label; its Thinking axis decides
        (when (eq thinking nil) (setf effort "off"))
        (let ((base (string-trim "-" (ppcre:regex-replace-all "[^a-z0-9]+" (string-downcase label) "-"))))
          (when (plusp (length base))
            (let* ((id (format nil "~a~:[~;-1m~]~:[~;-fast~]" base wide fast))
                   (lane (or (cdr (assoc id (car lanes) :test #'equal))
                             (let ((made (make-lane-family :id id
                                                           :name (format nil "~a~:[~; 1M~]~:[~; Fast~]" label wide fast))))
                               (setf (car lanes) (append (car lanes) (list (cons id made))))
                               made))))
              (setf (lane-family-members lane) (append (lane-family-members lane) (list uid)))
              (when (and (null (lane-family-default-member lane))
                         (or (getf config :is-default-in-family) (getf metadata :is-default)))
                (setf (lane-family-default-member lane) uid))
              (when (and effort (null (assoc effort (lane-family-routing lane) :test #'equal)))
                (push (cons effort uid) (lane-family-routing lane))))))))))

(defun dynamic-families (lanes)
  "The family lanes that declare an effort ladder, as families (devinDynamicFamilies)."
  (loop for (nil . lane) in lanes
        for routing = (lane-family-routing lane)
        for efforts = (remove-if-not (lambda (effort) (assoc effort routing :test #'equal)) +efforts+)
        for default = (lane-family-default-member lane)
        when efforts
          collect (list :id (lane-family-id lane)
                        :name (lane-family-name lane)
                        :members (if default
                                     (cons default (remove default (lane-family-members lane) :test #'equal))
                                     (lane-family-members lane))
                        :default-member default
                        :routing routing
                        :efforts efforts)))

(defun table-families ()
  "omp's reviewed Devin families, as families."
  (loop for family across (or (nlk:json-value +wire+ :array "families") #())
        collect (list :id (nlk:json-value family :string "id")
                      :name (nlk:json-value family :string "name")
                      :members (coerce (nlk:json-value family :array "members") 'list)
                      :default-member (nlk:json-value family :string "default_member")
                      :routing (let ((routing (nlk:json-value family :object "routing")))
                                 (loop for key in +routing-keys+
                                       for uid = (and routing (gethash key routing))
                                       when uid collect (cons key uid)))
                      :efforts (and (nth-value 1 (gethash "efforts" family))
                                    (coerce (nlk:json-value family :array "efforts") 'list)))))

(defun collapse (specs families)
  "SPECS with every family whose members are present folded into one logical
spec, at the place of its first member (collapseWithTable over raw specs)."
  (let ((by-id (make-hash-table :test #'equal))
        (family-of (make-hash-table :test #'equal))
        (replacement (make-hash-table :test #'equal)))
    (dolist (spec specs)
      (unless (gethash (getf spec :id) by-id) (setf (gethash (getf spec :id) by-id) spec)))
    (dolist (family families)
      (let* ((present (remove-if-not (lambda (id) (gethash id by-id)) (getf family :members)))
             (members (mapcar (lambda (id) (gethash id by-id)) present)))
        (when present
          (dolist (id present) (setf (gethash id family-of) (getf family :id)))
          (when (gethash (getf family :id) by-id)
            (setf (gethash (getf family :id) family-of) (getf family :id)))
          (let* ((routing (loop for key in +routing-keys+
                                for target = (cdr (assoc key (getf family :routing) :test #'equal))
                                when (and target (member target present :test #'equal))
                                  collect (cons key target)))
                 (effort-route (find "off" routing :key #'car :test-not #'equal))
                 (reasoning (or (some (lambda (spec) (getf spec :reasoning)) members) (and effort-route t)))
                 (default (or (and (getf family :default-member)
                                   (find (getf family :default-member) present :test #'equal))
                              (first present)))
                 (first (first members)))
            (setf (gethash (getf family :id) replacement)
                  (list* :id (getf family :id)
                         :name (getf family :name)
                         :reasoning reasoning
                         :input (append (and (some (lambda (spec) (member "text" (getf spec :input) :test #'equal)) members)
                                             '("text"))
                                        (and (some (lambda (spec) (member "image" (getf spec :input) :test #'equal)) members)
                                             '("image")))
                         :context (reduce #'max members :key (lambda (spec) (or (getf spec :context) 0)))
                         :output (reduce #'max members :key (lambda (spec) (or (getf spec :output) 0)))
                         :wire-id (and (not (equal default (getf family :id))) default)
                         ;; a surface-less family keeps reasoning and no ladder:
                         ;; every effort would reach the same uid
                         :routing (and routing reasoning (getf family :efforts) routing)
                         :efforts (and reasoning (getf family :efforts))
                         (alexandria:remove-from-plist first :id :name :reasoning :input :context
                                                             :output :wire-id :routing :efforts)))))))
    (let ((emitted (make-hash-table :test #'equal)))
      (loop for spec in specs
            for family = (gethash (getf spec :id) family-of)
            if (null family)
              collect spec
            else unless (gethash family emitted)
                   do (setf (gethash family emitted) t)
                   and when (gethash family replacement) collect it))))

(defun normalize-configs (configs)
  "The specs a GetCliModelConfigs answer lists: the live configs, internal
display slots dropped, fusion pairings routed to their lead, then the server's
families and omp's reviewed ones collapsed (normalizeDevinModels)."
  (let ((live (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (lanes (list '()))
        (specs '()))
    (dolist (config configs)
      (let ((uid (nlk:trimmed (getf config :model-uid))))
        (when (and (not (getf config :disabled)) (plusp (length uid)) (not (gethash uid live)))
          (setf (gethash uid live) config))))
    (dolist (config configs)
      (let* ((info (getf config :info))
             (display (or (getf info :display-option) 0))
             (uid (nlk:trimmed (getf config :model-uid))))
        (unless (or (getf config :disabled) (member display '(4 6))
                    (zerop (length uid)) (gethash uid seen))
          (setf (gethash uid seen) t)
          (let* ((router (or (= display 3) (getf info :model-router)))
                 (assign-router (and router (null (getf info :harness-uids))))
                 (lead (fusion-lead uid live)))
            (unless (eq lead :none)
              (let ((spec (config-spec config uid assign-router)))
                (push (if lead (route-to-lead spec (config-spec (gethash lead live) lead nil)) spec) specs)
                (unless router (collect-family-lane lanes config uid))))))))
    (let* ((specs (nreverse specs))
           (dynamic (collapse specs (dynamic-families (car lanes)))))
      (sort (collapse dynamic (table-families)) #'string< :key (lambda (spec) (getf spec :id))))))

;;; --- the roster this process knows ------------------------------------------------

(defvar *discovered* nil
  "The specs the last successful discovery answered, or NIL.")

(defvar *discovery-tried* nil
  "Whether this process has asked GetCliModelConfigs at all.")

(defun known-specs ()
  "The models the catalog lists: the discovered roster, which replaces the
seeds (dynamic-models-authoritative), else the seeds."
  (or *discovered* (seed-specs)))

(defun family-spec (model-id)
  "The reviewed family MODEL-ID names, as a spec no discovery confirmed: its
routing as the table writes it, its default member the declared one."
  (let ((family (find model-id (table-families) :key (lambda (family) (getf family :id)) :test #'equal)))
    (when family
      (list :id model-id :name (getf family :name) :reasoning (and (getf family :efforts) t)
            :input '("text") :tools t
            :wire-id (or (getf family :default-member) (first (getf family :members)))
            :routing (getf family :routing) :efforts (getf family :efforts)))))

(defun model-spec (model-id)
  "What the wire knows of MODEL-ID: the known roster's spec, else a reviewed family's, else NIL."
  (let ((id (or (nlk:json-value +wire+ :string "aliases" model-id) model-id)))
    (or (find id (known-specs) :key (lambda (spec) (getf spec :id)) :test #'equal)
        (family-spec id))))

(defun supported-effort (asked efforts)
  "ASKED as one of EFFORTS: itself, else the strongest below it, else the weakest."
  (cond ((member asked efforts :test #'equal) asked)
        (t (let ((rank (position asked +efforts+ :test #'equal)))
             (or (and rank (find-if (lambda (effort) (<= (position effort +efforts+ :test #'equal) rank))
                                    efforts :from-end t))
                 (first efforts))))))

(defun wire-uid (spec model-id effort)
  "The uid a round on MODEL-ID at EFFORT asks for (resolveWireModelId): the
effort's route, else the family's default uid, else the id itself."
  (let* ((routing (getf spec :routing))
         (key (if (or (null effort) (string-equal effort "off") (string-equal effort "none"))
                  "off"
                  (supported-effort (string-downcase effort) (or (getf spec :efforts) (list effort))))))
    (or (cdr (assoc key routing :test #'equal))
        (getf spec :wire-id)
        (getf spec :id)
        model-id)))

;;; --- the catalog --------------------------------------------------------------------

(defun catalog-model (spec)
  "SPEC as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (destructuring-bind (&optional (input 0) (output 0) cache-read cache-write) (getf spec :cost)
    (nle::make-catalog-model
     (getf spec :name)
     (getf spec :context)
     (getf spec :output)
     (coerce (getf spec :input) 'vector)
     #("text")
     (sort (remove-if-not #'nle::effort-rank (copy-list (getf spec :efforts))) #'< :key #'nle::effort-rank)
     (getf spec :reasoning)
     nil
     (getf spec :tools)
     ;; CATALOG-PRICE's shape; a row priced at nothing is a plan's, not a price
     (and (or (plusp input) (plusp output))
          (list input output cache-read cache-write)))))

(defun catalog-row (&optional prior)
  "Devin as a models.dev provider: this cell's lane package, this section's
base, the key variable, and the known roster over PRIOR's models (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (dolist (spec (known-specs))
      (setf (gethash (getf spec :id) models) (catalog-model spec)))
    (nlk:json-object "name" "Devin"
                     "npm" "nodecode-devin"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))
