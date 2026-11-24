;;;; wire.lisp --- the one network seam: OpenRouter's REST API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is one JSON POST with the user's own key, to the base the
;;;; section names (https://openrouter.ai/api/v1 by default), and answers one
;;;; JSON object. The two System One APIs (decisions, extractions) live under
;;;; /api/alpha, derived from the same base the way the plugin derives it, so
;;;; a base of the operator's own keeps working for both roots.
;;;;
;;;; Every byte goes through NLK:HTTP, so a test stubs it and never touches
;;;; the network. A refused status or a transport failure is
;;;; OPENROUTER-EVERYTHING-ERROR with the service's own words in it, which is
;;;; what the model repairs from.

(in-package #:nodecode-openrouter-everything)

(defparameter +default-base+ "https://openrouter.ai/api/v1"
  "Where a section that names no base_url sends its calls.")

(defparameter +key-page+ "https://openrouter.ai/settings/keys"
  "Where a user makes the key this cell spends.")

(defparameter +default-models+
  '(("decision" . "typesafe/jev-1.14")
    ("extraction" . "cloudflare/clairic")
    ("rerank" . "cohere/rerank-v3.5")
    ("embeddings" . "google/gemini-embedding-2")
    ("web-search" . "deepseek/deepseek-v4.1-flash-1017"))
  "The plugin's curated model per capability, used when the section names none.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value)
       (let ((trimmed (nlk:trimmed value)))
         (and (plusp (length trimmed)) trimmed))))

;;; --- the section ------------------------------------------------------------

(defun api-key ()
  "The key the section names inline or by variable, else OPENROUTER_API_KEY."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "OPENROUTER_API_KEY"))))

(defun require-key ()
  "The key, or the refusal that names where one comes from."
  (or (api-key)
      (fail "openrouter-everything has no key: set OPENROUTER_API_KEY, or \"api_key\" (or \"api_key_env\") ~
in the openrouter-everything section; a key is made at ~a" +key-page+)))

(defun base-url ()
  "The base this cell sends to, without a trailing slash."
  (string-right-trim "/" (or (nonempty (setting :base-url)) +default-base+)))

(defun api-url (route)
  "The v1 root with ROUTE under it."
  (format nil "~a~a" (base-url) route))

(defun ends-with (text suffix)
  "Whether TEXT ends with SUFFIX."
  (let ((n (length suffix))
        (m (length text)))
    (and (>= m n) (string= suffix text :start2 (- m n)))))

(defun alpha-url (route)
  "The alpha root derived from the v1 base, with ROUTE under it."
  (let* ((base (base-url))
         (suffix "/v1"))
    (if (ends-with base suffix)
        (format nil "~a/alpha~a" (subseq base 0 (- (length base) (length suffix))) route)
        (fail "openrouter-everything: decisions and extractions live under /api/alpha, so \"base_url\" must ~
end in /v1 (it is ~a)" base))))

(defun configured-model (capability)
  "The model this section names for CAPABILITY (a keyword), else the plugin's default."
  (or (nonempty (setting (intern (format nil "~a-MODEL" (symbol-name capability)) :keyword)))
      (cdr (assoc (string-downcase (symbol-name capability)) +default-models+ :test #'equal))
      (fail "openrouter-everything: no model for ~(~a~)" capability)))

(defun attribution-headers ()
  "The headers that say which app a request comes from, as the plugin sends them."
  '(("http-referer" . "https://nodecode.ai")
    ("x-title" . "Nodecode (openrouter-everything)")))

(defun message-headers ()
  "The headers every request carries: JSON, the key, and the app's name."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json")
                (cons "authorization" (format nil "Bearer ~a" (require-key))))
          (attribution-headers)))

;;; --- one exchange -----------------------------------------------------------

(defun clip (text limit)
  "TEXT cut to LIMIT characters."
  (cond ((null text) "")
        ((<= (length text) limit) text)
        (t (format nil "~a..." (subseq text 0 limit)))))

(defun body-text (body)
  "BODY — what NLK:HTTP answered — as a string, or NIL."
  (cond ((streamp body) (nlk:body-text body (* 4 1024 1024)))
        ((stringp body) body)
        ((and (vectorp body) (not (stringp body)))
         (handler-case (nlk:decode-octets body) (error () nil)))
        ((null body) nil)
        (t (format nil "~a" body))))

(defun error-message (answer text)
  "The service's own words from ANSWER, a decoded error object, else TEXT."
  (or (and (hash-table-p answer)
           (let ((error (nlk:json-value answer :object "error")))
             (or (nlk:json-value error :text "message")
                 (nlk:json-value answer :text "error"))))
      (clip (nlk:trimmed (or text "")) 300)))

(defun exchange (method url &key content)
  "One JSON exchange => the answer as a JSON object. A refusal or a
transport failure is OPENROUTER-EVERYTHING-ERROR naming the URL and the
service's answer."
  (let ((timeout (setting :timeout-seconds))
        (body nil)
        (status nil))
    (handler-case
        (multiple-value-setq (body status)
          (nlk:http method url
                    :headers (message-headers)
                    :content content
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))
      (error (condition)
        (fail "openrouter-everything could not reach ~a: ~a" url (clip (princ-to-string condition) 300))))
    (let* ((text (body-text body))
           (answer (and text
                        (plusp (length (nlk:trimmed text)))
                        (handler-case (nlk:decode-json text) (error () nil)))))
      (cond
        ((and (integerp status) (<= 200 status 299))
         (if (hash-table-p answer)
             answer
             (fail "openrouter-everything: ~a answered something that is not a JSON object" url)))
        (t (fail "openrouter-everything: ~a refused (~a): ~a"
                 url (or status "no status") (error-message answer text)))))))

;;; --- reading answers --------------------------------------------------------

(defun usage-of (answer)
  "The spend ANSWER reports, as a plist, or NIL."
  (let ((usage (nlk:json-value answer :object "usage")))
    (when (hash-table-p usage)
      (list :tokens (or (nlk:json-value usage :integer "total_tokens") 0)
            :cost (nlk:json-value usage :number "cost")))))

(defun usage-line (usage)
  "USAGE as one short line."
  (format nil "~@[~d tokens~]~@[ (~,6f USD)~]"
          (getf usage :tokens)
          (getf usage :cost)))

(defun object-of (value label)
  "VALUE as a JSON object: a hash table, or a plist of name to text."
  (cond
    ((hash-table-p value) value)
    ((and (listp value) (plusp (length value)) (evenp (length value)))
     (apply #'nlk:make-json-object
            (loop for (key member) on value by #'cddr
                  append (list (string-downcase (string key))
                               (if (stringp member) member (format nil "~a" member))))))
    ((null value) nil)
    (t (fail "openrouter-everything: ~a must be a JSON object (a hash table or a plist of name to text)" label))))

(defun string-list (value label)
  "VALUE as a list of strings: a string, a list, or a JSON array."
  (let ((items (cond ((stringp value) (list value))
                     ((arrayp value) (coerce value 'list))
                     ((listp value) value)
                     (t (fail "openrouter-everything: ~a must be a list of strings" label)))))
    (let ((texts (remove nil (mapcar #'nonempty items))))
      (or texts
          (fail "openrouter-everything: ~a must hold at least one non-empty string" label)))))

(defun array-value (object &rest path)
  "The JSON array at PATH, as a list of whatever it holds."
  (let ((value (apply #'nlk:json-value object :array path)))
    (cond ((arrayp value) (coerce value 'list))
          ((listp value) value)
          (t '()))))
