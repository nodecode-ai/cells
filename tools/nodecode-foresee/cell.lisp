;;;; cell.lisp --- the primer, the section, the /foresee command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was one MCP server entry plus three skills (quote-insurance,
;;;; compare-carriers, explain-coverage). The entry is the two tools
;;;; tools.lisp declares; the skills are +PRIMER+, which the model reads with
;;;; (help :foresee) and whose one line rides every request. Nothing hooks the
;;;; provider path: no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "foresee": {"endpoint": "https://agents.go-foresee.com/mcp",
;;;;               "timeout_seconds": 120, "max_chars": 6000,
;;;;               "api_key": "...", "api_key_env": "FORESEE_API_KEY"}
;;;; Foresee's own server needs no key. A vetoed section ("enabled": false)
;;;; installs nothing.

(in-package #:nodecode-foresee)

(defparameter +primer+
  "Foresee prices home, auto and renters insurance across carriers through two tools. Every
number comes from Foresee: never multiply a factor, interpolate a ladder rung, or average
carriers -- call the tool again for a different ask.
  quote_insurance       instant estimates. Needs profile (zip_code, age or dob, and for auto each
                        vehicle's year/make/model) and lines, the coverage asked as actual
                        numbers: {\"auto\": {\"bi\": \"100/300\", \"pd\": 100,
                        \"coll_deductible\": 500, \"comp_deductible\": 500}} on the first call,
                        declared as the assumption it is. Each carrier answers a C row (monthly
                        point estimate and ci), L rows of per-coverage cost, D price ladders as
                        +/- $/mo per rung, and carrier_quote_url. not_priced names who is
                        missing and why; W warnings and a (asked X) rung mean the carrier does
                        not offer it -- relay both. Facts the user did not give come back under
                        assumptions, with tighten_by naming what would move the price most:
                        answer point estimate plus assumptions, never a wider range.
  live_carrier_quotes   Foresee's agents complete the carriers' own quote forms and read back
                        the premium the page printed. Same profile and lines, plus
                        user_authorization (the user's go-ahead, verbatim) and usually identity
                        (first_name, last_name, dob, street, city, email; never an SSN). Ask
                        first, and say plainly that the carriers may make a soft credit-based
                        insurance score check and may email or phone. Re-call with the same
                        arguments after a minute or two to collect; a carrier that declined is
                        an answer, not an error.
How to present: cheapest first, a Carrier | Monthly | 6-month total table, and the annual spread
between cheapest and dearest. Two carriers inside each other's confidence interval are effectively
tied, not a $3/mo winner. Say these are Foresee's estimates, that Foresee binds nothing and takes
no payment, and end with the chosen carrier's carrier_quote_url. California auto is live today;
anything else comes back under skipped -- relay it once, in one sentence.
From the keyboard, /foresee shows the connection and the tools the server advertises."
  "What (help :foresee) answers while the cell runs: the plugin's three skills in one topic.")

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's values, with the endpoint
defaulted and the key resolved out of its variable."
  (declare (ignore table))
  (list :endpoint (let ((given (trim (getf values :endpoint)))) (or given +default-url+))
        :timeout-seconds (getf values :timeout-seconds)
        :max-chars (getf values :max-chars)
        :api-key-env (getf values :api-key-env)
        :api-key (or (nonempty (getf values :api-key))
                      (let ((variable (nonempty (getf values :api-key-env))))
                        (and variable (nonempty (uiop:getenv variable)))))))

;;; --- the operator's command --------------------------------------------------

(defun one-line (text limit)
  "TEXT as one line of at most LIMIT characters."
  (let* ((flat (substitute #\Space #\Newline (or text "")))
         (trimmed (nlk:trimmed flat)))
    (if (> (length trimmed) limit)
        (concatenate 'string (subseq trimmed 0 limit) "...")
        trimmed)))

(defun named-like-p (tool filter)
  "Whether TOOL's name answers to FILTER, a substring case-insensitively; every
one when FILTER is empty."
  (or (zerop (length filter))
      (let ((name (string-downcase (or (gethash "name" tool) ""))))
        (and (search (string-downcase filter) name) t))))

(defun status-text (&optional (filter ""))
  "What this cell is connected to: the endpoint, the session, and the tools
Foresee's server advertises -- every one whose name answers to FILTER."
  (handler-case
      (let* ((tools (server-tools))
             (matching (remove-if-not (lambda (tool) (named-like-p tool filter)) tools)))
        (with-output-to-string (out)
          (format out "foresee: ~a~%" (endpoint))
          (when *session* (format out "session ~a~%" *session*))
          (format out "~d of ~d tool~:p on the server~@[ matching ~s~]:~%"
                  (length matching) (length tools) (and (plusp (length filter)) filter))
          (loop for tool in matching
                do (format out "  ~a - ~a~%"
                           (or (gethash "name" tool) "?")
                           (one-line (gethash "description" tool) 160)))))
    (foresee-error (condition)
      (format nil "foresee: ~a" (foresee-error-detail condition)))))

(defun run-command (args session-id)
  "One /foresee invocation: the connection and the server's tools, narrowed
by the argument when one is typed."
  (declare (ignore session-id))
  (status-text (nlk:trimmed (or args ""))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell foresee
  (:section ("foresee")
    (:guide "nothing is required: the cell talks to Foresee's hosted server as it is; endpoint only to point at a proxy of your own; /foresee shows the connection and the server's tools; next step: ask for a quote")
    ("endpoint" :string
     :doc "the MCP address: https://agents.go-foresee.com/mcp unless you go through a proxy of your own")
    ("timeout_seconds" :integer :default 120 :min 5
     :doc "one call's ceiling; a live walk is collected over minutes, which is more than one call")
    ("max_chars" :integer :default 6000 :min 200
     :doc "how much of one answer the model reads before it is cut")
    ("api_key" :secret
     :doc "a bearer key, only for a server that asks for one; Foresee's own needs no sign-in")
    ("api_key_env" :env
     :doc "the environment variable holding that key, instead of writing it here"))
  (:settings 'read-settings)
  (:help :foresee
         "foresee: home and auto insurance quotes across carriers, with optional live premiums from the carriers' own sites - (help :foresee)"
         +primer+)
  (:command "foresee" 'run-command
            :description "Foresee: the connection, and the tools the server offers"
            :argument-hint "TOOL"
            :session nil)
  (:tool "quote_insurance" (nlk:contract 'quote-insurance-call) #'tool-quote)
  (:tool "live_carrier_quotes" (nlk:contract 'live-carrier-quotes-call) #'tool-live-quotes)
  (:start (lambda ()
            (setf *session* nil)
            (check-endpoint)
            (nle:on-stop (lambda () (setf *session* nil))))))
