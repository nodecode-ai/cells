;;;; support.lisp --- the test slice: a stubbed MCP server.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; CORPUS-CELL- name prefix; RUN-CORPUS-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NLK:HTTP, the cell's only way to the
;;;; network, and NLE::CREDENTIAL-ENV, the only way to the environment.
;;;; WITH-WIRE replaces both: a small MCP server -- an initialize that hands
;;;; out a session, and one answer per tool -- records every message, so a test
;;;; asserts on the exchange and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "corpus" "CORPUS-CELL-" :start nodecode-corpus:start-cell)

;;; --- the canned server ---------------------------------------------------------

(defparameter +endpoint+ "https://corpus.test/api/mcp"
  "The address a test's section names.")

(defparameter +initialize-answer+
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{\"tools\":{}},\"serverInfo\":{\"name\":\"corpus\",\"version\":\"0.1.0\"}}}"
  "What the stubbed server answers to initialize.")

(defparameter +session-headers+ '(("mcp-session-id" . "sess-1"))
  "What it hands out with that answer: one session for the whole start.")

(defparameter +search-text+
  "Tex. Loc. Gov't Code Sec. 217.042: a mobile food vendor in a residential zone
needs a municipal permit. citation: Tex. Loc. Gov't Code Sec. 217.042"
  "What the stubbed law.search answers.")

(defparameter +node-text+
  "Sec. 217.042. MOBILE FOOD VENDORS. (a) A vendor may operate from a
residentially zoned lot only under a permit issued by the municipality."
  "What the stubbed law.get_node answers.")

(defparameter +coverage-text+
  "Covered: US federal, all 50 states. Austin, TX: indexed. County ordinances
outside the indexed metros: not covered."
  "What the stubbed law.list_coverage answers.")

(defparameter +account-text+
  "Plan: anonymous floor. Searches left this month: 12. Formation: free."
  "What the stubbed account.status answers.")

(defparameter +formation-text+
  "llc in MS: name, principal office, contact email, NAICS code, management,
organizer. stateFeeCents 5000, totalCents 5000, einAvailable false."
  "What the stubbed formation.requirements answers.")

(defparameter +answers+
  (list (cons "law.search" +search-text+)
        (cons "law.get_node" +node-text+)
        (cons "law.list_coverage" +coverage-text+)
        (cons "account.status" +account-text+)
        (cons "formation.requirements" +formation-text+))
  "What the stub answers for each tool, by its dotted name.")

(defvar *calls* '()
  "Every JSON-RPC message the stub took, newest first: (:rpc :tool :body :headers).")

(defvar *routes* '()
  "((TOOL . FUNCTION) ...) a test scripts; FUNCTION takes the request body and
answers (values TEXT STATUS HEADERS). The newest entry for a tool wins.")

(defvar *env* '()
  "((NAME . VALUE) ...) the variables a test lets CREDENTIAL-ENV answer.")

(defun script (tool function)
  "Answer TOOL with FUNCTION for the rest of the test."
  (push (cons tool function) *routes*))

(defun body-of (content)
  "CONTENT -- a JSON request body -- decoded, or NIL."
  (let ((body (ignore-errors (nlk:decode-json (or content "")))))
    (and (hash-table-p body) body)))

(defun answer-message (body text)
  "A tools/call answer carrying TEXT, under BODY's own id."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0"
                     "id" (or (nlk:json-value body :integer "id") 1)
                     "result" (nlk:json-object
                               "content" (vector (nlk:json-object "type" "text"
                                                                   "text" text))))))

(defun stub-http (method url &rest arguments)
  "The stubbed NLK:HTTP: a small MCP server, and a record of every message."
  (declare (ignore method url))
  (let* ((body (body-of (getf arguments :content)))
         (name (and body (nlk:json-value body :string "method")))
         (tool (and body (nlk:json-value body :string "params" "name"))))
    (push (list :rpc name :tool tool :body body :headers (getf arguments :headers))
          *calls*)
    (let ((route (and tool (assoc tool *routes* :test #'equal))))
      (cond
        (route (funcall (cdr route) body))
        ((equal name "initialize") (values +initialize-answer+ 200 +session-headers+))
        ((equal name "notifications/initialized") (values "" 202 '()))
        ((equal name "tools/call")
         (values (answer-message body (or (cdr (assoc tool +answers+ :test #'equal))
                                          (format nil "~a answered" tool)))
                 200 '()))
        (t (values "" 404 '()))))))

(defun calls-of (rpc)
  "The messages the stub took for RPC, newest first."
  (remove-if-not (lambda (call) (equal rpc (getf call :rpc))) *calls*))

(defun header-of (call name)
  "What CALL carried for header NAME, case-insensitively."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun arguments-of (call)
  "The arguments object CALL sent."
  (nlk:json-value (nlk:json-value (getf call :body) :object "params") :object "arguments"))

(defmacro with-wire (&body body)
  "Run BODY with NLK:HTTP and the environment stubbed and the cell's session
state cleared."
  `(with-saved-globals (*calls* *routes* *env*
                        nodecode-corpus::*session-id*
                        nodecode-corpus::*stateless-p*
                        nodecode-corpus::*rpc-counter*)
     (setf *calls* '()
           *routes* '()
           *env* '()
           nodecode-corpus::*session-id* nil
           nodecode-corpus::*stateless-p* nil
           nodecode-corpus::*rpc-counter* 0)
     (with-stubbed-fdefinitions
         ((nlk:http (method url &rest arguments) (apply #'stub-http method url arguments))
          (nle::credential-env (name) (cdr (assoc name *env* :test #'equal))))
       ,@body)))

(defmacro with-corpus ((&rest pairs) &body body)
  "Run BODY with the cell started under PAIRS as the `corpus' section, its wire
stubbed, and stopped on unwind."
  `(with-wire
     (with-cell-stop ((corpus-start "endpoint" +endpoint+ ,@pairs))
       ,@body)))

;;; --- asking the cell what a model would -------------------------------------

(defun corpus-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))
