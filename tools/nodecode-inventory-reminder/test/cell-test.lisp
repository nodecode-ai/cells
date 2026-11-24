;;;; cell-test.lisp --- lifecycle, the digest, the report, the tools, /inventory-reminder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: that a start installs the two
;;;; tools, the command and the manual topic and a stop takes them back; that a
;;;; term finds a hit in every section the digest holds it in and a miss says so;
;;;; that the one-line digest reads the sections it was given; that a digest
;;;; that is not there refuses in words naming the file; that both tools answer
;;;; through the real dispatch seam and are held to the shape they advertise;
;;;; and that /inventory-reminder answers a digest when it is given no term.
;;;; Every test reads a digest written to a temporary file: no network, no scan.

(in-package #:nodecode.test)

(define-test-slice "inventory-reminder" "INVENTORY-REMINDER-CELL-"
  :start nodecode-inventory-reminder:start-cell)

;;; --- the fixture ----------------------------------------------------------------

(defparameter +digest-json+
  (concatenate 'string
   "{\"generated_at\":\"2026-10-20T11:02:33Z\","
   "\"repos\":{\"available\":true,\"owned_total\":3,\"local\":1,\"online_only\":2,"
   "\"local_repos\":[\"ether-btc/aider-notes\"]},"
   "\"skills\":{\"available\":true,\"total\":2,\"names\":[\"aider\",\"writer\"],"
   "\"by_source\":{\"aider\":2}},"
   "\"tools\":{\"available\":true,\"mcp_servers\":[{\"name\":\"aider-mcp\",\"enabled\":true,"
   "\"command\":\"aider --mcp\"}],\"mcp_enabled\":[\"aider-mcp\"],\"mcp_total\":1,"
   "\"cli_tools\":{\"aider\":\"/opt/bin/aider\",\"rg\":\"\"}},"
   "\"apps\":{\"available\":true,\"count\":42,\"key_packages\":{\"aider-chat\":\"0.50.1\"}},"
   "\"models\":{\"available\":true,\"count\":1,\"total_size\":\"2.7G\","
   "\"models\":[{\"name\":\"aider-q4.gguf\",\"size\":\"1.2G\",\"path\":\"models/aider-q4.gguf\"}]},"
   "\"services\":{\"available\":true,\"count\":1,\"running_total\":4}}")
  "A digest with one 3-section hit for `aider', a miss for anything else, and
the counts the one-line digest prints.")

(defun write-digest (&optional (text +digest-json+))
  "TEXT as a digest file in a temporary directory, its pathname."
  (let ((path (merge-pathnames (format nil "inventory-reminder-cell-~a.json" (gensym))
                                (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-string text out))
    path))

(defmacro with-digest ((path &optional (text '+digest-json+)) &body body)
  "Run BODY with the cell started on a digest holding TEXT, whatever PATH is."
  `(let ((,path (write-digest ,text)))
     (unwind-protect
          (with-cell-stop ((inventory-reminder-start "inventory_json" (namestring ,path)))
            ,@body)
       (ignore-errors (delete-file ,path)))))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "inventory-reminder"
  (:help :inventory-reminder)
  (:command "inventory-reminder")
  (:running (is (nle::find-tool "inventory_where") "the search tool is registered")
            (is (nle::find-tool "inventory_summary") "the summary tool is registered")
            (is (= 40 (nodecode-inventory-reminder::setting :max-hits)) "max_hits defaults to the original's cap"))
  (:stopped (is (null (nle::find-tool "inventory_where")) "stop unregisters the search tool")
            (is (null (nle::find-tool "inventory_summary")) "and the summary tool"))
  (:refused ("max_hits" 0))
  (:idle nodecode-inventory-reminder:inventory-reminder-error
         (inventory-reminder:digest)))

;;; --- the digest and the report --------------------------------------------------

(deftest inventory-reminder-cell-a-term-finds-every-section-it-is-in ()
  (with-digest (path)
    (let ((text (inventory-reminder:lookup "aider")))
      (is-carrying (text text)
        ("SKILLS" "the skill is named under its section")
        ("aider    (2)" "with the count the digest carries for it")
        ("REPOS (owned)" "the repository whose name holds the term")
        ("MCP SERVERS" "the server that would answer to it")
        ("enabled → aider --mcp" "with whether it is enabled and what runs it")
        ("CLI TOOLS" "the binary on PATH")
        ("/opt/bin/aider" "with the path that makes the hit usable")
        ("MODELS" "the model file")
        ("KEY PACKAGES" "and the package")))))

(deftest inventory-reminder-cell-a-miss-says-so-by-name ()
  (with-digest (path)
    (is-carrying (text (inventory-reminder:lookup "zzz"))
      ("MISS: zzz — not found in any section" "a miss is a fact, not an empty answer"))))

(deftest inventory-reminder-cell-the-digest-is-one-line ()
  (with-digest (path)
    (let ((line (inventory-reminder:digest)))
      (is-carrying (text line)
        ("INVENTORY 2026-10-20 11:02" "the digest's own stamp leads")
        ("repos 3 (1L/2O)" "cloned and online-only counted apart")
        ("skills 2 (2a)" "the skill total and its sources")
        ("mcp 1/1" "servers enabled over total")
        ("pkgs 42" "and the rest of the host"))
      (is (null (find #\Newline line)) "one line, for a session bootstrap"))))

(deftest inventory-reminder-cell-a-digest-that-is-not-there-refuses-in-words ()
  (with-cell-stop ((inventory-reminder-start "inventory_json" "/nonexistent/inventory.json"))
    (is-carrying (text (refusal-text nodecode-inventory-reminder:inventory-reminder-error
                                     (inventory-reminder:lookup "aider")))
      ("no inventory digest at /nonexistent/inventory.json" "the file it looked for is named"))
    (is-carrying (text (refusal-text nodecode-inventory-reminder:inventory-reminder-error
                                     (inventory-reminder:digest)))
      ("write one with the inventory-reminder scanners" "and what to do about it"))))

(deftest inventory-reminder-cell-a-term-that-is-blank-is-a-refusal ()
  (with-digest (path)
    (is-carrying (text (refusal-text nodecode-inventory-reminder:inventory-reminder-error
                                     (inventory-reminder:lookup "  ")))
      ("a search needs a term" "an empty search says what it wanted"))))

(deftest inventory-reminder-cell-the-command-answers-a-term-and-a-digest ()
  (with-digest (path)
    (is-carrying (text (nodecode-inventory-reminder::run-slash "writer" "s1"))
      ("writer" "a term searches"))
    (is-carrying (text (nodecode-inventory-reminder::run-slash "" "s1"))
      ("INVENTORY 2026-10-20 11:02" "no term answers the one-line digest"))))

;;; --- the tools ------------------------------------------------------------------

(deftest inventory-reminder-cell-the-search-tool-answers-through-the-seam
    (with-session-store ("s1"))
  (with-digest (path)
    (let ((result (execute-wire-call "call-1" "inventory_where" "{\"term\":\"aider\"}")))
      (is (equal "tool" (gethash "role" result)))
      (is-carrying (text (princ-to-string (nle::message-content result)))
        ("aider" "the tool answers the report, not a summary of it")))))

(deftest inventory-reminder-cell-the-summary-tool-answers-one-line (with-session-store ("s1"))
  (with-digest (path)
    (let ((result (execute-wire-call "call-2" "inventory_summary" "{}")))
      (is (equal "tool" (gethash "role" result)))
      (is-carrying (text (princ-to-string (nle::message-content result)))
        ("INVENTORY 2026-10-20 11:02" "the same line eval answers")))))

(deftest inventory-reminder-cell-json-answers-the-shape-the-plugin-emitted (with-session-store ("s1"))
  (with-digest (path)
    (let* ((raw (princ-to-string
                 (nle::message-content
                  (execute-wire-call "call-3" "inventory_where"
                                      "{\"term\":\"aider\",\"json\":true}"))))
           (decoded (ignore-errors (nlk:decode-json raw :whole t)))
           (hits (and (hash-table-p decoded) (gethash "hits" decoded))))
      (is (hash-table-p decoded) "the answer is a JSON object")
      (is (equal "aider" (gethash "query" decoded)) "with the query it answered")
      (is (and (vectorp hits) (plusp (length hits))) "and a hits array")
      (is (equal "skills" (gethash "section" (aref hits 0))) "each hit naming its section"))))

(deftest inventory-reminder-cell-the-tools-are-held-to-the-shape-they-advertise
    (with-session-store ("s1"))
  (with-digest (path)
    (let* ((entry (nle::find-tool "inventory_where"))
           (contract (second entry)))
      (is (nlk:contract-p contract) "the search tool is registered against its declaration")
      (is (equalp (nlk:contract-schema contract)
                  (nlk:json-value (aref (nle::tools-payload (list entry)) 0)
                                  :object "function" "parameters")) "and advertises its shape"))
    (loop for (arguments needle) in '(("{}" "term must be present")
                                      ("{\"term\":\"\"}" "contract_refused"))
          do (is-carrying (text (princ-to-string
                                 (nle::message-content
                                  (execute-wire-call "call-bad" "inventory_where" arguments))))
               ("contract_refused" arguments)
               (is (search needle text) needle)))))
