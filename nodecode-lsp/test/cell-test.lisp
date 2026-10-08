;;;; cell-test.lisp --- the lsp cell against a scripted language server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every server here is test/fake-server.py, a local python3 child serving
;;;; .fk files under a fake.toml; nothing touches the network, and every
;;;; project is a temp directory deleted after. The write-path tests run a
;;;; real eval through the :tool chain on its own worker thread, as a turn
;;;; does, because a write recorded on the test thread would pass even if
;;;; the record never crossed threads.

(in-package #:nodecode.test)

(define-test-slice "lsp" "LSP-CELL-" :start nodecode-lsp:start-cell)

(define-cell-lifecycle-tests "lsp"
  (:hooks :tool 'nle::write-file-text)
  (:help :lsp)
  (:refused ("wait_ms" -1)
            ("servers" 5)
            ("servers" (nlk:json-object "mine" (nlk:json-object "command" "my-ls"))))
  (:idle lsp:lsp-error (lsp:status) (lsp:definition "a.fk" "x") (lsp:diagnostics "a.fk")))

(defparameter *lsp-fake-server*
  (uiop:native-namestring (asdf:system-relative-pathname "nodecode-lsp" "test/fake-server.py"))
  "The scripted server.")

(defun lsp-servers (&rest flags)
  "The section's servers object: the fake server run with FLAGS, on .fk files
under a fake.toml."
  (nlk:json-object "fake" (nlk:json-object "command" "python3"
                                           "args" (coerce (list* "-I" *lsp-fake-server* flags) 'vector)
                                           "file_types" (vector ".fk")
                                           "root_markers" (vector "fake.toml")
                                           "settings" (nlk:json-object "fake" (nlk:json-object "depth" 3)))))

(defmacro with-lsp-project ((root &rest files) &body body)
  "BODY with ROOT a fresh directory's native path, trailing slash, holding
fake.toml and FILES, (NAME TEXT) each; deleted after."
  `(let ((,root (nodecode-lsp::native (temp-path "lsp"))))
     (unwind-protect
          (progn (write-temp-file (uiop:parse-native-namestring (concatenate 'string ,root "fake.toml")) "")
                 ,@(loop for (name text) in files
                         collect `(write-temp-file (uiop:parse-native-namestring (concatenate 'string ,root ,name))
                                                   ,text))
                 ,@body)
       (ignore-errors (uiop:delete-directory-tree (uiop:parse-native-namestring ,root) :validate t)))))

(defmacro with-lsp ((&rest flags) (&rest pairs) &body body)
  "BODY with the cell started on the fake server run with FLAGS, the section
holding PAIRS too."
  `(with-cell-stop ((lsp-start "servers" (lsp-servers ,@flags) ,@pairs))
     ,@body))

(defun lsp-file (root name)
  (concatenate 'string root name))

(defun lsp-server (name)
  "The server NAME the cell holds."
  (find name nodecode-lsp::*servers* :key #'nodecode-lsp::srv-name :test #'string=))

(defun lsp-pid (name)
  (let ((server (lsp-server name)))
    (and server (nodecode-lsp::srv-conn server) (nodecode-lsp::conn-pid (nodecode-lsp::srv-conn server)))))

(defun lsp-alive-p (pid)
  "Whether PID is still a process, a zombie included."
  (and pid (probe-file (format nil "/proc/~d/" pid))))

;;; --- the wire --------------------------------------------------------------------

(defmacro with-fake-connection ((conn &rest flags) &body body)
  "BODY with CONN a connection to the fake server, closed and reaped after."
  `(let ((,conn (nodecode-lsp::open-connection (list "python3" "-I" *lsp-fake-server* ,@flags)
                                               :label "fake")))
     (unwind-protect (progn ,@body)
       (nlk:kill-tree (nodecode-lsp::conn-pid ,conn))
       (nodecode-lsp::close-connection ,conn)
       (ignore-errors (sb-thread:join-thread (bt2:thread-native-thread (nodecode-lsp::conn-reader ,conn))
                                             :default nil :timeout 2))
       (await (:timeout 2) (nlk:child-exit (nodecode-lsp::conn-pid ,conn) nil)))))

(deftest lsp-cell-rpc-frames-by-bytes ()
  (with-fake-connection (conn)
    (let ((text (format nil "h~cllo ~c~c ~c" (code-char #xe9) (code-char #x4e16) (code-char #x754c)
                        (code-char #x1f980))))
      (multiple-value-bind (result state) (nodecode-lsp::call conn "fake/echo" (nlk:json-object "text" text "split" t))
        (is (eq :ok state))
        (is (equal text (nlk:json-value result :string "text"))
            "a multibyte body split across reads arrives whole"))
      (is (equal "x" (nlk:json-value (nodecode-lsp::call conn "fake/echo" (nlk:json-object "text" "x" "junk" t))
                                     :string "text"))
          "a block a wrapper printed before the header is skipped")
      (is (equal (format nil "a~cb" (code-char #xfffd)) (nodecode-lsp::call conn "fake/badbytes" nil))
          "a byte that is not UTF-8 costs one character, not the connection")
      (is (equal "y" (nlk:json-value (nodecode-lsp::call conn "fake/echo" (nlk:json-object "text" "y")) :string "text"))
          "and the connection reads on"))))

(deftest lsp-cell-rpc-cancels-a-request-it-stops-waiting-for ()
  (with-fake-connection (conn)
    (multiple-value-bind (result state) (nodecode-lsp::call conn "fake/never" nil :seconds 0.3)
      (is (null result))
      (is (eq :timeout state)))
    (let ((id (nodecode-lsp::conn-next-id conn)))
      (is (zerop (hash-table-count (nodecode-lsp::conn-pending conn))) "nothing is left pending")
      (is (find id (nodecode-lsp::call conn "fake/cancelled" nil)) "the server was told with $/cancelRequest"))
    (multiple-value-bind (result state) (nodecode-lsp::call conn "fake/unknown" nil)
      (declare (ignore result))
      (is (eq :error state) "a server's error is the call's state"))
    (multiple-value-bind (result state) (nodecode-lsp::call conn "fake/crash" nil :seconds 5)
      (declare (ignore result))
      (is (eq :closed state) "a server that dies answers every call closed"))
    (is (signals-error lsp:lsp-error (nodecode-lsp::notify conn "fake/echo")) "and takes nothing more")))

;;; --- positions and edits ------------------------------------------------------------

(deftest lsp-cell-text-counts-columns-in-the-servers-units ()
  (let* ((crab (string (code-char #x1f980)))
         (text (format nil "a~ab~%~c~cx" crab (code-char #x4e16) (code-char #x754c)))
         (starts (nodecode-lsp::line-starts text)))
    (flet ((at (index encoding)
             (let ((position (nodecode-lsp::lsp-position text starts index encoding)))
               (list (gethash "line" position) (gethash "character" position)))))
      (is (equal '(0 3) (at 2 :utf-16)) "an emoji is two UTF-16 units")
      (is (equal '(0 5) (at 2 :utf-8)) "and four UTF-8 bytes")
      (is (equal '(1 2) (at 6 :utf-16)) "a CJK character is one unit")
      (is (equal '(1 6) (at 6 :utf-8)) "and three bytes")
      (is (= 2 (nodecode-lsp::position-index text starts 0 3 :utf-16)) "and back")
      (is (= 6 (nodecode-lsp::position-index text starts 1 6 :utf-8)))
      (is (= 3 (nodecode-lsp::position-index text starts 0 99 :utf-16)) "past the line's end is its end")
      (is (= 3 (nodecode-lsp::position-column text starts 1 2 :utf-16)) "a 1-based character column"))))

(deftest lsp-cell-text-finds-a-symbol-by-name ()
  (let ((text (format nil ";; parse the input~%parser = parse(x) + parse(y)~%PARSE")))
    (flet ((at (spec &rest keys)
             (multiple-value-list (apply #'nodecode-lsp::locate-symbol text spec keys))))
      (is (equal '(0 3) (at "parse")) "the first mention, a comment included")
      (is (equal '(1 28) (at "parse" :line 2)) "on a word boundary: parser is not parse")
      (is (equal '(1 39) (at "parse#2" :line 2)) "the second on the line")
      (is (equal '(1 28) (at "parse#2")) "the second in the file")
      (is (equal '(2 48) (at "parse" :line 3)) "an exact match first, then any case")
      (is (search "not in the file" (refusal-text lsp:lsp-error (at "absent"))))
      (is (search "occurs 1 time" (refusal-text lsp:lsp-error (at "parse#2" :line 1))))
      (is (search "outside the file" (refusal-text lsp:lsp-error (at "parse" :line 9)))))))

(defun lsp-edit (line start end new)
  (nlk:json-object "range" (nlk:json-object "start" (nlk:json-object "line" line "character" start)
                                            "end" (nlk:json-object "line" line "character" end))
                   :opt "newText" new))

(deftest lsp-cell-text-applies-edits-as-if-at-once ()
  (let ((text (format nil "one two~%three")))
    (is (equal (format nil "ONE two~%3") (nodecode-lsp::apply-text-edits
                                          text (vector (lsp-edit 1 0 5 "3") (lsp-edit 0 0 3 "ONE")) :utf-16))
        "edits in any order apply against the original text")
    (is (equal (format nil "abone two~%three")
               (nodecode-lsp::apply-text-edits text (vector (lsp-edit 0 0 0 "a") (lsp-edit 0 0 0 "b")) :utf-16))
        "inserts at one point keep the order they came in")
    (is (search "overlap" (refusal-text lsp:lsp-error
                            (nodecode-lsp::apply-text-edits text (vector (lsp-edit 0 0 5 "x") (lsp-edit 0 2 4 "y"))
                                                            :utf-16))))
    (is (search "snippet" (refusal-text lsp:lsp-error
                            (nodecode-lsp::apply-text-edits text (vector (lsp-edit 0 0 1 nil)) :utf-16))))))

(deftest lsp-cell-text-reads-a-workspace-edit ()
  (let ((uri (nodecode-lsp::path-uri (format nil "/tmp/a b/~c.fk" (code-char #xe9)))))
    (is (equal "file:///tmp/a%20b/%C3%A9.fk" uri))
    (is (equal (format nil "/tmp/a b/~c.fk" (code-char #xe9)) (nodecode-lsp::uri-path uri)) "and back")
    (is (null (nodecode-lsp::uri-path "untitled:1")))
    (let ((files (nodecode-lsp::workspace-edit-files
                  (nlk:json-object "changes" (nlk:make-json-object uri (vector (lsp-edit 0 0 1 "x")))
                                   "documentChanges" (vector (nlk:json-object
                                                              "textDocument" (nlk:json-object "uri" uri "version" :null)
                                                              "edits" (vector (lsp-edit 1 0 1 "y"))))))))
      (is (= 1 (length files)) "one file, its edits gathered")
      (is (= 2 (length (cdr (first files))))))
    (is (search "would also create" (refusal-text lsp:lsp-error
                                      (nodecode-lsp::workspace-edit-files
                                       (nlk:json-object "documentChanges"
                                                        (vector (nlk:json-object "kind" "create" "uri" uri)))))))))

;;; --- which server a file gets ----------------------------------------------------------

(deftest lsp-cell-servers-merge-the-section-over-omps ()
  (let ((specs (nodecode-lsp::configured-specs
                (nlk:json-object "pyright" (nlk:json-object "disabled" t)
                                 "mine" (nlk:json-object "command" "my-ls" "file_types" (vector "xyz")
                                                         "root_markers" (vector ".git"))))))
    (is (= 56 (length specs)) "omp's 55 and one of the operator's")
    (is (nodecode-lsp::spec-disabled (find "pyright" specs :key #'nodecode-lsp::spec-name :test #'string=)))
    (is (equal '("--stdio") (nodecode-lsp::spec-args (find "pyright" specs :key #'nodecode-lsp::spec-name
                                                                           :test #'string=)))
        "an override keeps what it does not name")
    (is (equal '("mine") (mapcar #'nodecode-lsp::spec-name (nodecode-lsp::candidate-specs "/p/a.xyz" specs))))
    (is (equal '("rust-analyzer") (mapcar #'nodecode-lsp::spec-name (nodecode-lsp::candidate-specs "/p/a.rs" specs))))
    (is (equal "ruff" (nodecode-lsp::spec-name
                       (car (last (nodecode-lsp::candidate-specs "/p/a.py" specs)))))
        "a linter comes after the type checkers")
    (is (equal "dockerls" (nodecode-lsp::spec-name
                           (first (nodecode-lsp::candidate-specs "/p/Dockerfile" specs))))
        "a file type may be a whole name")
    (is (equal "python" (nodecode-lsp::language-id (first specs) "/p/a.py")))))

(deftest lsp-cell-servers-root-at-the-outermost-marker-inside-the-repository ()
  (with-lsp-project (root ("pkg/fake.toml" "") ("pkg/src/a.fk" "") ("solo/x.sln" ""))
    (let ((file (lsp-file root "pkg/src/a.fk")))
      (is (equal root (nodecode-lsp::find-root file '("fake.toml") root)) "inside the repository: the outermost")
      (is (equal (lsp-file root "pkg/") (nodecode-lsp::find-root file '("fake.toml") nil))
          "outside one: the nearest")
      (is (null (nodecode-lsp::find-root file '("absent.toml") root)))
      (is (equal (lsp-file root "solo/") (nodecode-lsp::find-root (lsp-file root "solo/y.cs") '("*.sln") nil))
          "a marker may be a pattern"))))

;;; --- a running server ---------------------------------------------------------------------

(deftest lsp-cell-diagnostics-reports-each-file ()
  (with-lsp-project (root ("a.fk" (format nil "fine~%x = ERROR here~%WARN~%")) ("b.fk" "clean"))
    (with-lsp () ()
      (let ((text (lsp:diagnostics (lsp-file root "a.fk"))))
        (is (search "1 error, 1 warning:" text) text)
        (is (search (format nil "~aa.fk:2:5 [error] [fake] found ERROR~%second line of the message (F1)" root) text)
            "omp's line: path, line, column, severity, source, message, code")
        (is (< (search "[error]" text) (search "[warning]" text)) "errors first"))
      (is (equal "OK" (lsp:diagnostics (lsp-file root "b.fk"))))
      (let ((text (lsp:diagnostics (list (lsp-file root "a.fk") (lsp-file root "b.fk")))))
        (is (search (format nil "~ab.fk: OK" root) text) "several files: a section each"))
      (is (search "no language server is configured for .txt files"
                  (progn (write-temp-file (uiop:parse-native-namestring (lsp-file root "n.txt")) "")
                         (lsp:diagnostics (lsp-file root "n.txt")))))
      (is (search "does not exist" (refusal-text lsp:lsp-error (lsp:diagnostics (lsp-file root "gone.fk"))))))))

(deftest lsp-cell-diagnostics-take-only-the-version-sent ()
  (with-lsp-project (root ("a.fk" "ERROR"))
    (with-lsp ("--stale") ()
      (is (search "1 error" (lsp:diagnostics (lsp-file root "a.fk"))))
      (write-temp-file (uiop:parse-native-namestring (lsp-file root "a.fk")) "fixed")
      (is (equal "OK" (lsp:diagnostics (lsp-file root "a.fk")))
          "a publish about the previous version is not the answer"))))

(deftest lsp-cell-diagnostics-pull-and-unversioned ()
  (with-lsp-project (root ("a.fk" "ERROR"))
    (with-lsp ("--pull") ()
      (is (search "1 error" (lsp:diagnostics (lsp-file root "a.fk"))) "a server that serves pulls is asked")))
  (with-lsp-project (root ("a.fk" "ERROR"))
    (with-lsp ("--no-version") ()
      (is (search "1 error" (lsp:diagnostics (lsp-file root "a.fk"))) "an unversioned publish that stands"))))

(deftest lsp-cell-server-lifecycle ()
  (with-lsp-project (root ("a.fk" "ERROR"))
    (with-lsp () ()
      (lsp:diagnostics (lsp-file root "a.fk"))
      (let ((pid (lsp-pid "fake"))
            (server (lsp-server "fake")))
        (is (lsp-alive-p pid) "a server starts on first use")
        (is (search (format nil "fake in ~a: ready, pid ~d, 1 open" root pid) (lsp:status)))
        (is (search "fake server started" (nlk:read-text (nodecode-lsp::srv-log server)))
            "its stderr goes to its log")
        (setf (nodecode-lsp::srv-used server) (- (get-internal-real-time) (* 3600 internal-time-units-per-second)))
        (nodecode-lsp::reap-idle)
        (is (not (lsp-alive-p pid)) "an idle server is stopped and reaped")
        (is (null (lsp-server "fake")))
        (lsp:diagnostics (lsp-file root "a.fk"))
        (let ((again (lsp-pid "fake")))
          (is (and again (/= again pid)) "and starts again on its next use")
          (funcall stop)
          (setf stop nil)
          (is (not (lsp-alive-p again)) "a stopped cell leaves no server behind"))))))

(deftest lsp-cell-server-failures-say-why ()
  (with-lsp-project (root ("a.fk" "ERROR"))
    (with-lsp ("--fail-init") ()
      (let ((text (lsp:diagnostics (lsp-file root "a.fk"))))
        (is (search "lsp: fake is not running: " text) text)
        (is (search "boom: the fake server will not start" text) "with what it printed"))
      (is (search "failed" (lsp:status)))))
  (with-lsp-project (root ("a.fk" "ERROR"))
    (with-lsp () ()
      (is (search "stopped while answering"
                  (refusal-text lsp:lsp-error (lsp:request "fake" "fake/crash" :path (lsp-file root "a.fk")))))
      (is (await (:timeout 3) (search "exited with status 3" (lsp:status))) "a crash is noticed")
      (setf (nodecode-lsp::srv-failed-at (lsp-server "fake"))
            (- (get-internal-real-time) (* 60 internal-time-units-per-second)))
      (is (search "1 error" (lsp:diagnostics (lsp-file root "a.fk"))) "and the next use respawns it"))))

;;; --- the verbs ------------------------------------------------------------------------------

(deftest lsp-cell-navigates-and-renames ()
  (with-lsp-project (root ("a.fk" (format nil "def parse(x)~%use = parse(1)~%"))
                          ("b.fk" (format nil "other = parse(2)~%")))
    (with-lsp () ()
      (let ((a (lsp-file root "a.fk")) (b (lsp-file root "b.fk")))
        (lsp:diagnostics b)
        (is (search (format nil "~a:1:5  def parse(x)" a) (lsp:definition a "parse" :line 2)))
        (is (search "found no definition of use" (lsp:definition a "use")))
        (let ((text (lsp:references a "parse")))
          (is (search "3 references:" text) text)
          (is (search (format nil "~a:1:9  other = parse(2)" b) text)))
        (is (equal "word `parse`" (lsp:hover a "parse")))
        (is (equal "parse (function) :1" (lsp:symbols a)))
        (is (search (format nil "parse (function)  ~a:1:5" a) (lsp:symbols a :query "PAR")))
        (let ((preview (lsp:rename a "parse" "parse_all" :apply nil)))
          (is (search "would make 3 edits in 2 files" preview) preview)
          (is (search (format nil "~a: lines 1, 2" a) preview)))
        (is (equal (format nil "def parse(x)~%use = parse(1)~%") (nlk:read-text a)) "a preview writes nothing")
        (is (search "renamed parse to parse_all: 3 edits in 2 files" (lsp:rename a "parse" "parse_all")))
        (is (equal (format nil "def parse_all(x)~%use = parse_all(1)~%") (nlk:read-text a)))
        (is (equal (format nil "other = parse_all(2)~%") (nlk:read-text b)))
        (is (search (format nil "~a:1:5  def parse_all(x)" a) (lsp:definition b "parse_all"))
            "the server was told the files changed")))))

(deftest lsp-cell-positions-cross-in-either-encoding ()
  (dolist (flags '(() ("--utf8")))
    (with-lsp-project (root ("a.fk" (format nil "def parse(x)~%~c~c = parse(1)~%" (code-char #x1f980) (code-char #x4e16))))
      (with-cell-stop ((lsp-start "servers" (apply #'lsp-servers flags)))
        (is (search ":1:5  def parse(x)" (lsp:definition (lsp-file root "a.fk") "parse" :line 2))
            (format nil "the position after an emoji, ~:[UTF-16~;UTF-8~]" flags))
        (is (eq (if flags :utf-8 :utf-16) (nodecode-lsp::srv-encoding (lsp-server "fake"))))))))

(deftest lsp-cell-rename-refuses-file-operations ()
  (with-lsp-project (root ("a.fk" "def parse(x)"))
    (with-lsp ("--resource-op") ()
      (is (search "would also create" (refusal-text lsp:lsp-error (lsp:rename (lsp-file root "a.fk") "parse" "p2"))))
      (is (equal "def parse(x)" (nlk:read-text (lsp-file root "a.fk")))))))

(deftest lsp-cell-request-reaches-any-method ()
  (with-lsp-project (root ("a.fk" "def parse(x)"))
    (with-lsp () ()
      (let ((a (lsp-file root "a.fk")))
        (is (search "\"textDocument\"" (lsp:request "fake" "fake/echo" :path a
                                                                       :params '(:text-document (:uri "x"))))
            "a plist's kebab-case keys go out camelCase")
        (is (search "\"x\"" (lsp:request "fake" "fake/echo" :params "{\"y\": \"x\"}")) "a JSON string as it is")
        (let ((text (lsp:request "fake" "fake/collide")))
          (is (search "\"depth\"" text) "the server's own request, under an id of ours, was answered")
          (is (search "-32601" text) "a method the cell does not serve is refused as such"))
        (is (search "true"
                    (lsp:request "fake" "fake/edit"
                                 :params (format nil "{\"changes\": {~s: [{\"range\": {\"start\": {\"line\": 0, \"character\": 4}, \"end\": {\"line\": 0, \"character\": 9}}, \"newText\": \"p\"}]}}"
                                                 (nodecode-lsp::path-uri a)))))
        (is (equal "def p(x)" (nlk:read-text a)) "workspace/applyEdit writes the file")
        (is (search "no gopls is running" (refusal-text lsp:lsp-error (lsp:request "gopls" "x"))))))))

(deftest lsp-cell-verbs-stamp-what-they-read ()
  (is-verb-receipts (nodecode-lsp::*lsp* (lsp:diagnostics "src/a.rs") (lsp:definition "src/b.rs" "parse"))
    :receipts (("lsp:diagnostics" . "src/a.rs") ("lsp:definition" . "src/b.rs"))
    :source "(progn (lsp:diagnostics \"src/a.rs\") (lsp:definition \"src/b.rs\" \"parse\"))"
    :groups "the reads make the snippet a routine one"
    :title "Read 2 files")
  (let ((nle::*executing-tool-call-id* "c1")
        (nle::*tool-result-metadata* nil)
        (nodecode-lsp::*lsp* nil))
    (ignore-errors (lsp:rename "src/a.rs" "parse" "p"))
    (is (equal "edit" (nlk:json-value (aref (nlk:json-array nle::*tool-result-metadata* "calls") 0)
                                      :string "family"))
        "a rename is an edit")))

;;; --- the write path ------------------------------------------------------------------------

(defun lsp-eval (session form &key (yield-ms 10000))
  "FORM run as the eval tool runs it, through the :tool chain, for SESSION.
=> (values TEXT METADATA)."
  (let ((nle::*live-turn* (nle::%make-live-turn :session-id session :turn-id "t1"))
        (nle::*executing-tool-call-id* "c1")
        (nle::*tool-result-metadata* nil))
    (values (nle::call-hooked :tool (list :name "eval" :arguments (nlk:json-object "form" form) :call-id "c1")
                              (lambda (op)
                                (declare (ignore op))
                                (values (nle::background-capable-eval form yield-ms))))
            nle::*tool-result-metadata*)))

(defun lsp-write-form (path text &optional (then ""))
  (format nil "(progn ~a(nle::write-file ~s ~s))" then path text))

(defun lsp-recorded (session)
  (bt2:with-lock-held (nodecode-lsp::*written-lock*)
    (gethash session nodecode-lsp::*written*)))

(deftest lsp-cell-a-write-earns-its-diagnostics ()
  (with-lsp-project (root)
    (with-lsp () ()
      (let* ((a (lsp-file root "a.fk"))
             (text (lsp-eval "s1" (lsp-write-form a (format nil "ok~%ERROR one~%WARN two~%ERROR three")))))
        (is (search "wrote " text) "the eval's own text first")
        (is (search (format nil "LSP diagnostics (2 errors, 1 warning):~%~a:2:1 [error] [fake] found ERROR (F1)" a) text)
            text)
        (is (not (search "second line" text)) "a message's first line only")
        (is (null (lsp-recorded "s1")) "the record is drained")
        (let ((clean (lsp-eval "s1" (lsp-write-form a "fine"))))
          (is (not (search "LSP" clean)) "a clean file adds nothing"))
        (is (not (search "LSP" (lsp-eval "s1" "(+ 1 2)"))) "an eval that wrote nothing adds nothing")))))

(deftest lsp-cell-an-empty-answer-before-the-project-loads-is-asked-again ()
  ;; rust-analyzer answers a pull at once with nothing while it loads; taken
  ;; for the answer, a cold first write read clean whatever it held.
  (with-lsp-project (root)
    (with-lsp ("--pull" "--empty-while-loading" "--progress" "800") ()
      (is (search "LSP diagnostics (1 error):" (lsp-eval "s1" (lsp-write-form (lsp-file root "a.fk") "ERROR")))
          "loaded when its progress run ends")))
  (with-lsp-project (root)
    (with-lsp ("--pull" "--empty-while-loading" "--status" "2500") ("wait_ms" 5000)
      (is (search "LSP diagnostics (1 error):" (lsp-eval "s1" (lsp-write-form (lsp-file root "a.fk") "ERROR")))
          "or when its status says it is quiescent, however long it stayed quiet"))))

(deftest lsp-cell-a-slow-server-says-still-checking ()
  (with-lsp-project (root)
    (with-lsp ("--silent") ("wait_ms" 300)
      (let ((text (lsp-eval "s1" (lsp-write-form (lsp-file root "a.fk") "ERROR"))))
        (is (search (format nil "lsp: fake still checking ~aa.fk; (lsp:diagnostics ~s) for the result"
                            root (lsp-file root "a.fk"))
                    text)
            text)))))

(deftest lsp-cell-a-backgrounded-eval-leaves-its-writes-for-the-wake ()
  (with-lsp-project (root)
    (with-lsp () ()
      (let ((a (lsp-file root "a.fk")))
        (multiple-value-bind (text metadata)
            (lsp-eval "s1" (lsp-write-form a "ERROR" "(sleep 0.6) ") :yield-ms 100)
          (is (search "\"running\"" text) "the running envelope")
          (is (not (search "LSP" text)) "is left alone")
          (let ((id (nlk:json-value metadata :integer "background_shell" "shell_session_id")))
            (is (await (:timeout 5) (lsp-recorded "s1")) "the write lands in the record")
            (sleep 0.2)
            (let ((wake (lsp-eval "s1" (format nil "(nle::eval-status ~d)" id))))
              (is (search "LSP diagnostics (1 error):" wake) wake))))))))

(deftest lsp-cell-the-write-path-fails-open ()
  (with-lsp-project (root ("a.fk" "file, not a folder"))
    (with-lsp () ()
      (let ((a (lsp-file root "a.fk")))
        (with-stubbed-fdefinition (nodecode-lsp::write-block (session) (error "the block broke for ~a" session))
          (let ((text (lsp-eval "s1" (lsp-write-form a "ERROR" "(identity \"done\") "))))
            (is (search "wrote " text))
            (is (not (search "LSP" text)) "an error in the hook leaves the eval's text as it was")
            (is (not (search "broke" text)))))
        (nodecode-lsp::forget-written)
        (lsp-eval "s1" (lsp-write-form (lsp-file root "a.fk/b.fk") "ERROR"))
        (is (null (lsp-recorded "s1")) "a write that failed records nothing")
        (let ((nle::*live-turn* (nle::%make-live-turn :session-id "s2" :turn-id "t1")))
          (nle::write-file a "ERROR"))
        (is (not (search "LSP" (lsp-eval "s1" "(+ 1 2)"))) "another session's writes are not this one's")
        (is (equal (list a) (lsp-recorded "s2")) "and stay for it")
        (lsp-eval "s1" (lsp-write-form (lsp-file root "notes.txt") "ERROR"))
        (is (null (lsp-recorded "s1")) "a file no server serves records nothing")))))

(deftest lsp-cell-diagnostics-on-write-can-be-off ()
  (with-lsp-project (root)
    (with-lsp () ("diagnostics_on_write" nil)
      (is (not (search "LSP" (lsp-eval "s1" (lsp-write-form (lsp-file root "a.fk") "ERROR")))))
      (is (null (lsp-recorded "s1"))))))
