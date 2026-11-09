;;;; cell-test.lisp --- the cartograph cell against a repository on disk.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every repository here is a temp directory this file writes, so the slice
;;;; needs no network, no key and no file of the operator's: one call per tool,
;;;; the task that selects, the refusals, and the slash command. What is
;;;; proved is the reading of a repository -- the counts, the resolved import
;;;; edges, the ranking and the excerpts -- and that a call which cannot be
;;;; honoured says why instead of answering nothing.

(in-package #:nodecode.test)

(define-test-slice "cartograph" "CARTOGRAPH-CELL-" :start nodecode-cartograph:start-cell)

(define-cell-lifecycle-tests "cartograph"
  (:help :cartograph)
  (:command "cartograph")
  (:running (is (nle::find-tool "cartograph_analyze") "the map tool is registered")
            (is (nle::find-tool "cartograph_context") "and the excerpt tool")
            (is (= 2000 (nodecode-cartograph::setting :max-files)) "max_files defaults to 2000")
            (is (= 40000 (nodecode-cartograph::setting :max-output-chars)) "and the answer to 40000 characters")
            (is (eq :any (nodecode-cartograph::setting :allowed-roots)) "an absent allowed_roots opens every directory"))
  (:stopped (is (null (nle::find-tool "cartograph_context")) "stop unregisters the tools"))
  (:refused ("max_files" 1) ("max_output_chars" 10) ("source_chars" 10))
  (:idle nodecode-cartograph:cartograph-error (cartograph:analyze "/")))

;;; --- fixtures ---------------------------------------------------------------------

(defun cartograph-write (root relative text)
  "Write TEXT at RELATIVE under ROOT, making its directories. => the file."
  (let ((file (merge-pathnames relative (uiop:ensure-directory-pathname root))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string text out))
    file))

(defun cartograph-repository (root)
  "A small repository under ROOT: Python and JavaScript, one module three
other files import, a test of it, and a README the cell does not read.
=> ROOT as a directory namestring."
  (cartograph-write root "src/util.py"
                    "def helper(value):
    return value + 1
")
  (cartograph-write root "src/core.py"
                    "from util import helper

def run(thing):
    return helper(thing)
")
  (cartograph-write root "src/app.py"
                    "from core import run

print(run(1))
")
  (cartograph-write root "tests/test_core.py"
                    "from src.core import run

def test_run():
    assert run(1) == 2
")
  (cartograph-write root "web/util.js"
                    "export function add(one, two) { return one + two; }
")
  (cartograph-write root "web/index.js"
                    "import { add } from \"./util.js\";

console.log(add(1, 2));
")
  (cartograph-write root "README.md" "# a repository cartograph does not read
")
  (namestring (uiop:ensure-directory-pathname root)))

(defun cartograph-arguments (&rest pairs)
  "PAIRS as the JSON object string a tool call carries."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun cartograph-tool (name &rest pairs)
  "The registered tool NAME called with PAIRS the way the engine calls it:
the arguments as JSON, the record materialized at the body."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry)
             (nlk:contract-decode (second entry)
                                   (nlk:decode-json (apply #'cartograph-arguments pairs))))))

;;; --- the map ----------------------------------------------------------------------

(deftest cartograph-cell-analyze-maps-the-repository-it-is-pointed-at ()
  (with-temp-directory (root "cartograph-map")
    (let ((repository (cartograph-repository root)))
      (with-cell-stop ((cartograph-start))
        (let ((answer (cartograph-tool "cartograph_analyze" "repo" repository)))
          (is (search "6 code files" answer) "every code file is counted, and the README is not")
          (is (search "4 import edges" answer) "the imports inside the repository are resolved")
          (is (search "src/core.py" answer) "a ranked file is named")
          (is (search "dependency hubs" answer) "the hubs are listed")
          (is (search "static analysis" answer) "and the answer says what it did"))))))

(deftest cartograph-cell-the-report-is-the-same-twice ()
  (with-temp-directory (root "cartograph-twice")
    (let ((repository (cartograph-repository root)))
      (with-cell-stop ((cartograph-start))
        (let ((one (cartograph-tool "cartograph_analyze" "repo" repository "top" 3)))
          (is (equal one (cartograph-tool "cartograph_analyze" "repo" repository "top" 3)) "one repository reads the same twice"))))))

(deftest cartograph-cell-a-package-specifier-finds-the-directory-it-names ()
  (with-temp-directory (root "cartograph-package")
    (cartograph-write root "pkg/util/__init__.py" "def helper():
    return 1
")
    (cartograph-write root "pkg/core.py" "from pkg.util import helper
")
    (with-cell-stop ((cartograph-start))
      (is (search "1 import edge" (cartograph-tool "cartograph_analyze" "repo" (namestring (uiop:ensure-directory-pathname root)))) "a dotted import finds the package under it"))))

;;; --- the excerpts -----------------------------------------------------------------

(deftest cartograph-cell-context-returns-the-source-a-task-names ()
  (with-temp-directory (root "cartograph-context")
    (let ((repository (cartograph-repository root)))
      (with-cell-stop ((cartograph-start))
        (let ((answer (cartograph-tool "cartograph_context" "repo" repository "task" "helper util")))
          (is (search "src/util.py:1-" answer) "the file the task names, with its line range")
          (is (search "def helper" answer) "and the source itself")
          (is (search "of 6 files" answer) "the selection is counted against the repository"))))))

(deftest cartograph-cell-a-task-nobody-matches-says-so ()
  (with-temp-directory (root "cartograph-nomatch")
    (let ((repository (cartograph-repository root)))
      (with-cell-stop ((cartograph-start))
        (let ((answer (cartograph-tool "cartograph_context" "repo" repository
                                       "task" "kubernetes frobnicator")))
          (is (search "no file in this repository answers" answer) "a task nothing names is answered as a miss"))))))

;;; --- the refusals -----------------------------------------------------------------

(deftest cartograph-cell-a-call-without-a-repository-is-refused-before-the-body ()
  (with-cell-stop ((cartograph-start))
    (multiple-value-bind (arguments error)
        (nle::tool-call-preflight "cartograph_analyze" "{\"top\": 3}"
                                   (nle::find-tool "cartograph_analyze"))
      (is (null arguments) "a call with no repo never reaches the body")
      (is (search "contract_refused" error) "and the refusal names the contract"))))

(deftest cartograph-cell-an-empty-allowed-roots-refuses-every-repository ()
  (with-temp-directory (root "cartograph-roots")
    (let ((repository (cartograph-repository root)))
      (with-cell-stop ((cartograph-start "allowed_roots" (vector)))
        (let ((answer (cartograph-tool "cartograph_analyze" "repo" repository)))
          (is (search "ERROR:" answer) "the call answers failed")
          (is (search "allowed_roots" answer) "and the refusal names the setting"))))))

(deftest cartograph-cell-a-repository-outside-allowed-roots-refuses ()
  (with-temp-directory (root "cartograph-outside")
    (with-temp-directory (other "cartograph-inside")
      (let ((repository (cartograph-repository root)))
        (with-cell-stop ((cartograph-start "allowed_roots" (vector (namestring other))))
          (let ((answer (cartograph-tool "cartograph_analyze" "repo" repository)))
            (is (search "outside allowed_roots" answer) "a repository under no configured root is refused")))))))

(deftest cartograph-cell-allowed-roots-lets-a-repository-under-one-through ()
  (with-temp-directory (root "cartograph-allowed")
    (let ((repository (cartograph-repository root)))
      (with-cell-stop ((cartograph-start "allowed_roots" (vector repository)))
        (is (search "6 code files" (cartograph-tool "cartograph_analyze" "repo" repository)) "a repository under a configured root is read")))))

(deftest cartograph-cell-a-file-where-a-directory-belongs-refuses ()
  (with-temp-directory (root "cartograph-file")
    (let ((file (cartograph-write root "one.py" "x = 1
")))
      (with-cell-stop ((cartograph-start))
        (is (search "not a directory" (cartograph-tool "cartograph_analyze" "repo" (namestring file))) "a file path is refused, and says which")
        (is (search "no directory at" (cartograph-tool "cartograph_analyze" "repo" "/no/such/repository")) "so is a path that is not there")))))

;;; --- the operator's command --------------------------------------------------------

(deftest cartograph-cell-the-slash-command-maps-and-selects ()
  (with-temp-directory (root "cartograph-slash")
    (let ((repository (cartograph-repository root)))
      (with-cell-stop ((cartograph-start))
        (is (search "6 code files" (nodecode-cartograph::run-slash repository nil)) "a typed path is mapped")
        (is (search "src/util.py" (nodecode-cartograph::run-slash (format nil "~a helper" repository) nil)) "a path and a task select the source")))))
