;;;; reference-test.lisp --- a header or env value that names its secret.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun mcp-secret-file (directory name text)
  "A file NAME under DIRECTORY holding TEXT; its native namestring."
  (let ((path (merge-pathnames name directory)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    (uiop:native-namestring path)))

(deftest mcp-cell-reference-parses-and-refuses-what-it-cannot-read ()
  (let ((section (mcp-section
                  "env" (mcp-entry "url" "https://h/mcp"
                                   "headers" (mcp-entry "Authorization" "Bearer ${MCP_TEST_TOKEN}"
                                                        "X-Key" "${env:MCP_TEST_TOKEN}"))
                  "file" (mcp-entry "command" "x" "env" (mcp-entry "K" "${file:~/.secrets/k}"))
                  "default" (mcp-entry "url" "https://h/mcp"
                                       "headers" (mcp-entry "Authorization" "Bearer ${MCP_TEST_TOKEN:-x}"))
                  "unclosed" (mcp-entry "url" "https://h/mcp"
                                        "headers" (mcp-entry "Authorization" "Bearer ${MCP_TEST_TOKEN"))
                  "relative" (mcp-entry "command" "x" "env" (mcp-entry "K" "${file:secrets/k}")))))
    (flet ((refusal (name) (mcp::server-spec-refusal (mcp-spec section name))))
      (is (null (refusal "env")) "${NAME} and ${env:NAME} are read")
      (is (null (refusal "file")) "${file:~/...} is read")
      (is (equal '(("Authorization" . "Bearer ${MCP_TEST_TOKEN}") ("X-Key" . "${env:MCP_TEST_TOKEN}")) (mcp::server-spec-headers (mcp-spec section "env"))) "the spec keeps the reference, never a value")
      (is (search "headers.Authorization" (refusal "default")) "a refusal names the header")
      (is (search "${MCP_TEST_TOKEN:-x}" (refusal "default")) "and the reference it cannot read")
      (is (search "unclosed" (refusal "unclosed")) "an unclosed ${ is refused")
      (is (search "absolute" (refusal "relative")) "a file is named from the root or ~/"))))

(deftest mcp-cell-reference-resolves-env-and-file ()
  (with-temp-directory (directory "mcp-references")
    (let ((file (mcp-secret-file directory "token" (format nil "  from-file~%")))
          (broken (mcp-secret-file directory "broken" (format nil "sec-alpha~%sec-beta~%")))
          (home (uiop:getenv "HOME")))
      (sb-posix:setenv "MCP_TEST_TOKEN" "from-env" 1)
      (sb-posix:unsetenv "MCP_TEST_UNSET")
      (is (equal "Bearer from-env" (mcp::resolve-references "Bearer ${MCP_TEST_TOKEN}" "headers.Authorization")) "${NAME} is the variable")
      (is (equal "from-env" (mcp::resolve-references "${env:MCP_TEST_TOKEN}" "headers.X")) "${env:NAME} the same")
      (is (equal "Bearer from-file" (mcp::resolve-references (format nil "Bearer ${file:~a}" file) "headers.Authorization")) "${file:PATH} is the file's text, trimmed")
      (is (equal "plain text" (mcp::resolve-references "plain text" "headers.X")) "a value without one is itself")
      (unwind-protect
           (progn (sb-posix:setenv "HOME" (uiop:native-namestring directory) 1)
                  (is (equal "from-file" (mcp::resolve-references "${file:~/token}" "env.K")) "~/ is the home"))
        (sb-posix:setenv "HOME" home 1))
      (let ((unset (refusal-text mcp::reference-unresolved (mcp::resolve-references "Bearer ${MCP_TEST_UNSET}" "headers.Authorization")))
            (missing (refusal-text mcp::reference-unresolved (mcp::resolve-references (format nil "${file:~a.absent}" file) "env.K")))
            (multiline (refusal-text mcp::reference-unresolved (mcp::resolve-references (format nil "${file:~a}" broken) "headers.X"))))
        (is (search "headers.Authorization: ${MCP_TEST_UNSET} is unset" unset) "an unset variable is named, with its header")
        (is (search "cannot read" missing) "an unreadable file is named")
        (is (search "line break" multiline) "a value that would split the header is refused")
        (is (null (search "sec-alpha" multiline)) "and never quoted")))))

(deftest mcp-cell-reference-env-reaches-the-child ()
  (with-temp-directory (directory "mcp-references")
    (let* ((file (mcp-secret-file directory "key" (format nil "file-key~%")))
           (spec (mcp-spec (mcp-section "s" (mcp-entry "command" "x"
                                                       "env" (mcp-entry "FROM_FILE" (format nil "${file:~a}" file)
                                                                        "FROM_ENV" "pre-${MCP_TEST_TOKEN}")))
                           "s")))
      (sb-posix:setenv "MCP_TEST_TOKEN" "env-key" 1)
      (let ((environment (mcp::child-environment spec '())))
        (is (member "FROM_FILE=file-key" environment :test #'string=) "a file reference resolves for the child")
        (is (member "FROM_ENV=pre-env-key" environment :test #'string=) "a variable reference too, beside literal text")))))

(deftest mcp-cell-reference-header-connects-and-is-never-shown ()
  (with-mcp-http-fixture (fixture :require-header '("authorization" . "Bearer tok-7f3a9"))
    (with-temp-directory (directory "mcp-references")
      (let ((file (mcp-secret-file directory "token" (format nil "tok-7f3a9~%"))))
        (sb-posix:setenv "MCP_TEST_TOKEN" "tok-7f3a9" 1)
        (sb-posix:unsetenv "MCP_TEST_UNSET")
        (with-mcp-cell ("remote" (mcp-http-entry fixture "headers" (mcp-entry "Authorization" "Bearer ${MCP_TEST_TOKEN}")))
          (is (eq :ready (mcp-wait-ready "remote")) "a header from the environment connects")
          (is (equal "echo: hi" (mcp:call "remote" "echo" :message "hi")) "and calls")
          (is (null (search "tok-7f3a9" (prin1-to-string (mcp-server "remote")))) "printing the server shows no resolved value")
          (is (null (search "tok-7f3a9" (mcp:status))) "nor does the status"))
        (with-mcp-cell ("remote" (mcp-http-entry fixture "headers" (mcp-entry "Authorization" (format nil "Bearer ${file:~a}" file))))
          (is (eq :ready (mcp-wait-ready "remote")) "a header from a file connects"))
        (let ((before (length (mcp-fixture-requests fixture))))
          (with-mcp-cell ("remote" (mcp-http-entry fixture "headers" (mcp-entry "Authorization" "Bearer ${MCP_TEST_UNSET}")))
            (is (eq :error (mcp-wait-ready "remote")) "an unset variable fails the connect")
            (is (search "${MCP_TEST_UNSET} is unset" (mcp::server-error-text (mcp-server "remote"))) "naming the variable")
            (is (= before (length (mcp-fixture-requests fixture))) "before any request is sent")))))))
