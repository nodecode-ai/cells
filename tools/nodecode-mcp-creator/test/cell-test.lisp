;;;; cell-test.lisp --- the cell's lifecycle, its nine tools over a scripted
;;;; server, and the packages it builds.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches the network or starts a process: *CONNECT* answers a
;;;; catalog, and every folder written is under a temporary directory.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(deftest mcp-creator-cell-starts-and-stops-cleanly ()
  (with-temp-directory (directory "mc-home")
    (let ((config (mc-config "data_dir" (namestring directory) "log_calls" nil)))
      (with-creator (config)
        (is (not (null (nle::find-tool "mcp_creator_introspect"))) "the introspect tool is registered")
        (is (not (null (nle::find-tool "mcp_creator_review"))) "and the last one")
        (is (mc-command-named "mcp-creator") "/mcp-creator is in the one slash catalog")
        (is (search "a skill a later session reads" nodecode-mcp-creator::+primer+) "the manual is on its topic")
        (is (not (null (nle::find-tool "mcp_creator_servers"))) "the first tool is registered"))
      (is (null (nle::find-tool "mcp_creator_introspect")) "stop takes the tools back out")
      (is (null (mc-command-named "mcp-creator")) "and the command")
      (nodecode-mcp-creator:stop-cell)
      (nodecode-mcp-creator:stop-cell)
      (is t "stopping twice does nothing the second time"))))

(deftest mcp-creator-cell-refuses-a-setting-it-cannot-use ()
  (with-temp-directory (directory "mc-refuse")
    (nodecode-mcp-creator:stop-cell)
    (signals-error nlk:config-refusal
                   (nodecode-mcp-creator:start-cell
                    (mc-config "data_dir" (namestring directory) "timeout_seconds" 0)))
    (is (null nodecode-mcp-creator::*mcp-creator*) "a refused start leaves no settings behind")))

;;; --- the reading tools -----------------------------------------------------------

(deftest mcp-creator-cell-lists-and-converts-a-configured-server ()
  (with-temp-directory (directory "mc-servers")
    (with-creator ((mc-config "data_dir" (namestring directory) "log_calls" nil))
      (let* ((listed (nodecode-mcp-creator:run-servers
                      (nodecode-mcp-creator:decode-servers-arguments (nlk:make-json-object))))
             (text (nodecode-mcp-creator::json-string listed)))
        (is (search "\"files\"" text) "the stdio server is listed")
        (is (search "\"docs\"" text) "and the HTTP one")
        (is (search "FILES_TOKEN" text) "an env key is named")
        (is (not (search "sk-live-abcdef1234567890abcdef" text)) "and its secret value is never shown"))
      (let* ((converted (nodecode-mcp-creator:run-convert
                         (nodecode-mcp-creator:decode-convert-arguments
                          (nlk:json-object "server" "files"))))
             (text (nodecode-mcp-creator::json-string converted)))
        (is (search "${FILES_TOKEN}" text) "a literal secret becomes a reference")
        (is (search "not pinned" text) "an unpinned npx launcher is a warning")
        (is (search "absolute path" text) "and so is a machine path in an argument")))))

(deftest mcp-creator-cell-introspects-scaffolds-validates-installs-and-drifts ()
  (with-temp-directory (directory "mc-build")
    (let* ((cells (merge-pathnames "cells/" directory))
           (config (mc-config "data_dir" (namestring directory) "install_dir" (namestring cells)
                               "log_calls" nil))
           (nodecode-mcp-creator::*connect*
            (mc-connect (list (mc-tool "read_file" :description "read one file")
                              (mc-tool "list_dir" :description "list a folder")))))
      (with-creator (config)
        ;; introspect: a snapshot on disk, a fingerprint, the tools
        (let* ((snapshot (nodecode-mcp-creator:run-introspect
                          (nodecode-mcp-creator:decode-introspect-arguments
                           (nlk:json-object "server" "files"))))
               (text (nodecode-mcp-creator::json-string snapshot)))
          (is (= 2 (gethash "tool_count" (nodecode-mcp-creator::decode text))) "two tools were listed, to a number")
          (is (search "read_file" text) "and named")
          (is (probe-file (nodecode-mcp-creator::snapshot-path "files")) "the snapshot is on disk"))
        ;; scaffold: a knowledge cell, and the skill proves every tool
        (let* ((built (nodecode-mcp-creator:run-scaffold
                       (nodecode-mcp-creator:decode-scaffold-arguments
                        (nlk:json-object "plugin_name" "files-mcp" "server" "files"
                                         "description" "The filesystem server's tools, as a skill."
                                         "build" "local")))))
          (is (search "\"files-mcp\"" (nodecode-mcp-creator::json-string built)) "the package was written")
          (let* ((folder (merge-pathnames "files-mcp/" (nodecode-mcp-creator::builds-root)))
                 (skill (uiop:read-file-string (merge-pathnames "skill.lisp" folder))))
            (is (search "(define-skill files-mcp" skill) "the skill is the package's centre")
            (is (search "`read_file`" skill) "its table names every tool")
            (is (probe-file (merge-pathnames "nodecode-files-mcp.asd" folder)) "the cell's system is there"))
          ;; validate: the build is whole
          (multiple-value-bind (problems warnings)
              (nodecode-mcp-creator::package-problems
               (merge-pathnames "files-mcp/" (nodecode-mcp-creator::builds-root)))
            (declare (ignore warnings))
            (is (null problems) "a fresh build validates clean"))
          ;; install: copied into the cells folder, and refused twice over
          (let ((installed (nodecode-mcp-creator::install-package
                            (merge-pathnames "files-mcp/" (nodecode-mcp-creator::builds-root)))))
            (is (probe-file (merge-pathnames "skill.lisp" (merge-pathnames "files-mcp/" cells))) "the build is in the cells folder")
            (is (gethash "installed" (nodecode-mcp-creator::decode
                                              (nodecode-mcp-creator::json-string installed))) "and says so"))
          ;; drift: unchanged against itself, additive against a bigger catalog
          (let ((same (nodecode-mcp-creator:run-drift
                       (nodecode-mcp-creator:decode-drift-arguments
                        (nlk:json-object "server" "files")))))
            (is (search "\"unchanged\"" (nodecode-mcp-creator::json-string same)) "the same catalog drifts nowhere"))
          (let ((nodecode-mcp-creator::*connect*
                 (mc-connect (list (mc-tool "read_file" :description "read one file")
                                   (mc-tool "list_dir" :description "list a folder")
                                   (mc-tool "write_file" :description "write one file")))))
            (let ((drift (nodecode-mcp-creator:run-drift
                          (nodecode-mcp-creator:decode-drift-arguments
                           (nlk:json-object "server" "files")))))
              (is (search "\"additive\"" (nodecode-mcp-creator::json-string drift)) "a new tool is additive, not a recompile")
              (is (search "write_file" (nodecode-mcp-creator::json-string drift)) "and it is named"))))
        ;; review: no drift, and the reasons say why
        (let ((review (nodecode-mcp-creator:run-review
                       (nodecode-mcp-creator:decode-review-arguments
                        (nlk:json-object "plugin" "files-mcp")))))
          (is (search "files-mcp" (nodecode-mcp-creator::json-string review)) "a build is reviewable")
          (is (search "\"reasons\"" (nodecode-mcp-creator::json-string review)) "and says why a refresh is or is not worth it"))))))

;;; --- mining and the call log --------------------------------------------------------

(deftest mcp-creator-cell-mines-the-calls-the-model-made ()
  (with-temp-directory (directory "mc-mine")
    (with-creator ((mc-config "data_dir" (namestring directory) "log_calls" t))
      (is (equal '("files/read_file" "files/list_dir")
                 (nodecode-mcp-creator::mcp-calls-in
                  "(progn (mcp:files/read_file :path \"x\") (mcp:files/list_dir :path \".\"))")) "an eval form's MCP calls are read, in order")
      (nodecode-mcp-creator::note-call "files/read_file" :ok t :detail "content")
      (nodecode-mcp-creator::note-call "files/read_file" :ok nil
                                        :detail "ERROR: no such file /home/someone/secret")
      (let* ((mine (nodecode-mcp-creator:run-mine
                    (nodecode-mcp-creator:decode-mine-arguments
                     (nlk:json-object "server" "files"))))
             (text (nodecode-mcp-creator::json-string mine)))
        (let ((folded (nodecode-mcp-creator::decode text)))
          (is (= 2 (gethash "calls" folded)) "both calls were counted")
          (is (= 1 (gethash "errors" folded)) "and the failure"))
        (is (search "no such file" text) "with its sample")))
    ;; the hook records a call it observed, and the log is the tool's source
    (with-creator ((mc-config "data_dir" (namestring directory) "log_calls" t))
      (nodecode-mcp-creator::observe-tool
       (list :name "eval" :arguments (nlk:json-object "form" "(mcp:files/list_dir :path \".\")")
             :call-id "c1")
       (lambda (op) (declare (ignore op)) "listed"))
      (is (search "list_dir" (nodecode-mcp-creator::json-string
                               (nodecode-mcp-creator:run-mine
                                (nodecode-mcp-creator:decode-mine-arguments (nlk:make-json-object))))) "the hook's line is what mining reads"))))

;;; --- refusals ---------------------------------------------------------------------

(deftest mcp-creator-cell-refuses-what-it-cannot-do ()
  (with-temp-directory (directory "mc-refuse-2")
    (with-creator ((mc-config "data_dir" (namestring directory) "log_calls" nil))
      (let ((result (mc-tool-text "mcp_creator_introspect" "{\"server\": \"nope\"}")))
        (is (search "ERROR:" (nle::message-content result)) "an unknown server is a refusal the model reads")
        (is (search "nope" (nle::message-content result)) "and it names the server asked for"))
      (let ((result (mc-tool-text "mcp_creator_scaffold"
                                   "{\"plugin_name\": \"nothing\", \"server\": \"files\"}")))
        (is (search "introspect first" (nle::message-content result)) "a build with no snapshot says what to run first"))))
)