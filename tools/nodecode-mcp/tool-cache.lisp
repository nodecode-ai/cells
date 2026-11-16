;;;; tool-cache.lisp --- the catalog a server listed last, kept on disk.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every catalog a server publishes -- at connect, and again on a list
;;;; that follows tools/list_changed -- is kept beside its stderr log,
;;;; under the way the server is reached: TRANSPORT-TEXT, the command and
;;;; its args or the url and a header count, never a secret. The next
;;;; START-CELL reads it back: the functions and the (help :mcp) lines come
;;;; from the kept catalog, the server stands :IDLE with no process and no
;;;; connection, and its first call connects it inside that call's own
;;;; deadline. A session that calls no MCP tool starts no MCP server -- no
;;;; node or uv start, no package-registry lookup, no handshake.
;;;;
;;;; No file, an unreadable one, an older version or another way of
;;;; reaching the server is no catalog: that server connects at boot as
;;;; before, and its connect keeps the catalog for the next one. A catalog
;;;; gone stale costs its first call a fresh listing, which republishes
;;;; the functions and the primer; a tool the server dropped meanwhile
;;;; answers the server's own error once. `lazy': false, on the section
;;;; or on one entry, connects at boot whatever is kept.

(in-package #:nodecode-mcp)

(defparameter +tool-cache-version+ 1
  "The kept catalog's format; a file of another version is no catalog.")

(defun tool-cache-path (name)
  (merge-pathnames (format nil "~a.tools.json" name) *log-directory*))

(defun save-tools (spec tools)
  "Keep TOOLS as the catalog SPEC's server listed last. A failure warns and
keeps nothing: the next boot connects that server instead."
  (nlk:with-handlers ((error (condition)
                        (warn "mcp ~a: its catalog could not be kept: ~a" spec.name condition)
                        nil))
    (nlk:write-file-atomically
     (tool-cache-path spec.name)
     (nlk:encode-json-object
      (nlk:json-object "version" +tool-cache-version+
                       "reached" (transport-text spec)
                       "tools" (map 'vector (lambda (tool)
                                              (nlk:json-object "name" (getf tool :name)
                                                               "description" (getf tool :description)
                                                               "inputSchema" (getf tool :schema)))
                                    tools))))
    t))

(defun saved-tools (spec &aux (path (tool-cache-path spec.name)))
  "The catalog SPEC's server listed last, as tool plists; NIL when none is
kept for the way SPEC reaches it."
  (when (probe-file path)
    (nlk:with-handlers ((error (condition)
                          (warn "mcp ~a: its kept catalog is unreadable: ~a" spec.name condition)
                          nil))
      (let ((object (nlk:decode-json (uiop:read-file-string path))))
        (when (and (hash-table-p object)
                   (eql (nlk:json-value object :integer "version") +tool-cache-version+)
                   (equal (nlk:json-value object :string "reached") (transport-text spec)))
          (loop for entry across (or (nlk:json-array object "tools") #())
                for tool = (and (hash-table-p entry) (tool-plist entry))
                when tool collect tool))))))
