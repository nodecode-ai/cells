;;;; package.lisp --- the cell's one package, named after the system.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-photofresco
  (:use #:cl)
  (:nicknames #:photofresco)
  (:export #:photofresco-error
           #:connect
           #:disconnect
           #:status
           #:documents
           #:operations
           #:execute
           #:call
           #:open-file
           #:export-file
           #:+primer+))
