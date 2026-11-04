;;;; package.lisp --- the terminal cell's package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-terminal
  (:use #:cl)
  (:export #:open-terminal #:find-terminal #:list-terminals #:close-terminal
           #:terminal-write #:terminal-resize #:terminal-attach #:terminal-detach
           #:terminal-id #:terminal-pid #:terminal-exit #:terminal-cwd
           #:terminal-cols #:terminal-rows #:terminal-error))
