;;;; package.lisp --- one package, named after the system, nicknamed for the command.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-network-xray
  (:use #:cl)
  (:nicknames #:network-xray)
  (:export #:probe #:import-bundle #:compare-records #:history
           #:parse-target #:collect #:seal #:canonical-json #:overall
           #:compare-records-by-id
           ;; the seams a layer or a test rebinds
           #:*resolver* #:*connector* #:*handshaker* #:*head* #:*closer*
           #:network-xray-error #:network-xray-error-detail))
