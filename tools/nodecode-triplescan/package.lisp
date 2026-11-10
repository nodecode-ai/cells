;;;; package.lisp --- the TRIPLESCAN package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed TRIPLESCAN so an eval form reads
;;;; (triplescan:audit "/a/folder") or (triplescan:report "~/project"). Every
;;;; exported verb answers a STRING and every refusal is TRIPLESCAN-ERROR,
;;;; rendered by the eval snippet as "ERROR: TRIPLESCAN-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `triplescan' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :KEY).

(defpackage #:nodecode-triplescan
  (:use #:cl)
  (:nicknames #:triplescan)
  (:export
   ;; verbs, every one a string
   #:audit                 ; one folder, three scanners, the result as JSON
   #:report                ; the same audit in words
   ;; data, so a layer can reword it
   #:*standing* #:*primer* #:+scanners+
   ;; the one condition
   #:triplescan-error))
