;;;; scan.lisp --- what is installed, what the catalog knows, and the one OSV call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An inventory is read off the disk, never by running a package manager:
;;;; every package.json under a node_modules folder, and every .dist-info
;;;; folder Python leaves beside an installed distribution. Each
;;;; (ecosystem, name, version) is then matched exactly against the catalog
;;;; below: a curated list of releases that were compromised in their own
;;;; right, with the incident each belongs to. Nothing is fuzzy and nothing
;;;; is a version range, so a match is a fact about exactly these bytes.
;;;;
;;;; The one network call is OSV.dev's batch endpoint, which asks advisory
;;;; counts for installed versions; it is a count, never a verdict, and the
;;;; report says so.

(in-package #:nodecode-bumblebee)

(defvar *http* #'nlk:http
  "The cell's one network call, NLK:HTTP; a test binds another.")

(defparameter *osv-batch-url* "https://api.osv.dev/v1/querybatch"
  "OSV.dev's batch query endpoint: one POST, one answer per query.")

;;; --- the catalog ----------------------------------------------------------------
;;; Each entry is one release that was compromised in its own right: the
;;; ecosystem, the package, the exact version, how bad, which incident, and
;;; one line a person can act on. Sources are the projects' own advisories and
;;; the public incident write-ups; this is a sample of what the original's
;;; catalogs carried (see README, "What is left out").

(defparameter *catalog*
  '((:ecosystem "npm" :name "event-stream" :version "3.3.6" :severity :critical
     :catalog "event-stream-2018"
     :why "pulled flatmap-stream, which read a wallet library and shipped a bitcoin stealer")
    (:ecosystem "npm" :name "flatmap-stream" :version "0.1.1" :severity :critical
     :catalog "event-stream-2018"
     :why "the payload package the event-stream hijack depended on")
    (:ecosystem "npm" :name "ua-parser-js" :version "0.7.29" :severity :critical
     :catalog "ua-parser-2021"
     :why "a hijacked release ran a cryptominer and read saved passwords")
    (:ecosystem "npm" :name "ua-parser-js" :version "1.0.0" :severity :critical
     :catalog "ua-parser-2021"
     :why "a hijacked release ran a cryptominer and read saved passwords")
    (:ecosystem "npm" :name "coa" :version "2.0.3" :severity :critical
     :catalog "coa-rc-2021"
     :why "a hijacked release ran a postinstall script that rewrote files")
    (:ecosystem "npm" :name "rc" :version "1.2.9" :severity :critical
     :catalog "coa-rc-2021"
     :why "a hijacked release ran a postinstall script that rewrote files")
    (:ecosystem "npm" :name "node-ipc" :version "10.1.1" :severity :medium
     :catalog "node-ipc-2022"
     :why "protestware that overwrote files on hosts geolocated in two countries")
    (:ecosystem "npm" :name "node-ipc" :version "11.0.0" :severity :medium
     :catalog "node-ipc-2022"
     :why "protestware that overwrote files on hosts geolocated in two countries")
    (:ecosystem "npm" :name "colors" :version "1.4.44" :severity :low
     :catalog "colors-faker-2022"
     :why "protestware: an endless loop instead of the library's output")
    (:ecosystem "npm" :name "faker" :version "5.5.3" :severity :low
     :catalog "colors-faker-2022"
     :why "protestware: an endless loop instead of the library's output")
    (:ecosystem "npm" :name "@solana/web3.js" :version "1.95.7" :severity :critical
     :catalog "solana-web3js-2024"
     :why "a hijacked release sent private keys to a third-party host")
    (:ecosystem "npm" :name "debug" :version "4.4.2" :severity :critical
     :catalog "npm-phishing-2025"
     :why "a release posted in a maintainer's name that ran a dropper on install")
    (:ecosystem "npm" :name "chalk" :version "5.6.1" :severity :critical
     :catalog "npm-phishing-2025"
     :why "a release posted in a maintainer's name that ran a dropper on install")
    (:ecosystem "npm" :name "ansi-styles" :version "6.2.2" :severity :critical
     :catalog "npm-phishing-2025"
     :why "a release posted in a maintainer's name that ran a dropper on install")
    (:ecosystem "npm" :name "supports-color" :version "10.0.2" :severity :critical
     :catalog "npm-phishing-2025"
     :why "a release posted in a maintainer's name that ran a dropper on install")
    (:ecosystem "npm" :name "strip-ansi" :version "7.1.1" :severity :critical
     :catalog "npm-phishing-2025"
     :why "a release posted in a maintainer's name that ran a dropper on install")
    (:ecosystem "npm" :name "wrap-ansi" :version "9.0.1" :severity :critical
     :catalog "npm-phishing-2025"
     :why "a release posted in a maintainer's name that ran a dropper on install")
    (:ecosystem "npm" :name "color-convert" :version "3.1.1" :severity :critical
     :catalog "npm-phishing-2025"
     :why "a release posted in a maintainer's name that ran a dropper on install")
    (:ecosystem "npm" :name "error-ex" :version "1.3.5" :severity :critical
     :catalog "npm-phishing-2025"
     :why "a release posted in a maintainer's name that ran a dropper on install")
    (:ecosystem "pypi" :name "ctx" :version "0.1.2" :severity :critical
     :catalog "ctx-2022"
     :why "sent the environment, cloud keys included, to a third-party host on import")
    (:ecosystem "pypi" :name "colourama" :version "0.1.6" :severity :high
     :catalog "colourama-2024"
     :why "a typosquat of colorama that read the clipboard and startup folder")
    (:ecosystem "pypi" :name "ultralytics" :version "8.3.41" :severity :high
     :catalog "ultralytics-2024"
     :why "a hijacked release started a cryptominer on import")
    (:ecosystem "pypi" :name "ultralytics" :version "8.3.42" :severity :high
     :catalog "ultralytics-2024"
     :why "a hijacked release started a cryptominer on import"))
  "Releases compromised in their own right, matched exactly by ecosystem, name
and version. A sample of the original's catalogs; see README.")

(defun catalog-size ()
  (length *catalog*))

;;; --- names --------------------------------------------------------------------

(defun normalize-name (ecosystem name)
  "NAME as its ecosystem spells it for a match: lowercased, and a PyPI
name's underscores read as dashes."
  (let ((name (string-downcase (princ-to-string name))))
    (if (string-equal ecosystem "pypi") (substitute #\- #\_ name) name)))

(defun last-dir-name (dir)
  "The name of the folder DIR holds, as a person writes it: off the
namestring, so a name the pathname parser splits at its last dot -- an
installed Python distribution's NAME-VERSION.dist-info -- comes back whole."
  (let* ((native (string-right-trim "/\\" (ignore-errors (uiop:native-namestring dir))))
         (slash (and native (position #\/ native :from-end t))))
    (and native (subseq native (if slash (1+ slash) 0)))))

(defun trimmed-native (path)
  "PATH as a native namestring without the trailing separator a folder keeps."
  (string-right-trim "/\\" (uiop:native-namestring path)))

;;; --- the inventory ---------------------------------------------------------------

(defun read-package-json (dir)
  "DIR's package.json as a JSON object, or NIL when there is none or it is
unreadable: a broken manifest is not a scan failure."
  (let ((file (merge-pathnames "package.json" dir)))
    (and (probe-file file)
         (handler-case (nlk:decode-json (uiop:read-file-string file))
           (error () nil)))))

(defun npm-package (dir &optional fallback-name)
  "One installed npm package, from its package.json: (ECOSYSTEM NAME VERSION
PATH) as a plist, or NIL when it names no version."
  (let* ((json (read-package-json dir))
         (name (or (and json (nlk:json-value json :string "name"))
                   fallback-name (last-dir-name dir)))
         (version (and json (nlk:json-value json :string "version"))))
    (and name version (plusp (length version))
         (list :ecosystem "npm" :name name :version version
               :path (trimmed-native dir)))))

(defun node-modules-packages (modules)
  "Every package installed directly under the node_modules folder MODULES,
scoped ones included."
  (let ((found '()))
    (dolist (entry (ignore-errors (uiop:subdirectories modules)))
      (let ((name (last-dir-name entry)))
        (if (and name (plusp (length name)) (char= #\@ (char name 0)))
            (dolist (scoped (ignore-errors (uiop:subdirectories entry)))
              (let ((package (npm-package scoped (format nil "~a/~a" name (last-dir-name scoped)))))
                (when package (push package found))))
            (let ((package (npm-package entry name)))
              (when package (push package found))))))
    (nreverse found)))

(defun dist-info-name (name)
  "The (values NAME VERSION) of a NAME-VERSION.dist-info folder name."
  (let* ((suffix ".dist-info")
         (stem (subseq name 0 (- (length name) (length suffix))))
         (dash (position #\- stem :from-end t)))
    (when (and dash (plusp dash) (< dash (1- (length stem))))
      (values (subseq stem 0 dash) (subseq stem (1+ dash))))))

(defun inventory (root &key (max-depth 6) (max-packages 20000))
  "Every installed package under ROOT, to MAX-DEPTH folders deep, at most
MAX-PACKAGES of them. => (values PACKAGES TRUNCATED-P), a plist each."
  (let ((found '())
        (truncated nil))
    (labels ((add (packages)
               (dolist (package packages)
                 (cond (truncated)
                       ((>= (length found) max-packages) (setf truncated t))
                       (t (push package found)))))
             (walk (dir depth)
               (when (and (<= depth max-depth) (not truncated))
                 (dolist (sub (ignore-errors (uiop:subdirectories dir)))
                   (let ((name (last-dir-name sub)))
                     (cond
                       ((and name (string= name "node_modules"))
                        (add (node-modules-packages sub)))
                       ((and name (uiop:string-suffix-p name ".dist-info"))
                        (multiple-value-bind (package-name version) (dist-info-name name)
                          (when package-name
                            (add (list (list :ecosystem "pypi" :name package-name :version version
                                             :path (trimmed-native sub)))))))
                       ((and name (string= name ".git")))
                       (t (walk sub (1+ depth)))))))))
      (walk root 0))
    (values (nreverse found) truncated)))

;;; --- what the catalog knows -------------------------------------------------------

(defun catalog-entry (package)
  "The catalog row PACKAGE matches exactly, or NIL."
  (find-if (lambda (entry)
             (and (string-equal (getf entry :ecosystem) (getf package :ecosystem))
                  (string= (normalize-name (getf entry :ecosystem) (getf entry :name))
                           (normalize-name (getf package :ecosystem) (getf package :name)))
                  (string= (getf entry :version) (getf package :version))))
           *catalog*))

(defun findings (packages)
  "Every PACKAGE the catalog knows, as a plist carrying what the entry says."
  (loop for package in packages
        for entry = (catalog-entry package)
        when entry
          collect (list :ecosystem (getf package :ecosystem)
                        :name (getf package :name)
                        :version (getf package :version)
                        :path (getf package :path)
                        :severity (string-downcase (symbol-name (getf entry :severity)))
                        :catalog (getf entry :catalog)
                        :why (getf entry :why))))

;;; --- OSV, the one network call -----------------------------------------------------

(defun osv-ecosystem (ecosystem)
  "OSV.dev's spelling of an ecosystem."
  (if (string-equal ecosystem "pypi") "PyPI" "npm"))

(defun osv-queries (packages)
  (coerce (mapcar (lambda (package)
                    (nlk:json-object
                     "package" (nlk:json-object "name" (getf package :name)
                                                 "ecosystem" (osv-ecosystem (getf package :ecosystem)))
                     "version" (getf package :version)))
                  packages)
          'vector))

(defun osv-counts (packages &key (limit 50) (timeout 20))
  "Advisory counts for the first LIMIT of PACKAGES, in one POST.
=> (values TOTAL CHECKED ROWS), ROWS being (NAME VERSION COUNT) for what
carries any. A refusal is the condition OSV's answer brought."
  (let* ((sample (subseq packages 0 (min limit (length packages))))
         (checked (length sample)))
    (if (zerop checked)
        (values 0 0 '())
        (let* ((body (format nil "{\"queries\":~a}" (nlk:encode-json-object (osv-queries sample))))
               (answer (multiple-value-list
                        (funcall *http* :post *osv-batch-url*
                                 :content body
                                 :headers '(("content-type" . "application/json"))
                                 :timeout timeout)))
               (text (first answer))
               (status (second answer)))
          (unless (and (integerp status) (<= 200 status 299))
            (fail "OSV answered HTTP ~a" status))
          (let* ((object (nlk:decode-json (if (stringp text) text (princ-to-string text))))
                 (results (or (nlk:json-value object :array "results") #())))
            (loop for index from 0 below (min checked (length results))
                  for package = (elt sample index)
                  for vulns = (or (nlk:json-value (elt results index) :array "vulns") #())
                  when (plusp (length vulns))
                    collect (list (getf package :name) (getf package :version) (length vulns)) into rows
                  sum (length vulns) into total
                  finally (return (values total checked rows))))))))
