;;;; store.lisp --- where the bundle lives: a folder, or an https URL.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original had three backends (a local folder, S3, and a git
;;;; repository) behind one engine, roam/backends/. A cell may call HTTP
;;;; and the CLI the user already has; it should not carry a SigV4 signer
;;;; for one vendor. So there are two here: a FOLDER -- a mounted drive, a
;;;; synced folder, a USB stick -- and one HTTPS URL the operator can PUT
;;;; to, which is what a presigned S3 or R2 URL, a WebDAV endpoint or a
;;;; small signing proxy gives them. The address decides which it is: a
;;;; path is a folder, an http(s) URL is the wire.
;;;;
;;;; One bundle per profile, so two machines of one person share a store
;;;; without sharing a name.

(in-package #:nodecode-roam)

(defvar *http* 'nlk:http
  "The one HTTP seam, as a function designator, so a test can answer
without a wire without this cell knowing it is being tested.")

(defun store-address ()
  "The store the section names, trimmed, or a refusal naming the member."
  (let ((address (nlk:trimmed (or (setting :store) ""))))
    (if (plusp (length address))
        address
        (fail "roam: store is not set: name the folder or https:// URL the bundle lives at, in the roam section of config.jsonc or in /settings"))))

(defun http-store-p (address)
  "Whether ADDRESS is a URL rather than a folder."
  (let ((address (string-downcase address)))
    (or (uiop:string-prefix-p "https://" address)
        (uiop:string-prefix-p "http://" address))))

(defun store-backend ()
  "`http' or `folder', for the answers."
  (if (http-store-p (store-address)) "http" "folder"))

(defun bundle-profile ()
  "The profile the section names, as a file name."
  (let ((profile (nlk:trimmed (or (setting :profile) "default"))))
    (if (and (plusp (length profile))
             (every (lambda (character) (or (alphanumericp character) (find character "-_.")))
                    profile))
        profile
        (fail "roam: profile ~s is not a bundle name: use letters, digits, - _ and ." profile))))

(defun bundle-pathname (address)
  "The bundle's path in the folder store ADDRESS, for this profile."
  (merge-pathnames (format nil "roam-~a.nroam" (bundle-profile))
                    (uiop:ensure-directory-pathname (nlk:expand-home address))))

(defun store-headers ()
  "The headers the URL store is read and written with: an Authorization
header, when the section names the variable that holds one."
  (let* ((name (nlk:trimmed (or (setting :authorization-env) "")))
         (value (and (plusp (length name)) (uiop:getenv name)))
         (value (and value (nlk:trimmed value))))
    (if (and value (plusp (length value)))
        (list (cons "authorization" value))
        '())))

(defun timeout ()
  "One store read or write's ceiling, in seconds."
  (or (setting :operation-timeout) 60))

(defun store-read ()
  "The bundle the store holds, as octets, or NIL when it holds none."
  (let ((address (store-address)))
    (if (http-store-p address)
        (multiple-value-bind (body status)
            (funcall *http* :get address
                            :headers (store-headers)
                            :binary t
                            :max-bytes +max-bundle+
                            :timeout (timeout))
          (cond ((eql status 404) nil)
                ((and (integerp status) (< status 300)) (octets-of body))
                (t (fail "roam: the store answered ~@[~a ~]~@[on a read~]"
                         (and (integerp status) status)))))
        (let ((file (bundle-pathname address)))
          (when (probe-file file) (read-octets file))))))

(defun store-head ()
  "The bundle's head as the store holds it, or NIL when it holds none.
The head is what a reader may see without the passphrase: version, date,
profile, counts and one digest of the contents."
  (let ((octets (store-read)))
    (and octets (bundle-head octets))))

(defun store-write (octets)
  "OCTETS to the store as this profile's bundle."
  (let ((address (store-address)))
    (if (http-store-p address)
        (multiple-value-bind (body status)
            (funcall *http* :put address
                              :headers (cons (cons "content-type" "application/octet-stream")
                                             (store-headers))
                              :content octets
                              :timeout (timeout))
          (declare (ignore body))
          (unless (and (integerp status) (< status 300))
            (fail "roam: the store answered ~@[~a ~]on a write" (and (integerp status) status))))
        (let ((file (bundle-pathname address)))
          (ensure-directories-exist file)
          (nlk:write-file-atomically file octets)))
    octets))

(defun store-what ()
  "The store as the answers name it: the URL, or the folder's namestring."
  (let ((address (store-address)))
    (if (http-store-p address)
        address
        (namestring (uiop:ensure-directory-pathname (nlk:expand-home address))))))
