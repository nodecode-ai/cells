;;;; cell-test.lisp --- lifecycle, the tools, the sandbox, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Yandex: the config
;;;; surface (a section naming no token refuses the start), each tool through
;;;; the real dispatch seam against a stubbed disk, the path sandbox, and the
;;;; action allow-list withholding a tool.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "yandex-disk"
  (:fixture with-wire)
  (:config "token" +token+)
  (:help :yandex-disk)
  (:command "yadisk")
  (:running
   (is (nle::find-tool "yadisk_disk_info") "yadisk_disk_info is registered")
   (is (nle::find-tool "yadisk_list") "yadisk_list is registered")
   (is (nle::find-tool "yadisk_trash_empty") "and the last of the fifteen"))
  (:stopped
   (is (null (nle::find-tool "yadisk_list")) "stop unregisters the list tool")
   (is (null (nle::find-tool "yadisk_disk_info")) "and the account tool"))
  (:refused
   ("token" "" "token_env" "UNSET_YADISK_TOKEN")
   ("token" +token+ "base_url" "http://example.invalid/v1/disk")
   ("token" +token+ "timeout_seconds" 0)))

(deftest yandex-disk-cell-a-token-is-named-when-it-is-missing ()
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (yandex-disk-start "token" "" "token_env" "UNSET_YADISK_TOKEN"))))
      (is (cl:search "no OAuth token" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "yadisk_list")) "and installs nothing"))))

;;; --- the space, and the folder ----------------------------------------------------

(deftest yandex-disk-cell-the-quota-is-one-paragraph ()
  (with-yandex-disk ()
    (let ((text (call-tool "yadisk_disk_info" "{}")))
      (is (cl:search "488.3 KB of 1.9 MB used" text) "the account's own numbers, sized for a reader")
      (is (cl:search "bin: 1.0 KB" text) "the bin")
      (is (cl:search "account: someone" text) "and who it belongs to"))))

(deftest yandex-disk-cell-a-listing-answers-every-child-with-its-path ()
  (with-yandex-disk ()
    (let ((text (call-tool "yadisk_list" "{\"path\":\"/Documents\"}")))
      (is (cl:search "/Documents/report.pdf -- file, 2.0 KB" text) (subseq text 0 (min 300 (length text))))
      (is (cl:search "md5 0123456789abcdef" text) "with its checksum")
      (is (cl:search "/Documents/notes -- dir" text) "the folder, as a folder")
      (is (cl:search "9 entries in the folder" text) "and the folder's own count"))
    (let ((call (last-call)))
      (is (equal "disk:/Documents" (query-param call "path")) "the path Yandex is asked for carries its scheme")
      (is (equal "50" (query-param call "limit")) "the default page")
      (is (equal (format nil "OAuth ~a" +token+) (header-of call "authorization")) "the section's token")
      (is (cl:search "/resources" (getf call :url)) "on the resources endpoint"))))

(deftest yandex-disk-cell-a-path-returned-by-one-tool-is-taken-by-the-other ()
  (with-yandex-disk ()
    (call-tool "yadisk_list" "{\"path\":\"/Documents\"}")
    (let ((text (call-tool "yadisk_list" "{}")))
      (is (stringp text) "a call with no path lists the root, not an error"))
    (is (cl:search "disk:/" (query-param (last-call) "path")) "which is the disk's own root")))

;;; --- the sandbox -------------------------------------------------------------------

(deftest yandex-disk-cell-a-root-confines-every-path ()
  (with-yandex-disk ("root" "/Backup")
    (call-tool "yadisk_list" "{\"path\":\"/Documents\"}")
    (is (equal "disk:/Backup/Documents" (query-param (last-call) "path")) "a bare path hangs off the root")
    (let ((text (call-tool "yadisk_list" "{\"path\":\"disk:/Elsewhere\"}")))
      (is (cl:search "outside the configured root" text) "an absolute path outside it is refused"))))

(deftest yandex-disk-cell-paths-normalise-and-never-climb-out ()
  (is (equal "disk:/a/b" (nodecode-yandex-disk::resolve-path "/a/./b/")) "a path is normalised")
  (is (equal "disk:/a/b" (nodecode-yandex-disk::resolve-path "a/x/../b")) "and its .. collapse")
  (is (signals-error nodecode-yandex-disk:yandex-disk-error
                     (nodecode-yandex-disk::resolve-path "/../etc")) "one .. past the root is refused")
  (is (equal "/Documents/x" (nodecode-yandex-disk::display-path "disk:/Backup/Documents/x" "/Backup")) "the sandbox prefix is hidden")
  (is (equal "disk:/Backup/Documents/x"
               (nodecode-yandex-disk::resolve-path
                (nodecode-yandex-disk::display-path "disk:/Backup/Documents/x" "/Backup")
                :root "/Backup")) "and a path it showed comes back to the same file"))

;;; --- reading -----------------------------------------------------------------------

(deftest yandex-disk-cell-a-text-file-is-read-into-the-answer ()
  (with-yandex-disk ()
    (let ((text (call-tool "yadisk_read_file" "{\"path\":\"/Documents/readme.txt\"}")))
      (is (cl:search "{\"hello\":true" text) "the file's own bytes, decoded"))
    (is (cl:search "https://down.example/one" (getf (last-call) :url)) "read through the download link")
    (is (null (header-of (last-call) "authorization")) "which carries no token")))

(deftest yandex-disk-cell-a-search-is-confined-and-says-which-half-matched ()
  (with-yandex-disk ("root" "/Documents")
    (let ((text (call-tool "yadisk_search" "{\"query\":\"match\"}")))
      (is (cl:search "/match.txt" text) "the hit under the root")
      (is (not (cl:search "away.txt" text)) "and not the one outside it")))
  (with-yandex-disk ()
    (let ((text (call-tool "yadisk_search" "{\"query\":\"match\"}")))
      (is (cl:search "away.txt" text) "with no root, the whole disk is searched"))))

(deftest yandex-disk-cell-a-token-that-may-not-search-is-told-so ()
  (with-yandex-disk ()
    (push (cons "/resources/search" (answer "" 403)) *routes*)
    (let ((text (call-tool "yadisk_search" "{\"query\":\"x\"}")))
      (is (cl:search "not permitted to search" text) "the refusal names the one tool that fails")
      (is (cl:search "list the folder instead" text) "and what to do instead"))))

;;; --- writing ------------------------------------------------------------------------

(deftest yandex-disk-cell-a-written-file-goes-up-without-the-token-on-the-transfer ()
  (with-yandex-disk ()
    (let ((text (call-tool "yadisk_write_file" "{\"path\":\"/Documents/new.txt\",\"content\":\"hi there\"}")))
      (is (cl:search "written" text) "the answer says it was written"))
    (let* ((put (first (remove-if-not (lambda (call) (eq (getf call :method) :put)) *calls*)))
           (octets (getf put :content)))
      (is (equal "hi there" (map 'string #'code-char octets)) "the text as octets")
      (is (null (header-of put "authorization")) "on the transfer host, without the token"))
    (is (some (lambda (call) (cl:search "/resources/upload" (getf call :url))) *calls*) "an upload link was asked for")))

(deftest yandex-disk-cell-a-folder-is-made-with-its-parents ()
  (with-yandex-disk ()
    (call-tool "yadisk_mkdir" "{\"path\":\"/a/b\"}")
    (let ((puts (remove-if-not (lambda (call) (eq (getf call :method) :put)) *calls*)))
      (is (= 2 (length puts)) "one call a level")
      (is (equal "disk:/a" (query-param (second puts) "path")) "the parent first")
      (is (equal "disk:/a/b" (query-param (first puts) "path")) "then the folder"))))

(deftest yandex-disk-cell-a-download-lands-on-this-machine ()
  (with-yandex-disk ()
    (let* ((folder (format nil "~a/yadisk-test-~a" (uiop:temporary-directory) (random 1000000)))
           (target (format nil "~a/notes.json" folder))
           (text (call-tool "yadisk_download"
                             (format nil "{\"path\":\"/Documents/readme.txt\",\"save_to\":\"~a\",\"overwrite\":true}"
                                     target))))
      (is (cl:search "saved to" text) "the answer names the file it wrote")
      (let ((written (probe-file target)))
        (is written "the file is there, in the folder the call named")
        (when written
          (is (cl:search "hello" (uiop:read-file-string written)) "with the bytes it downloaded")))
      (is (cl:search "already exists"
                     (call-tool "yadisk_download"
                                (format nil "{\"path\":\"/Documents/readme.txt\",\"save_to\":\"~a\"}"
                                        target))) "a second download refuses to replace it"))))

(deftest yandex-disk-cell-delete-and-the-bin ()
  (with-yandex-disk ()
    (let ((text (call-tool "yadisk_delete" "{\"path\":\"/Documents/readme.txt\"}")))
      (is (cl:search "moved to the bin" text) "a delete is recoverable, and says so"))
    (let ((text (call-tool "yadisk_trash_list" "{}")))
      (is (cl:search "the bin -- 1 item" text) "the bin lists what is in it")
      (is (cl:search "from disk:/Documents/match.txt" text) "and where each item came from")))
  (with-yandex-disk ("root" "/Documents")
    (let ((text (call-tool "yadisk_trash_empty" "{}")))
      (is (cl:search "whole bin cannot be emptied" text) "a sandboxed install cannot empty the whole bin"))))

;;; --- the allow-list -------------------------------------------------------------------

(deftest yandex-disk-cell-a-read-only-install-shows-no-write-tool ()
  (with-yandex-disk ("actions" #("read"))
    (is (nle::find-tool "yadisk_list") "the read tools are there")
    (is (nle::find-tool "yadisk_read_file") "including the one that reads a file")
    (is (null (nle::find-tool "yadisk_delete")) "and no tool that can delete")
    (is (null (nle::find-tool "yadisk_write_file")) "or write")))
