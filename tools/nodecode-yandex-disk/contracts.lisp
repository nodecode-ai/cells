;;;; contracts.lisp --- the fifteen tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept these in schemas.py, apart from the handlers so the
;;;; wire contract read in one place; this is the same separation, and here it
;;;; is also the one authority: the JSON Schema the provider is shown is
;;;; projected from a declaration, the value a body runs on is materialized
;;;; from it, and a call that does not satisfy one is refused before the body
;;;; sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-yandex-disk)

(defparameter +sort-orders+
  '("name" "path" "created" "modified" "size"
    "-name" "-path" "-created" "-modified" "-size")
  "The sort orders the listing accepts, as the original's enum listed them: a
leading '-' sorts in reverse.")

(defparameter +media-types+
  '("" "audio" "backup" "book" "compressed" "data" "development" "diskimage"
    "document" "encoded" "executable" "flash" "font" "image" "settings"
    "sprite" "text" "unknown" "video")
  "What the search may keep, by Yandex's own media_type names; empty is all.")

(nlk:define-contract list-request
  (:doc "List a folder on Yandex Disk, or show the metadata of a single file.
Every entry's path is answered, and the other tools take a path exactly as it
comes back. With no path it lists the root of the disk (the section's root
folder when one is configured).")
  ("path" :string :default "/"
   :doc "path on Yandex Disk, e.g. '/Documents' or 'disk:/Documents'. A leading slash is optional, and a path another tool answered can be passed back as it came")
  ("limit" :integer
   :doc "the most entries to answer (default 50, at most 1000); page on with offset")
  ("offset" :integer
   :doc "entries to skip, for paging")
  ("sort" :string :slot sort-order :options +sort-orders+
   :doc "the order of the listing; a leading '-' sorts in reverse"))

(nlk:define-contract search-request
  (:doc "Search Yandex Disk by file name or by file contents. Yandex ranks the
hits; the answer says which of the two matched. Use it when you know a name or
a phrase but not the folder, then read or download what it finds. Some tokens
are not permitted to search at all -- the answer says so, and every other tool
still works.")
  ("query" :text :required t
   :doc "what to look for, in file names or in their contents")
  ("path" :string :default ""
   :doc "keep only hits under this folder; the whole disk when empty")
  ("media_type" :string :options +media-types+
   :doc "keep only files of this kind, e.g. document, image, audio; empty for every kind")
  ("limit" :integer
   :doc "the most hits to answer (default 20, at most 100)"))

(nlk:define-contract read-file-request
  (:doc "Read a text file from Yandex Disk into the conversation. Use
yadisk_download instead for a file that is not text, or one that is large.")
  ("path" :text :required t
   :doc "the file to read")
  ("encoding" :string :default "utf-8"
   :doc "the file's text encoding: utf-8 (the default), ascii, latin-1, cp1251 or koi8-r")
  ("max_bytes" :integer
   :doc "refuse the file when it is larger than this many bytes; the section's max_read_bytes answers when this is absent, and caps it when it is not"))

(nlk:define-contract download-request
  (:doc "Download a file from Yandex Disk to a path on this machine. The file
lands complete or not at all: a download stopped by the size budget leaves the
older copy where it was.")
  ("path" :text :required t
   :doc "the file on Yandex Disk")
  ("save_to" :text :required t
   :doc "where to write it on this machine: a file path, or a folder path to put the file in under its own name")
  ("overwrite" :boolean
   :doc "replace a file that is already there; without it an existing one is refused"))

(nlk:define-contract mkdir-request
  (:doc "Create a folder on Yandex Disk, and any missing folder above it.
A folder that is already there is not an error.")
  ("path" :text :required t
   :doc "the folder to create"))

(nlk:define-contract write-file-request
  (:doc "Write text to a file on Yandex Disk, creating the folder above it.
Use yadisk_upload for a file that is already on this machine.")
  ("path" :text :required t
   :doc "the file to write")
  ("content" :string :required t
   :doc "the text to write; an empty string is a real, empty file")
  ("encoding" :string :default "utf-8"
   :doc "the encoding to write the text in: utf-8 (the default), ascii, latin-1, cp1251 or koi8-r")
  ("overwrite" :boolean
   :doc "replace a file that is already there; without it Yandex refuses"))

(nlk:define-contract upload-request
  (:doc "Upload to Yandex Disk. Give exactly one source: local_path, a file on
this machine, or url, a public link Yandex fetches itself.")
  ("path" :text :required t
   :doc "where the file goes on Yandex Disk")
  ("local_path" :string
   :doc "the file on this machine to send")
  ("url" :string
   :doc "a public URL that Yandex fetches straight into the disk, without this machine proxying it")
  ("overwrite" :boolean
   :doc "replace a file that is already there"))

(nlk:define-contract copy-request
  (:doc "Copy a file or folder on Yandex Disk. The copy is made on Yandex's
side, so nothing passes through this machine.")
  ("source" :text :required t
   :doc "what to copy")
  ("destination" :text :required t
   :doc "where the copy goes; a folder path keeps the original's name under it")
  ("overwrite" :boolean
   :doc "replace what is already at the destination"))

(nlk:define-contract move-request
  (:doc "Move or rename a file or folder on Yandex Disk.")
  ("source" :text :required t
   :doc "what to move")
  ("destination" :text :required t
   :doc "where it goes, which is how a rename is spelled")
  ("overwrite" :boolean
   :doc "replace what is already at the destination"))

(nlk:define-contract delete-request
  (:doc "Delete a file or folder on Yandex Disk. Without permanently it goes to
the bin, where yadisk_trash_restore can bring it back; with it the bytes are
gone.")
  ("path" :text :required t
   :doc "what to delete")
  ("permanently" :boolean
   :doc "skip the bin and destroy it now; nothing can restore it afterwards"))

(nlk:define-contract publish-request
  (:doc "Publish a file or folder on Yandex Disk as a public link, or unpublish
it. Anyone with the link can read what is published: publish deliberately.")
  ("path" :text :required t
   :doc "what to share")
  ("unpublish" :boolean
   :doc "take the public link away instead of making one"))

(nlk:define-contract trash-list-request
  (:doc "List the Yandex Disk bin: what was deleted, where it came from, and when.
The paths it answers are trash:/ paths, which yadisk_trash_restore and
yadisk_trash_empty take.")
  ("limit" :integer
   :doc "the most items to answer (default 50, at most 1000)")
  ("offset" :integer
   :doc "items to skip, for paging"))

(nlk:define-contract trash-restore-request
  (:doc "Restore an item from the Yandex Disk bin to where it was deleted from.
Call yadisk_trash_list first: this takes the trash:/ path it answered.")
  ("path" :text :required t
   :doc "the bin item to restore, as yadisk_trash_list answered it")
  ("name" :string
   :doc "restore it under this new name instead of its old one")
  ("overwrite" :boolean
   :doc "replace a file that has taken its old place since"))

(nlk:define-contract trash-empty-request
  (:doc "Permanently destroy items in the Yandex Disk bin: one item when a path
is given, the whole bin when it is not. Nothing can restore what this
destroys.")
  ("path" :string :default ""
   :doc "the one bin item to destroy, as yadisk_trash_list answered it; every item when empty"))
