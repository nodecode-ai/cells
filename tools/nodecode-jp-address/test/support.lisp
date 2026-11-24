;;;; support.lisp --- the jp-address test runner, its fixtures and its seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; JP-ADDRESS-CELL- prefix, and RUN-JP-ADDRESS-TESTS runs exactly that
;;;; slice, so this system's test-op never re-runs the core suite.
;;;;
;;;; The one network call is NODECODE-JP-ADDRESS::HTTP-BODY;
;;;; WITH-SCRIPTED-ADDRESS stubs that name, records every URL and answers the
;;;; fixture filed under a needle, so no test ever opens a socket.

(in-package #:nodecode.test)

(define-test-slice "jp-address" "JP-ADDRESS-CELL-" :start nodecode-jp-address:start-cell)

;;; --- config and lifecycle fixtures ----------------------------------------------

(defmacro with-jp-address ((&rest config-pairs) &body body)
  "Run BODY with the cell started on CONFIG-PAIRS and stopped on unwind."
  `(with-cell-stop ((jp-address-start ,@config-pairs))
     ,@body))

;;; --- the scripted seam -----------------------------------------------------------

(defstruct (jp-http (:copier nil)) url)

(defmacro with-scripted-address ((calls &key routes) &body forms)
  "Run FORMS with the cell's one network call stubbed: every URL recorded
into the list CALLS, oldest first, and ROUTES -- an alist of (NEEDLE . BODY),
NEEDLE a string searched in the URL -- answered as HTTP 200. A URL no needle
matches answers HTTP 404, which is the upstream case."
  `(let ((,calls '()))
     (with-stubbed-fdefinition (nodecode-jp-address::http-body (url timeout)
                                  (declare (ignore timeout))
                                  (setf ,calls (append ,calls (list url)))
                                  (let ((route (find-if (lambda (route) (search (car route) url))
                                                        ,routes)))
                                    (if route
                                        (values (cdr route) 200)
                                        (values "{\"error\": \"no such file\"}" 404))))
       ,@forms)))

;;; --- the address files the tests answer with --------------------------------------

(defun jp-object (&rest pairs)
  "One JSON object built from alternating key/value pairs."
  (apply #'nlk:make-json-object pairs))

(defun jp-json (object)
  "OBJECT as the JSON text a peer would answer with."
  (nlk:encode-json-object object))

(defun jp-list-file ()
  "The national list: Tokyo, Nagano's Karuizawa, Hokkaido's two Tomari villages
and Sapporo's Chuo ward."
  (jp-json
   (jp-object
    "data"
    (vector
     (jp-object "pref" "東京都"
                 "point" (vector 139.691717d0 35.689568d0)
                 "cities" (vector (jp-object "city" "千代田区"
                                              "point" (vector 139.753634d0 35.694003d0))))
     (jp-object "pref" "長野県"
                 "point" (vector 138.181028d0 36.651297d0)
                 "cities" (vector (jp-object "county" "北佐久郡" "city" "軽井沢町"
                                              "point" (vector 138.596966d0 36.348310d0))))
     (jp-object "pref" "北海道"
                 "point" (vector 141.346822d0 43.064312d0)
                 "cities" (vector (jp-object "county" "古宇郡" "city" "泊村")
                                   (jp-object "county" "国後郡" "city" "泊村")
                                   (jp-object "city" "札幌市" "ward" "中央区"
                                              "point" (vector 141.340835d0 43.055545d0))))))))

(defun jp-chiyoda-file ()
  "Chiyoda's town file: Kasumigaseki, as a row with a chome."
  (jp-json
   (jp-object
    "data"
    (vector (jp-object "oaza_cho" "霞が関" "chome" "二丁目" "chome_n" 2
                        "machiaza_id" "0054002"
                        "point" (vector 139.750413d0 35.675551d0))))))

(defun jp-chuo-file ()
  "Sapporo's Chuo ward: a bare Miyagaoka row beside its chome rows, and a
Miyanomori row whose own name is written with a kanji numeral."
  (jp-json
   (jp-object
    "data"
    (vector (jp-object "oaza_cho" "宮ケ丘" "machiaza_id" "0075000")
            (jp-object "oaza_cho" "宮ケ丘" "chome" "三丁目" "chome_n" 3
                        "machiaza_id" "0075003"
                        "point" (vector 141.309325d0 43.057254d0))
            (jp-object "oaza_cho" "宮の森一条" "chome" "一丁目"
                        "machiaza_id" "0091001"
                        "point" (vector 141.324000d0 43.055000d0))))))

;;; --- calling what the cell contributes -------------------------------------------

(defun jp-address-tool (address)
  "The registered `address_split' tool, called the way the engine calls it:
the declaration materializes the record, the body answers JSON text."
  (let ((entry (nle::find-tool "address_split")))
    (assert entry () "the address_split tool is not registered")
    (funcall (third entry)
             (nlk:contract-decode (second entry)
                                   (nlk:make-json-object "address" address)))))

(defun jp-address-result (address)
  "ADDRESS split, as the JSON object the tool answered."
  (let ((body (jp-address-tool address)))
    (assert (stringp body) () "the tool answers JSON text")
    (cell-json body)))

(defun jp-address-field (result key)
  "RESULT's member KEY."
  (and (hash-table-p result) (gethash key result)))
