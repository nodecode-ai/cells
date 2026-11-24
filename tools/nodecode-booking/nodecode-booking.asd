;;;; nodecode-booking.asd --- free time and real bookings on the operator's own calendar.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `booking' section and registers the five Google Calendar tools,
;;;; /booking and the manual on (help :booking).
;;;;
;;;; Rewritten from kyle-bartlett's Hermes Agent plugin of the same subject;
;;;; see NOTICE. Every dependency rides with nodecode: nlk:http is the one
;;;; HTTP client, and the RFC3339 arithmetic is this folder's own.

(defsystem "nodecode-booking"
  :description "Google Calendar booking: free slots, upcoming events, book, move, cancel"
  :author "kyle-bartlett (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "time")
               (:file "calendar")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-booking/test"))))

(defsystem "nodecode-booking/test"
  :description "Booking tests. Registered into the shared nodecode.test registry; RUN-BOOKING-TESTS filters by the BOOKING-CELL- name prefix."
  :author "kyle-bartlett (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-booking" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-booking-tests)))
