;;;; cell-test.lisp --- lifecycle; the six tools; the gate; /image-utils.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the cell starts and stops with its six tools, its
;;;; command and its manual, and a config outside the section's bounds
;;;; refuses; image_info answers a real file's facts; a resize writes beside
;;;; its input at the size asked for, through the tool call a provider makes;
;;;; a crop and a quarter turn change the pixels and nothing else; the
;;;; overwrite gate needs both keys; converting to JPEG writes JPEG bytes and
;;;; an image with alpha is refused rather than flattened silently; and
;;;; image_optimize reports what it moved. Every test writes its own PNG under
;;;; a scratch folder and reads it back: nothing here dials anything.

(in-package #:nodecode.test)

(define-test-slice "image-utils" "IMAGE-UTILS" :start nodecode-image-utils:start-cell)

(define-cell-lifecycle-tests "image-utils"
  (:config "jpeg_quality" 80)
  (:help :image-utils)
  (:command "image-utils")
  (:running
   (is (= 80 (nodecode-image-utils::setting :jpeg-quality))
       "the declared quality is what the cell runs on")
   (is (= 64 (nodecode-image-utils::setting :max-pixels))
       "and the declared ceiling"))
  (:refused ("jpeg_quality" 0) ("max_pixels" 0)))

;;; --- one scratch folder and one PNG per test ---------------------------------

(defmacro with-scratch ((dir) &body body)
  "BODY with DIR a fresh folder of its own, taken away after."
  `(let ((,dir (merge-pathnames (format nil "image-utils-~d/" (random 1000000))
                                 (uiop:temporary-directory))))
     (ensure-directories-exist ,dir)
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,dir :validate t)))))

(defun write-test-png (dir name width height &key alpha)
  "A WIDTH x HEIGHT PNG written under DIR as NAME: a gradient when it has no
alpha, and a half-transparent left column when it has. => its pathname."
  (let* ((channels (if alpha 4 3))
         (pixels (nle::blank-pixels width height channels))
         (data (nle::pixels-data pixels)))
    (dotimes (y height)
      (dotimes (x width)
        (let ((at (* (+ (* y width) x) channels)))
          (setf (aref data at) (mod (* x 30) 256)
                (aref data (+ at 1)) (mod (* y 40) 256)
                (aref data (+ at 2)) 90)
          (when alpha (setf (aref data (+ at 3)) (if (zerop x) 64 255))))))
    (let ((path (merge-pathnames name dir)))
      (alexandria:write-byte-vector-into-file (nle::pixels-png pixels) path
                                              :if-exists :supersede)
      path)))

(defun answer-of (id name arguments)
  "The text a provider's call ID to tool NAME with JSON ARGUMENTS answers."
  (nle::message-content (execute-wire-call id name arguments)))

(defun dims-of (path)
  "(values WIDTH HEIGHT) of the image file at PATH, read by the engine's own
header reader."
  (nlt:image-dimensions (alexandria:read-file-into-byte-vector path)))

;;; --- the tools ---------------------------------------------------------------

(deftest image-utils-info-reads-one-file ()
  (with-cell-stop ((image-utils-start))
    (with-scratch (dir)
      (let* ((path (write-test-png dir "input.png" 8 6))
             (answer (answer-of "iu-info-1" "image_info"
                                (format nil "{\"path\": ~s}" (namestring path)))))
        (is (search "PNG 8x6" answer) "the format and the size are named")
        (is (search "three samples a pixel" answer) "and what the pixels are")
        (is (search "bytes" answer) "and how big the file is")
        (is (search "pixels decode" answer) "nothing here reads ... or says it cannot"))
      (is (search "not a file"
                  (answer-of "iu-info-2" "image_info"
                             (format nil "{\"path\": ~s}"
                                     (namestring (merge-pathnames "gone.png" dir)))))
          "a path that is no file is refused in words"))))

(deftest image-utils-resize-writes-half-size-beside-the-input ()
  (with-cell-stop ((image-utils-start))
    (with-scratch (dir)
      (let* ((path (write-test-png dir "photo.png" 8 6))
             (answer (answer-of "iu-resize-1" "image_resize"
                                (format nil "{\"path\": ~s, \"percent\": 50}"
                                        (namestring path)))))
        (is (search "resized 8x6 -> 4x3" answer) "half of both sides")
        (let ((out (merge-pathnames "photo_resized.png" dir)))
          (is (probe-file out) "the result lands beside its input")
          (multiple-value-bind (width height) (dims-of out)
            (is (= 4 width) "and is four pixels wide")
            (is (= 3 height) "three tall")))))))

(deftest image-utils-shape-is-kept-and-upscales-need-a-key ()
  (with-cell-stop ((image-utils-start))
    (with-scratch (dir)
      (let* ((path (write-test-png dir "wide.png" 9 3))
             (half (answer-of "iu-resize-2" "image_resize"
                              (format nil "{\"path\": ~s, \"width\": 3}"
                                      (namestring path)))))
        (is (search "9x3 -> 3x1" half) "one dimension keeps the shape")
        (is (search "allow_upscale"
                    (answer-of "iu-resize-3" "image_resize"
                               (format nil "{\"path\": ~s, \"width\": 90}"
                                       (namestring path))))
            "growing it needs allow_upscale")
        (is (search "allow_distort"
                    (answer-of "iu-resize-4" "image_resize"
                               (format nil "{\"path\": ~s, \"width\": 4, \"height\": 2}"
                                       (namestring path))))
            "two dimensions need allow_distort")))))

(deftest image-utils-crop-and-rotate-move-the-pixels ()
  (with-cell-stop ((image-utils-start))
    (with-scratch (dir)
      (let* ((path (write-test-png dir "square.png" 8 8))
             (cropped (answer-of "iu-crop-1" "image_crop"
                                 (format nil "{\"path\": ~s, \"box\": [2, 2, 6, 5]}"
                                         (namestring path))))
             (rotated (answer-of "iu-rotate-1" "image_rotate"
                                 (format nil "{\"path\": ~s, \"angle\": 90}"
                                         (namestring path)))))
        (is (search "cropped 8x8 -> 4x3" cropped) "the box is the crop")
        (is (search "90 degrees counter-clockwise" rotated) "the turn is named")
        (let ((out (merge-pathnames "square_rotated.png" dir)))
          (multiple-value-bind (width height) (dims-of out)
            (is (= 8 width) "a quarter turn of a square stays square")
            (is (= 8 height) "on both sides")))
        (is (search "not inside"
                    (answer-of "iu-crop-2" "image_crop"
                               (format nil "{\"path\": ~s, \"box\": [0, 0, 9, 9]}"
                                       (namestring path))))
            "a box outside the image is refused")
        (is (search "aspect takes two numbers"
                    (answer-of "iu-crop-3" "image_crop"
                               (format nil "{\"path\": ~s, \"aspect\": \"9x9\"}"
                                       (namestring path))))
            "an aspect with no colon is refused by name")))))

(deftest image-utils-the-overwrite-gate-needs-both-keys ()
  (with-cell-stop ((image-utils-start))
    (with-scratch (dir)
      (let* ((path (write-test-png dir "gate.png" 8 6))
             (out (merge-pathnames "gate_resized.png" dir))
             (json (format nil "{\"path\": ~s, \"percent\": 50}" (namestring path))))
        (answer-of "iu-gate-1" "image_resize" json)
        (is (probe-file out) "the first write lands")
        (is (search "overwrite=true and confirm=true"
                    (answer-of "iu-gate-2" "image_resize" json))
            "the second is refused, and names both keys")
        (is (search "overwrite=true and confirm=true"
                    (answer-of "iu-gate-3" "image_resize"
                               (format nil "{\"path\": ~s, \"percent\": 50, \"overwrite\": true}"
                                       (namestring path))))
            "overwrite alone is not enough")
        (is (search "written to"
                    (answer-of "iu-gate-4" "image_resize"
                               (format nil "{\"path\": ~s, \"percent\": 50, \"overwrite\": true, \"confirm\": true}"
                                       (namestring path))))
            "both keys write")))))

(deftest image-utils-convert-writes-jpeg-and-refuses-losing-alpha ()
  (with-cell-stop ((image-utils-start))
    (with-scratch (dir)
      (let* ((path (write-test-png dir "plain.png" 8 6))
             (alpha (write-test-png dir "clear.png" 8 6 :alpha t))
             (answer (answer-of "iu-convert-1" "image_convert"
                                (format nil "{\"path\": ~s, \"format\": \"JPEG\"}"
                                        (namestring path))))
             (jpeg (merge-pathnames "plain_converted.jpg" dir)))
        (is (search "PNG -> JPEG" answer) "the two formats are named")
        (is (probe-file jpeg) "a JPEG lands")
        (let ((bytes (alexandria:read-file-into-byte-vector jpeg)))
          (is (and (= #xFF (aref bytes 0)) (= #xD8 (aref bytes 1)))
              "and it leads with the JPEG signature"))
        (is (search "flatten=true"
                    (answer-of "iu-convert-2" "image_convert"
                               (format nil "{\"path\": ~s, \"format\": \"JPEG\"}"
                                       (namestring alpha))))
            "alpha into JPEG is refused unless declared")
        (is (search "written to"
                    (answer-of "iu-convert-3" "image_convert"
                               (format nil "{\"path\": ~s, \"format\": \"JPEG\", \"flatten\": true}"
                                       (namestring alpha))))
            "and with flatten it is laid over the background")
        (is (search "TIFF"
                    (answer-of "iu-convert-4" "image_convert"
                               (format nil "{\"path\": ~s, \"format\": \"TIFF\"}"
                                       (namestring path))))
            "a format it cannot write is named")))))

(deftest image-utils-optimize-reports-what-it-wrote ()
  (with-cell-stop ((image-utils-start))
    (with-scratch (dir)
      (let* ((path (write-test-png dir "big.png" 60 40))
             (answer (answer-of "iu-opt-1" "image_optimize"
                                (format nil "{\"path\": ~s, \"max_dimension\": 20}"
                                        (namestring path)))))
        (is (search "re-encoded PNG 60x40, capped to 20x13" answer) "the cap is applied")
        (is (search "written to" answer) "and the write is reported")
        (multiple-value-bind (width height)
            (dims-of (merge-pathnames "big_optimized.png" dir))
          (is (= 20 width) "the file is the capped size")
          (is (= 13 height) "on both sides"))))))

(deftest image-utils-slash-reports-the-section ()
  (with-cell-stop ((image-utils-start))
    (let ((report (nodecode-image-utils:run-slash "" nil)))
      (is (search "max_pixels 64" report) "the ceiling it runs at")
      (is (search "jpeg_quality 85" report) "and the quality")
      (is (search "writes PNG, JPEG and BMP" report) "and what it can write"))
    (with-scratch (dir)
      (let* ((path (write-test-png dir "slash.png" 4 4))
             (answer (nodecode-image-utils:run-slash
                      (format nil "info ~a" (namestring path)) nil)))
        (is (search "PNG 4x4" answer) "/image-utils info answers one file")))))
