(in-package :plot)

;; some defaults 
(defparameter *default-im-width* 640)
(defparameter *default-im-height* 480)

(defparameter *default-axes-width* 600)
(defparameter *default-axes-height* 440)

(defparameter *default-x-offset* 20)
(defparameter *default-y-offset* 20)

(defparameter *default-line-color* '(0 0 0))
(defparameter *default-line-thickness* 2.0)

(defclass plot-image (plot-object)
  ;; the image's bbox is just the size of the actual image in pixels
  ;; the image holds the set of all plots represented on it
  ((plots :accessor plot-image-plots
          :initarg :plots
          :initform nil)
   ;; width and height dictate the size of the image in pixels
   (width :accessor plot-image-width
          :initarg :width)
   (height :accessor plot-image-height
           :initarg :height)
   ;; background color as an rgb triple
   (bg-color :accessor plot-image-bg-color
             :initarg :bg-color)
   ;; the output type of the image e.g. pdf, svg, png, etc.
   (output-type :accessor plot-image-output-type
                :initarg :output-type)
   ;; image filename
   (filename :accessor plot-image-filename
             :initarg :filename)))

(defmethod initialize-instance :after ((im image) &rest args)
  (unless (getf args :width)
    (setf (image-width im) *default-im-width*))
  (unless (getf args :height)
    (setf (image-width im) *default-im-height*))

  (setf (bbox im) (list 0 0 (image-width im) (image-height im))))
