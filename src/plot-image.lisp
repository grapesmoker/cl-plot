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

(defparameter *default-bg-color* '(1 1 1))

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
             :initarg :bg-color
             :initform '(1 1 1))
   ;; the output type of the image e.g. pdf, svg, png, etc.
   (output-type :accessor plot-image-output-type
                :initarg :output-type
                :initform :png)
   ;; image filename
   (filename :accessor plot-image-filename
             :initarg :filename)))

(defmethod initialize-instance :after ((im plot-image) &rest args)
  (unless (getf args :width)
    (setf (plot-image-width im) *default-im-width*))
  (unless (getf args :height)
    (setf (plot-image-height im) *default-im-height*))

  (setf (bbox im) (list 0 0 (plot-image-width im) (plot-image-height im)))

  ;; make sure our children know who their parents are
  (when (plot-image-plots im)
    (dolist (pl (plot-image-plots im))
      (setf (plot-object-parent pl) im))))

(defmethod print-object ((im plot-image) stream)
  (format stream "#IMAGE<(~Dx~D)>" (plot-image-width im) (plot-image-height im)))

(defmethod draw-plot-object ((im plot-image) context)
  (declare (ignore context))
  (let* ((filename (plot-image-filename im))
         (width (plot-image-width im))
         (height (plot-image-height im))
         (output-type (plot-image-output-type im))
         (bg-color (plot-image-bg-color im))
         (surface (case output-type
                    (:png
                     (create-image-surface :argb32 
                                           width
                                           height))
                         (t nil)))
         (context (case output-type
                    (:png
                     (create-context surface))
                    (:pdf
                     (create-pdf-context filename
                                         width
                                         height))
                    (:svg
                     (create-svg-context filename
                                         width
                                         height)))))
    (destructuring-bind (r g b) bg-color
      (unwind-protect
           (with-context (context)
             (set-source-rgb r g b)
             (paint)
             (dolist (pl (plot-image-plots im))
               (draw-plot-object pl context)))
        (destroy context))
      (case output-type
        (:png
         (surface-write-to-png surface filename))))))
