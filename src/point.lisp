(in-package :plot)

(defparameter *default-point-color* '(0 0 1))
(defparameter *default-point-weight* 2.5)

(defclass point (plot-object)
  ;; a point is just a plot object with x and y coordinates, plus some
  ;; style information
  ((x :accessor point-x
      :initarg :x
      :initform nil)
   (y :accessor point-y
      :initarg :y
      :initform nil)
   ;; point weight
   (weight :accessor point-weight
           :initarg :weight
           :initform *default-point-weight*)
   ;; don't know what "style" means yet, but things like dashed, etc.
   (style :accessor point-style
	  :initarg :style
          :initform nil)
   ;; color as an rgb-triplet
   (color :accessor point-color
	  :initarg :color
          :initform *default-point-color*)))

(defmethod initialize-instance :after ((p point) &rest args)
  (declare (ignore args))
  ;; points do have bounding-boxes!
  (when (and (point-x p) (point-y p) (point-weight p))
    (let ((offset (/ (point-weight p) 2)))
      (setf (bbox p)
            (list (- (point-x p) offset)
                  (- (point-y p) offset)
                  (+ (point-x p) offset)
                  (+ (point-y p) offset))))))

(defmethod draw-plot-object ((p point) context)
  ;; drawing a point is like drawing a line from the point to itself
  (with-context (context)
    (destructuring-bind (r g b) (point-color p)
      (let ((saved-line-cap (get-line-cap)))
        (set-line-cap :round)
        (set-source-rgb r g b)
        (set-line-width (point-weight p))
        (move-to (point-x p)
                 (point-y p))
        (line-to (point-x p)
                 (point-y p))
        (stroke)
        (set-line-cap saved-line-cap)))))
