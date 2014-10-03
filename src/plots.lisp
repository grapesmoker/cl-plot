(in-package :plot)

(defclass plot (plot-object)
  ;; a plot owns the axes that belong to it and the labels; its
  ;; bounding box is the right size to contain all of it
  ((title :accessor plot-title
          :initarg :title
          :initform nil)
   (x-label :accessor plot-x-label
            :initarg :x-label
            :initform nil)
   (y-label :accessor plot-y-label
            :initarg :y-label
            :initform nil)
   (axes :accessor plot-axes
         :initarg :axes
         :initform nil)
   (plot-type :accessor plot-type
              :initarg :plot-type
              :initform :scatter)))

;; at some point this function will have to draw the labels as well
;; but not yet
(defmethod draw-plot-object ((pl plot) context)
  (dolist (ax (plot-axes pl))
    (draw-plot-object ax context)))
