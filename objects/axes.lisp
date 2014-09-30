(in-package :plot)

(defclass axis (plot-object)
  ;; beginning and end of the line that represents the axis
  ((start-x  :accessor axis-start-x
	     :initarg :start-x
	     :initform nil)   
   (start-y  :accessor axis-start-y
	     :initarg :start-y
	     :initform nil)
   (end-x  :accessor axis-end-x
	   :initarg :end-x
	   :initform nil)   
   (end-y  :accessor axis-end-y
	   :initarg :end-y
	   :initform nil)
   ;; line thickness
   (thickness :accessor axis-thickness
	      :initarg :thickness)
   ;; don't know what "style" means yet, but things like dashed, etc.
   (style :accessor axis-style
	  :initarg :style)
   ;; color as an rgb-triplet
   (color :accessor axis-color
	  :initarg :color)
   ;; min and max values being plotted on the axis
   (min-value :accessor axis-min-value
	      :initarg :min)
   (max-value :accessor axis-max-value
	      :initarg :max)
   ;; list of major and minor ticks
   ;; ticks should probably be their own objects so we can just call
   ;; some generic draw-line method on them
   (major-ticks :accessor axis-major-ticks
		:initarg major-ticks)
   (minor-ticks :accessor axis-minor-ticks
		:initarg minor-ticks)
   ;; label for the axis
   ;; formatters are functions that control the label/tick text
   (label :accessor axis-label
	  :initarg :label)
   (label-formatter :accessor axis-label-formatter
		    :initarg :label-formatter)
   (tick-formatter :accessor axis-tick-formatter
		   :initarg :tick-formatter)))
	      

(defclass axes (plot-object)
  ((x-axis :accessor x-axis
	   :initarg :x-axis
	   :initform nil)
   (y-axis :accessor y-axis
	   :initarg :y-axis
	   :initform nil)
   (x-pos  :accessor x-pos
	   :initarg :x-pos
	   :initform nil)
   (y-pos  :accessor y-pos
	   :initarg :y-pos
	   :initform nil)
   (x-off  :accessor x-off
	   :initarg :x-off
	   :initform nil)
   (y-off  :accessor y-off
	   :initarg :y-off
	   :initform nil)))


(defmethod draw-axis ((ax axis) context)
  (with-context (context)
    (destructuring-bind (r g b) (axis-color ax)
      (set-source-rgb r g b)
      (set-line-width (axis-thickness ax))
      (move-to (axis-start-x ax)
	       (axis-start-y ax))
      (line-to (axis-end-x ax)
	       (axis-end-y ax))
      (stroke))))
