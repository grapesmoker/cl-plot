(in-package :plot)

(defclass axis (line)
  ;; we'll go ahead and inherit any line-related properties from
  ;; the line class itself
  ;; min and max values being plotted on the axis
  ((min-value :accessor axis-min-value
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
  ;;(with-context (context)
  (draw-line ax context))
