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
  ((x-axis :accessor axes-x-axis
	   :initarg :x-axis
	   :initform nil)
   (y-axis :accessor axes-y-axis
	   :initarg :y-axis
	   :initform nil)
   ;; I'm not sure what these are for, seemingly redundant with the
   ;; individual axes
   (x-pos  :accessor axes-x
	   :initarg :x
	   :initform nil)
   (y-pos  :accessor axes-y
	   :initarg :y
	   :initform nil)
   ;; offsets
   (x-off  :accessor axes-x-off
	   :initarg :x-off
	   :initform nil)
   (y-off  :accessor axes-y-off
	   :initarg :y-off
	   :initform nil)
   ;; how much space the axes are allowed to occupy
   (width  :accessor axes-width
           :initarg :width)
   (height :accessor axes-height
           :initarg :height)))

(defmethod initialize-instance :before ((ax axes) &rest args)
  (declare (ignore args))
  ())

(defmethod initialize-instance :after ((ax axes) &rest args)
  ;; after the axes have been initialized we should initialize the
  ;; individual axis elements and make sure the axes 
  ;; some parameters must be initialized so they'll go here if not
  ;; passed in otherwise
  (unless (getf args :width)
    (setf (axes-width ax) *default-axes-width*))
  (unless (getf args :height)
    (setf (axes-height ax) *default-axes-height*))
  (unless (getf args :x-off)
    (setf (axes-x-off ax) *default-x-offset*))
  (unless (getf args :y-off)
    (setf (axes-y-off ax) *default-y-offset*))

  ;; now that we can guarantee that the offset and dimensions are set,
  ;; we should also set the bounding box
  (unless (getf args :bbox)
    (let ((lower-left-x (axes-x-off ax))
          (lower-left-y (axes-y-off ax))
          (upper-right-x (+ (axes-x-off ax) (axes-width ax)))
          (upper-right-y (+ (axes-y-off ax) (axes-height ax))))
      (setf (bbox ax) `(,lower-left-x ,lower-left-y ,upper-right-x ,upper-right-y))))

  ;; now we should initialize the actual axes if they were not passed
  ;; in explicitly

  ;; WARNING! Creating your own axis objects is definitely something
  ;; the end-user should NOT do! The init functions will take care to
  ;; align the children axis objects properly relative to the parent.
  ;; Safety not guaranteed if this functionality is overwritten.
  (unless (getf args :x-axis)
    (setf (axes-x-axis ax) (make-instance 'axis
                                          :thickness *default-line-thickness*
                                          :color *default-line-color*)))
  (unless (getf args :y-axis)
    (setf (axes-y-axis ax) (make-instance 'axis
                                          :thickness *default-line-thickness*
                                          :color *default-line-color*)))
  ;; once we've created the children, normalize them
  (normalize-axes ax))
  
(defmethod normalize-axes ((ax axes))
  "Normalizes the children axis objects so that they fit properly within
the bounding box of the parent axes."
  (let ((x-axis (axes-x-axis ax))
        (y-axis (axes-y-axis ax))
        (width (axes-width ax))
        (height (axes-height ax))
        (x-off (axes-x-off ax))
        (y-off (axes-y-off ax)))
        ;;(axes-bbox (bbox ax)))
    ;; Note that normalize-axes is brutal; it will force the child
    ;; axis objects to have the right coordinates regardless of what
    ;; they had before
    (setf (line-start-x x-axis) x-off)
    (setf (line-end-x x-axis) (+ x-off width))
    (setf (line-start-y y-axis) y-off)
    (setf (line-end-y y-axis) (+ y-off height))))
  
    

(defmethod draw-axis ((ax axis) context)
  ;;(with-context (context)
  (draw-line ax context))

(defmethod draw-plot-object ((ax axis) context)
  (with-context (context)
    (draw-line ax context)))

(defmethod copy-axis-line ((ax axis))
  "Creates a line with all the properties of the axis."
  (make-instance 'line 
                 :color (line-color ax)
                 :thickness (line-thickness ax)
                 :style (line-style ax)
                 :start-x (line-start-x ax)
                 :end-x (line-end-x ax)
                 :start-y (line-start-y ax)
                 :end-y (line-end-y ax)))


(defmethod draw-plot-object ((ax axes) context)
  (with-context (context)
    ;; do some math here to transform from the data coordinates to the
    ;; picture coordinates

    ;; hmm, we might not want to use the draw-plot-object generic to
    ;; draw the axes. reason being that axes know their coordinate
    ;; system, while the children of the axes do not. so the function
    ;; that draws the axes needs to take some sort of coords as an
    ;; argument. need to think carefully about whether we want to
    ;; expand the args of the generic or make the draw-axes function
    ;; special 

    (let* ((x-axis (axes-x-axis ax))
           (y-axis (axes-y-axis ax))
           (width (axes-width ax))
           (height (axes-height ax)))
           ;; at this point we are assuming the axes are normalized so that
           ;; the children's start/end points line up with the corners of
           ;; the bounding box
      (do axis `(,x-axis ,y-axis)
        ;; image-y = image-height - y
        (let ((line 


      ;; offset the children by the parent offset
      ;; (incf (line-start-x x-axis) (axes-x-off ax))
      ;; (incf (line-end-x x-axis) (axes-x-off ax))
      ;; (incf (line-start-x y-axis) (axes-x-off ax))
      ;; (incf (line-end-x y-axis) (axes-x-off ax))
      
      ;; (incf (line-start-y y-axis) (axes-y-off ax))
      ;; (incf (line-end-y y-axis) (axes-y-off ax))
      ;; (incf (line-start-y y-axis) (axes-y-off ax))
      ;; (incf (line-end-y y-axis) (axes-y-off ax))
      ;; transform the children into the image coordinates

      


    (draw-plot-object (axes-x-axis ax) context)
    (draw-plot-object (axes-y-axis ax) context)))
    
