(in-package :plot)

(defclass plot-object ()
  ;; every object that we plot needs to have a bounding box
  ;; which is a 4-tuple of (lower-left-x lower-left-y upper-right-x upper-right-y)
  ((bbox   :accessor bbox
           :initarg :bbox
           :initform nil)))

(defmethod lower-left ((obj plot-object))
  (subseq (bbox obj) 0 2))

(defmethod upper-right ((obj plot-object))
  (subseq (bbox obj) 2 4))

(defgeneric draw-plot-object (obj  context)
  (:documentation "A generic function that draws the plot-object in the given context.
Every class that derives from plot-object should implement this function."))
