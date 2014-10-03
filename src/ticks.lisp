(in-package :plot)

(defclass tick (line)
  ;; a tick is just a subclass of line, but it also knows where on the
  ;; axis it lives. this is a data coordinate 
  ((axis-coord :accessor tick-axis-coord
               :initarg :axis-coord
               :initform nil)
   ;; is it a label?
   (label :accessor tick-label
          :initarg :label
          :initform nil)
   ;; is it minor or major?
   (type :accessor tick-type
         :initarg :type
         :initform nil)))

(defmethod draw-plot-object ((tt tick) context)
  ;; do some stuff here to transform from the data coordinates to
  ;; physical coordinates; as a rule a tick doesn't need to know its
  ;; image coordinates until it's ready to be drawn, so we can do the
  ;; calculation here
  (call-next-method))

