(in-package :plot)

(defclass line (plot-object)
  ((start-x  :accessor line-start-x
	     :initarg :start-x
	     :initform nil)   
   (start-y  :accessor line-start-y
	     :initarg :start-y
	     :initform nil)
   (end-x  :accessor line-end-x
	   :initarg :end-x
	   :initform nil)   
   (end-y  :accessor line-end-y
	   :initarg :end-y
	   :initform nil)
   ;; line thickness
   (thickness :accessor line-thickness
	      :initarg :thickness)
   ;; don't know what "style" means yet, but things like dashed, etc.
   (style :accessor line-style
	  :initarg :style)
   ;; color as an rgb-triplet
   (color :accessor line-color
	  :initarg :color)))

;; it's not clear to me whether a line actually needs to have a
;; bounding box; put it in for now and we'll figure it out if we need
;; to remove it for some reason
(defmethod initialize-instance :after ((l line) &rest args)
  (declare (ignore args))
  (cond ((and (numberp (line-start-x l))
              (numberp (line-start-y l))
              (numberp (line-end-x l))
              (numberp (line-end-y l)))
         (let ((lower-left-x (min (line-start-x l) (line-end-x l)))
               (lower-left-y (min (line-start-y l) (line-end-y l)))
               (upper-right-x (max (line-start-x l) (line-end-x l)))
               (upper-right-y (max (line-start-y l) (line-end-y l))))
           (setf (bbox l) `(,lower-left-x ,lower-left-y ,upper-right-x ,upper-right-y))))))


(defmethod draw-line ((l line) context)
  ;; in reality this should be a generic (draw plot-object) method and
  ;; then we should just dispatch on the object type. also we don't
  ;; need to create a context in this function because it's so low
  ;; level, the context will be passed down to us from above
  (with-context (context)
    (destructuring-bind (r g b) (line-color l)
      (set-source-rgb r g b)
      (set-line-width (line-thickness l))
      (move-to (line-start-x l)
	       (line-start-y l))
      (line-to (line-end-x l)
	       (line-end-y l))
      (stroke))))

