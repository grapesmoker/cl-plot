(in-package :plot)

;; a class that holds the actual data that gets plotted

(defclass data-object (plot-object)
  ((x-data :accessor data-obj-x-data
           :initarg :x-data
           :initform nil)
   (y-data :accessor data-obj-y-data
           :initarg :y-data
           :initform nil)
   (style  :accessor data-obj-style
           :initarg :style
           :initform nil)
   (color  :accessor data-obj-color
           :initarg :color
           :initform *default-point-color*)
   (thickness :accessor data-obj-thickness
              :initarg :thickness
              :initform *default-point-weight*)
   ;; type indicates how to actually render the data contained in the object
   (type :accessor data-obj-type
         :initarg :type
         :initform nil)))

