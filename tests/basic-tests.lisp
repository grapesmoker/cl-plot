(in-package :plot)

(defun basic-test (filename)
  (let* ((surface (create-image-surface :argb32 640 480))
	 (context (create-context surface))
	 (axis (make-instance 'axis
			      :thickness 5.0
			      :start-x 25
			      :start-y 25
			      :end-x 25
			      :end-y 275
			      :color '(0 0 0))))
    (unwind-protect
	 (with-context (context)
	   (set-source-rgb 1 1 1)
	   (paint)
	   (draw-axis axis context))
      (destroy context))
    (surface-write-to-png surface filename)))
	   
(defun draw-points-test (filename num-points)
  (let* ((surface (create-image-surface :argb32 640 480))
         (context (create-context surface)))
    (unwind-protect
         (with-context (context)
           (set-source-rgb 1 1 1)
           (paint)
           (dotimes (n num-points)
             (let* ((pt-x (random 640))
                    (pt-y (random 480))
                    (color `(,(random 1.0) ,(random 1.0) ,(random 1.0)))
                    (weight (random 10.0))
                    (new-point (make-instance 'point :x pt-x :y pt-y :color color :weight weight)))
               (draw-plot-object new-point context))))
      (destroy context))
    (surface-write-to-png surface filename)))


(defun draw-axes-test (filename)
  (let* ((surface (create-image-surface :argb32 640 480))
         (context (create-context surface)))
    (unwind-protect
         (with-context (context)
           (set-source-rgb 1 1 1)
           (paint)
           (let ((axes (make-instance 'axes
                                      :x-axis (make-instance 'axis 
                                                             :color '(0 0 0)
                                                             :thickness 5
                                                             :start-x 20
                                                             :start-y 460
                                                             :end-x 620
                                                             :end-y 460)
                                      :y-axis (make-instance 'axis
                                                             :color '(0 0 0)
                                                             :thickness 5
                                                             :start-x 20
                                                             :start-y 460
                                                             :end-x 20
                                                             :end-y 20))))
             (draw-plot-object axes context)))
      (destroy context))
    (surface-write-to-png surface filename)))

(defun draw-axes-test-2 (filename)
  (let* ((surface (create-image-surface :argb32 640 480))
         (context (create-context surface)))
    (unwind-protect
         (with-context (context)
           (set-source-rgb 1 1 1)
           (paint)
           (let* ((new-image (make-instance 'plot-image))
                  (new-plot (make-instance 'plot :parent new-image))
                  (axes (make-instance 'axes :parent new-plot)))
             (draw-plot-object axes context)))
      (destroy context))
    (surface-write-to-png surface filename)))

(defun draw-axes-with-data-test (filename x-data y-data)
  (let* ((surface (create-image-surface :argb32 640 480))
         (context (create-context surface)))
    (unwind-protect
         (with-context (context)
           (set-source-rgb 1 1 1)
           (paint)
           (let* ((new-image (make-instance 'plot-image))
                  (new-plot (make-instance 'plot :parent new-image))
                  (axes (make-instance 'axes :parent new-plot :data (list (make-instance 'data-object :x-data x-data :y-data y-data :color '(1 0 0))))))
                                       
             (draw-plot-object axes context)))
      (destroy context))
    (surface-write-to-png surface filename)))
  
(defun scale-test (filename num-points)
  (let* ((surface (create-image-surface :argb32 640 480))
         (context (create-context surface)))
    (unwind-protect
         (with-context (context)
           (set-source-rgb 1 1 1)
           (paint)
           (scale 640 480)
           (rotate (deg-to-rad 90))
           (dotimes (n num-points)
             (let* ((pt-x (random 1.0))
                    (pt-y (random 1.0))
                    (color `(,(random 1.0) ,(random 1.0) ,(random 1.0)))
                    (weight (random 100.0)))
               (multiple-value-bind (x y) (user-to-device pt-x pt-y)
                 (format t "x: ~F, y: ~F~%" x y)
                 (let ((new-point (make-instance 'point :x x :y y :color color :weight weight)))
                   (draw-plot-object new-point context))))))
      (destroy context))
    (surface-write-to-png surface filename)))

(defun draw-points-pdf-test (filename num-points)
  (let* ((context (create-pdf-context filename 640 480)))
    (unwind-protect
         (with-context (context)
           (set-source-rgb 1 1 1)
           (paint)
           (dotimes (n num-points)
             (let* ((pt-x (random 640))
                    (pt-y (random 480))
                    (color `(,(random 1.0) ,(random 1.0) ,(random 1.0)))
                    (weight (random 10.0))
                    (new-point (make-instance 'point :x pt-x :y pt-y :color color :weight weight)))
               (draw-plot-object new-point context))))
      (destroy context))))

;;    (surface-write-to-png surface filename)))
