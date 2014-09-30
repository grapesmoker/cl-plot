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
	   
