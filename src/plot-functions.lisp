(in-package :plot)

(defun plot (x y filename &key (style :point) (color *default-point-color*) (thickness *default-point-weight*)
                            (output-type :png) (im-height *default-im-height*) (im-width *default-im-width*))
  "The main interface to the plotter."
  ;; we'll want to handle multiple cases:
  ;; x, y are both integers -> plot dot at location (x, y)
  ;; x, y are both equal-length sequences -> plot y as function of x
  ;; x is a sequence but y is a list of sequences of equal length ->
  ;; plot all sequences in y as functions of x
  ;; x, y are both lists of sequences -> plot each y as function of
  ;; each x
  ;; x is a point or a sequence and y is a function -> plot mapcar y x
  ;; as function of x
  ;; x and y are both functions -> at each step, evaluate x, then
  ;; evaluate y(x), collect, and plot the results

  ;; function returns the axes object plotted

  ;; a bunch of assertions should go here about the data

  ;; this stuff is independent of the form of the data

  (let* ((axes (make-instance 'axes))
         (plot (make-instance 'plot :plot-type :scatter :axes (list axes)))
         (image (make-instance 'plot-image 
                               :filename filename
                               :output-type output-type
                               :height im-height
                               :width im-width
                               :plots (list plot)))
         (data (make-instance 'data-object      
                              :color color
                              :thickness thickness
                              :style style)))
    (setf (plot-object-parent plot) image)
    (setf (plot-object-parent axes) plot)

    (cond ((and (numberp x)
                (numberp y))
           (setf (data-obj-x-data data) (list x))
           (setf (data-obj-y-data data) (list y))
           (push data (axes-data axes))
           (setf (plot-object-parent data) axes))

          ((and (listp x)
                (listp y)
                (every #'numberp x)
                (every #'numberp y))
           (unless (= (length x) (length y))
             (error 'error))
           (setf (data-obj-x-data data) x)
           (setf (data-obj-y-data data) y))

          ((and (listp x)
                (listp y)
                (every #'listp x)
                (every #'listp y))
           (print "multiple lists")
           (setf (axes-data ax) nil)
           (unless (= (length x) (length y))
             (error 'error))
           (loop
              for x-sub in x
              for y-sub in y
              do
                (unless (and (= (length x-sub) (length y-sub))
                             (every #'numberp x-sub)
                             (every #'numberp y-sub))
                  (error 'error))
                (let ((new-data (make-instance 'data-object  
                                               :color color
                                               :thickness thickness
                                               :style style
                                               :parent axes
                                               :x-data x-sub
                                               :y-data y-sub)))
                  (push new-data (axes-data axes)))))
          (t ()))
    (when (axes-data axes)
      (format t "plotting ~%")
      (draw-plot-object image nil))))
