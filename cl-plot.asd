(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cairo2))

(defpackage :cl-plot
  (:nicknames :plot)
  (:use :cl :cairo)
  (:documentation "Plotting routines based on cl-cairo2"))


(defsystem "cl-plot"
  :name "cl-plot"
  :description "A plotting library in Lisp written using cl-cairo2."
  :version "0.1"
  :author "Jerry Vinokurov <jerryv@cmu.edu>"
  :license "GPL v3"
  :defsystem-depends-on (#:cl-cairo2)
  :depends-on (#:cl-cairo2)
  :components
  ((:module objects
	    :components
	    ((:file "plot-objects")
             (:file "line")
	     (:file "axes"))
	    :serial t)
   (:module tests
	    :components
	    ((:file "basic-tests")))

	     ))
