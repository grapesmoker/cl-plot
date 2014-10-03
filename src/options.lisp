(in-package :plot)

;; a parameter to hold all of our options and the means for setting
;; them. it will actually hold a list of conses whose car is the
;; option and whose cdr is a function for setting those options

(defparameter *plot-options* '())

(defmacro define-option (option-name option-handler)
  "This macro takes as an argument an option and a handler that is used to
set that option. It stores it in the alist *plot-options*. option-name must
be a keyword, option-handler is the name of the function used to set it.
e.g. (define-option :line-width set-line-width)"
  (let ((option-handler-name (gensym)))
    `(progn
       (let ((,option-handler-name ',option-handler))
         (push (cons ,option-name ,option-handler-name) *plot-options*)))))

(defmacro with-options ((&rest options) &rest body)
  (if (oddp (length options))
      (error 'error "Odd number of arguments to with-options!"))
  `(progn
     (loop
        for (opt val) on '(,@options)
        for i upfrom 0
        if (evenp i)
        do
          (let ((option-handler (cdr (assoc opt *plot-options*))))
            (funcall option-handler val)))
     ,@body))

(defmacro with-test ((&rest args) &rest body)
  (format t "~A~%" args)
  `(progn
     ,@body))
  
