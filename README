                               TEMPLATE
                               ========

                           By Robert Smith

TEMPLATE is a package for templates and template functions, similar to
C++ templates.

Example:

;;; Define the templated function.
(define-templated-function add (type) (x y)
  `(locally (declare (type ,type x y))
     (the ,type (+ x y))))

;;; Instantiate it with a few types.
(instantiate-templated-function add fixnum)
(instantiate-templated-function add single-float)

;;; Call it.
(add '(fixnum) 1 2)
(add '(single-float) 1.0 2.0)
