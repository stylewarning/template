;;;; templates.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:template)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun keywordify (x)
    (intern (symbol-name x) :keyword))
  (defun ensure-kw-list (x &optional first-elt)
    (mapcar #'keywordify
            (if (listp x)
                x
                (if first-elt
                    (list first-elt x)
                    (list x)))))
  
  (defun mangle (name &rest arguments)
    (let ((mangled-name (format nil "~S&&~{~S~^!!~}" name arguments)))
      (intern mangled-name (symbol-package name))))
  
  (defvar *templates* (make-hash-table))
  (defstruct template
    name
    arity
    code-generator
    function-arguments                  ; Optional
    )
  
  #+ignore
  (progn
    (set-macro-character
     #\[
     (lambda (stream char)
       (declare (ignore char))
       (let ((immediate nil)
             (next-char (read-char stream)))
         (if (char= #\! next-char)
             (setq immediate t)
             (unread-char next-char stream))
         (let ((contents (read-delimited-list #\] stream t)))
           (assert (not (null contents)) nil "The contents of [] cannot be empty!")
           (if immediate
               (apply #'mangle (car contents) (cdr contents))
               `(mangle ',(car contents) ,@(cdr contents)))))))
    
    (set-macro-character
     #\]
     (get-macro-character #\)))))

(defmacro template ((&rest params) &body body)
  `(make-template :arity ,(length params)
                  :code-generator (lambda (,@params)
                                    ,@body)))

(defmacro define-template (name params &body body)
  (let ((g (gensym)))
    `(let ((,g (template (,@params)
                 ,@body)))
       (setf (template-name ,g) ',name)
       (setf (gethash ',name *templates*) ,g)
       ',name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-template (name &rest args)
    (let ((template (gethash name *templates*)))
      (assert template (name) "The name ~S doesn't designate a template." name)
      (assert (= (length args)
                 (template-arity template))
              (args)
              "Expected ~D args, but received ~D."
              (template-arity template)
              (length args))

      ;; Generate the template code.
      (apply (template-code-generator template) args))))

(defmacro instantiate-template (name &rest args)
  (apply #'expand-template name args))

(defmacro define-templated-function (name (&rest template-args) (&rest lambda-list) &body body)
  (check-type name symbol)
  (assert (every #'symbolp template-args))
  `(progn
     (parameterized-function:define-dispatch-function ,name ,template-args ,lambda-list)
     (define-template ,name ,template-args ,@body)
     (setf (template-function-arguments (gethash ',name *templates*))
           ',lambda-list)
     ',name))

(defmacro instantiate-templated-function (name &rest template-values)
  (let ((lambda-list (template-function-arguments (gethash name *templates*))))
    `(progn
       (parameterized-function:define-parameterized-function ,name ,template-values ,lambda-list
         ,(apply #'expand-template name template-values)))))

#+#:example
(define-template add (s)
  `(defun ,[add s] (x y)
     (declare (type ,s x y))
     (the ,s (+ x y))))

#+#:example
(instantiate-template add single-float)

#+#:example
(define-templated-function add (type) (x y)
  `(locally (declare (type ,type x y))
     (the ,type (+ x y))))

#+#:example
(instantiate-templated-function add single-float)

#+ignore
(defmacro instantiate-template (name &rest args)
  (let ((template (gethash name *templates*)))
    (assert template (name) "The name ~S doesn't designate a template." name)
    (assert (= (length args)
               (template-arity template))
            (args)
            "Expected ~D args, but received ~D."
            (template-arity template)
            (length args))

    ;; Generate the template code.
    (apply (template-code-generator template) args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Other Stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+IGNORE
(progn
  (defmacro instantiate-template (template-var (&rest template-vals) &body body)
    (let ((template (gensym "TEMPLATE-")))
      `(macrolet ((,template (,template-var)
                    ,@body))
         ,@(loop :for template-val :in template-vals
                 :collect `(,template ,template-val)))))

  (defmacro instantiate-float-template (template-var &body body)
    `(instantiate-template ,template-var (short-float single-float double-float long-float)
       ,@body))

  (defmacro define-parametrically (template-var name args &body body)
    (flet ((expand-definition ()
             `(LIST* 'parameterized-function:define-parameterized-function
                     ',name
                     (LIST ,template-var)
                     ',args
                     ,@body)))
      `(progn
         (parameterized-function:define-dispatch-function ,name (type) (,@args))
         
         (instantiate-float-template ,template-var
           ,(expand-definition))))))

#+ignore
(progn
  #+old
  (defmacro instantiate-template (template-var template-values &body body)
    (let ((macro-names (mapcar (lambda (x)
                                 (declare (ignore x)) 
                                 (gensym))
                               template-values)))
      `(macrolet ,(loop :for macro-name :in macro-names
                        :collect `(,macro-name (,template-var)
                                               ,@body))
         ,@(loop :for macro-name :in macro-names
                 :for template-value :in template-values
                 :collect `(,macro-name ,template-value)))))

  #+medium
  (defmacro instantiate-template (template-var template-values &body body)
    (let ((macro-names (mapcar (lambda (x)
                                 (declare (ignore x)) 
                                 (gensym))
                               template-values)))
      `(macrolet ,(loop :for macro-name :in macro-names
                        :for template-value :in template-values
                        :collect `(,macro-name (,template-var)
                                               ,@(subst template-value template-var body)))
         ,@(loop :for macro-name :in macro-names
                 :for template-value :in template-values
                 :collect `(,macro-name ,template-value)))))

  #+new
  (defmacro instantiate-template (template-var template-values &body body)
    (flet ((expand-template (val body)
             (subst val template-var body)))
      `(progn
         ,@(loop :for template-value :in template-values
                 :append (expand-template template-value body)))))

  (defmacro instantiate-real-float-types (template-var &body body)
    `(instantiate-template ,template-var (short-float
                                          single-float
                                          double-float
                                          long-float)
       ,@body))

  (defmacro instantiate-complex-float-types (template-var &body body)
    `(instantiate-template ,template-var ((complex short-float) 
                                          (complex single-float)
                                          (complex double-float)
                                          (complex long-float))
       ,@body))

  (defmacro instantiate-float-types (template-var &body body)
    `(progn
       (instantiate-real-float-types ,template-var
         ,@body)
       (instantiate-complex-float-types ,template-var ,@body)))

  (defmacro define-float-function (template-var name args &body body)
    `(progn
       (parameterized-function:define-dispatch-function ,name (field float-type) ,args)
       (instantiate-float-types ,template-var
         `(parameterized-function:define-parameterized-function ,',name
              ,(ensure-kw-list ,template-var :real)
              ,',args
            ,@',body)))))

;;;; Testing
#+ignore
(instantiate-float-types <s>
  `(de)
  )

#+ignore
(progn
  (parameterized-function:define-dispatch-function f (field float-type) (x y))
  (instantiate-float-types <s>
    `(parameterized-function:define-parameterized-function f 
         ,(ensure-kw-list <s> :real) (x y)
       (+ x y))))
