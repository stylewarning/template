;;;; package.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(defpackage #:template
  (:use #:cl)
  (:export
   #:template                           ; STRUCT, MACRO
   #:define-template                    ; MACRO
   #:expand-template                    ; FUNCTION
   #:instantiate-template               ; MACRO
   
   #:define-templated-function          ; MACRO
   #:instantiate-templated-function     ; MACRO
   )
  )

