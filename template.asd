;;;; template.asd
;;;;
;;;; Copyright (c) 2014 Robert Smith

(asdf:defsystem #:template
  :description "A library for templates and template functions."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on (#:parameterized-function #:alexandria)
  :serial t
  :components ((:static-file "LICENSE")
               (:file "package")
               (:file "templates")))
