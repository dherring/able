(defpackage #:able-asd
  (:use :cl :asdf))

(in-package :able-asd)

(defsystem able
  :name "able"
  :version "0.20"
  :author "Phil Armitage"
  :licence "MIT"
  :description "A Common Lisp editor"
  :long-description "A Common Lisp editor for MacOS X, Linux and Windows"
  :depends-on (:ltk :trivial-gray-streams :cl-fad :cl-ppcre :translate)
  :serial t
  :components ((:file "defpackage")
               (:file "config")
	       (:file "intl")
               (:file "macros")
               (:file "utils")
               (:file "tstree")
               (:file "gui")
               (:file "hyperspec")
               (:file "controls")
               (:file "parser")
               (:file "main")))































































