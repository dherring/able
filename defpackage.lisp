(defpackage able
  (:use :common-lisp)
  (:use :cl-user)
  (:export :start))

(defpackage tstree
  (:export node make-node add-node get-node memberp prefix-match add-metadata get-metadata)
  (:use :common-lisp)
  (:use :cl-user))

;;; SBCL needs to load it's introspection library.
#+:sbcl (require :sb-introspect)
