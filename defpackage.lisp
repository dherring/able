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

;;; Operating system detection.
#+(or (and :clisp :win32) (and :sbcl :win32) (and :ccl :windows))
  (push :able-windows *features*)
#+(or (and :clisp :unix (not :macos)) (and :sbcl :linux) (and :ccl :linux))
  (push :able-linux *features*)
#+(or (and :ccl :darwin) (and :clisp :macos) (and :sbcl :darwin))
  (push :able-macosx *features*)











