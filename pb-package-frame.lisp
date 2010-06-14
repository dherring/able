#| TODO: add buttons to manipulate the package
intern, import, export, rename, delete, use-package, etc.
Basically everything in CLHS 11.2.
|#

(in-package :able.pb)

(defclass package-frame (frame)
  ((package :accessor pf-package
            :documentation "Show information about this package.")
   (name :accessor pf-name)
   (doc :accessor pf-doc)
   (nickname :accessor pf-nickname)
   (shadows :accessor pf-shadows)
   (uses :accessor pf-uses)
   (used-by :accessor pf-used-by))
  (:documentation
   "Show various information about a package."))


(defmethod (setf pf-package) :after (package (pf package-frame))
  (setf (text (pf-name pf)) (or (package-name package)
                                "<no-name>")
        (text (pf-doc pf)) (documentation package t))
  (flet ((sorted-package-names (package-list)
           (sort (mapcar #'package-name package-list)
                 #'string<))
         (set-list (place list)
           (setf (text (car place)) (format nil "~A" (length list))
                 (text (cdr place)) (car list)
                 (options (cdr place)) list)))
    (set-list (pf-nickname pf)
              (package-nicknames package))
    (set-list (pf-shadows pf)
              (sort (mapcar #'symbol-name
                            (package-shadowing-symbols package))
                    #'string<))
    (set-list (pf-uses pf)
              (sorted-package-names
               (package-use-list package)))
    (set-list (pf-used-by pf)
              (sorted-package-names
               (package-used-by-list package)))
    ))

(defmethod initialize-instance :after ((pf package-frame)
                                       &key
                                       package-designator)
  (grid pf 0 0 :sticky :news)
  (let ((row 0))
    (flet ((show (name)
             (let ((tag (make-instance 'label
                                       :master pf
                                       :text name))
                   (value (make-instance 'label
                                         :master pf)))
               (grid tag row 0 :sticky :w)
               (grid value row 2 :sticky :ew)
               (incf row)

               value))
           (show-list (name)
             (let ((tag (make-instance 'label
                                       :master pf
                                       :text name))
                   (count (make-instance 'label
                                         :master pf))
                   (value (make-instance 'combobox
                                         :master pf
                                         :width 40
                                         :state :readonly)))
               (grid tag
                     row 0 :sticky :w)
               (grid count
                     row 1 :sticky :e)
               (grid value
                     row 2 :sticky :ew)
               (incf row)

               (cons count value))))

      (setf (pf-name pf) (show "name")
            (pf-doc pf)  (show "doc")
            (pf-nickname pf) (show-list "nicknames")
            (pf-shadows pf) (show-list "shadows")
            (pf-uses pf) (show-list "uses")
            (pf-used-by pf) (show-list "used by"))))
      
  (when package-designator
    (setf (pf-package pf) package-designator)))


(defun show-package (package-designator)
  "Simple wrapper around package-frame."
  (with-ltk ()
    (make-instance 'package-frame
                   :package-designator package-designator)))
