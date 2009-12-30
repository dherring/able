(defpackage :able.pb
  (:use :cl :ltk :able))

(in-package :able.pb)

(defun package-browser ()
  (with-ltk ()
    (let* ((top (make-instance 'frame))
           (tree (make-instance 'treeview
                                :master top
                                :columns 2
                                ))
           (sc (make-instance 'scrollbar :master top)))

      (format-wish "~a heading #~a -~(~a~) ~a" (widget-path tree) 0 :text "name")
      (format-wish "~a heading #~a -~(~a~) ~a" (widget-path tree) 1 :text "#syms")
      
      (dolist (p (sort (mapcar #'package-name (list-all-packages))
                       #'string<))
        (make-instance 'ltk::treeitem
                       :tree tree
                       :text p
                       ;; need to pass -values {name #syms}...
                       ))
      (configure tree "yscrollcommand" (format nil "~A set" (widget-path sc)))
      (configure sc "command" (format nil "~A yview" (widget-path tree)))

      (pack top :side :left :fill :both :expand t)
      (pack tree :side :left :fill :both :expand t)
      (pack sc :side :left :fill :y :expand nil)

      (bind tree "<1>" (lambda (ev)
                         (declare (ignore ev))
                         (let ((item (treeview-focus tree)))
                           (unless (string= item "")
                             (format t "package: ~A~%" (ltk::treeview-item tree item :text)))))))))

(defun comma-sep-string (list)
  (format nil "~{~A~#[~:;, ~]~}" list))
(defun sorted-package-names (package-list)
  (comma-sep-string
   (sort (mapcar #'package-name package-list)
         #'string<)))
(defun show-package (name)
  #| TODO: add buttons to manipulate the package
  intern, import, export, rename, delete, use-package, etc.
  Basically everything in CLHS 11.2.
  |#
  (with-ltk ()
    (let* ((package (find-package name))
           (top (make-instance 'frame))
           (row 0))
      ;;(wm-title *tk* "show-package")
      (pack top :fill :both :expand t)
      (flet ((show (name text)
               (let ((tag (make-instance 'label
                                         :master top
                                         :text name))
                     (value (make-instance 'label
                                           :master top
                                           :text text)))
                 (grid tag row 0 :sticky :w)
                 (grid value row 1 :sticky :ew)
                 (incf row)
                 ;;(pack tag)
                 ;;(pack value)
                 )))
        (show "name" (or (package-name package)
                         "<no-name>"))
        (show "nicknames" (comma-sep-string (package-nicknames package)))
        (show "shadows" (comma-sep-string
                         (sort (mapcar #'symbol-name
                                       (package-shadowing-symbols package))
                               #'string<)))
        (show "uses" (sorted-package-names (package-use-list package)))
        (show "used by" (sorted-package-names (package-used-by-list package)))
        ))))
