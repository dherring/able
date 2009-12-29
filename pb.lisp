(defpackage :able.pb
  (:use :cl :ltk :able))

(in-package :able.pb)

(defun package-browser ()
  (with-ltk ()
    (let* ((top (make-instance 'frame))
           (tree (make-instance 'treeview
                                :master top))
           (sc (make-instance 'scrollbar :master top)))
      (dolist (p (sort (mapcar #'package-name (list-all-packages))
                       #'string<))
        (make-instance 'ltk::treeitem
                       :tree tree
                       :text p
                       ))
      (configure tree "yscrollcommand" (format nil "~A set" (widget-path sc)))
      (configure sc "command" (format nil "~A yview" (widget-path tree)))

      ;;(grid top 0 0 :sticky :nwse)
      ;;(pack top :fill :both :expand t)
      ;;(grid tree 0 0)
      ;;(grid sc 0 1 :sticky :ns)
      (pack top :side :left :fill :both :expand t)
      (pack tree :side :left :fill :both :expand t)
      (pack sc :side :left :fill :y :expand nil)

      (bind tree "<1>" (lambda (ev)
                         (declare (ignore ev))
                         (let ((item (treeview-focus tree)))
                           (unless (string= item "")
                             (format t "package: ~A~%" (ltk::treeview-item tree item :text)))))))))
