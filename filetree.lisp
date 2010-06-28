(defpackage :filetree
  (:use :cl :ltk))

(in-package :filetree)

(defclass file-tree (frame)
  ((tree :accessor ft-tree
         :initform nil)
   (scroll :accessor ft-scroll)
   (root-dir :accessor ft-root-dir
             :initform nil)
   (dir-paths :accessor ft-dir-paths
              :initform (make-hash-table :test 'equal)
              :documentation "store the path for unvisited directory nodes")))

(defun make-id () (symbol-name (gensym "FILETREE-ID-")))
(defun make-stub-id (id) (concatenate 'string id "-STUB"))

;; idea: lazy initialization of sub-trees.
;; Show a stub for directories; populate it when a directory is selected and is nonempty.
;; Needs a mechanism for relisting directories if things change...
(defun update-dir (ft parent)
  (let* ((stub (make-stub-id parent))
         (tree (ft-tree ft))
         (dir-paths (ft-dir-paths ft))
         (dir (gethash parent dir-paths))
         paths)
    (if dir
        (setf paths (cl-fad:list-directory dir))
        (print :no-dir))
    (unless (equal parent "{}")
      ;; the stub for "{}" is invalid TCL.
      (if (and paths
               (treeview-exists tree stub))
          (treeview-delete tree stub)
          (return-from update-dir)))
    (dolist (p paths)
      (if (cl-fad:directory-pathname-p p)
          (let* ((id (make-id))
                 (stub (make-stub-id id)))
            (setf (gethash id dir-paths) p)
            (treeview-insert tree
                             :parent parent
                             :id id
                             :text (car (last (pathname-directory p))))
            (treeview-insert tree
                             :parent id
                             :id stub))
          (let ((id (make-id))
                (name (pathname-name p))
                (type (pathname-type p)))
            (treeview-insert tree
                             :parent parent
                             :id id
                             :text (if type
                                       (concatenate 'string
                                                    name
                                                    "."
                                                    type)
                                       name)))))))

(defmethod (setf ft-root-dir) :after (root (ft file-tree))
  (setf (gethash "{}" (ft-dir-paths ft)) root)
  (update-dir ft "{}"))

(defmethod initialize-instance :after ((ft file-tree)
                                       &key
                                       root-directory)
  (with-accessors ((tree ft-tree)
                   (scroll ft-scroll))
      ft
    (setf tree
          (make-instance 'treeview
                         :master ft
                         :columns "{1 2 3 4}")
          scroll
          (make-instance 'scrollbar
                         :master ft))


    (treeview-heading tree :#0
                      :text "name")
    (treeview-heading tree 1
                      :text "attrs")
    
    ;; connect the tree and scrollbar
    (configure tree "yscrollcommand" (format nil "~A set" (widget-path scroll)))
    (configure scroll "command" (format nil "~A yview" (widget-path tree)))
    ;; pack everything nicely
    (pack ft :side :left :fill :both :expand t)
    (pack tree :side :left :fill :both :expand t)
    (pack scroll :side :left :fill :y :expand nil)

    (bind tree "<<TreeviewOpen>>"
          (lambda (e)
            (declare (ignore e))
            (update-dir ft (treeview-focus tree)))))

  (when root-directory
    (setf (ft-root-dir ft) root-directory)))

(defun show-directory (&optional (directory *default-pathname-defaults*))
  "Simple wrapper around file-tree."
  (with-ltk ()
    (make-instance 'file-tree
                   :root-directory directory)))
