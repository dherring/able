(in-package :able.pb)

(defclass symbol-frame (frame)
  ((package :accessor sf-package
            :documentation "Show symbols in this package."
            :initform nil)
   (symbols :accessor sf-symbols
            :initform nil)
   (rows :accessor sf-rows
         :initform nil)
   (tree :accessor sf-tree)
   (sc :accessor sf-scroll)
   (font-width :accessor sf-font-width :initform 10))
  (:documentation
   "Render a table showing all the symbols in a package."))


(defmethod (setf sf-package) :after (package-name (sf symbol-frame))
  (with-accessors ((package sf-package)
                   (tree sf-tree)
                   (symbols sf-symbols)
                   (rows sf-rows))
      sf

    ;; remove any old symbols
    (dolist (symbol symbols)
      (let ((name (symbol-name symbol)))
        (treeview-delete tree name)))
    (setf symbols nil
          rows nil)

    ;; guard against do-symbols processing the same symbol multiple times (allowed behavior)
    (do-symbols (symbol package)
      (pushnew symbol symbols :test #'eq))
    
    (dolist (symbol (reverse symbols))
      (let* ((name (symbol-name symbol))
             (pack (symbol-package symbol))
             (pname (package-name pack))
             (values (list (concatenate
                            'string
                            (when (boundp symbol)
                              (if (constantp symbol)
                                  "c"
                                  "b"))
                            (when (fboundp symbol)
                              (let ((mods (concatenate
                                           'string
                                           (when (macro-function symbol) "m")
                                           (when (special-operator-p symbol) "o"))))
                                (if (string= mods "")
                                    "f"
                                    mods)))
                            (when (keywordp symbol) "k")
                            ;; this one is problematic
                            ;;(when (compiler-macro-function name) "M")
                            )
                           (length (symbol-plist symbol))
                           ;; status
                           (second (multiple-value-list (find-symbol name package)))
                           pname)))
        (push (cons name values) rows)
        (treeview-insert tree
                         :id name
                         :text name
                         :values values)))))


(defmethod initialize-instance :after ((sf symbol-frame)
                                       &key
                                       package-designator)
  (with-accessors ((tree sf-tree)
                   (sc sf-scroll)
                   (rows sf-rows)
                   (font-width sf-font-width))
      sf
    ;; create the subwidgets
    (setf tree
          (make-instance 'treeview
                         :master sf
                         :columns "{1 2 3 4}")
          sc
          (make-instance 'scrollbar
                         :master sf))

    ;; set up the columns
    (macrolet
        ((sort-col (accessor &optional (up '#'string<) (down '#'string>))
           `(let ((up t))
              (lambda ()
                (dolist (item (setf rows
                                    (stable-sort rows
                                                 (if up ,up ,down)
                                                 :key ,accessor)))
                  (treeview-move tree (car item)))
                (setf up (not up))))))
      (treeview-heading tree :#0
                        :text "name"
                        :command (sort-col #'first))
      (treeview-heading tree 1
                        :text "attrs"
                        :command (sort-col #'second))
      (treeview-column tree 1 :width (* 5 font-width))
      
      (treeview-heading tree 2
                        :text "#plist"
                        :command (sort-col #'third #'< #'>))
      (treeview-column tree 2 :width (* 5 font-width))
      
      (treeview-heading tree 3
                        :text "status"
                        :command (sort-col #'fourth))
      (treeview-column tree 3 :width (* 9 font-width))
      
      (treeview-heading tree 4
                        :text "package"
                        :command (sort-col #'fifth)))
    
    ;; connect the tree and scrollbar
    (configure tree "yscrollcommand" (format nil "~A set" (widget-path sc)))
    (configure sc "command" (format nil "~A yview" (widget-path tree)))

    ;; pack everything nicely
    (pack sf :side :left :fill :both :expand t)
    (pack tree :side :left :fill :both :expand t)
    (pack sc :side :left :fill :y :expand nil))
  (when package-designator
    (setf (sf-package sf) package-designator)))

(defun show-symbols (package-designator)
  "Simple wrapper around symbol-frame."
  (with-ltk ()
    (make-instance 'symbol-frame
                   :package-designator package-designator)))
