(in-package :tstree)

(defstruct node
  splitchar
  value
  metadata
  lo-kid
  eq-kid
  hi-kid)

(defun add-node (tree key &key (metadata nil))
  (labels ((add-node-inner (tree key metadata index)
             (let* ((this-char (char key index))
                    (split-char (node-splitchar tree)))
               (cond ((char= this-char split-char)
                      (incf index)
                      (cond ((= index (length key))
                             (setf (node-value tree) key)
                             (setf (node-metadata tree) metadata))
                            (t (when (not (node-eq-kid tree))
                                 (setf (node-eq-kid tree)
                                   (make-node :splitchar this-char)))
                               (add-node-inner (node-eq-kid tree) key metadata index))))
                     ((char< this-char split-char)
                      (unless (node-lo-kid tree)
                        (setf (node-lo-kid tree)
                          (make-node :splitchar this-char)))
                      (add-node-inner (node-lo-kid tree) key metadata index))
                     ((char> this-char split-char)
                      (unless (node-hi-kid tree)
                        (setf (node-hi-kid tree)
                          (make-node :splitchar this-char)))
                      (add-node-inner (node-hi-kid tree) key metadata index))))))
      (add-node-inner tree key metadata 0)))

(defun get-node (tree key)
  (labels ((get-node-inner (tree key index)
             (when tree
               (cond ((eq (char key index) (node-splitchar tree))
                      (incf index)
                      (if (eq index (length key))
                          tree
                          (get-node-inner (node-eq-kid tree) key index)))
                     ((char< (char key index) (node-splitchar tree))
                      (get-node-inner (node-lo-kid tree) key index))
                     ((char> (char key index) (node-splitchar tree))
                      (get-node-inner (node-hi-kid tree) key index))))))
    (get-node-inner tree key 0)))

(defun memberp (tree key)
  (let ((item (get-node tree key)))
    (and item (node-value item))))

(defun add-metadata (tree key metadata)
  (setf (node-metadata (get-node tree key)) metadata))

(defun get-metadata (tree key)
  (let ((node (get-node tree key)))
    (when node
      (node-metadata node))))

(defun flatten-sub-tree (tree results)
  (let ((this-val (node-value tree)))
    (if this-val (push (node-value tree) results)))
  (when (node-lo-kid tree)
    (setf results (flatten-sub-tree (node-lo-kid tree) results)))
  (when (node-eq-kid tree)
    (setf results (flatten-sub-tree (node-eq-kid tree) results)))
  (when (node-hi-kid tree)
    (setf results (flatten-sub-tree (node-hi-kid tree) results)))
  results)

(defun prefix-match (tree key)
  (when (and tree key (not (equal key "")))
    (let ((match-node (get-node tree key)))
      (when match-node            
        (let (retval)
          (when (node-eq-kid match-node)
            (flatten-sub-tree (node-eq-kid match-node) retval)))))))











