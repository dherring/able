(in-package :able)

(defun make-symbol-metadata (type line file)
  (let ((symbol (gensym)))
    (setf (symbol-plist symbol)
      (list :type type :filename file :line line))
    symbol))

(defun parse-symbol-definition (string defining-form filename line-no)
  (let ((position (search defining-form string)))
    (when position
      (let* ((symbol-starts (+ (length defining-form) position 1))
             (symbol-ends (position #\Space string :start symbol-starts))
             (name (subseq string symbol-starts symbol-ends)))
        (tstree:add-node *symbols* name
          :metadata (make-symbol-metadata 'user line-no filename))))))

(defun parse-line (line line-no filename)
  (handler-case
    (progn
      (parse-symbol-definition line "defun" filename line-no)
      (parse-symbol-definition line "defmacro" filename line-no)
      (parse-symbol-definition line "defmethod" filename line-no)
      (parse-symbol-definition line "defparameter" filename line-no)
      (parse-symbol-definition line "defvar" filename line-no)
      (parse-symbol-definition line "defconstant" filename line-no)
      (parse-symbol-definition line "defclass" filename line-no)
      (parse-symbol-definition line "defstruct" filename line-no))
    (error (ex) nil)))

(defun load-definitions (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil 'foo)
          for i = 1 then (1+ i)
          until (eq line 'foo)
          do
          (parse-line line i filename))))

(defun load-definitions-for-directory (directory)
  (cl-fad:walk-directory directory
    (lambda (pathname)
        (load-definitions (namestring pathname)))
    :test (lambda (pathname) (lisp-file? (namestring pathname)))
    :if-does-not-exist :ignore))

(defun find-definition (name)
  (let ((definition (tstree:get-metadata *symbols* name)))
    (when definition
      (values (get definition :filename)
        (get definition :line)))))

(defun on-navigate-to-definition (symbol &optional (retry nil))
  (multiple-value-bind (file line) (find-definition symbol)
    (cond ((and file line)
           (open-file-at-line file line))
          ((not retry)
           (load-definitions-for-directory (get-last-directory))
           (on-navigate-to-definition symbol t))
          (retry
            (error-message "unable to find definition")))))

(defun open-file-at-line (file line)
  (open-file (correct-path file))
  (goto (get-current-text-ctrl *buffer-manager*) line))

