(in-package :able)

(defmacro when-string (string &optional (other t) &body body)
  "Evaluate body if string is valid (non-null and non-empty)."
  `(when (and ,string (> (length ,string) 0) ,other)
     ,@body))

(defmacro when-let ((var form) &body body) 
  `(let ((,var ,form)) 
     (when ,var 
       ,@body)))

(defmacro equal-case (keyform &body clauses) 
  `(cond ,@(loop for clause in clauses collect 
             (destructuring-bind (test exec) clause 
               (if (or (eql test t) (eql test 'otherwise)) 
                   `(t ,exec) 
                   `((equal ,keyform ,test) ,exec))))))

(defmacro with-status-msg (msg &rest body)
  `(progn (info-message ,msg) ,@body (no-message)))

(defmacro with-able-streams (stream &body body)
  "Bind all interesting streams to STREAM during BODY"
  `(let* ((stream (make-instance ,stream))
          (*standard-output* stream)
          (*trace-output* stream)
          ;(*error-output* stream)
          (*standard-input* stream)
          ;; rebinding *terminal-io* breaks SBCL's terminal debugger
          ;(*terminal-io* stream)
          ;; rebinding *debug-io* breaks Slime's debugger and SBCL's terminal
          ;(*debug-io* stream)
          )
     ,@body
     (flush stream)))

(defmacro with-temporary-value (place temporary &body body)
  "A simple transactional re-binding of a variable which is unwound at the
  end of the block. Note that the following semantics are in operation: an
  error condition raised by code in the body will be caught, the original
  value will be restored and the error condition wil be re-raised for the
  calling code to handle. Not intended as a replacement for specials!"
  `(let ((original ,place))
     (setf ,place ,temporary)
     (handler-case
       (progn ,@body)
       (error (ex)
         (progn
           (setf ,place original)
           (error ex))))
     (setf ,place original)))

(defmacro get-indent-level (token)
  "Deduce how much to indent based on the token supplied. The user
  can supply their own indentation rules in the configuration file."
  `(cond
     ((equal (char ,token 0) +lparen+) 1)
     ((equal (length ,token) 1) 3)
     ,@(mapcar #'(lambda (rule)
                   `((equalp ,token ,(car rule)) ,(cdr rule)))
         *indentation-rules*)
     (t 2)))



























