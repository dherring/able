(in-package :able)

;;; Sequence and string utils

(defun split (string char)
  "Split string on char."
  (let ((string (string-right-trim " " string)))
    (loop for i = 0 then (1+ j)
      as j = (position char string :start i)
      collect (subseq string i j)
      while j)))

(defun string-replace (string from to)
  (map 'string (lambda (char)
                 (if (eq char from)
                     to
                     char))
    string))

(defun split-at (collection n)
  "Split a sequence at position 'n', returning both halves."
  (when (< n (length collection))
    (values (subseq collection 0 n)
      (subseq collection n))))

;(defun cl-user::sload (name)
;  (asdf:oos 'asdf:load-op name))

(defun take (list n)
  (let ((len (length list)))
    (butlast list (max (min len (- len n)) 0))))

(defun foldl (fun id list)
  (reduce (lambda (x y) (funcall fun x y)) list :initial-value id))
  
(defun randoms (count &optional (limit 64))
  "Generate a list of size 'count' random numbers bounded by 'limit'"
  (loop repeat count collect (random limit)))

(defun prefix-p (sequence prefix &optional (start 0))
  "Is 'prefix' the prefix of 'sequence', optionally starting at 'start'."
  (let ((seq-len (length sequence))
        (pre-len (length prefix)))
    (if (> (+ start pre-len) seq-len)
        nil
        (equalp (subseq sequence start (+ start pre-len)) prefix))))


;;;;;;;;;;;;;; file and directry handling ;;;;;;;;;;;;;;

(defun deduce-path-separator (pathstring)
  "Is this a forwardslash or backslash platform?"
  (if (find #\/ pathstring) #\/ #\\))

(defun dirname-from-pathstring (pathstring)
  (subseq pathstring 0
    (position
      (deduce-path-separator pathstring)
      pathstring :from-end t)))

(defun dirname-from-pathname (pathname)
  (dirname-from-pathstring (namestring pathname)))

(defun filename-from-pathname (pathname)
  (pathname-name (parse-namestring pathname)))

(defun filetype-from-pathstring (pathstring)
  (let ((start (position #\. pathstring :from-end t)))
    (when start
      (subseq pathstring (1+ start)))))

(defun filename-from-pathstring (pathstring)
  (let* ((separator (deduce-path-separator pathstring))
         (start (position separator pathstring :from-end t)))
    (when start
      (let ((end (position #\. pathstring :from-end t)))
        (if (and end (< end (length pathstring)) (> end start))
            (subseq pathstring (1+ start) end)
            (subseq pathstring (1+ start)))))))

(defun filetype? (path types)
  "Is the file specified in path of one of the types specified in types?"
  (member (filetype-from-pathstring path) types :test #'string=))

(defun lisp-file? (path)
  "Does path represent a lisp source file?"
  (filetype? path '("lisp" "cl" "l" "lsp")))

(defgeneric correct-path (pathname)
  (:documentation "Normalises all paths such that backslashes become forward slashes"))

(defmethod correct-path ((pathname pathname))
  (correct-path (namestring pathname)))

(defmethod correct-path ((pathname string))
  (string-replace pathname #\\ #\/))

;;;;;;;;;;;;;; functions for dealing with Tk text indices ;;;;;;;;;;;;;;

(defun get-row-integer (text-index)
  "Extracts the row number from a Tk text index."
  (let ((temp nil))
    (setf temp (first (split text-index #\.)))
    (setf temp (read-from-string temp))))

(defun get-col-integer (text-index)
  "Extracts the col number from a Tk text index."
  (let ((temp nil))
    (setf temp (second (split text-index #\.)))
    (setf temp (read-from-string temp))))

(defun text-row-add (text-index increment)
  "Increments the row of a Tk text index by increment (i.e. 5.0 -> 6.0)."
  (let ((temp (get-row-integer text-index)))
    (incf temp increment)
    (if (> temp 0)
        (format nil "~a.0" temp)
        "1.0")))

(defun text-col-add (text-index increment)
  "Increments the col of a Tk text index by increment (i.e. 4.1 -> 4.2)."
  (let ((row (get-row-integer text-index))
        (col (get-col-integer text-index)))
    (incf col increment)
    (format nil "~a.~a" row col)))

(defun strpos-to-textidx (str pos)
  "Convert a string position into a Tk text index."
  (let ((row 1)
        (col 0)
        (col-inc))
    (incf row (count #\Newline str :end pos))
    (setf col-inc (position #\Newline str :end pos :from-end t))
    (if col-inc
        (setf col (- pos col-inc 1))
        (setf col pos))
    (values (format nil "~a.~a" row col) row col)))

;;;;;;;;;;;;;; functions for dealing with strings ;;;;;;;;;;;;;;

(defun find-next-open (code-string start)
  (let ((x 0) (pos start))
    (loop while (> pos 0)
          while (>= x 0) do
          (decf pos)
          (case (char code-string pos)
            ((#\() (decf x))
             ((#\)) (incf x))))
  (if (< x 0) pos nil)))

(defun find-next-close (code-string start)
  (let ((x 0) (pos start))
    (labels ((find-next-close-inner (code-string)
               (when (< pos (length code-string))
                 (case (char code-string pos)
                   ((#\)) (decf x))
                 ((#\() (incf x)))
                 (when (>= x 0)
                   (incf pos)
                   (find-next-close-inner code-string)))))
      (find-next-close-inner code-string)
      (if (< x 0) pos nil))))

(defun find-current-sexp (code-string pos)
  (let ((start pos) (end pos))
    (labels ((get-inner-form ()
               (let* ((open (find-next-open code-string start))
                      (close (find-next-close code-string end)))
                 (when (and open close)
                   (setf start open)
                   (setf end (+ 1 close))
                   (when (and (> start 0) (< end (length code-string)))
                     (get-inner-form))))))
      (get-inner-form)
      (values (subseq code-string start end) start end))))

(defun trim-code (codestring)
  "Tidy strings front and back."
  (string-trim '(#\Newline #\Linefeed #\Space #\Tab #\Return) codestring))

(defun find-current-function (code-string pos)
  "From the cursor, finds the current function as token, start and end indices."
  (let ((next-open (find-next-open code-string pos))
        start end token)
    (when next-open
      (setf start (get-col-integer (strpos-to-textidx code-string next-open)))
      (loop :for i :from (1+ next-open) :below (length code-string)
         :do (if (member (char code-string i) '(#\Space #\Tab #\Newline) :test #'char=)
                 (setf next-open i)
                 (return)))
      (setf end (position #\Space code-string :start (1+ next-open) :test #'equal))
      (setf token (subseq code-string (+ next-open 1) end)))
    (values token start end)))

(defun longest-prefix-match (list)
  "Takes a list of strings and returns their longest common lexical prefix."
  (let ((best (if list (car list) ""))
        (longest most-positive-fixnum))
    (loop for this in (rest list) do
          (let ((len (mismatch this best)))
            (when (< len longest)
              (setf longest len)
              (setf best (subseq this 0 len)))))
    best))

;;;;;;;;;;;;;; environment ;;;;;;;;;;;;;;

(defun shutdown ()
  #+:clisp (ext:quit)
  #+:sbcl (sb-ext:quit)
  #+:ccl (ccl:quit))

(defun deliver ()
  ;;; SBCL seems to get upset if ABLE is running when this is called.
  ;;; It's best to load ABLE but not call start-able before invoking
  ;;; this on SBCL. Problem only manifests on Linux??
  #+:sbcl (sb-ext:save-lisp-and-die "able"
            :toplevel 'able:start
            :executable t)
  #+:clisp (ext:saveinitmem "able"
             :init-function 'able:start
             :executable t
             :quiet t
             :norc t)
  #+:ccl (ccl:save-application "able"
           :toplevel-function 'able:start
           :prepend-kernel t
           :error-handler :quit-quietly))

(defun function-lambda-list (fn)
  "Return an argument list for the supplied function."
  (let ((arglist))
    (handler-case
      #+:clisp (setf arglist (sys::arglist fn))
      #+:sbcl (setf arglist (sb-introspect:function-lambda-list fn))
      #+:ccl (setf arglist (ccl:arglist fn))
      (error (ex) (declare (ignore ex)) (setf arglist nil)))
    arglist))

(defun start-process (command-line)
  (let ((process))
    (progn
      #+:clisp
      (setf process
        (ext:run-program (car command-line) :arguments (cdr command-line)
          :input :stream :output :stream :wait t))
      #+:sbcl
      (let ((p (sb-ext:run-program (car command-line) (cdr command-line)
                 :input :stream :output :stream :error :output :wait nil :search t)))
        (setf process
          (make-two-way-stream
            (sb-ext:process-output p)
            (sb-ext:process-input p))))
      #+:ccl
      (let ((p (ccl:run-program (car command-line) (cdr command-line)
                 :input :stream :output t)))
        (setf process
          (make-two-way-stream
            (ccl:external-process-output-stream p)
            (ccl:external-process-input-stream p))))
      (sleep 1))
    process))
