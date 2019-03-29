(in-package :able)

(defconstant +lparen+ #\()
(defconstant +rparen+ #\))

(defparameter *editor-frame* nil "Container for the text-editor control")
(defparameter *listener* nil "The Lisp listener interface and command interpreter")
(defparameter *statusbar* nil "The bar at the bottom of the screen")
(defparameter *buffer-menubar* nil "store a reference to the buffer menubar for listing the open buffers")

;;; allocate sequential filenames for new files
(let ((untitled 0))
  (defun reset-untitled ()
    (setf untitled 0))
  (defun get-untitled ()
    (concatenate 'string
      #t"untitled #" (format nil "~d"
                     (incf untitled)))))

;;; maintain the last directory accessed by load and save operations
(let ((last-file (user-homedir-pathname)))
  (defun reset-last-directory ()
    (setf last-file (user-homedir-pathname)))
  (defun update-last-directory (filepath)
    (when (and filepath (not (string= filepath "")))
      (setf last-file (parse-namestring filepath))))
  (defun get-last-file ()
    last-file)
  (defun get-last-directory ()
    (substitute #\/ #\\ (dirname-from-pathname (get-last-file)))))

(defun get-selected-text ()
  (ltk::selected (get-current-text-ctrl *buffer-manager*)))

(defun load-symbol-tree ()
  "Load symbol metadata used for syntax highlighting, indentation,
  symbol completion, hyperspec lookup and call-tips."
  (setf *symbols* (tstree:make-node :splitchar #\m))
  (do-symbols
    (s (find-package 'common-lisp))
    (tstree:add-node *symbols*
      (string-downcase (symbol-name s))
      :metadata (make-symbol-metadata 'system nil nil))))

(defun error-message (msg)
  (set-message *statusbar* msg *highlight-error*))

(defun info-message (msg &optional (color *highlight-text*))
  (set-message *statusbar* msg color))

(defun no-message ()
  (set-message *statusbar* ""))

(defun pathname-message ()
  (let* ((buffer (selected-buffer *buffer-manager*))
         (file-path (file-path buffer))
         (new-file (new-buffer-p buffer))
         (saved-file (saved-buffer-p buffer))
         (unsaved (or new-file (not saved-file))))
    (ltk:wm-title ltk::*tk* (format nil "~a ~:[~;[*]~]" file-path unsaved))))

(defmethod sexp-before-cursor ((txt ltk:text) &optional (str (ltk::get-text-to-cursor txt)))
  "Get the SEXP preceding the current cursor position."
  (let* ((len (length str))
         (end (- len 1))
         start)
    (when (and (>= end 0) (< end (length str)))
      (when (equal (char str end) +rparen+)
        (setf start (find-next-open str end))
        (when (and start (> start -1))
          (subseq str start len))))))

(defmethod get-token-prefix ((text ltk:text))
  (let* ((str (ltk::get-current-line-to-cursor text))
         (start (position '(#\( #\Space #\: #\Newline #\Tab #\))
                  str :from-end t :test (lambda (o li) (member li o))))
         (end (length str))
         token)
    (when (and start end)
      (subseq str (1+ start) end))))

(defmethod show-calltip ((txt ltk:text) &optional (str (ltk::get-text-to-cursor txt)))
  (let* ((pos (length str))
         (token (find-current-function
                  (ltk::get-to-end-current-line txt) pos))
         args)
    (if token
        (progn
          (setf args (load-arglist token))
          (if args
              (info-message
                (remove #\Newline
                  (format nil "~a"
                    (cons (string-upcase token) args))))
              (hide-calltip)))
        (hide-calltip))))

(defun hide-calltip ()
  (no-message))

(defun load-arglist (token)
  (let* ((symb))
    (setf symb
      (find-symbol
        (string-upcase
          (string-trim '(#\Space #\Newline #\Linefeed)
            token))
        'common-lisp))
    (when symb
      (function-lambda-list symb))))

(defmethod hide-paren-match ((txt ltk:text))
  (ltk::remove-tag txt "parens" "1.0" "end"))

(defmethod match-paren ((txt ltk:text) &optional (str (ltk::get-text-to-cursor txt)))
  (let* ((pos (- (length str) 1))
         (next) (start) (end)
         (cursor-pos (ltk::get-cursor-pos txt)))
    (hide-paren-match txt)
    (when (and (>= pos 0) (< pos (length str)))
      (when (equal (char str pos) +rparen+)
        (setf next (find-next-open str pos))
        (when (and next (> next -1))
          (setf start (strpos-to-textidx str next))
          (setf end (text-col-add start 1))
          (ltk::add-tags txt (format nil
                               "~a ~a ~a-1c" start end cursor-pos cursor-pos)
            "parens"))))))

(defmethod get-current-token ((text ltk:text))
  (let ((str (ltk:text text))
        (str-to-cursor (ltk::get-text-to-cursor text)))
    (when (and str-to-cursor (> (length str-to-cursor) 0))
      (let* ((pos (length str-to-cursor))
             (start (position-if
                      (lambda (x)
                        (member x '(#\( #\Space #\: #\'))) str :end pos :from-end t))
               (end (position-if
                      (lambda (x)
                        (member x '(#\) #\Space #\Newline))) str :start pos))
             token)
        (unless start (setf start -1))
        (unless end (setf end (length str)))
        (setf start (1+ start))
        (setf token (string-trim " " (subseq str start end)))
        (when (string= token "") (setf token nil))
        (values token start end)))))

(defmethod indent-current-line ((txt ltk:text)
                                &optional (text-to-cursor (ltk::get-text-to-cursor txt)))
  "Indents code as you type using the extensible rules of GET-INDENT-LEVEL."
  (let ((str (ltk:text txt))
        (pos 0) (indent))
    (setf pos (- (length text-to-cursor) 1))
    (when (and (> pos -1) (<= pos (length str)))
      (multiple-value-bind (token start end) (find-current-function str pos)
        (declare (ignore end))
        (when token
          (setf indent (get-indent-level token))
          (setf start (+ start indent))
          (ltk::insert-text txt (make-string start :initial-element #\Space)))))))

(defun unformat-lisp-form (code-string)
  "Strips out the indentation for a block of code. Also 'corrects'
  code where Tab has been used for indentation."
  (let ((reading t) (new-str ""))
    (loop for c across code-string do
      (case c
        (#\Newline
          (setf new-str (concatenate 'string new-str (string c)))
          (setf reading nil))
        (otherwise
          (if (and (not reading) (not (member c '(#\Space #\Tab))))
              (setf reading t))))
      (when reading
        (setf new-str (concatenate 'string new-str (string c)))))
    new-str))

(defun format-lisp-form (code-string)
  "Re-indents the supplied code-string."
  (let* ((new-str "") (spaces "") (token "")
         (last-open 0) (last-newline 0) (next-space 0))
    (when code-string
      (setf code-string (unformat-lisp-form code-string))
      (loop for c across code-string do
        (setf new-str (concatenate 'string new-str (string c)))
        (case c
          (#\Newline
            (progn
              (setf last-open (find-next-open new-str (1- (length new-str))))
              (when last-open
                (setf last-newline (position #\Newline new-str
                                     :test #'equalp :end last-open :from-end t))
                (when (not last-newline) (setf last-newline -1))
                (setf next-space (position #\Space new-str :test #'equalp :start last-open))
                (setf token (subseq new-str (1+ last-open) next-space))
                (let ((num-spaces (+ (get-indent-level token) (- last-open (1+ last-newline)))))
                  (setf spaces (make-string num-spaces :initial-element #\Space)))
                (setf new-str (concatenate 'string new-str spaces))))))))
    new-str))

(defmethod indent-block ((txt ltk:text))
  "Formats (re-indents) the outer block of code surrounding the cursor."
  (let ((str (ltk:text txt))
        (new-str "")
        (cur-pos (length (ltk::get-text-to-cursor txt))))
    (handler-case
      (multiple-value-bind (block start end) (find-current-sexp str cur-pos)
        (let ((new-str (format-lisp-form block))
              (start-index (strpos-to-textidx str start))
              (end-index (strpos-to-textidx str end)))
          ;; the pretty printer likes adding extra newlines (see e.g. http://www.clisp.org/impnotes/faq.html#faq-pp-newline)
          ;; these generally aren't appropriate when just changing indentation...
          #| leaves two steps in the undo history:
          (ltk::delete-text txt start-index end-index)
          (let ((*print-pretty* nil))
            (ltk::insert-text txt new-str))
          |#
          (let ((*print-pretty* nil))
            (ltk::format-wish "~a replace ~a ~a \"~a\"" (ltk::widget-path txt) start-index end-index (ltk::tkescape new-str)))
          (apply-highlight txt start-index end-index)))
      (error (ex) (error-message ex)))))

(defmethod apply-highlight ((txt ltk:text) start end)
  (let ((row 1) (col 0) token-start (cur-token "") keywords comments user
        (sub-string "") (state 'none))
    (flet ((add-token ()
             (when-string cur-token
               (let ((metadata (tstree:get-metadata *symbols* cur-token)))
                 (when metadata
                   (case (get metadata :type)
                     (user (setf user (concatenate 'string
                                        user " " token-start " "
                                        (format nil "~a.~a" row col))))
                     (system (setf keywords (concatenate 'string
                                              keywords " " token-start " "
                                              (format nil "~a.~a" row col)))))))))
           (add-comment ()
             (setf comments (concatenate 'string comments " " (format nil "~a.~a" row col))))
           (un-highlight-range ()
             (ltk::remove-tag txt "keywords" start end)
             (ltk::remove-tag txt "user" start end)
             (ltk::remove-tag txt "comments" start end)))
      (un-highlight-range)
      (setf sub-string (ltk::get-text-range txt start end)
        row (get-row-integer start) col (get-col-integer start))
      (loop for c across sub-string do
            (cond ((and (member c '(#\Space #\( #\))) (eq state 'token))
                   (add-token)
                   (setf state 'none cur-token ""))
                  ((and (equal c #\Newline) (eq state 'token))
                   (add-token)
                   (setf state 'none cur-token "" col -1)
                   (incf row))
                  ((and (equal c #\Newline) (eq state 'comment))
                   (add-comment)
                   (setf state 'none col -1)
                   (incf row))
                  ((and (equal c #\Newline) (eq state 'none))
                   (setf col -1)
                   (incf row))
                  ((and (equal c +lparen+) (not (eq state 'comment)))
                   (setf token-start (format nil "~a.~a" row (1+ col)) state 'token))
                  ((and (equal c #\;) (not (eq state 'comment)))
                   (add-comment)
                   (setf state 'comment))
                  ((eq state 'token)
                   (setf cur-token (format nil "~a~a" cur-token c))))
            (incf col))
      (when user (ltk::add-tags txt user "user"))
      (when keywords (ltk::add-tags txt keywords "keywords"))
      (when comments (ltk::add-tags txt comments "comments")))))

(defmethod highlight ((txt ltk:text) event &optional amount)
  (let ((cur-pos (ltk::get-cursor-pos txt)))
    (cond ((member event '(return delete))
           (apply-highlight txt (text-row-add cur-pos -1) (text-row-add cur-pos 1)))
          ((member event '(text space complete))
           (apply-highlight txt (text-row-add cur-pos 0) (text-row-add cur-pos 1)))
          ((equal event 'paste)
           (apply-highlight txt (text-row-add cur-pos (- amount)) (text-row-add cur-pos 1)))
          ((equal event 'load)
           (with-status-msg #t"highlighting..." (apply-highlight txt "1.0" "end"))))))

(defmethod goto ((text ltk:text) line)
  (let ((cursor-pos (format nil "~a.0" line)))
    (ltk::scroll-to text cursor-pos)
    (ltk::set-cursor-pos text cursor-pos)
    (ltk::force-focus text)))

(let (matches (next-match 0) (prefix ""))
  (defun clear-completion-data ()
    (setf matches nil next-match 0 prefix ""))
  (defun get-next-completion ()
    (let ((num-completions (1- (length matches))))
      (if (< next-match num-completions)
          (incf next-match)
          (setf next-match 0))))
  (defmethod code-complete ((text ltk:text))
    (unless matches
      (setf prefix (get-token-prefix text))
      (setf matches (append
                      (reverse
                        (tstree:prefix-match *symbols*
                          (string-downcase prefix)))
                      (list prefix))))
    (when (> (length matches) 1)
      (ltk::delete-chars text (length prefix))
      (ltk::insert-text text (elt matches next-match))
      (setf prefix (elt matches next-match))
      (get-next-completion))))

(let ((last-search "") (last-find-at 0))
  (defmethod search-text ((editor ltk:text))
    (find-text editor (input-prompt *listener*
                        #t"find:" (ltk::selected editor))))
  (defmethod search-text-again ((editor ltk:text))
    (find-text editor last-search))
  (defmethod find-text ((editor ltk:text) search-text)
    (let* ((txt-to-find-len (length search-text))
           (txt-to-search (ltk:text editor))
           (start-pos) (end-pos) (start-idx) (end-idx))
      (ltk::remove-tag editor "sel" "1.0" "end")
      (when (not (string-equal search-text last-search))
        (setf last-search search-text)
        (setf last-find-at 0))
      (cond ((< last-find-at (length txt-to-search))
             ; Search for the text string starting from the last found position
             ; and if it's not found, try again from the start (wrap).
             (setf start-pos (search search-text txt-to-search :start2 last-find-at))
             (unless start-pos
               (setf start-pos (search search-text txt-to-search :start2 0))
               (info-message #t"search wrapped around file")))
            (t
              ; Previous search result was found in a longer file which has set
              ; the last found result beyond the maximum index of the text being
              ; searched, so search from the start of this file.
              (setf start-pos (search search-text txt-to-search :start2 0))))
      (case start-pos
        ((nil)
         (setf last-find-at 0)
         (error-message #t"search reached end of file"))
        (otherwise
          (setf start-idx (strpos-to-textidx txt-to-search start-pos))
          (setf end-pos (+ start-pos txt-to-find-len))
          (setf end-idx (strpos-to-textidx txt-to-search end-pos))
          (setf last-find-at end-pos)
          (ltk::add-tag editor "sel" start-idx end-idx)
          (ltk:see editor start-idx)
          (ltk::set-cursor-pos editor start-idx)
          (ltk::force-focus editor))))))

(defun hyperspec-lookup (token)
  "Look-up the current symbol in the Hyperspec."
  (let ((url (gethash token *hyperspec-index*)))
    (when url
      (let ((url (format nil "~a~a" *hyperspec-root* url)))
        (handler-case
          (start-process (list *web-browser* url))
          (error (ex) (error-message ex)))))))

(defun add-key-binding (control binding function &optional (exclusive nil))
  (ltk::remove-binding "Text" binding)
  (ltk::remove-binding "Toplevel" binding)
  (ltk::un-bind control binding)
  (ltk:bind control binding function :exclusive t))
    
(defclass user-stream (trivial-gray-streams:trivial-gray-stream-mixin
                        trivial-gray-streams:fundamental-input-stream
                        trivial-gray-streams:fundamental-output-stream)
  ((input-buffer
     :accessor input-buffer
     :initform :eof)
   (output-buffer
     :accessor output-buffer
     :initform "")
   (index
    :accessor index
    :initform -1)
   (wrstamp
    :accessor wrstamp
    :initform 0))
  
  (:documentation "Provides a bi-directional stream to act as a conduit
    for the user input and output in the listener. Typically bind an
    instance of this to all interesting streams during evaluation."))

(defmethod trivial-gray-streams:stream-read-char ((user-stream user-stream))
  "Get program input when issued from a call to READ-CHAR. Note that the
  complexity here results from the need to handle repeated calls from
  READ hence the maintaining a buffer of the data read from."
  (labels ((reached-end-of-output ()
             "In repeated calls to READ, need to force immediate output"
             (flush user-stream)
             #\Space))
    (handler-case
      (progn
        (incf (index user-stream))
        (cond ((eq (input-buffer user-stream) :eof)
               (setf (input-buffer user-stream) (input-prompt *listener* "?"))
               (char (input-buffer user-stream) (index user-stream)))
              ((= (index user-stream) (length (input-buffer user-stream)))
               (reached-end-of-output))
              (t (char (input-buffer user-stream) (index user-stream)))))
      (error (ex) (declare (ignore ex)) (reached-end-of-output)))))

(defmethod trivial-gray-streams:stream-clear-input ((user-stream user-stream))
  (setf (input-buffer user-stream) :eof))

(defmethod trivial-gray-streams:stream-read-line ((user-stream user-stream))
  "Get program input when issued from a call to READ-LINE."
  (input-prompt *listener* "?"))

(defmethod trivial-gray-streams:stream-peek-char ((user-stream user-stream))
  (let ((character (stream-read-char user-stream)))
    (unless (eq character :eof)
      (stream-unread-char user-stream character))
    character))

(defmethod trivial-gray-streams:stream-unread-char ((user-stream user-stream) c)
  "Simulate the effect of moving back in the stream by decrementing
  the pointer into the internally held input-buffer."
  (decf (index user-stream)))

(defmethod trivial-gray-streams:stream-write-char ((user-stream user-stream) character)
  (buffer-output user-stream character))

(defmethod trivial-gray-streams:stream-write-string ((user-stream user-stream) seq &optional start end)
  (buffer-output user-stream (subseq seq start end)))

(defmethod trivial-gray-streams:stream-write-sequence ((user-stream user-stream) seq start end &key)
  (buffer-output user-stream (subseq seq start end)))

(defmethod trivial-gray-streams:stream-terpri ((user-stream user-stream))
  (trivial-gray-streams:stream-write-char user-stream #\Newline))

(defmethod trivial-gray-streams:stream-fresh-line ((user-stream user-stream))
  nil)

(defmethod trivial-gray-streams:stream-line-column ((user-stream user-stream))
  nil)

#+:sbcl
(defmethod sb-gray:stream-line-length ((user-stream user-stream))
  nil)

(defmethod buffer-output ((user-stream user-stream) string)
  "Buffer 100 characters of output to reduce calls across the sub-process to Tk"
  (setf (output-buffer user-stream)
    (format nil "~A~A" (output-buffer
			user-stream) string))
  (let* ((timestamp (get-internal-real-time))
	 (elapsed (- timestamp (wrstamp user-stream))))
    (when (or (> elapsed 200;internal-time-units-per-second
		 )
	      (> (length (output-buffer user-stream)) 100))
      (flush user-stream)
      (setf (wrstamp user-stream) timestamp))))

(defmethod flush ((user-stream user-stream))
  (output *listener* (output-buffer user-stream))
  (setf (output-buffer user-stream) "")
  (finish-output user-stream)
  (setf (input-buffer user-stream) :eof)
  (setf (index user-stream) -1))

(defclass buffer (ltk:scrolled-text)
  ((edit-ctrl
     :accessor edit-ctrl)
   (file-path
     :initarg :file-path
     :accessor file-path)
   (plaintextp
     :initform nil
     :accessor plaintextp)))

(defclass buffer-manager ()
  ((buffers
     :initarg :buffers
     :initform '()
     :accessor buffers)
   (selected-buffer
     :initarg :selected-buffer
     :initform nil
     :accessor selected-buffer)))

(defmethod get-buffer-index ((buffer-manager buffer-manager) path-name)
  (let ((count -1))
    (loop for buffer in (reverse (buffers buffer-manager)) do
      (incf count)
      (when (string= path-name (file-path buffer))
        (return count)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *text-keys*
    (list ;; format: (key action &optional suppress-in-repl)
     (list *key-cut* 'on-cut t)
     (list *key-copy* 'on-copy)
     (list *key-paste* 'on-paste)
     (list *key-line-start* 'on-cursor-line-start)
     (list *key-line-end* 'on-cursor-line-end)
     (list *key-forward* 'on-cursor-forward)
     (list *key-backward* 'on-cursor-backward)
     (list *key-select-all* 'on-select-all t)
     (list *key-reformat* 'on-re-indent t)
     (list *key-macro-expand* 'on-macro-expand t)
     (list *key-copy-to-repl* 'on-copy-sexp-to-repl t)
     ;; suppress code-complete; it needs special treatment
     (list *key-code-complete* 'on-code-complete t)
     ;; lookup should be suppressed unless (plaintextp txt)
     (list *key-lookup* 'on-lookup-definition))))

(defmethod initialize-instance :after ((txt buffer) &key)
  (setf (edit-ctrl txt) (ltk:textbox txt))
  (ltk:grid-forget (ltk::hscroll txt))  ; remove the horizontal scroll-bar
  (let ((edit-ctrl (ltk:textbox txt)))
    (ltk:configure (edit-ctrl txt) :font *buffer-font* :background *highlight-background*
      :foreground *highlight-text* :relief :groove :undo 0 :insertbackground *highlight-text*)
    (ltk:bind edit-ctrl "<KeyPress>"
      (lambda (evt) (unless (plaintextp txt) (on-key-down edit-ctrl evt))))
    (ltk:bind edit-ctrl "<FocusIn>"
      (lambda (evt) (declare (ignore evt)) (unless (plaintextp txt) (on-focus-in edit-ctrl))))
    (ltk:bind edit-ctrl "<FocusOut>" (lambda (evt) (declare (ignore evt)) (on-focus-out edit-ctrl)))
    (ltk:bind edit-ctrl "<Button-1>"
      (lambda (evt) (declare (ignore evt)) (unless (plaintextp txt) (on-left-click edit-ctrl))))
    (ltk:bind edit-ctrl "<Map>" (lambda (evt) (declare (ignore evt)) (ltk:focus edit-ctrl)))
    (ltk:bind edit-ctrl "<<Modified>>" (lambda (evt) (on-file-modified evt)))
    (dolist (binding *text-keys*)
      ;; dolist may re-use the binding... force unique bindings (test in clisp)
      (destructuring-bind (key action &optional suppress-in-repl) binding
        (declare (ignore suppress-in-repl))
        ;;(format t "~%binding ~A to ~A" key action)
        (add-key-binding edit-ctrl key
          (lambda (evt)
            (declare (ignore evt))
            ;;(format t "~%calling ~A" action)
            (funcall action edit-ctrl)))))
    (ltk:bind edit-ctrl "<Return>"
      (lambda (evt) (unless (plaintextp txt) (on-return-key edit-ctrl evt))))
    (ltk:bind edit-ctrl "<space>"
      (lambda (evt) (unless (plaintextp txt) (on-space-key edit-ctrl evt))))
    (ltk:bind edit-ctrl "<Delete>"
      (lambda (evt) (unless (plaintextp txt) (on-delete-key edit-ctrl evt))))
    (ltk:bind edit-ctrl "<BackSpace>"
      (lambda (evt) (unless (plaintextp txt) (on-delete-key edit-ctrl evt))))
    (ltk:bind edit-ctrl "<Key-bracketleft>"
      (lambda (evt) (unless (plaintextp txt) (on-left-bracket-key edit-ctrl evt))) :exclusive t)
    (ltk:bind edit-ctrl "<Control-Key-bracketleft>"
      (lambda (evt) (unless (plaintextp txt)
                      (on-ctrl-left-bracket-key edit-ctrl evt))) :exclusive t)
    (ltk:bind edit-ctrl "<Escape>" #'on-escape :exclusive t)
    (ltk:tag-configure (edit-ctrl txt) "parens" "background" *highlight-paren-match*)
    (ltk:tag-configure (edit-ctrl txt) "keywords" "foreground" *highlight-primary*)
    (ltk:tag-configure (edit-ctrl txt) "user" "foreground" *highlight-secondary*)
    (ltk:tag-configure (edit-ctrl txt) "comments" "foreground" *highlight-comments*)))

(defmethod get-current-text-ctrl ((buffer-manager buffer-manager))
  (edit-ctrl (selected-buffer buffer-manager)))

(defun update-current-buffers (buffer-manager)
  (ltk:menu-delete *buffer-menubar* 5)
  (loop for b in (reverse (buffers buffer-manager))
    for c from 1
    do (ltk:make-menubutton
         *buffer-menubar*
         (format nil "~A: ~A" c (file-path b))
         (let ((n c))
           (lambda ()
             (on-select-file n))))))

(defmethod add-buffer ((buffer-manager buffer-manager) file-path &optional open-file-p)
  (if (not (find-buffer buffer-manager file-path))
      (let* ((newbuffer (make-instance 'buffer :file-path file-path :master *editor-frame*))
             (text-ctrl (ltk:textbox newbuffer)))
        (when open-file-p (load-text newbuffer file-path))
        ; Once the text control is created, enable undo. Don't want to enable it earlier
        ; as when loading files, the file load becomes the first undoable event.
        (ltk:configure text-ctrl :undo 1)
        (ltk::set-cursor-pos text-ctrl "1.0")
        (push newbuffer (buffers buffer-manager))))
  (select-buffer buffer-manager file-path)
  (update-current-buffers buffer-manager))

(defmethod find-buffer ((buffer-manager buffer-manager) path-name)
  (loop for buffer in (reverse (buffers buffer-manager)) do
    (when (string= path-name (file-path buffer))
      (return buffer))))

(defmethod new-buffer-p ((buffer buffer))
  "Is this a new file?"
  (search #t"untitled" (slot-value buffer 'file-path)))

(defmethod saved-buffer-p ((buffer buffer))
  "Is this buffer saved?"
  (not (equal (ltk::get-modify (edit-ctrl buffer)) "1")))

(defmethod all-saved-buffer-p ((buffer-manager buffer-manager))
  "Are all files saved?"
  (remove-if #'saved-buffer-p (buffers buffer-manager)))

(defmethod select-buffer ((buffer-manager buffer-manager) (file-path string))
  (let* ((curbuffer (selected-buffer buffer-manager))
         (newbuffer (find-buffer buffer-manager file-path))
         (newtext (ltk:textbox newbuffer)))
    (when curbuffer
      (ltk:pack-forget curbuffer))
    (setf (selected-buffer buffer-manager) newbuffer)
    (pathname-message)
    (ltk:pack newbuffer :side :top :fill :both :expand t)
    (ltk:force-focus newtext)
    (match-paren newtext)))

(defmethod select-buffer ((buffer-manager buffer-manager) (index integer))
  (select-buffer buffer-manager
    (file-path
      (nth index (buffers buffer-manager)))))

(defmethod save-buffer ((buffer buffer) file-path)
  (let* ((text-ctrl (ltk:textbox buffer)))
    (ltk::reset-modify text-ctrl)
    (setf (file-path buffer) file-path))
  (pathname-message))

(defmethod get-next-buffer ((buffer buffer) (buffer-manager buffer-manager))
  (let* ((next)
         (retval)
         (buffers (reverse (buffers buffer-manager))))
    (loop for b in buffers do
      (unless retval
        (setf retval b))
      (when next
        (setf retval b)
        (setf next nil))
      (if (string= (file-path b) (file-path buffer))
          (setf next t)))
    (when retval
      (file-path retval))))

(defmethod select-next-buffer ((buffer-manager buffer-manager) (buffer buffer))
  (let ((next-buffer (get-next-buffer buffer buffer-manager)))
    (when (and next-buffer (not (string= (file-path buffer) next-buffer)))
      (select-buffer buffer-manager next-buffer))))

(defmethod at-least-one-buffer-p ((buffer-manager buffer-manager))
  (> (length (buffers buffer-manager)) 0))

(defmethod load-text ((txt buffer) filepath)
  (let ((editor (edit-ctrl txt)))
    (ltk:load-text (edit-ctrl txt) filepath)
    (if (lisp-file? filepath)
        (highlight editor 'load)
        (setf (plaintextp txt) t))
    (ltk::reset-modify editor)))

(defmethod close-buffer ((buffer buffer) (buffer-manager buffer-manager))
  (setf (buffers buffer-manager) (remove buffer (buffers buffer-manager) :test #'equalp))
  (update-current-buffers buffer-manager)
  (let ((nextfile (get-next-buffer buffer buffer-manager)))
    (select-next-buffer buffer-manager buffer)
    (ltk:pack-forget buffer)
    (case (at-least-one-buffer-p buffer-manager)
      ((t) (pathname-message))
      ((nil) (shutdown)))))

(defclass listener (ltk:frame)
  ((inferior-win
     :accessor inferior-win)
   (command-history
     :initform (make-array 0 :fill-pointer 0 :adjustable t :element-type 'string)
     :accessor command-history)
   (current-command
     :initform 0
     :accessor current-command)
   (complete-mode
     :initform 'symbol
     :accessor complete-mode)
   (insert-point
     :initform "1.0"
     :accessor repl-insert-point)))

(defmethod initialize-instance :after ((listener listener) &key)
  (setf (inferior-win listener) (make-instance 'ltk:text :master listener
                                  :height *listener-lines*))
  (let ((text (inferior-win listener)))
    (ltk:bind text "<KeyPress>" (lambda (evt) (declare (ignore evt)) (on-key-press listener)))
    (ltk:bind text "<FocusIn>" (lambda (evt) (declare (ignore evt)) (focus listener)))
    (ltk:bind text "<FocusOut>" (lambda (evt) (declare (ignore evt)) (unfocus listener)))
    (ltk:bind text "<Map>" (lambda (evt) (declare (ignore evt)) (ltk:focus text)))
    (ltk:bind text "<Configure>" (lambda (evt) (declare (ignore evt)) (on-resize listener)))
    (ltk:bind text "<Return>" (lambda (evt) (declare (ignore evt)) (submit-repl-input listener)) :exclusive t)
    (ltk:bind text "<Up>" (lambda (evt) (declare (ignore evt)) (prev-command listener)) :exclusive t)
    (ltk:bind text "<Down>" (lambda (evt) (declare (ignore evt)) (next-command listener)) :exclusive t)
    (ltk:bind text "<BackSpace>" (lambda (evt) (declare (ignore evt)) (repl-delete-key-down listener)) :exclusive t)
    (ltk:bind text "<Escape>" (lambda (evt) (declare (ignore evt)) (clear listener)) :exclusive t)
    (dolist (binding *text-keys*)
      (destructuring-bind (key action &optional suppress-in-repl) binding
        (unless suppress-in-repl
          ;;(format t "~%repl: binding ~A to ~A" key action)
          (add-key-binding text key
            (lambda (evt)
              (declare (ignore evt))
              ;;(format t "~%calling ~A" action)
              (funcall action text))))))
    (add-key-binding text *key-code-complete*
      (lambda (evt)
        (declare (ignore evt))
        (on-code-complete listener)))
    (ltk:bind text "<Escape>" (lambda (evt) (declare (ignore evt)) (clear listener)) :exclusive t)
    (ltk:bind text "<Key-bracketleft>"
      (lambda (evt) (on-left-bracket-key text evt)) :exclusive t)
    (ltk:bind text "<Control-Key-bracketleft>"
      (lambda (evt) (on-ctrl-left-bracket-key text evt)) :exclusive t)
    (ltk:tag-configure text "normal" "foreground" *highlight-text*)
    (ltk:tag-configure text "error" "foreground" *highlight-error*)
    (ltk:tag-configure text "parens" "background" *highlight-paren-match*)
    (ltk:tag-configure text "prompt" "foreground" *highlight-secondary*)
    (ltk:configure text :font *buffer-font* :background *highlight-background*
      :foreground *highlight-primary* :relief :groove :insertbackground *highlight-text*)
    (ltk:pack text :side :left :fill :both :expand t)
    (prompt listener)))

(defmethod pathname-complete ((listener listener))
  (labels ((transform-file-list (pathnames)
             "Turns a list of pathnames into a list of pairs containing the pathname and
             either the directory name for directories or the filename with extension
             for lisp code files"
             (remove nil
               (mapcar
                 (lambda (pathname)
                   (cond ((cl-fad:directory-pathname-p pathname)
                          (cons (car (last (pathname-directory pathname))) pathname))
                         ((lisp-file? (namestring pathname))
                          (cons (format nil "~a.lisp" (pathname-name pathname)) pathname))))
                 pathnames)))
           (filter-matches (candidates test)
             "Return all candidate strings for which test is a substring"
             (remove-if-not (lambda (candidate)
                              (eq 0 (search test (correct-path candidate))))
               candidates))
           (pathname-completions-to-string (completions &optional (limit-to 9))
             "Turn the list of dotted pairs created by transform-file-list into a flat
             string of just the filenames without the full path for display in the statusbar.
             This list is delimited by commas for readability and if the list is longer than
             limit-to is suffixed with ellipsis."
             (format nil "~{~A~^, ~}" (append
                                        (take
                                          (remove-if
                                            (lambda (s) (equal (char s 0) #\.))
                                            (mapcar #'car completions))
                                          limit-to)
                                        (if (> (length completions) limit-to) '("...") '()))))
           (ensure-directory-slash (directory)
             "Ensures that the directory string has a closing forward slash."
             (if (equal (char (reverse directory) 0) #\/)
                 directory
                 (format nil "~a/" directory))))
    (handler-case
      (let ((text (correct-path (get-user-input listener)))
            directory file pathname-matches)
        (setf directory
          (ensure-directory-slash
            (let ((split-at (position #\/ text :test #'equal :from-end t)))
              (setf file (subseq text (1+ split-at)))
              (subseq text 0 split-at))))
        (let ((files (cl-fad:list-directory directory)))
          (when files
            (setf pathname-matches
              (transform-file-list
                (filter-matches files text)))
            (case (length pathname-matches)
              (0 (error-message #t"no matches"))
              (1 (insert-command listener (correct-path (cdr (first pathname-matches)))))
              (otherwise
                (insert-command listener
                  (format nil "~a~a" directory
                    (longest-prefix-match (mapcar #'car pathname-matches))))
                (info-message
                  (pathname-completions-to-string pathname-matches)))))))
      (error (ex) (ltk:do-msg ex)))))

(defmethod on-code-complete ((listener listener))
  (case (complete-mode listener)
    (symbol
      (code-complete (inferior-win listener))
      (show-calltip (inferior-win listener)))
    (pathname
      (pathname-complete listener))))

(defmethod evaluator ((listener listener) code-string &optional (append-input t))
  (let ((code-string (trim-code code-string)))
    (when (and code-string (> (length code-string) 0))
      (store-command listener code-string)
      (when append-input (insert-command listener code-string))
      ; Re-bind a new instance of USER-STREAM on each EVAL, in case it's
      ; required by a call to READ*, ensuring a pristine input stream.
      (with-able-streams 'user-stream
        (with-status-msg #t"evaluating..."
          ; Note the explicit handling of REPL variables.
          (handler-case (let ((form (read-from-string code-string)))
                          (setq +++ ++ ++ + + - - form)
                          (let ((vals (multiple-value-list (eval form))))
                            (setq /// // // / / vals *** ** ** * * (first vals))
                            (format t "~{~%~s~^~&~}" vals)))
            (error (ex) (output listener (format nil "~%~a" ex) "error")))
          (flush listener))))))

(defmethod output ((listener listener) message &optional (tag "normal"))
  (when message
    (ltk:append-text (inferior-win listener) message tag)
    (ltk::set-cursor-pos (inferior-win listener) "end")
    (ltk::scroll-to (inferior-win listener) "end")
    (setf (repl-insert-point listener)
      (ltk::get-cursor-pos (inferior-win listener)))
    (hide-paren-match (inferior-win listener))))

(defmethod flush ((listener listener))
  (finish-output))

(defmethod reset ((listener listener) &key hard)
  (flush listener)
  (ltk::delete-text (inferior-win listener) "1.0" "end")
  (when hard
    (setf
      (current-command listener)
      (make-array 0 :fill-pointer 0 :adjustable t :element-type 'string)
      (command-history listener) 0))
  (prompt listener))

(defun prompt1 (prompt)
  (if prompt
      (format nil "~%~a" prompt)
      (format nil "~%~a> " (or (first (package-nicknames *package*))
			       (package-name *package*)))))

(defmethod prompt ((listener listener) &key prompt (clear nil))
  (when clear
    (let ((start-of-line (text-row-add (repl-insert-point listener) 0)))
      (ltk::delete-text (inferior-win listener) start-of-line "end")
      (setf (repl-insert-point listener) (ltk::get-cursor-pos (inferior-win listener)))))
  (output listener (prompt1 prompt) "prompt"))

(defmethod get-user-input ((listener listener))
  (trim-code (ltk::get-text-range (inferior-win listener)
               (repl-insert-point listener) "end")))

(defmethod get-last-command-pos ((rb listener))
  (- (length (command-history rb)) 1))

(defmethod get-last-command ((rb listener))
  (let ((history-length (get-last-command-pos rb)))
    (if (> history-length 0)
        (elt (command-history rb) history-length)
        "")))

(defmethod store-command ((rb listener) command-text)
  "Store a command in the command history."
  (unless (or (string= command-text "")
              (string= command-text (get-last-command rb)))
    (vector-push-extend command-text (command-history rb)))
  (setf (current-command rb) (length (command-history rb))))

(defmethod next-command ((rb listener))
  "Move to the next command in the command history."
  (let ((current-command (current-command rb))
        (last-command (get-last-command-pos rb)))
    (when (< current-command last-command)
      (incf (current-command rb))
      (insert-command rb (elt (command-history rb) (current-command rb)))
      (match-paren (inferior-win rb)))))

(defmethod prev-command ((rb listener))
  "Move to the previous command in the command history."
  (let ((current-command (current-command rb)))
    (when (> current-command 0)
      (decf (current-command rb))
      (insert-command rb (elt (command-history rb) (current-command rb)))
      (match-paren (inferior-win rb)))))

(defmethod insert-command ((listener listener) text)
  "Insert something into the listener ready for evaluation. Unlike output, don't make the
  entered command text locked (read-only)."
  (ltk::delete-text (inferior-win listener) (repl-insert-point listener) "end")
  (ltk::insert-text (inferior-win listener) text :position "end")
  (ltk::scroll-to (inferior-win listener) "end")
  (focus listener))

(defmethod clear ((listener listener))
  "Clear the current contents of the output window, resetting everything
  that needs to be reset in the process. If there's nothing to clear,
  focus on the current editor control instead."
  (if (/= (length (get-user-input listener)) 0)
      (progn
        (setf (current-command listener)
          (length (command-history listener)))
        (clear-completion-data)
        (prompt listener :clear t)
        (no-message))
      (focus-editor)))

(defmethod on-resize ((listener listener))
  (ltk::set-cursor-pos (inferior-win listener) "end")
  (ltk::scroll-to (inferior-win listener) "end"))

(defmethod key-press-allowed-p ((listener listener))
  "Checks to see if the key press has been in a locked region of the REPL."
  (let ((cur-pos (ltk::get-cursor-pos (inferior-win listener))))
    (if cur-pos
        (let ((len1 (ltk::get-text-length (inferior-win listener)
                      "1.0" cur-pos))
              (len2 (ltk::get-text-length (inferior-win listener)
                      "1.0" (repl-insert-point listener))))
          (> len1 len2)))))

(defmethod on-key-press ((listener listener))
  (clear-completion-data)
  (show-calltip (inferior-win listener))
  ; If the user tries to type before the REPL prompt, reset their cursor position.
  (if (not (key-press-allowed-p listener))
      (ltk::set-cursor-pos (inferior-win listener) (repl-insert-point listener))
      (match-paren (inferior-win listener))))

(defmethod repl-delete-key-down ((listener listener))
  "Don't allow the user to delete before the REPL prompt."
  (let ((txt (inferior-win listener)))
    (when (key-press-allowed-p listener)
      (ltk::delete-chars txt)
      (show-calltip txt)
      (clear-completion-data)
      (match-paren txt))))

(defmethod submit-repl-input ((listener listener))
  (let ((user-input (get-user-input listener)))
    (when (and user-input (> (length user-input) 0))
      (evaluator listener user-input nil)
      (clear-completion-data)
      (hide-paren-match (inferior-win listener))
      (prompt listener))))
          
(defmethod rebind-keys ((listener listener))
  (ltk:bind (inferior-win listener) "<Return>"
    (lambda (evt) (submit-repl-input listener)) :exclusive t)
  (ltk:bind (inferior-win listener) "<Escape>"
    (lambda (evt) (clear listener)) :exclusive t))

(defmethod input-prompt ((listener listener) prompt-string
                         &optional default-input)
  (focus-listener)
  (prompt listener :prompt prompt-string :clear t)
  (when default-input
    (insert-command listener default-input))
  (let* ((ltk:*exit-mainloop* nil)
         (ok t))
    (ltk:bind (inferior-win listener) "<Return>"
      (lambda (event)
        (declare (ignore event))
        (ltk::break-mainloop)) :exclusive t)
    (ltk:bind (inferior-win listener) "<Escape>"
      (lambda (event)
        (declare (ignore event))
        (ltk::break-mainloop)
        (setf ok nil)) :exclusive t)
    (focus listener)
    (ltk:mainloop)
    (rebind-keys listener)
    (and ok (get-user-input listener))))

(defmethod one-of ((listener listener) message allowed-answers default-answer)
  "Prompts the user for input which must be one of allowed-answers."
  (let ((user-answer (input-prompt listener message default-answer)))
    (case (member user-answer allowed-answers :test 'string-equal)
      ((nil)
       (one-of listener message allowed-answers default-answer))
      (otherwise user-answer))))

(defmethod yes-no ((listener listener) message
                   &optional
		   (affirmative-answers (list #t"y" #t"yes"))
		   (default-answer #t"yes"))
  "Prompts the user for input where any of affirmative-answers constitues 'yes'."
  (let ((user-answer (input-prompt listener message default-answer)))
    (prompt listener)
    (member user-answer affirmative-answers :test 'string-equal)))

(defmethod get-filename ((listener listener) &optional (text #t"open:"))
  (let (filepath)
    (with-temporary-value (complete-mode listener) 'pathname
      (let ((last-directory (format nil "~a/" (correct-path (get-last-directory)))))
        (setf filepath (input-prompt listener text last-directory))
        (prompt listener :clear t)))
    filepath))

(defmethod focus ((listener listener))
  (ltk:force-focus (inferior-win listener))
  (ltk::set-cursor-pos (inferior-win listener) "end")
  (ltk::scroll-to (inferior-win listener) "end")
  (match-paren (inferior-win listener))
  (show-calltip (inferior-win listener)))

(defmethod unfocus ((listener listener))
  (hide-paren-match (inferior-win listener))
  (hide-calltip))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *keytable*
    (list
     (list *key-new-file* 'on-new-file)
     (list *key-close-file* 'on-close-file)
     (list *key-open-file* 'on-open-file)
     (list *key-open-file-browser* 'on-open-file-browser)
     (list *key-save-file* 'on-save-file)
     (list *key-save-as-file* 'on-save-as-file)
     (list *key-save-as-file-browser* 'on-save-as-file-browser)
     (list *key-load-file* 'on-load-file)
     (list *key-find* 'on-search)
     (list *key-find-again* 'on-search-again)
     (list *key-goto-line* 'on-goto)
     (list *key-asdf-load* 'on-asdf-load)
     (list *key-next-file* 'on-next-file)
     (list *key-select-file* 'on-select-file)
     (list *key-reload-file* 'on-reload-file)
     (list *key-compile-file* 'on-compile-file)
     (list *key-invoke-debugger* 'on-invoke-debugger)
     (list *key-quit-able* 'on-quit)
     (list *key-reset-listener* 'on-reset-listener))
    "plist mapping key bindings to functions"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun key-function (key)
    "look up the function for this key"
    (second (find-if (lambda (entry) (equal key (first entry))) *keytable*)))

  (defun function-key (fun)
    "look up the key for this function"
    (or
     (first (find-if (lambda (entry) (equal fun (second entry))) *keytable*))
     (first (find-if (lambda (entry) (equal fun (second entry))) *text-keys*)))))

(defun bind-commands ()
  (dolist (entry *keytable*)
    (destructuring-bind (key action) entry
      (add-key-binding ltk::*tk* key action))))

(defun create-menus ()
  (let* ((mb (ltk:make-menubar))
         (mfile (ltk:make-menu mb #t"File"))
         (medit (ltk:make-menu mb #t"Edit"))
         (mbuffer (ltk:make-menu mb #t"Buffers"))
         (mlisp (ltk:make-menu mb #t"Lisp")))
    (setf *buffer-menubar* mbuffer)

    (macrolet ((with-menu (menu &body body)
                 `(macrolet
                      ((action (name op)
                         (let ((key (function-key op)))
                           (if key
                               `(ltk:make-menubutton ,',menu (format nil "~A ~A" ,name ,key) #',op)
                               `(ltk:make-menubutton ,',menu ,name #',op))))
                       (text-action (name op)
                         ;; an action that needs the current buffer
                         (let ((key (function-key op))
			       (op-current-text
				 `(lambda ()
				    (,op (get-current-text-ctrl *buffer-manager*)))))

			   (if key
			     `(ltk:make-menubutton ,',menu (format nil "~A ~A" ,name ,key)
						   ,op-current-text)
			     `(ltk:make-menubutton ,',menu ,name
						   ,op-current-text))))
                       (separator ()
                         `(ltk:add-separator ,',menu)))
                    ,@body)))
      (with-menu mfile
        (action #t"New file" on-new-file)
        (action #t"Open file" on-open-file)
	(action #t"Open file browser" on-open-file-browser)
        (separator)
        (action #t"Save file" on-save-file)
        (action #t"Save as file" on-save-as-file)
	(action #t"Save as file browser" on-save-as-file-browser)
        (separator)
        (action #t"Exit" on-quit))
      (with-menu medit
        (text-action #t"Cut" on-cut)
        (text-action #t"Copy" on-copy)
        (text-action #t"Paste" on-paste)
        (separator)
        (text-action #t"Select all" on-select-all)
        (text-action #t"Reindent" on-re-indent)
        (separator)
        (action #t"Find" on-search)
        (action #t"Find again" on-search-again)
        (action #t"Goto line" on-goto))
      (with-menu mbuffer
        (action #t"Next buffer" on-next-file)
        (separator)
        (action #t"Close buffer" on-close-file)
	(separator)
	(action #t"Select buffer" on-select-file)
	;; sacrificial placeholder for the current buffer list
	;; update the magic number in update-current-buffers when adding/removing entries
	(ltk:make-menubutton mbuffer "" nil))
      (with-menu mlisp
        (text-action #t"Macroexpand" on-macro-expand)
        (text-action #t"Copy to REPL" on-copy-sexp-to-repl)
        (ltk:make-menubutton mlisp #t"Complete symbol"
                             (lambda ()
                               (let* ((buffer (selected-buffer *buffer-manager*))
                                      (text (ltk:textbox buffer)))
                                 (unless (plaintextp buffer)
                                   (on-code-complete text)))))
        (text-action #t"CLHS lookup" on-lookup-definition)
        (separator)
        (action #t"(Re)load buffer" on-reload-file)
        (action #t"Load file" on-load-file)
        (action #t"Load ASDF" on-asdf-load)
        (separator)
        (action #t"Compile file" on-compile-file)
        (separator)
        (action #t"Invoke native debugger" on-invoke-debugger)
        (action #t"Reset listener" on-reset-listener)))))

(defun on-reset-listener (&optional event)
  (reset *listener*))

(defun on-search (&optional event)
  (search-text (get-current-text-ctrl *buffer-manager*))
  (prompt *listener* :clear t))

(defun on-search-again (&optional event)
  (search-text-again (get-current-text-ctrl *buffer-manager*)))

(defun on-goto (&optional event)
  (let ((input (input-prompt *listener* #t"goto:")))
    (cond ((eq (length input) 0)
           nil)
          ((typep (read-from-string input) 'integer)
           (goto (get-current-text-ctrl *buffer-manager*) (parse-integer input)))
          (t (error-message #t"non integer argument supplied to goto"))))
  (prompt *listener* :clear t))

(defun open-file (filepath)
  (when filepath
    (let* ((pathname (parse-namestring filepath))
           (file-exists-p (cl-fad:file-exists-p pathname)))
      (cond ((and file-exists-p
                  (not (cl-fad:directory-exists-p pathname)))
             (update-last-directory filepath)
             (load-definitions filepath)
             (if (equal (find-buffer *buffer-manager* filepath) nil)
                 (add-buffer *buffer-manager* filepath t)
                 (select-buffer *buffer-manager* filepath)))
            (t (error-message #t"file not found")))
      file-exists-p)))

(defun on-open-file (&optional event)
  (let ((filepath (get-filename *listener*)))
    (open-file filepath)))

(defun on-open-file-browser (&optional event)
  (declare (ignore event))
  (open-file (ltk:get-open-file)))

(defun on-load-file (&optional event)
  (let* ((filepath (get-filename *listener* #t"load:"))
         (pathname (open-file filepath)))
    (when pathname
      (evaluator *listener*
        (concatenate 'string "(load \"" filepath "\")"))
      (prompt *listener*))))

(defun save-file (file path)
  "Saves a file (a buffer) under the specified path"
  (let ((text-ctrl (edit-ctrl (selected-buffer *buffer-manager*))))
    (when (> (length path) 0)
      (ensure-directories-exist path)
      (update-last-directory path)
      (unless (find #\. path)
        (setf path (concatenate 'string path ".lisp")))
      (ltk:save-text text-ctrl path)
      (save-buffer file path)
      (load-definitions path))))
  
(defun on-save-file (&optional event)
  (let* ((file (selected-buffer *buffer-manager*))
         (path (if (new-buffer-p file)
                   (get-filename *listener* #t"save:")
                   (file-path file))))
    (save-file file path)))

(defun on-save-as-file (&optional event)
  (let* ((file (selected-buffer *buffer-manager*))
         (path (get-filename *listener* #t"save:")))
    (save-file file path)))

(defun on-save-as-file-browser (&optional event)
  (save-file
    (selected-buffer *buffer-manager*)
    (ltk:get-save-file)))

(defun on-reload-file (&optional event)
  (let* ((buffer (selected-buffer *buffer-manager*))
         (file-path (file-path buffer)))
    (if (saved-buffer-p buffer)
        (when (lisp-file? file-path)
          (evaluator *listener*
            (format nil "(load \"~a\")" file-path))
          (prompt *listener*))
        (error-message #t"please save before loading"))))

(defun on-compile-file (&optional event)
  (let ((buffer (selected-buffer *buffer-manager*)))
    (if (saved-buffer-p buffer)
        (when (lisp-file? (file-path buffer))
          (evaluator *listener*
            (format nil
              "(compile-file \"~a\")" (file-path (selected-buffer *buffer-manager*))))
          (prompt *listener*))
        (error-message #t"please save before compiling"))))

(defun on-new-file (&optional event)
  (add-buffer *buffer-manager* (get-untitled)))

(defun on-close-file (&optional event)
  (let ((curbuffer (selected-buffer *buffer-manager*)))
    (cond ((saved-buffer-p curbuffer)
           (close-buffer curbuffer *buffer-manager*))
          (t (when (yes-no *listener* #t"unsaved file...close anyway?")
               (close-buffer curbuffer *buffer-manager*))))))

(defun on-asdf-load (&optional event)
  (let ((system (input-prompt *listener* #t"system:")))
    (when system
      (evaluator *listener*
        (concatenate 'string
          "(asdf:oos 'asdf:load-op '\""
             (string-trim '(#\' #\:) system)
             "\")")
        nil)))
  (prompt *listener* :clear t))

(defun on-next-file (&optional event)
  (select-next-buffer *buffer-manager* (selected-buffer *buffer-manager*)))

(defun on-select-file (&optional n)
  (unless (integerp n)
    (setf n (parse-integer (input-prompt *listener* #t"buffer number:"))))
  (when (integerp n)
    (select-buffer *buffer-manager*
      ;; compensate for the reversed order
      (- (length (buffers *buffer-manager*)) n)))
  (prompt *listener* :clear t))

(defun on-file-modified (evt)
  (pathname-message))

;; When invoked as a Tk event, this bypasses the I/O redirection.
;; Attempts to invoke the debugger within ABLE's listener are confounded
;; by (with-able-streams).
(defun on-invoke-debugger (&optional event)
  "invoke the native CL debugger (including hooks such as slime)"
  (declare (ignore event))
  (restart-case
      (invoke-debugger
       (make-condition 'simple-condition
                       :format-control "ABLE breakpoint"
                       :format-arguments nil))
    (return-to-able ()
      :report "Return to ABLE"
      t)))

(defun on-quit (&optional event)
  (let ((unsaved-buffers (all-saved-buffer-p *buffer-manager*)))
    (cond ((= (length unsaved-buffers) 0)
           (shutdown))
          (t (when (yes-no *listener* #t"unsaved files exist...quit anyway?")
               (shutdown))))))

(defun on-escape (&optional event)
  (focus-listener))

(defmethod on-lookup-definition ((text ltk:text))
  (let ((symbol (get-current-token text)))
    (when symbol
      (case (get (tstree:get-metadata *symbols* symbol) :type)
        (user (on-navigate-to-definition symbol))
        (system (hyperspec-lookup symbol))
        (otherwise (info-message #t"No Hyperspec entry or src location found"))))))

(defmethod on-macro-expand ((text ltk:text))
  (let ((sexp (sexp-before-cursor text)))
    (when-string sexp
      (insert-command *listener* (format nil "(macroexpand-1 '~a)" sexp))
      (focus-listener))))

(defmethod on-copy-sexp-to-repl ((text ltk:text))
  (let ((sexp (sexp-before-cursor text)))
    (when-string sexp
      (evaluator *listener* sexp t)
      (prompt *listener*))))

(defmethod on-key-down ((txt ltk:text) event)
  (let ((str (ltk::get-text-to-cursor txt)))
    (show-calltip txt str)
    (clear-completion-data)
    (highlight txt 'text)
    (match-paren txt str)))

(defmethod on-return-key ((txt ltk:text) event)
  (let ((str (ltk::get-text-to-cursor txt)))
    (highlight txt 'return)
    (indent-current-line txt str)
    (show-calltip txt str)
    (hide-paren-match txt)))

(defmethod on-space-key ((txt ltk:text) event)
  (highlight txt 'space)
  (hide-paren-match txt)
  (show-calltip txt))

(defmethod on-delete-key ((txt ltk:text) event)
  (let ((str (ltk::get-text-to-cursor txt)))
    (show-calltip txt str)
    (clear-completion-data)
    (highlight txt 'delete)
    (match-paren txt str)))

(defmethod on-left-bracket-key ((txt ltk:text) event)
  (ltk::insert-text txt (format nil "~a~a" +lparen+ +rparen+))
  (ltk::move-cursor-pos txt -1))

(defmethod on-ctrl-left-bracket-key ((txt ltk:text) event)
  (ltk::insert-text txt "["))

(defmethod on-left-click ((txt ltk:text))
  (let ((str (ltk::get-text-to-cursor txt)))
    (show-calltip txt str)
    (match-paren txt str)))

(defmethod on-focus-in ((txt ltk:text))
  (let ((str (ltk::get-text-to-cursor txt)))
    (match-paren txt str)
    (show-calltip txt str)))

(defmethod on-focus-out ((txt ltk:text))
  (hide-calltip)
  (hide-paren-match txt))

(defmethod on-cut ((txt ltk:text))
  (ltk::cut txt))

(defmethod on-copy ((txt ltk:text))
  (ltk::copy txt))

(defmethod on-paste ((txt ltk:text))
  "Calls the internal paste function which allows paste to be bound to any key."
  (let ((to-paste (ltk:clipboard-get)))
    (let ((newlines (count #\Newline to-paste)))
      (when (> (length to-paste) 0)
        (ltk::paste txt)
        (highlight txt 'paste newlines)
        (match-paren txt)))))

(defmethod on-select-all ((txt ltk:text))
  (ltk::select-all txt))

(defmethod on-code-complete ((text ltk:text))
  (code-complete text)
  (highlight text 'complete)
  (show-calltip text))

(defmethod on-re-indent ((txt ltk:text))
  (indent-block txt)
  (match-paren txt))

(defmethod on-cursor-line-start ((txt ltk:text))
    (ltk::set-to-start-current-line txt))

(defmethod on-cursor-line-end ((txt ltk:text))
    (ltk::set-to-end-current-line txt))

(defmethod on-cursor-forward ((txt ltk:text))
    (ltk::move-cursor-pos txt 1))

(defmethod on-cursor-backward ((txt ltk:text))
    (ltk::move-cursor-pos txt -1))

(defun focus-editor ()
  (ltk:focus (get-current-text-ctrl *buffer-manager*)))

(defun focus-listener ()
  (focus *listener*))

(defun parse-watch-systems ()
  "If the user specified directories to watch, parse them here."
  (when (consp *watch-directories*)
    (mapcar #'load-definitions-for-directory
      *watch-directories*)))

(defun add-user-load-paths ()
  (loop for path in *user-load-paths* do
       (push path asdf::*central-registry*)))

(defun load-user-config ()
  "Allows the user to provide a ~/.able file to override any of the
  default configuration settings."
  (let ((config (merge-pathnames
                  (user-homedir-pathname) ".able")))
    (when (probe-file config)
      (handler-case
        (load (namestring config))
        (error (ex) nil)))))

(defun create-widgets ()
  "Creates main window, layout controls and set-up some key bindings."
  (ltk:with-ltk (:handle-errors nil :handle-warnings nil :debugger nil)
    (setf *editor-frame* (make-instance 'ltk:frame :padding "\"1 1\""))
    ;; reload buffers when restarting ABLE in the same CL session
    (let ((old-manager *buffer-manager*))
      (setf *buffer-manager* (make-instance 'buffer-manager))
      (setf *listener* (make-instance 'listener :padding "\"1 1\""))
      (setf *statusbar* (make-instance 'statusbar))
      (ltk:pack *statusbar* :side :bottom :fill :both)
      (ltk:pack *listener* :side :bottom :fill :both)
      (ltk:pack *editor-frame* :side :bottom :fill :both :expand t)
      (ltk:on-close ltk::*tk* 'on-quit)
      (ltk:set-geometry ltk::*tk* *window-width* *window-height* *window-x* *window-y*)
      (ltk:minsize ltk::*tk* 320 200)
      (bind-commands)
      (create-menus)
      (on-new-file) ;; create the new file after the menus so it is listed in the current buffer list

      (when old-manager
        (dolist (buf (reverse (buffers old-manager)))
          (let ((filename (and buf (file-path buf))))
            (when (probe-file filename)
              (format t "~%re-opening ~A" filename)
              (open-file filename))))))

      (focus-editor)))

(defun start ()
  (load-user-config)
  (load-symbol-tree)
  (in-package :cl-user)
  (reset-untitled)
  (reset-last-directory)
  (add-user-load-paths)
  (parse-watch-systems)
  (create-widgets))

