(in-package :able)

(defclass statusbar (ltk:frame)
  ((label :accessor label)
   (cached-message :initform "" :accessor cached-message)))

(defmethod initialize-instance :after ((sb statusbar) &key)
  (setf (label sb) (make-instance 'ltk:label :master sb :font *buffer-font*))
  ;;(ltk:configure sb :background *highlight-background*)
  (ltk:configure (label sb)
                 :foreground *highlight-text*)
  (ltk:pack (label sb) :side :left :pady 1 :padx 5 :fill :both))

(defmethod set-message ((sb statusbar) msg &optional (highlight *highlight-text*))
  (unless (equalp msg (cached-message sb))
    (setf (cached-message sb) msg)
    (setf (ltk:text (label sb)) msg)
    (ltk:configure (label sb) :foreground highlight)))











