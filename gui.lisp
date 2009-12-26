;;; Some simple LTk extensions.

(in-package :ltk)

;;; This replaces the current ltk implementation of this method to remove
;;; the trailing CR that Tk leaves on.
(defmethod text ((text text))
  (format-wish "senddatastring [~a get 1.0 end-1c]" (widget-path text))
  (read-data))

(defmethod insert-text ((txt text) text &rest tags &key (position "insert"))
  (format-wish "~a insert ~a \"~a\" {~{ ~(~a~)~}}"
    (widget-path txt) position (tkescape text) tags)
  txt)

(defmethod delete-text ((txt text) start end)
  (format-wish "~a delete ~a ~a" (widget-path txt) start end))

(defmethod delete-chars ((txt text) &optional (num 1))
  (format-wish "~a delete \"insert -~a chars\" \"insert\"" (widget-path txt) num))

(defmethod get-cursor-pos ((text text))
  (format-wish "senddatastring [~a index insert]" (widget-path text))
  (read-data))

(defgeneric set-cursor-pos (widget pos)
  (:documentation "Sets the position of the cursor in the widget"))

(defmethod set-cursor-pos ((text text) pos)
  (format-wish "~a mark set insert ~a" (widget-path text) pos))

(defmethod set-cursor-pos ((ent entry) pos)
  (format-wish "~a icursor ~a" (widget-path ent) pos))

(defmethod move-cursor-pos ((text text) count)
  (format-wish "~a mark set insert \"insert ~a~ac\""
    (widget-path text) (if (plusp count) #\+ #\-) (abs count)))

(defmethod scroll-to ((txt text) pos)
  (format-wish "~a see ~a" (widget-path txt) pos))

(defgeneric get-text-range (object start end)
  (:documentation "Gets a sub-string from an object using the Tk style text inices start and end"))

(defmethod get-text-range ((text text) start end)
  "Gets the sub-string by directly querying the Tk text widget"
  (format-wish "senddatastring [~a get ~a ~a]" (widget-path text) start end)
  (read-data))

(defmethod get-text-length ((txt text) start end)
  "Gets the length of text between two tk text indices. May be quicker than sending
  the text over the process boundaries and then calling length on it."
  (format-wish "senddatastring [string length [~a get ~a ~a]]" (widget-path txt) start end)
  (read-from-string (read-data)))

(defmethod get-text-to-cursor ((text text))
  (format-wish "senddatastring [~a get \"1.0\" \"insert\"]" (widget-path text))
  (read-data))

(defmethod get-current-char ((text text))
  (format-wish "senddatastring [~a get insert]" (widget-path text))
  (read-data))

(defmethod get-current-word ((text text))
  (format-wish "senddatastring [~a get \"insert wordstart\" \"insert wordend\"]" (widget-path text))
  (read-data))

(defmethod get-current-line ((text text))
  (format-wish "senddatastring [~a get \"insert linestart\" \"insert lineend\"]" (widget-path text))
  (read-data))

(defmethod get-to-end-current-line ((text text))
  (format-wish "senddatastring [~a get \"1.0\" \"insert lineend\"]" (widget-path text))
  (read-data))

(defmethod get-current-line-to-cursor ((text text))
  (format-wish "senddatastring [~a get \"insert linestart\" \"insert\"]" (widget-path text))
  (read-data))

(defmethod selection-start ((text text))
  (format-wish "senddatastring [~a tag ranges sel]" (widget-path text))
  (read-data))

(defmethod selected ((text text))
  (when (not (equal (selection-start text) ""))
    (format-wish "senddatastring [~a get sel.first sel.last]" (widget-path text))
    (read-data)))

(defmethod get-modify ((text text))
  (format-wish "senddatastring [~a edit modified]" (widget-path text))
  (read-data))

(defmethod reset-modify ((text text))
  (format-wish "~a edit modified 0" (widget-path text)))

(defmethod un-bind ((w widget) key)
  "Unbind an event for a specific instance of a widget."
  (format-wish "bind ~a ~a {break}" (widget-path w) key))

(defun remove-binding (widget event)
  "Unbind an event for all instances of a widget class."
  (format-wish "bind ~a ~a { }" widget event))

(defun withdraw-wish-toplevel ()
   (format-wish "wm withdraw ."))

(defmethod add-tag ((txt text) name start end)
  (format-wish "~a tag add ~a ~a ~a" (widget-path txt) name start end))

(defmethod remove-tag ((txt text) name start end)
  (format-wish "~a tag remove ~a ~a ~a" (widget-path txt) name start end))

(defmethod select-range ((txt text) start end)
  (format-wish "~a tag add sel ~a ~a" (widget-path txt) start end))

(defmethod select-all ((txt text))
  (format-wish "~a tag add sel 1.0 end" (widget-path txt)))

(defmethod deselect-all ((txt text))
  (format-wish "~a tag remove sel 1.0 end" (widget-path txt)))

(defmethod add-tags ((txt text) indexed-tokens tag-name)
  (format-wish "eval ~a tag add ~a ~a" (widget-path txt) tag-name indexed-tokens))

(defmethod cut ((txt text))
  (format-wish "tk_textCut ~a" (widget-path txt)))

(defmethod copy ((txt text))
  (format-wish "tk_textCopy ~a" (widget-path txt)))

(defmethod paste ((txt text))
  (format-wish "tk_textPaste ~a" (widget-path txt)))

(defgeneric get-selected-value (l))

(defmethod get-selected-value ((l listbox))
  (format-wish "senddatastring \"[~a get [~a curselection]]\"" (widget-path l) (widget-path l))
  (read-data))

;;; listbox-configure ??
(defmethod item-configure ((listbox listbox) index option value)
  (format-wish "~a itemconfigure ~a -~a ~a" (widget-path listbox) index option value))

(defmethod listbox-activate ((listbox listbox) index)
  (format-wish "~a activate ~a" (widget-path listbox) index))

(defmethod configure-pane ((pw paned-window) pane attribute value)
  (format-wish "~a paneconfigure ~a -~a ~a" (widget-path pw) (widget-path pane) attribute value))

(defmethod panecget ((pw paned-window) pane attribute)
  (format-wish "senddatastring [~a panecget ~a -~a]" (widget-path pw) (widget-path pane) attribute)
  (read-data))





