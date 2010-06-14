#| Note: I'm gonna have to stop working on this for a while; the following is a dump of some ideas I had.

The current code "works" from a single (package-browser) entry, but it is not ready for end users.  SHOW-SYMBOLS has probably the "best" coding style in this file; but I'm still learning Ltk.

The goal was to have a menu entry in ABLE that opens a window something like

=ABLE CL browser=
+-------+-------+
|package|show   |
|browser|symbols|
+-------+-------+
|inspect|inspect|
|package|symbol |
+-------+-------+

Clicking a package in package-browser would update (inspect package) and show-symbols.  Clicking a symbol in show-symbols would update (inspect symbol).  (inspect symbol) would be like (inspect package) but for symbol-name, symbol-function, etc.

Eventually add in some inspector-like functionality, a web-browser style navigation system, etc.

Navigation:
forward, backward, history dropdown, package name entry, symbol name entry (both with fuzz)
"bookmarks" for storing symbols (grey out those not in current image)

Inspector interface:
press i to inspect the currently highlighted entry
press m to modify it

When showing lists/arrays, have a way to (auto?)select between comma-separated lists, listboxes, and comboboxes.
|#

(defpackage :able.pb
  (:use :cl :ltk :able)
  (:shadow :inspect))

(in-package :able.pb)

(defgeneric inspect (thing &optional parent-frame))

(defun package-browser ()
  (with-ltk ()
    (let* ((top (make-instance 'frame))
           (tree (make-instance 'treeview
                                :master top
                                :columns "{1}"
                                ))
           (sc (make-instance 'scrollbar :master top)))

      (format-wish "~a heading #~a -~(~a~) ~a" (widget-path tree) 0 :text "name")
      (format-wish "~a heading #~a -~(~a~) ~a" (widget-path tree) 1 :text "#syms")
      
      (dolist (p (sort (mapcar #'package-name (list-all-packages))
                       #'string<))
        (treeview-insert tree
                         :text p
                         :values (let ((count 0)
                                       (pack (find-package p)))
                                   (do-symbols (symbol pack)
                                     (when (eql (symbol-package symbol)
                                                pack)
                                       (incf count)))
                                   (list count))
                         ))
      (configure tree "yscrollcommand" (format nil "~A set" (widget-path sc)))
      (configure sc "command" (format nil "~A yview" (widget-path tree)))

      (pack top :side :left :fill :both :expand t)
      (pack tree :side :left :fill :both :expand t)
      (pack sc :side :left :fill :y :expand nil)

      (bind tree "<1>" (lambda (ev)
                         (declare (ignore ev))
                         (let ((item (treeview-focus tree)))
                           (unless (string= item "")
                             (let ((name (treeview-item tree item :text))
                                   (top (make-instance 'toplevel))
                                   (tops (make-instance 'toplevel)))
                               (format t "package: ~A~%" name)
                               (wm-title top "Package inspector")
                               (inspect (find-package name) top)
                               (wm-title tops "Symbol inspector")
                               (make-instance 'symbol-frame
                                              :package-designator name
                                              :master tops))
                             )))))))

(defun comma-sep-string (list)
  (format nil "~{~A~#[~:;, ~]~}" list))
(defun sorted-package-names (package-list)
  (sort (mapcar #'package-name package-list)
        #'string<))

(defmacro ensure-ltk ((&rest options &key (debug 2) &allow-other-keys)
                      &body body)
  "Wrap BODY in with-ltk unless a connection already exists."
  (declare (ignore debug))
  (let ((fname (gensym)))
    `(flet ((,fname () ,@body))
       (if (wish-stream *wish*)
           (,fname)
           (with-ltk ,options (,fname))))))
