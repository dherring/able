;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is the master configuration file for ABLE. All settings in this file
;;; can be overriden by creating a file named .able in your home directory,
;;; e.g. ~/.able, and then using (setf able::*setting* ...) as required.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :able)

(defparameter *window-width* 640)
(defparameter *window-height* 480)
(defparameter *window-x* 30)
(defparameter *window-y* 30)

(defparameter *listener-lines* 10
  "The number of text output lines to display in the listener. The listener can
  be scrolled using the mouse wheel but only this number of lines are displayed
  at a time.")

(defparameter *buffer-font* "Courier 12 normal roman"
  "The font used for the editor and REPL. Courier is guaranteed to be
  available on all Tk platforms.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax highlighting.
;;; 
;;; These are configurable using 'html style' hex values or standard colour
;;; names (refer to the Tcl/Tk documentation).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *highlight-text* "#2D1E27")
(defparameter *highlight-background* "#FFFFFF")
(defparameter *highlight-primary* "#1900D5")
(defparameter *highlight-secondary* "#991C1C")
(defparameter *highlight-comments* "#00732A")
(defparameter *highlight-paren-match* "#F3752F")
(defparameter *highlight-error* "#FF4343")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keyboard bindings.
;;;
;;; Some keyboard bindings are pre-set by Tcl and can't be changed without
;;; modifying the Tk installation. For a list of all available key symbol names
;;; please refer to the Tcl/Tk documentation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *key-new-file* "<Control-n>" "Create a new file")
(defparameter *key-open-file* "<Control-o>" "Open a new file")
(defparameter *key-open-file-browser* "<Control-Alt-o>" "Open a new file, with browser GUI")
(defparameter *key-load-file* "<Control-l>" "Load a file from disc")
(defparameter *key-close-file* "<Control-w>" "Close a file")
(defparameter *key-save-file* "<Control-s>" "Save the current file")
(defparameter *key-save-as-file* "<Control-S>" "Save the current file under a new name")
(defparameter *key-save-as-file-browser* "<Control-Alt-s>" "Save the current file under a new name, with browser GUI")
(defparameter *key-reload-file* "<Control-r>" "Re-load the current file")
(defparameter *key-compile-file* "<Control-k>" "Compile the current file")
(defparameter *key-previous-file* "<Control-B>" "Cycle to previous open files")
(defparameter *key-next-file* "<Control-b>" "Cycle to next open files")
(defparameter *key-select-file* "<Control-Alt-b>" "Select open file")
(defparameter *key-find* "<Control-f>" "Find text in current file")
(defparameter *key-find-again* "<Control-g>" "Repeat the previous search")
(defparameter *key-cut* "<Control-x>" "Cut the selected text")
(defparameter *key-copy* "<Control-c>" "Copy the selected text")
(defparameter *key-paste* "<Control-v>" "Paste the selected text")
(defparameter *key-select-all* "<Control-a>" "Select all text in the file")
(defparameter *key-replace* "<Control-t>" "Replace text with regexp")
(defparameter *key-goto-line* "<Control-i>" "Goto line number")
(defparameter *key-reformat* "<Control-j>" "Indent the current form")
(defparameter *key-macro-expand* "<Control-m>" "Macroexpand form in listener")
(defparameter *key-copy-to-repl* "<Control-e>" "Evaluate form")
(defparameter *key-invoke-debugger* "<Control-D>" "Invoke the native debugger")
(defparameter *key-quit-able* "<Control-q>" "Quit ABLE")
(defparameter *key-code-complete* "<Tab>" "Cycle available code completions")
(defparameter *key-lookup* "<Control-d>" "Find definition in Hyperspec or src")
(defparameter *key-asdf-load* "<Control-p>" "Load an ASDF system")
(defparameter *key-reset-listener* "<Control-0>" "Reset the listener")
(defparameter *key-line-start* "<Alt-s>" "Goto start of line")
(defparameter *key-line-end* "<Alt-e>" "Goto end of line")

(defparameter *indentation-rules*  '(("if" . 4)
                                     ("cond" . 6)
                                     ("and" . 5)
                                     ("or" . 4)
                                     ("eq" . 4)
                                     ("list" . 6)
                                     ("loop" . 6))
  "This is a list of pairs comprising the symbol and the required indentation
  size. There are additional rules for let blocks as well as a default
  2-character indent.")

(defparameter *hyperspec-root* "http://www.lispworks.com/documentation/HyperSpec/Body/"
  "Where to find the Common Lisp Hyperspec. The default is to use the version
  on the LispWorks server but this can be overriden with a local URL to a
  copy on your machine. You must ensure that this uses a URL syntax understood
  by the browser specified in *web-browser* below")

(defparameter *web-browser*
  (first
   (list
    #+(or (and :clisp :win32) (and :sbcl :win32) (and :ccl :windows))
    "C:/Progra~1/Intern~1/iexplore.exe"
    #+(or (and :clisp :unix (not :macos)) (and :sbcl :linux) (and :ccl :linux))
    "/usr/bin/firefox"
    #+(or (and :ccl :darwin) (and :clisp :macos) (and :sbcl :darwin))
    "open"
    ;; default
    "firefox")))

(defparameter *user-load-paths* nil
  "A list of additional user specified places to find systems")

(defparameter *watch-directories* nil
  "A list of directories to watch and parse for defining forms")

;; Interface language.
;; Valid options: 'en (English), 'eo (Esperanto)
(setf translate:*language* 'en)






