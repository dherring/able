(in-package :able)

;; translate is nice, but sends a bunch of annoying
;; and unnecessary text to stdout.
(with-open-stream (*standard-output* (make-broadcast-stream))
  (translate:add-translations
    'en
    "File"	"File"
    ;; ----------
    "New file"	"New file"
    "Open file"	"Open file"
    "Open file browser" "Open file browser"
    "Save file"	"Save file"
    "Save as file" "Save as file"
    "Save as file browser" "Save as file browser"
    "Exit"	"Exit"

    "Edit"	"Edit"
    ;; ----------
    "Cut"  	"Cut"
    "Copy"  	"Copy"
    "Paste" 	"Paste"

    "Select all"	"Select all"
    "Reindent"	"Reindent"
    "Find"  	"Find"
    "Find again"	"Find again"
    "Goto line"	"Goto line"

    "Buffers"	"Buffers"
    ;; ----------
    "Next buffer" "Next buffer"
    "Close buffer" "Close buffer"
    "Select buffer" "Select buffer"

    "Lisp"	"Lisp"
    ;; ----------
    "Macroexpand"	"Macroexpand"
    "Copy to REPL" "Copy to REPL"
    "Complete symbol" "Complete symbol"
    "CLHS lookup"	"CLHS lookup"
    "(Re)load buffer" "(Re)load buffer"
    "Load file"	"Load file"
    "Load ASDF"	"Load ASDF"
    "Compile file" "Compile file"
    "Invoke native debugger" "Invoke native debugger"
    "Reset listener" "Reset listener"

    "evaluating..." "evaluating..."
    "highlighting..." "highlighting..."

    ;; info messages
    "search wrapped around file" "search wrapped around file"
    "No Hyperspec entry or src location found"
    "No Hyperspec entry or src location found"

    ;; error messages
    "no matches"  "no matches"
    "non integer argument supplied to goto"
    "non integer argument supplied to goto"
    "file not found" "file not found"
    "please save before loading" "please save before loading"
    "unsaved files exist...quit anyway?"
    "unsaved files exist...quit anyway?"


    "please save before compiling" "please save before compiling"
    "search reached end of file" "search reached end of file"

    ;; misc
    "untitled"	"untitled"
    "untitled #"	"untitled #"
    "find:"	"find:"

    "y"   	"y"
    "yes"  	"yes"
    "n"   	"n"
    "no"   	"no"

    "open:"	"open:"
    "goto:"	"goto:"
    "system:"	"system:"
    "buffer number:" "buffer number:"
    "load:"	"load:"
    "save:"	"save:"
    "unsaved file...close anyway?" "unsaved file...close anyway?"
    "unsaved files exist...quit anyway?" "unsaved files exist...quit anyway?")

  (translate:add-translations
    'eo
    "File"	"Dosiero"
    ;; ----------
    "New file"	"Nova"
    "Open file"	"Malfermi"
    "Open file browser" "Malfermi per foliumilo"
    "Save file"	"Konservi"
    "Save as file" "Konservi kiel"
    "Save as file browser" "Konservi per foliumilo"
    "Exit"	"Ĉesi"

    "Edit"	"Redakti"
    ;; ----------
    "Cut"  	"Eltondi"
    "Copy"  	"Kopii"
    "Paste" 	"Alglui"

    "Select all"	"Elekti ĉion"
    "Reindent"	"Rekrommarĝeni"
    "Find"  	"Serĉi"
    "Find again"	"Reserĉi"
    "Goto line"	"Aliri linion"

    "Buffers"	"Bufroj"
    ;; ----------
    "Next buffer" "Sekva bufro"
    "Close buffer"
    "Fermi bufron"
    "Select buffer" "Elekti bufron"

    "Lisp"	"Lispo"
    ;; ----------
    "Macroexpand"	"Makrogeneradi"
    "Copy to REPL" "Kopii al REPL"
    "Complete symbol" "Kompletigi simbolon"
    "CLHS lookup" "Serĉi en CLHS"
    "(Re)load buffer" "(Re)ŝarĝi bufron"
    "Load file"	"Ŝarĝi dosieron"
    "Load ASDF"	"Ŝarĝi ASDF'on"
    "Compile file" "Kompili dosieron"
    "Invoke native debugger" "Alvoki erarserĉilon"
    "Reset listener" "Restartigi aŭskultanton"

    "evaluating..." "taksanta..."
    "highlighting..." "prilumanta..."

    ;; info messages
    "search wrapped around file" "serĉado ĉirkaŭfluis en dosiero"
    "No Hyperspec entry or src location found"
    "Hyperspec enigo aŭ src loko netrovita"

    ;; error messages
    "no matches"  "sen rezultoj"
    "non integer argument supplied to goto"
    "argumento nenombra donita al aliri"
    "file not found" "dosiero netrovita"
    "please save before loading" "bonvolu, konservi antaŭ ŝarĝi"


    "please save before compiling" "bonvolu, konservi antaŭ kompili"
    "search reached end of file" "serĉado alvenis finon de doserion"

    ;; misc
    "untitled"	"sentitolo"
    "untitled #"	"sentitolo #"
    "find:"	"serĉi:"

    "y"   	"j"
    "yes"  	"jes"
    "n"   	"n"
    "no"   	"ne"

    "open:"	"malfermi:"
    "goto:"	"aliri:"
    "system:"	"sistemo:"
    "buffer number:" "bufrnombro:"
    "load:"	"ŝarĝi:"
    "save:"	"konservi:"
    "unsaved file...close anyway?"
    "nekonservita doserio...ĉu kvankam ĉesi?"
    "unsaved files exist...quit anyway?"
    "nekonservitaj dosieroj ekzistas...ĉu kvankam ĉesi?"))
