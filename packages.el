;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; systemrdl mode
(package! systemrdl-mode
  :recipe (:host github :repo "paul-donahue/systemrdl-mode"))

;; Required to support LSP on VHDL
(package! vhdl-ext
  :recipe (:host github :repo "gmlarumbe/vhdl-ext"))

;; Better integrated symbol highlight
(package! symbol-overlay
  :recipe (:host github :repo "wolray/symbol-overlay"))

;; Claude Code IDE
(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))

;; EAT terminal easier than vterm (do not need to compile C library)
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))
