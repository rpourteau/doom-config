;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; systemrdl mode
(package! systemrdl-mode
  :recipe (:host github :repo "paul-donahue/systemrdl-mode"))

(package! vhdl-ext
  :recipe (:host github :repo "gmlarumbe/vhdl-ext"))

(package! symbol-overlay
  :recipe (:host github :repo "wolray/symbol-overlay"))

(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))
