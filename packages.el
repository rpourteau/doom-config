;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; systemrdl mode
(package! systemrdl-mode
  :recipe (:host github :repo "paul-donahue/systemrdl-mode"))

;; VHDL tree sitter and LSP
(package! vhdl-ts-mode
  :recipe (:host github :repo "gmlarumbe/vhdl-ts-mode"))

(package! vhdl-ext
  :recipe (:host github :repo "gmlarumbe/vhdl-ext"))
