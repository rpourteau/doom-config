;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))

;; Load configuration modules
(require 'core)
(require 'keybindings)
(require 'languages)
(require 'tools)
