;;; lisp/core.el --- Core settings -*- lexical-binding: t; -*-

;; User identity
(setq user-full-name "rpourteau"
      user-mail-address "remy.pourteau@cosylab.com")

;; Theme and font
(setq doom-font (font-spec :family "JetBrains Mono NL" :size 18))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type 't)

;; Highlight current line globally
(setq global-hl-line-modes t)

;; Org directory
(setq org-directory "~/org/")

;; Enable case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'core)
;;; core.el ends here
