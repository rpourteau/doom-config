;;; lisp/keybindings.el --- Custom keybindings -*- lexical-binding: t; -*-

;;; Global keybindings
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c d") #'lsp-find-definition)

;;; Leader keybindings (SPC prefix)

;; SPC l - custom commands
(map! :leader
      :prefix "l"
      :desc "Remove symbol overlay" "s" #'symbol-overlay-remove-all
      :desc "Launch Claude Code IDE" "i" #'claude-code-ide
      :desc "List flycheck errors" "l" #'flycheck-list-errors
      :desc "Start python interpreter" "p" #'run-python
      :desc "Comment-line" "c" #'comment-line
      :desc "Call rgrep" "g" #'rgrep)

;; SPC t - toggles
(map! :leader
      :prefix "t"
      :desc "Toggle menu bar mode" "m" #'menu-bar-mode
      :desc "Toggle treemacs" "t" #'+treemacs/toggle)

;; SPC o - open
(map! :leader
      :prefix "o"
      :desc "New frame" "f" #'make-frame-command
      :desc "Toggle terminal" "t" #'my/toggle-term)

;;; Evil mode customizations

;; Enable C-x C-s in insert mode
(define-key evil-insert-state-map (kbd "C-x C-s") #'save-buffer)

;; Make :q kill the buffer instead of the frame
(evil-ex-define-cmd "q" 'kill-current-buffer)

;; Restore C-y to yank from kill-ring in insert mode
(map! :i "C-y" #'yank)

;; Make C-e work like vanilla Emacs (end of line)
(map! :n "C-e" #'evil-end-of-line
      :v "C-e" #'evil-end-of-line)

;; Easy escape from insert mode
(after! evil-escape
  (setq evil-escape-delay 0.5)
  (setq evil-escape-key-sequence "jk"))

;; Center after half-page scrolls and searches
(after! evil
  ;; Half-page scrolls: C-d / C-u
  (defun my/evil-scroll-down-center ()
    "Half-page down, then center."
    (interactive)
    (evil-scroll-down nil)
    (recenter))

  (defun my/evil-scroll-up-center ()
    "Half-page up, then center."
    (interactive)
    (evil-scroll-up nil)
    (recenter))

  (map! :n "C-d" #'my/evil-scroll-down-center
        :n "C-u" #'my/evil-scroll-up-center
        :v "C-d" #'my/evil-scroll-down-center
        :v "C-u" #'my/evil-scroll-up-center)

  ;; Searches: *, #, n, N - recenter after search
  (defun my/recenter-after-search (&rest _)
    (recenter))

  (dolist (cmd '(evil-search-word-forward
                 evil-search-word-backward
                 evil-ex-search-next
                 evil-ex-search-previous))
    (advice-add cmd :after #'my/recenter-after-search)))

(provide 'keybindings)
;;; keybindings.el ends here
