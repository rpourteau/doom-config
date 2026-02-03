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
      :desc "Toggle terminal" "t" #'my/toggle-term)

;; SPC o - open
(map! :leader
      :prefix "o"
      :desc "New frame" "f" #'make-frame-command)

;;; Evil mode customizations

;; Enable C-x C-s in insert mode
(define-key evil-insert-state-map (kbd "C-x C-s") #'save-buffer)

;; Make :q kill the buffer instead of the frame
(evil-ex-define-cmd "q" 'kill-current-buffer)

;; C-y paste from register 0 (most recent yank, unaffected by deletes)
(defun my/paste-from-register-0 ()
  "Paste from register 0 (most recent yank) after cursor."
  (interactive)
  (evil-paste-after 1 ?0))

(defun my/insert-from-register-0 ()
  "Insert contents of register 0 at point."
  (interactive)
  (insert (evil-get-register ?0)))

(defun my/visual-paste-from-register-0 ()
  "In visual mode, paste from register 0 replacing selection."
  (interactive)
  (evil-paste-from-register ?0))

(map! :n "C-y" #'my/paste-from-register-0
      :i "C-y" #'my/insert-from-register-0
      :v "C-y" #'my/visual-paste-from-register-0
      :v "p" #'my/visual-paste-from-register-0
      :v "P" #'my/visual-paste-from-register-0)

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
