;;; lisp/tools.el --- Tools and utilities -*- lexical-binding: t; -*-

;;; Symbol overlay
(use-package! symbol-overlay
  :bind (("C-<f3>" . symbol-overlay-put)
         ("<f3>"   . symbol-overlay-jump-next)
         ("S-<f3>" . symbol-overlay-jump-prev)
         ("M-<f3>" . symbol-overlay-query-replace))
  :hook (prog-mode . symbol-overlay-mode)
  :config
  ;; Unbind keybinds to prevent conflict with evil
  (define-key symbol-overlay-map (kbd "h") nil)
  (define-key symbol-overlay-map (kbd "d") nil)
  (define-key symbol-overlay-map (kbd "e") nil)
  (define-key symbol-overlay-map (kbd "i") nil)
  (define-key symbol-overlay-map (kbd "n") nil)
  (define-key symbol-overlay-map (kbd "p") nil)
  (define-key symbol-overlay-map (kbd "q") nil)
  (define-key symbol-overlay-map (kbd "r") nil)
  (define-key symbol-overlay-map (kbd "s") nil)
  (define-key symbol-overlay-map (kbd "t") nil)
  (define-key symbol-overlay-map (kbd "w") nil)
  (define-key symbol-overlay-map (kbd "<") nil)
  (define-key symbol-overlay-map (kbd ">") nil))

;;; Doom modeline
(after! doom-modeline
  (setq
   doom-modeline-github t
   doom-modeline-major-mode-icon t))

;;; Terminal toggle
(defvar my/term-buffer-name "*my-term*")

(defun my/toggle-term ()
  "Toggle a horizontal `term` split."
  (interactive)
  (let ((buf (get-buffer my/term-buffer-name)))
    (if (and buf (get-buffer-window buf))
        ;; If term is visible, delete its window
        (delete-window (get-buffer-window buf))
      ;; Otherwise, show it in a horizontal split
      (let ((term-window (split-window-vertically -15)))
        (select-window term-window)
        (if buf
            (switch-to-buffer buf)
          (let ((term-shell (getenv "SHELL")))
            (term term-shell)
            (rename-buffer my/term-buffer-name)))))))

(add-hook 'term-mode-hook #'ansi-color-for-comint-mode-on)

;;; Workspace / Frames
;; Prevent new frames from switching/creating workspaces
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override -1))

;;; Claude Code IDE
(after! claude-code-ide
  (setq claude-code-ide-terminal-backend 'eat))

(after! eat
  (add-hook 'eat-mode-hook
            (lambda ()
              (when (string-match-p "claude-code" (buffer-name))
                (evil-emacs-state)))))

(provide 'tools)
;;; tools.el ends here
