;;;; Evil configs

;; Config & Hotfixes
;; Fixes orgmode TAB functionality (see https://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working)
;; (setq evil-want-C-i-jump nil)

;; Packages
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :hook
  ('slime-repl-mode . (lambda ()
                        (define-key evil-normal-state-map (kbd "C-r") 'helm-slime-repl-history)
                        (define-key evil-normal-state-map (kbd "C-m") 'slime-repl-return)
                        (define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition)))
  :config
  (evil-mode t)
  
  ;; Repeat search with 'S' and 's'
  (define-key evil-normal-state-map "s" 'evil-search-next)
  (define-key evil-normal-state-map "S" 'evil-search-backward)

  ;; to the last non-blank character of a line with 'L' and 'N'
  (define-key evil-motion-state-map "L" 'evil-last-non-blank)
  (define-key evil-visual-state-map "L" 'evil-last-non-blank)
  (define-key evil-motion-state-map "N" 'evil-last-non-blank)
  (define-key evil-visual-state-map "N" 'evil-last-non-blank)

  ;; to the beginning of line with 'H'
  (define-key evil-motion-state-map "H" 'beginning-of-line-text)
  (define-key evil-visual-state-map "H" 'beginning-of-line-text)

  ;; remap change word to 'C' (capital 'c')
  (define-key evil-visual-state-map "c" 'evil-change)
  (define-key evil-normal-state-map "C" 'evil-change)
  
  ;; remap movement keys to htcn (dvorak)
  (define-key evil-normal-state-map "c" 'evil-previous-line)
  (define-key evil-visual-state-map "t" 'evil-next-line)
  (define-key evil-normal-state-map "t" 'evil-next-line)
  (define-key evil-visual-state-map "n" 'evil-forward-char)
  (define-key evil-normal-state-map "n" 'evil-forward-char)
  
  ;;  Ctrl + g as ESC
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Ctrl + y as yank
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-motion-state-map (kbd "C-y") 'yank)
  (define-key evil-visual-state-map (kbd "C-y") 'yank)

  ;; Ctrl + h as delete backward char
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char)
  (define-key evil-motion-state-map (kbd "C-h") 'evil-delete-backward-char)
  (define-key evil-visual-state-map (kbd "C-h") 'evil-delete-backward-char)

  (define-key evil-normal-state-map "-" 'comment-line)
  (define-key evil-visual-state-map "-" 'comment-region))

(use-package evil-leader
  :ensure t
  :after evil
  :diminish
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ":"     'helm-M-x
    "<SPC>" 'helm-M-x
    ;; f for files
    "f"   'helm-find-file-or-projectile
    "n"   'helm-find-my-notes
    "w"   'helm-find-my-workspace
    ;; b for buffers
    "b"   'helm-buffers-list
    ;; g for git
    "g"   'magit-status
    ;; w for word
    "/"   'helm-occur
    ;; t for terminal
    "t"   'vterm-toggle
    ;; d for dired 
    "d"   'treemacs
    ;; s for slime
    "s"   'slime
    ;; random
    "F"   'toggle-frame-fullscreen
    "|"   'split-window-horizontally
    "_"   'split-window-vertically))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1))

;; (use-package key-chord
;;   :ensure t
;;   :config
;;   (setq key-chord-two-keys-delay 0.5)
;;   (key-chord-define evil-insert-state-map "hh" 'evil-normal-state)
;;   (key-chord-mode 1))

(provide 'init-evil)
