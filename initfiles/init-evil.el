;;;; Evil configs

;; Config & Hotfixes
;; Fixes orgmode TAB functionality (see https://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working)
(setq evil-want-C-i-jump nil)

;; Packages
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (define-key evil-normal-state-map (kbd "C-m") 'slime-repl-return)
  (define-key evil-normal-state-map (kbd "C-r") 'helm-slime-repl-history)
  (define-key evil-motion-state-map "H" 'beginning-of-line-text)
  (define-key evil-motion-state-map "L" 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition))

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
    "f f"   'helm-find-files
    "f o"   'helm-find-my-notes
    "f w"   'helm-find-my-workspace
    "f r"   'helm-recentf
    "g c"   'ace-jump-char-mode
    "g w"   'ace-jump-word-mode
    "g o"   'helm-occur
    "b"     'helm-buffers-list
    "m"     'magit-status
    "x o"   'other-window
    "/"     'helm-projectile-ag
    "t"     'vterm-toggle
    "d"     'treemacs
    "o c"   'org-capture
    "o f"   'helm-find-my-notes
    "s l"   'slime
    "f s"   'toggle-frame-fullscreen
    "s v"   'split-window-horizontally
    "s h"   'split-window-vertically))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "hh" 'evil-normal-state)
  (key-chord-mode 1))

(provide 'init-evil)
