;;;; Evil configs

;; Config & Hotfixes
;; Fixes orgmode TAB functionality (see https://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working)
;; (setq evil-want-C-i-jump nil)

;; Packages
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (define-key evil-visual-state-map "c" 'evil-change)
  (define-key evil-normal-state-map "C" 'evil-change)
  (define-key evil-normal-state-map "c" 'evil-previous-line)
  (define-key evil-visual-state-map "t" 'evil-next-line)
  (define-key evil-normal-state-map "t" 'evil-next-line)
  (define-key evil-visual-state-map "n" 'evil-forward-char)
  (define-key evil-normal-state-map "n" 'evil-forward-char)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-d") 'backward-delete-char)
  (define-key evil-normal-state-map "-" 'comment-line)
  (define-key evil-visual-state-map "-" 'comment-region)
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
    ;; f for files
    "f f"   'helm-find-file-or-projectile
    "f n"   'helm-find-my-notes
    "f w"   'helm-find-my-workspace
    ;; b for buffers
    "b l"   'helm-buffers-list
    ;; g for git
    "g s"   'magit-status
    ;; w for word
    "w f"   'helm-occur
    "x o"   'other-window
    ;; t for terminal
    "t"     'vterm-toggle
    ;; d for dired 
    "d"     'treemacs
    ;; s for slime
    "s l"   'slime
    ;; random
    "F"     'toggle-frame-fullscreen
    "|"     'split-window-horizontally
    "_"     'split-window-vertically))

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
