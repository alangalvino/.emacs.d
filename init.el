(server-start)

;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Use-package 
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Aesthetic
(setq-default ispell-dictionary "american")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 100)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq epa-pinentry-mode 'loopback)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq sentence-end-double-space nil)
(setq whitespace-line-column 120)
(setq whitespace-style '(face lines-tail)) ;; highlights chars in lines over whitespace-line-column size

(display-time-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
(blink-cursor-mode 0)

(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 200
                    :weight 'normal)

;; Global keybindings
(global-set-key (kbd "M-q") 'set-justification-full)
(global-set-key (kbd "C-x _") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-horizontally)
(global-set-key (kbd "C-x |") 'split-window-vertically)

;; Load Quicklisp
(if (file-exists-p "~/.quicklisp/slime-helper.el") 
    (load (expand-file-name "~/.quicklisp/slime-helper.el")))

;; Load Slime helpers
(if (file-exists-p "~/.roswell/helper.el")
    (progn
      (load (expand-file-name "~/.roswell/helper.el"))
      (setq inferior-lisp-program "/usr/local/bin/ros -L sbcl -Q -l ~/.sbclrc run")))

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

(use-package projectile
  :ensure t
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map))

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-o") 'helm-occur)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line))

(use-package helm-slime
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t))

(use-package magit
  :ensure t)

(use-package paredit
  :ensure t
  :config
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook       #'aggressive-indent-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'aggressive-indent-mode)
  (add-hook 'ielm-mode-hook             #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook             #'aggressive-indent-mode)
  (add-hook 'lisp-interaction-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook           #'aggressive-indent-mode))

(use-package org-bullets
  :ensure t
  :config
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook (lambda()
                             (org-bullets-mode 1))))

(use-package treemacs 
  :ensure t)

;; Orgmode
(setq org-startup-folded t)

(org-babel-do-load-languages
 'org-babel-load-languages '((lisp . t)))

(add-hook 'org-mode-hook (lambda ()
                           (org-toggle-pretty-entities)
                           (auto-fill-mode)
                           (org-indent-mode)))

(setq org-notes-home-dir "~/Dropbox/Personal/orgnotes/")

(setq org-capture-templates
      '(("i" "Adicionar nota à Caixa de idéias" entry (file+headline "~/Dropbox/Personal/orgnotes/caderno.org.gpg" "Caixa de idéias")
         "* %?\n%T\n" :prepend t)
        ("n" "Adicionar nota ao Caderno de notas avulsas" entry (file+headline "~/Dropbox/Personal/orgnotes/caderno.org.gpg" "Caderno de notas avulsas")
         "* %?\n%T\n" :prepend t)
        ("r" "Random note" entry (file+headline "~/Dropbox/Personal/orgnotes/cto.org.gpg" "Notes") "* %?\n%T\n" :prepend t)
        ("t" "Add a To-do item" entry (file+headline "~/Dropbox/Personal/orgnotes/cto.org.gpg" "To-do")
         "* TODO %?\n%T\n")))

;; Functions
(defun pgformatter-on-region ()
  "A function to invoke pgFormatter as an external program."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max)))
        (pgfrm "/usr/bin/pg_format" ) )
    (shell-command-on-region b e pgfrm (current-buffer) 1)))

(defun helm-find-in-dir (dir)
  (let ((default-directory dir))
    (helm-find-files nil)))

(defun helm-find-my-notes ()
  (interactive)
  (helm-find-in-dir org-notes-home-dir))

(defun helm-find-my-workspace ()
  (interactive)
  (helm-find-in-dir "~/workspace/"))

;; macOS specific configs

;; Enables non native fullscreen
(setq ns-use-native-fullscreen nil)

;; Macs OS Command as Meta Key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'control)

;; Open just one frame
(setq ns-pop-up-frames nil)

;; Don't pass command to Mac OS, ex.: Cmd + h would not hide emacs
(setq mac-pass-command-to-system nil)

;; Setup custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
