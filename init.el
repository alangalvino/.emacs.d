(server-start)
(require 'cl)

;; Inits 'package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

; Bootstraps 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Sets up misc configurations
(setq org-notes-home-dir "~/Dropbox/Koala/orgnotes/")

(setq org-capture-templates
      '(("i" "Adicionar nota à Caixa de idéias" entry (file+headline "~/Dropbox/Koala/orgnotes/caderno.org.gpg" "Caixa de idéias")
         "* %?\n%T\n" :prepend t)
        ("n" "Adicionar nota ao Caderno de notas avulsas" entry (file+headline "~/Dropbox/Koala/orgnotes/caderno.org.gpg" "Caderno de notas avulsas")
         "* %?\n%T\n" :prepend t)
         ("t" "Adicionar tarefa" entry (file+headline "~/Dropbox/Koala/orgnotes/caderno.org.gpg" "Tarefas")
         "* TODO %?\n" :prepend t)))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)))

;; Org export: removes brackets from a timestamp
(defun org-time-string-format (string-time format-string)
  (format-time-string  format-string (org-time-string-to-time string-time)))

(defun remove-brackets (string)
  (replace-regexp-in-string "[<>]\\|[][]" "" string))

(defun org-export-filter-timestamp-remove-brackets (time-string backend info)
  "removes relevant brackets from a timestamp"
  (let ((ts (org-time-string-format time-string "%d de %B de %Y")))
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (remove-brackets ts)))))

(eval-after-load 'ox '(add-to-list
                       'org-export-filter-timestamp-functions
                       'org-export-filter-timestamp-remove-brackets))

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


;; Global keybindings
(global-set-key (kbd "M-q") 'set-justification-full)

(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 150
                    :weight 'normal)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook (lambda ()
                            'whitespace-mode
                            'flyspell-mode))

(add-hook 'org-mode-hook (lambda ()
                           (auto-fill-mode)
                           (org-indent-mode)))

(add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)


;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; My Functions

(defun helm-find-in-dir (dir)
  (let ((default-directory dir))
    (helm-find-files nil)))

(defun helm-find-my-notes ()
  (interactive)
  (helm-find-in-dir org-notes-home-dir))

(defun helm-find-my-workspace ()
  (interactive)
  (helm-find-in-dir "~/workspace/"))

(defun evil-collection-vterm-escape-stay ()
  "Go back to normal state but don't move cursor backwards.
Moving cursor backwards is the default vim behavior but
it is not appropriate in some cases like terminals."
  (setq-local evil-move-cursor-back nil))

;; Installs and configures new packages
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(use-package diminish
  :ensure t
  :config
  (diminish 'undo-tree-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-fill-mode)
  (diminish 'org-indent-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package vterm
  :ensure t
  :after evil
  :config
	(evil-define-key 'insert vterm-mode-map (kbd "C-y")      #'vterm-yank)
  (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay))

(use-package go-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ((go-mode .lsp)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package company
  :ensure t
  :diminish
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-lsp
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package treemacs-evil
  :ensure t)

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package ace-jump-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
    (setq web-mode-markup-indent-offset 2)))

(use-package helm
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

(use-package helm-ag
  :ensure t)

(use-package evil
  :init (evil-mode t)
  :diminish
  :config
  (define-key evil-motion-state-map "H" 'beginning-of-line-text)
  (define-key evil-motion-state-map "L" 'evil-end-of-line))

(use-package evil-leader
  :init (global-evil-leader-mode)
  :after evil
  :diminish
  :config
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
    "t"     'vterm
    "d"     'treemacs
    "o c"   'org-capture
    "o f"   'helm-find-my-notes
    "f s"   'toggle-frame-fullscreen
    "s v"   'split-window-horizontally
    "s h"   'split-window-vertically))

(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "hh" 'evil-normal-state))

(use-package evil-surround
  :ensure t
  :config
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package magit
  :ensure t)

(use-package evil-magit
  :after evil
  :ensure t)

(use-package paredit
  :ensure t
  :diminish
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package markdown-mode
  :ensure t
  :config
  (set-face-attribute 'markdown-code-face nil :background nil)
  (add-hook 'markdown-mode 'auto-fill-mode))

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t))

(use-package projectile
  :ensure t
  :diminish
  :init
  (setq projectile-completion-system 'helm)
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t)
  :config
  (progn
    (helm-projectile-on)))

;; Org mode
(use-package org-download
  :ensure t
  :config
  (setq-default org-download-image-dir (concat org-notes-home-dir "pictures")))

(use-package org-bullets
  :config
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook (lambda()
                             (org-bullets-mode 1))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default)))
 '(package-selected-packages
   (quote
    (flymake-shellcheck diminish zenburn-theme which-key web-mode vterm use-package treemacs-evil solarized-theme slime request paredit org-download org-bullets neotree lsp-ui lsp-treemacs key-chord helm-projectile helm-org helm-lsp helm-ag go-mode exec-path-from-shell evil-surround evil-org evil-magit evil-lispy evil-leader company-lsp clojure-mode ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
