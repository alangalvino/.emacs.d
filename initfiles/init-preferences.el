;;;; Personal configs

;; allow 256MB of memory (instead of 0.76MB) before calling garbage collection
(setq gc-cons-threshold (* 256 1024 1024))

(setq-default ispell-dictionary "american")

;; indentation cannot insert tabs
(setq-default indent-tabs-mode nil)

;; use 2 spaces instead of a tab
(setq-default tab-width 2)

(setq-default fill-column 100)
(setq epa-pinentry-mode 'loopback)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq sentence-end-double-space nil)
(setq whitespace-line-column 120)

;; Setup custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Autoreload file from disk
(global-auto-revert-mode t)

(provide 'init-preferences)
