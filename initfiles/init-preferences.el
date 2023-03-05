;;;; Personal configs

(setq-default ispell-dictionary "american")
(setq-default indent-tabs-mode nil)
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
