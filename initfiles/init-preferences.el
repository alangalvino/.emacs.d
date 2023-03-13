;;;; Personal configs

;; Allow 256MB of memory (instead of 0.76MB) before calling garbage collection
(setq gc-cons-threshold (* 256 1024 1024))

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

;; Disable warnings
(setq warning-minimum-level :emergency)

(setq-default ispell-dictionary "american")

;; Indentation cannot insert tabs
(setq-default indent-tabs-mode nil)

;; Use 2 spaces instead of a tab
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

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Start server after loading init file
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(provide 'init-preferences)
