;;;; Use-package configs

;; Add initfiles folder to load-path
(setq initfiles-folder (concat user-emacs-directory "initfiles"))
(setq load-path (cons initfiles-folder load-path))

;; Load package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Install and load use-package 
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(provide 'init-use-package)
