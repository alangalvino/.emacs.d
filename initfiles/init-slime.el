;;;; Common Lisp specific configs as Quicklisp and Slime

;; (require 'slime)

;; Load Quicklisp
(if (file-exists-p "~/.roswell/lisp/quicklisp/slime-helper.el")
    (progn
      (load (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el"))))

;; Load Slime helpers
(if (file-exists-p "~/.roswell/helper.el")
    (progn
      (load (expand-file-name "~/.roswell/helper.el"))
      (setq inferior-lisp-program "/usr/bin/ros -L sbcl -Q -l ~/.sbclrc run")))

(use-package helm-slime
  :ensure t)

(use-package slime-company
  :ensure t)

(global-set-key (kbd "C-c s") 'slime)

(provide 'init-slime)
