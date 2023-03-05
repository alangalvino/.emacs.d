;;;; Common Lisp specific configs as Quicklisp and Slime

;; Load Quicklisp
(if (file-exists-p "~/.quicklisp/slime-helper.el") 
    (load (expand-file-name "~/.quicklisp/slime-helper.el")))

;; Load Slime helpers
(if (file-exists-p "~/.roswell/helper.el")
    (progn
      (load (expand-file-name "~/.roswell/helper.el"))
      (setq inferior-lisp-program "/usr/local/bin/ros -L sbcl -Q -l ~/.sbclrc run")))

(use-package helm-slime
  :ensure t)

(provide 'init-common-lisp)
