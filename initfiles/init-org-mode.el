;;;; Org-mode configs

(with-eval-after-load "org"
  ;;; Org configs

  (setq org-notes-home-dir user-orgmode-dir)
  (setq org-startup-folded t)

  ;; Fix superscripts: https://necromuralist.github.io/posts/disabling-subscripting-in-org-mode/
  (setq org-use-sub-superscripts '{})

  
  ;;; Org modules and packages
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (shell . t)))

  (require 'org-tempo))

(use-package org-bullets
  :ensure t
  :custom
  (org-hide-emphasis-markers t))

;; Org module hook

(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (org-toggle-pretty-entities)
                           (auto-fill-mode)
                           (org-indent-mode)))

(provide 'init-org-mode)
