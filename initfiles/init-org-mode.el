;;;; Org-mode configs

;;; Org modules and packages

(require 'org-tempo)

(use-package org-bullets
  :ensure t
  :config
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook (lambda()
                             (org-bullets-mode 1))))

;;; Org configs

(setq org-startup-folded t)

;; Fix superscripts: https://necromuralist.github.io/posts/disabling-subscripting-in-org-mode/
(setq org-use-sub-superscripts '{})

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (shell . t)))

(add-hook 'org-mode-hook (lambda ()
                           (org-toggle-pretty-entities)
                           (auto-fill-mode)
                           (org-indent-mode)))

(setq org-notes-home-dir user-orgmode-dir)

(provide 'init-org-mode)
