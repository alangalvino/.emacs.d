;;;; Org-mode configs

(use-package org-bullets
  :ensure t
  :config
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook (lambda()
                             (org-bullets-mode 1))))

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

(provide 'init-org-mode)
