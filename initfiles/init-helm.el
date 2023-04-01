;;;; Helm configs

(use-package helm
  :ensure t
  :delight
  :bind
  ("M-x" . 'helm-M-x)
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-o" . 'helm-occur)
  ("C-x C-b" . 'helm-buffers-list)
  ("C-b f" . 'helm-find-file-or-projectile)
  ("C-b o" . 'helm-find-my-notes)
  ("C-b w" . 'helm-find-my-workspace)
  (:map helm-command-map
        ("C-j" . 'helm-next-line)
        ("C-k" . 'helm-previous-line))
  :custom
  (helm-split-window-in-side-p t)
  (helm-recentf-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-M-x-fuzzy-match t)
  (helm-semantic-fuzzy-match t)
  (helm-apropos-fuzzy-match t)
  (helm-lisp-fuzzy-completion t))

(defun helm-find-file-or-projectile ()
  "If on projectile tries to find file with projectile if not with helm"
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'helm-projectile-find-file)
    (call-interactively 'helm-find-files)))

(defun helm-find-in-dir (dir)
  (let ((default-directory dir))
    (helm-find-files nil)))

(defun helm-find-my-notes ()
  (interactive)
  (helm-find-in-dir org-notes-home-dir))

(defun helm-find-my-workspace ()
  (interactive)
  (helm-find-in-dir user-workspace-dir))

;; Open files as sudo
;; This advises helm-find-files to reopen the selected file as root
;; Based on https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
;; (defadvice helm-find-files (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'init-helm)
