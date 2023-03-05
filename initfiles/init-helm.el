;;;; Helm configs

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-o") 'helm-occur)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line))

(defun helm-find-in-dir (dir)
  (let ((default-directory dir))
    (helm-find-files nil)))

(defun helm-find-my-notes ()
  (interactive)
  (helm-find-in-dir org-notes-home-dir))

(defun helm-find-my-workspace ()
  (interactive)
  (helm-find-in-dir "~/workspace/"))

(provide 'init-helm)
