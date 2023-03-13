;;;; Helm configs

(use-package helm
  :ensure t
  :delight
  :config
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

;; Open files as sudo
;; This advises helm-find-files to reopen the selected file as root
;; Based on https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice helm-find-files (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'init-helm)
