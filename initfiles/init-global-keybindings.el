;;;; Global keybindings

(global-set-key (kbd "M-q") 'set-justification-full)
(global-set-key (kbd "C-c _") 'split-window-vertically)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c |") 'split-window-horizontally)
(global-set-key (kbd "C-c f") 'helm-find-files)
(global-set-key (kbd "C-c o") 'helm-find-my-notes)
(global-set-key (kbd "C-c w") 'helm-find-my-workspace)
(global-set-key (kbd "C-c d") 'treemacs)
(global-set-key (kbd "C-c s") 'slime)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c F") 'toggle-frame-fullscreen)

(provide 'init-global-keybindings)
