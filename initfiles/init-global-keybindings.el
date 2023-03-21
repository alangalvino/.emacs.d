;;;; Global keybindings

(global-set-key (kbd "M-q") 'set-justification-full)
(global-set-key (kbd "C-c _") 'split-window-vertically)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c |") 'split-window-horizontally)
(global-set-key (kbd "C-c F") 'toggle-frame-fullscreen)

(provide 'init-global-keybindings)
