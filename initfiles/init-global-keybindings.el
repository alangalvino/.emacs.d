;;;; Global keybindings

(global-set-key (kbd "C-x h") 'help-command)
(global-set-key (kbd "M-q")   'set-justification-full)
(global-set-key (kbd "C-b _") 'split-window-vertically)
(global-set-key (kbd "C-b -") 'split-window-vertically)
(global-set-key (kbd "C-b |") 'split-window-horizontally)
(global-set-key (kbd "C-b F") 'toggle-frame-fullscreen)

(provide 'init-global-keybindings)
