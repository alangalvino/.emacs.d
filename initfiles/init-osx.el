;;;; macOS configs

;; Open just one frame
(setq ns-pop-up-frames nil)

;; Enables non native fullscreen
(setq ns-use-native-fullscreen nil)

;; Macs OS Command as Meta Key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'option)

;; Don't pass command to Mac OS, ex.: Cmd + h would not hide emacs
(setq mac-pass-command-to-system nil)

(provide 'init-osx)
