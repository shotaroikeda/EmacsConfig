;;OS Specific binds
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'control)
(setq mac-control-modifier 'control)
(setq mac-command-modifier 'alt)

;;Getting native feel of os
(global-set-key (kbd "A-c") 'evil-yank)
(global-set-key (kbd "A-v") 'evil-paste-after)

;; Scrolling settings
;; scroll one line at a time (less "jumpy" than defaults)

;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;
;; ;; don't accelerate scrolling
;;
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;
;;(setq scroll-step 1)
;; keyboard scroll one line at a time

;; (setq scroll-margin 1)
;; scroll-step 1
;; scroll-conservatively 10000
;; scroll-preserve-screen-position 1
;; mouse-wheel-progressive-speed nil

;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;;(setq auto-window-vscroll nil)

;;(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")
