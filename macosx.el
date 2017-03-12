
(defvar macos-required '(exec-path-from-shell))
(package-initialize)

;; Check for installation requirement and install
(if fresh-install
    (progn (package-refresh-contents)
           (dolist (p macos-required)
             (when (not (package-installed-p p))
               (package-install p)))))

;; env variables
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")

(message "Binding for MAC OS X")

;;OS Specific binds
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'control)
(setq mac-control-modifier 'control)
(setq mac-command-modifier 'alt)

;; prevent insertion of phi and other osx special characters
(set-keyboard-coding-system nil)

(set-face-attribute 'default nil
                    :family "Inconsolata for Powerline" :height 130 :weight 'normal)

;;; Install epdfinfo via 'brew install pdf-tools' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.
(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
(pdf-tools-install)
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
;; Turn off evil mode for pdf tools
(add-hook 'pdf-view-mode-hook '(lambda ()
                                 (turn-off-evil-mode)
                                 (turn-off-evil-mc-mode)))
