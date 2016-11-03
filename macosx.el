
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

;;OS Specific binds
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'control)
(setq mac-control-modifier 'control)
(setq mac-command-modifier 'alt)

;; prevent insertion of phi and other osx special characters
(set-keyboard-coding-system nil)

(set-face-attribute 'default nil
                    :family "Inconsolata for Powerline" :height 130 :weight 'normal)
