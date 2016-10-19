(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defvar macos-required '(exec-path-from-shell
                         ))

(package-initialize)

(if fresh-install
    (package-refresh-contents)
  nil)

(dolist (p macos-required)
  (when (not (package-installed-p p))
    (package-install p)))

;; TLDR; you need to install:
;; jedi
;; ipython
;; ghostscript

;; You will also have to add the installation directory to $PATH

;;OS Specific binds
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'control)
(setq mac-control-modifier 'control)
(setq mac-command-modifier 'alt)

;; env variables
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")

;; prevent insertion of phi and other osx special characters
(set-keyboard-coding-system nil)

;; Set system font for railwaycat version
;; (set-face-attribute 'default nil :height 110)

