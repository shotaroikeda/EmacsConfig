(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defvar macos-required '(exec-path-from-shell))

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

;;; Install epdfinfo via 'brew install pdf-tools' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.
(if fresh-install
    (use-package pdf-tools
                 :ensure t
                 :config
                 (custom-set-variables
                  '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
                 (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
  (pdf-tools-install))
