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
;; flake8
;; ghostscript

;; You will also have to add the installation directory to $PATH
;; on OSX 10.10 it's in /etc/path
;; To finish setting up, you need to install a couple of things
;; flake8/pyflake and jedi
;; install it from pip (pip install jedi)

;; to view pdfs you need a pdf renderer. ghostscript works fine

;;OS Specific binds
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'control)
(setq mac-control-modifier 'control)
(setq mac-command-modifier 'alt)

;; env variables
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")
