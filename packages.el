(require 'package)

(when (not (equal system-type 'darwin))

      (add-to-list 'package-archives
		   '("melpa" . "http://melpa.milkbox.net/packages/") t))

(package-initialize)

(defvar my-packages '(evil
		      evil-god-state
		      evil-leader

		      god-mode

		      bash-completion
		      better-defaults
		      company
		      rainbow-delimiters

		      find-file-in-project
		      projectile
		      magit
		      yasnippet

		      auctex

		      smex
		      ido-ubiquitous

		      paredit
		      smartparens

		      clojure-mode
		      cider

		      irony
		      company-irony
		      flycheck-irony

		      elpy
		      pyenv-mode
		      anaconda-mode
		      company-anaconda

		      web-mode

		      ample-theme
		      monokai-theme))

(package-initialize)

(when (not (equal system-type 'darwin)) 
      (if fresh-install
	  (package-refresh-contents)
	nil))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
;; TLDR; you need to install:
;; jedi
;; flake8
;; ghostscript

;; You will also have to add the installation directory to $PATH
;; on OSX 10.10 it's in /etc/paths
;; To finish setting up, you need to install a couple of things
;; flake8/pyflake and jedi
;; install it from pip (pip install jedi)

;; to view pdfs you need a pdf renderer. ghostscript works fine

