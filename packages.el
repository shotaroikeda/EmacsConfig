(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defvar my-packages '(evil
                      evil-leader

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
                      clojure-mode
                      cider

					  irony
					  company-irony
					  flycheck-irony
					  
					  elpy
					  pyenv-mode
					  anaconda-mode
					  company-anaconda

					  ample-theme
                      monokai-theme))

(package-initialize)

(if fresh-install
	(package-refresh-contents)
  nil)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
