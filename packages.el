(require 'package)

(when (not (equal system-type 'darwin))

  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

(package-initialize)

(defvar my-packages '(evil
                      evil-god-state
                      evil-leader
                      evil-mc

                      god-mode
                      diminish

                      bash-completion
                      company
                      company-quickhelp
                      rainbow-delimiters

                      projectile
                      magit
                      yasnippet

                      auctex

                      rudel

                      helm
                      better-defaults

                      paredit
                      smartparens

                      clojure-mode
                      cider

                      haskell-mode
                      ghc
                      hindent
                      company-ghc

                      lua-mode

                      irony
                      company-irony
                      flycheck-irony
                      c-eldoc

                      pyenv-mode
                      anaconda-mode
                      company-anaconda
                      company-jedi

                      web-mode
                      markdown-mode
                      markdown-mode+

                      js2-mode

                      popwin
                      rich-minority

                      ample-theme
                      monokai-theme
                      zenburn-theme
                      ample-zen-theme
                      material-theme
                      ))

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
;; pyenv-virtualenv
;; pyenv-virtualenvwrapper
;; ghostscript

;; You will also have to add the installation directory to $PATH
;; on OSX 10.10 it's in /etc/paths
;; To finish setting up, you need to install a couple of things
;; flake8/pyflake and jedi
;; install it from pip (pip install jedi)

;; to view pdfs you need a pdf renderer. ghostscript works fine
