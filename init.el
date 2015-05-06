;; Set fesh install to 1 for a fresh install
(setq fresh-install nil)

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/general.el")
(load "~/.emacs.d/custom-funcs.el")
(load "~/.emacs.d/keybinds.el")
(load-directory "~/.emacs.d/languages")

(when (equal system-type 'darwin)
  (load "~/.emacs.d/macosx.el")
  (require exec-path-from-shell)
  (exec-path-from-shell-initialize))

