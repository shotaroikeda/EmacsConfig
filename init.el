;; Set fesh install to 1 for a fresh install
(setq fresh-install nil)

;; Debugging mode
(setq debug-on-error nil)

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/general.el")
(load "~/.emacs.d/custom-funcs.el")
(load-directory "~/.emacs.d/languages")
(load "~/.emacs.d/keybinds.el")

(when (equal system-type 'darwin)
  (load "~/.emacs.d/macosx.el"))

