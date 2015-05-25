;; Set fesh install to 1 for a fresh install
(setq fresh-install nil)

;; Debugging mode
(setq debug-on-error nil)

;; Must load PATH variables in OSX before initializing packages
(when (equal system-type 'darwin)
  (load "~/.emacs.d/macosx.el"))

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/general.el")
(load "~/.emacs.d/custom-funcs.el")
(load-directory "~/.emacs.d/languages")
(load "~/.emacs.d/keybinds.el")

;; Loads other keybinds for mac os dependent of packages
(when (equal system-type 'darwin)
  (load "~/.emacs.d/macosx_binds.el"))
