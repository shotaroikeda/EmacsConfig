;; Set bigger memeory allocation before garbage collection
(setq gc-cons-threshold 100000000)

;; Set fresh install to 1 for a fresh install
(setq fresh-install nil)

;; Debugging mode
(setq debug-on-error nil)


;; Must load PATH variables in OSX before initializing packages
(let ((file-name-handler-alist nil))
  (when (equal system-type 'darwin)
    (load "~/.emacs.d/macosx.el"))
  (load "~/.emacs.d/packages.el")
  (load "~/.emacs.d/vendor/cobalt-theme.el")
  (load "~/.emacs.d/theme.el")
  (load "~/.emacs.d/general.el")
  (load "~/.emacs.d/custom-funcs.el")
  (load "~/.emacs.d/keybinds.el")
  (load-directory "~/.emacs.d/languages")
  ;; Loads other keybinds for mac os dependent of packages
  (when (equal system-type 'darwin)
    (load "~/.emacs.d/macosx_binds.el")))
