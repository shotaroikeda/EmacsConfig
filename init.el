;; Set bigger memeory allocation before garbage collection
;; (setq gc-cons-threshold 100000000)

;; Set fresh install to 1 for a fresh install

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq fresh-install nil)

;; Debugging mode
(setq debug-on-error nil)

;; Move the custom variable (auto generated) to a different file
(setq custom-file "~/.emacs.d/custom.el")

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
  ;; Load custom file after generation
  (load custom-file)
  ;; Loads other keybinds for mac os dependent of packages
  (when (equal system-type 'darwin)
    (load "~/.emacs.d/macosx_binds.el")))
