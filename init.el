;; Set bigger memeory allocation before garbage collection
;; (setq gc-cons-threshold 100000000)

;; Set fresh install to 1 for a fresh install

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq fresh-install t)

;; Debugging mode
(setq debug-on-error nil)

;; Move the custom variable (auto generated) to a different file
(setq custom-file "~/.emacs.d/custom.el")

;; Generate custom file
(if (not (file-exists-p "~/.emacs.d/custom.el"))
    (shell-command "touch ~/.emacs.d/custom.el"))

(require 'org)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

;; Must load PATH variables in OSX before initializing packages
(let ((file-name-handler-alist nil))
  ;; Load custom file after generation
  (load custom-file))
