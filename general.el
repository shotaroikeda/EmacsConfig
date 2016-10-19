;;;;
;; General Editing and Navigation Enhancements
;;;;

;; force spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Force y/n questions instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(require 'evil)
;; (require 'ido)
(require 'bash-completion)
(bash-completion-setup)

(enable-paredit-mode)
(paredit-mode t)
;; Improve startip buffer
(setq evil-move-cursor-back nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Change autosave location
(defvar my-auto-save-folder "~/.emacs.d/auto-save/")
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save/.saves-"); set prefix for auto-saves
(setq auto-save-file-name-transforms `((".*", my-auto-save-folder t))); location for all auto-save files
(setq tramp-auto-save-directory my-auto-save-folder);

;; Vim like navigation/editing
(evil-mode 1)
(global-evil-leader-mode)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; Multiple cursors!
(global-evil-mc-mode 1)

;; Some autocompletion
;; (ido-mode t)
;; (setq ido-everywhere t)
;; (ido-ubiquitous-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'doc-view-mode (lambda () (setq linum-mode nil)))

;; Quickhelp
(require 'company-quickhelp)
(company-quickhelp-mode 1)

;; Remove warning bell
(setq ring-bell-function 'ignore)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
                                        ; Make horizontal movement cross lines
(setq-default evil-cross-lines t)

(add-to-list 'auto-mode-alist '("\\.s\\'" . mips-mode))

;; Makes *scratch* empty.
(setq initial-scratch-message "")

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; SmartParens Mode
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

;; Globally enable whitespace-mode
;;(global-whitespace-mode)

;; Prevent Magit Warning From Appearing
(setq magit-last-seen-setup-instructions "1.4.0")

;; Scroll wheel settings
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; popwin configuration
(require 'popwin)
(popwin-mode 1)


;; Dired
(push '(dired-mode :position top) popwin:special-display-config)

;; Projectile mode
(projectile-global-mode 1)
(setq projectile-enable-caching t)

;; Helm
(require 'helm)
(helm-mode 1)

;; Helm functions to make it seem more like ido
(defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))
(advice-add 'helm-execute-persistent-action :around #'fu/helm-find-files-navigate-forward)

(define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)

(defun fu/helm-find-files-navigate-back (orig-fun &rest args)
  (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))

(advice-add 'helm-ff-delete-char-backward :around #'fu/helm-find-files-navigate-back)


(setq helm-display-function #'pop-to-buffer)

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

(require 'helm-projectile)
(helm-projectile-on)

(require 'use-package)
;;; Install epdfinfo via 'brew install pdf-tools' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.
(use-package pdf-tools
                 :ensure t
                 :config
                 (custom-set-variables
                  '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
                 (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
(pdf-tools-install)
