;;;;
;; General Editing and Navigation Enhancements
;;;;

										;(cd "/home/akshay")

(require 'evil)
(require 'ido)

(enable-paredit-mode)
(paredit-mode t)
(load-theme 'ample-flat t)
;; Improve startip buffer
(setq evil-move-cursor-back nil)
(setq inhibit-startup-message t) 
(setq initial-buffer-choice "~/Development")
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

;; Some autocompletion
(ido-mode t)
(setq ido-everywhere t)
(ido-ubiquitous-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'doc-view-mode (lambda () (setq linum-mode nil)))

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
