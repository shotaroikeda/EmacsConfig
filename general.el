;;;;
;; General Editing and Navigation Enhancements
;;;;

;; force spaces instead of tabs
(setq-default indent-tabs-mode nil)

(require 'evil)
(require 'ido)
(require 'bash-completion)
(bash-completion-setup)

(enable-paredit-mode)
(paredit-mode t)
(load-theme 'monokai t)
;; Improve startip buffer
(setq evil-move-cursor-back nil)
(setq inhibit-startup-message t)
;; (setq initial-buffer-choice "~/Development")
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

;; SmartParens Mode
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

;; Globally enable whitespace-mode
;;(global-whitespace-mode)

;; Highlight Current Line configuration
(global-hl-line-mode 1)
(set-face-background 'hl-line "#552661")

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

;; clojure
(push "*cider-error*" popwin:special-display-config)
