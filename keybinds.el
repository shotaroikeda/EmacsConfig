;;;;
;; Global Navigation Commands
;;;;

(global-set-key (kbd "<f11>") 'fullscreen)

(global-set-key (kbd "<A-return>") 'newline)
(global-set-key (kbd "A-a") 'smex)
(global-set-key (kbd "A-s") 'save-buffer)
(global-set-key (kbd "A-n") 'previous-buffer)
(global-set-key (kbd "A-m") 'next-buffer)
(global-set-key (kbd "A-b") 'ido-switch-buffer)
(global-set-key (kbd "A-o") 'find-file)
(global-set-key (kbd "A-K") 'kill-this-buffer)
(global-set-key (kbd "A-O") 'other-window)
(global-set-key (kbd "A-P") 'previous-multiframe-window)

(global-set-key (kbd "A-9") 'scroll-down)
(global-set-key (kbd "A-0") 'scroll-up)
(global-set-key (kbd "M-c") 'configure)
(global-set-key (kbd "C-k") 'paredit-kill)

(global-set-key (kbd "A-RET") 'newline-and-indent)
(require 'paredit)

(define-key paredit-mode-map (kbd "M-s") nil)
(global-set-key (kbd "M-s") 'shell)
(global-set-key (kbd "M-u") 'universal-argument)
(define-key evil-normal-state-map (kbd "A-<down>") 'evil-window-down)
(define-key evil-normal-state-map (kbd "A-<up>") 'evil-window-up)
(define-key evil-normal-state-map (kbd "A-<left>") 'evil-window-left)
(define-key evil-normal-state-map (kbd "A-<right>") 'evil-window-right)
(define-key evil-normal-state-map (kbd "A-<right>") 'evil-window-right)

(require 'comint)

(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

(evil-leader/set-leader ";")

(evil-leader/set-key "sc" 'clear-shell)
(evil-leader/set-key "i" 'indent-buffer)
(evil-leader/set-key-for-mode 'latex-mode "ck" 'revert-all-buffers)


;; Setup company vim-like bindings
(require 'company)
(global-set-key (kbd "M-j") 'nil)
(global-set-key (kbd "M-j") 'company-manual-begin)
(define-key company-active-map (kbd "M-j") 'company-select-next)
(define-key company-active-map (kbd "M-k") 'company-select-previous)
(define-key company-search-map (kbd "M-j") 'company-search-repeat-forward)
(define-key company-search-map (kbd "M-k") 'company-search-repeat-backward)
(define-key company-active-map (kbd "/") 'company-search-candidates)
										; Use escape to quit stuff
(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key company-search-map [escape] 'company-search-abort)
(define-key company-active-map [escape] 'company-abort)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)



(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-motion-state-map (kbd " ") nil)  

(require 'doc-view)

(setf doc-view-continuous t)
(setf doc-view-resolution 144)

(define-key doc-view-mode-map (kbd "/") 'doc-view-search-next-match)
(define-key doc-view-mode-map (kbd "?") 'doc-view-search-previous-match)
(define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)
(define-key doc-view-mode-map (kbd "h") 'image-backward-hscroll)
(define-key doc-view-mode-map (kbd "l") 'image-forward-hscroll)
