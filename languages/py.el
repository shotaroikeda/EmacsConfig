(require 'python)

(defun my/run-python ()
  (interactive)
  (run-python "ipython"))

(defun my/python-shell ()
  (interactive)
  (my/run-python)
  (python-shell-switch-to-shell))

(defun py-load-switch ()
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

(define-key python-mode-map (kbd "C-c C-z") 'my/python-shell)
;; Python indentation settings
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq python-indent 4)))

(fset 'python-force-indent-all
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([103 103 22 71 73 tab escape] 0 "%d")) arg)))

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(define-key python-mode-map (kbd "A-r") 'py-load-switch)

;; Some smarparens things
(sp-local-pair 'python-mode "\\\'" "\\\'")

(require 'popwin)
(push '("*Anaconda*" :height 20) popwin:special-display-config)
