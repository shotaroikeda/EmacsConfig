(require 'python)
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)

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

(defun test-for-lols ()
  (interactive)
  (evil-insert-line "k"))


(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(defun py-load-switch ()
  (interactive)
  (python-shell-send-buffer)
  (elpy-shell-switch-to-shell)
  )

(define-key python-mode-map (kbd "A-r") 'py-load-switch)

(defun py-open-doc-and-go ()
  (interactive)
  (elpy-doc)
  (other-window 1))

(defun py-open-doc-fast ()
  (interactive)
  (elpy-doc))

(define-key python-mode-map (kbd "A-d") 'py-open-doc-fast)
(define-key python-mode-map (kbd "A-D") 'py-open-doc-and-go)
;;(define-key python-mode-map (kbd "A-D") '(kill-buffer "*Python Doc*"))

;; Some smarparens things
(sp-local-pair 'python-mode "\\\'" "\\\'")
