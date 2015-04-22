(require 'python)
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
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

