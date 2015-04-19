(require 'python)
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)








