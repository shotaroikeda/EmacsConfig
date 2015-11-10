(load "~/.emacs.d/languages/sourcepawn-mode-0.1/sourcepawn-mode.el")
(add-to-list 'auto-mode-alist '(".sp\\'" . sourcepawn-mode))
(add-to-list 'auto-mode-alist '(".inc\\'" . sourcepawn-mode))

;; Make sure that brackets get inserted with proper indentation
(sp-local-pair 'sourcepawn-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
