(require 'js2-mode)
;; Use js2 for all js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))

;; Use node to interpert
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(setq-default js2-basic-offset 4)

;; Make sure that brackets get inserted with proper indentation
;; my-create-newline-and-enter-sexp definited in "c.el"
(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
