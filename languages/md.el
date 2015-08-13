;; Custom configuration files for markdown mode
(require 'markdown-mode)
(require 'markdown-mode+)

;; Documentaion available here:
;; http://jblevins.org/projects/markdown-mode/

;; Recommended defaults:
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Recommended mode for git markdown files
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("CHANGELOG\\.md\\'" . gfm-mode))
