;; Haskell Langauge Configuration
;; Pulled from https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md#ghc-mod

;; ghs, cabal, happy, ghs-mod, hindent, hasktags, stylish-haskell are REQUIRED to use haskell mode

;; Enable haskell auto indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook #'hindent-mode)

;; Add a import navigation section
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

;; Look at the home directory for cabal packages
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables '(haskell-tags-on-save t))
;; Enable Stylish buffer formatting
(evil-leader/set-key-for-mode 'haskell-mode "i" 'haskell-mode-stylish-buffer)

;; Set Keybinds
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
                                  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;; Set to Cabal Repl instead of ghci
(custom-set-variables '(haskell-process-type 'cabal-repl))

;; Autocomplete configuration
(require 'company)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

;; Custom keybinds
(defun my/haskell-load ()
  (interactive)
  (save-buffer)
  (haskell-process-load-or-reload)
  (haskell-interactive-switch))

(evil-leader/set-key-for-mode 'haskell-mode "cr" 'my/haskell-load)
(evil-leader/set-key-for-mode 'haskell-mode "cz" 'haskell-interactive-switch)
(define-key haskell-mode-map (kbd "A-r") 'my/haskell-load)
