(load-theme 'cobalt t)

(setq curr-bg-color (face-attribute 'default :background))

(custom-set-variables)
(custom-set-faces '(linum ((t nil))))

;; disable fringes by blending background color
(set-face-attribute 'fringe nil :background curr-bg-color :foreground curr-bg-color)

;; Margin windows
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d")))
    ad-do-it))

(setq linum-format 'dynamic)

;; Powerline custom configuration
(require 'powerline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode)
(setq-default powerline-default-separator 'wave)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

;; Set mode line colors
(set-face-attribute 'powerline-active1 nil
		    :background (face-attribute 'font-lock-keyword-face :foreground))
(set-face-attribute 'powerline-active2 nil
		    :background "#2C3E50")
(set-face-attribute 'mode-line nil
		    :background (face-attribute 'web-mode-json-context-face :foreground)
		    :box nil)
(set-face-attribute 'mode-line-inactive nil
		    :foreground (face-attribute 'default :foreground)
		    :background (face-attribute 'font-lock-regexp-grouping-construct :foreground)
		    :box nil)

;; disable the outline for mode line

;; Highlight Current Line configuration
(global-hl-line-mode 1)

;; Hide ugly mode line text
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "anaconda" '(diminish 'anaconda-mode))
(eval-after-load "evil-mc" '(diminish 'evil-mc-mode))
(eval-after-load "auto-revert" '(diminish 'auto-revert-mode))
(eval-after-load "helm" '(diminish 'helm-mode))
;; (diminish 'major-mode)
(spaceline-compile)
