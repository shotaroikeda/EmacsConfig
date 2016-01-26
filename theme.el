(load-theme 'monokai t)

(setq curr-bg-color (face-attribute 'default :background))

(custom-set-variables)
(custom-set-faces '(linum ((t nil))))

(set-face-attribute 'fringe nil :background curr-bg-color :foreground curr-bg-color)

;; Margin windows
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d")))
    ad-do-it))

(setq linum-format 'dynamic)

;; TODO: Find a way to get srgb colors without making powerline look terrible
(setq ns-use-srgb-colorspace nil)

;; Powerline custom configuration
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(setq powerline-arrow-shape 'arrow)

(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#FD971F" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;; (custom-set-faces
;;  '(mode-line ((t (:foreground "#030303" :background (face-attribute 'font-lock-warning-face :foreground) :box nil))))
;;  '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
;; 
(set-face-attribute 'mode-line nil
                    :background (face-attribute 'font-lock-warning-face :foreground))

;; Highlight Current Line configuration
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "#552661")

;; (setq ns-use-srgb-colorspace t)

;; Hide ugly mode line text
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "anaconda" '(diminish 'anaconda-mode))
(eval-after-load "evil-mc" '(diminish 'evil-mc-mode))
(eval-after-load "auto-revert" '(diminish 'auto-revert-mode))
;; (diminish 'major-mode)
