(setq ns-use-native-fullscreen t)
(load-theme 'monokai t)

(setq curr-bg-color (face-attribute 'default :background))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t nil))))

(set-face-attribute 'fringe nil :background curr-bg-color :foreground curr-bg-color)

;; Margin windows
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d")))
    ad-do-it))

(setq-default left-margin-width 0)         
(setq-default right-margin-width 0)
(setq linum-format 'dynamic)

;; TODO: Find a way to get srgb colors without making powerline look terrible
(setq ns-use-srgb-colorspace nil)

;; Powerline custom configuration
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(setq powerline-arrow-shape 'space14)

;; (custom-set-faces
;;  '(mode-line ((t (:foreground "#030303" :background "#FD971F" :box nil))))
;;  '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
;; (set-face-attribute 'mode-line nil
;;                     :background "#bdbdbd")

(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#FD971F" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
(set-face-attribute 'mode-line nil
                    :background "#FD971F")

;; (set-face-attribute 'default nil :font "Meslo for Powerline")
;; (set-frame-font "Meslo for Powerline" nil t)
