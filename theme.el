(load-theme 'zenburn t)

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
;; (set-face-attribute 'vertical-border-face curr-bg-color)
;; Reverse colors for the border to have nicer line  
;; (set-face-inverse-video-p 'vertical-border nil)
;; (set-face-background 'vertical-border (face-background 'default))

;; ;; Set symbol for the border
;; (set-display-table-slot standard-display-table
;;                         'vertical-border 
;;                         (make-glyph-code ?┃))

;; (set-face-attribute 'vertical-border curr-bg-color)

;; Margin windows
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d")))
    ad-do-it))

(setq-default left-margin-width 0)         
(setq-default right-margin-width 0)
(setq linum-format 'dynamic)

;; (require 'cl)
;; (powerline-default-theme)
;; (set-face-attribute 'mode-line nil
;;                     :foreground "Black"
;;                     :background "DarkOrange"
;;                     :box nil)

