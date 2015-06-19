(require 'cider)
(add-hook 'cider-mode-hook 'eldoc-mode)
;; Log Error messages from the REPL
(setq nrepl-log-messages t)
;; Hide special buffers that appear
(setq nrepl-hide-special-buffers t)
;; Show message even on test success for test cases
(setq cider-test-show-report-on-success t)

(defun clj-send-to-repl ()
  (interactive)
  (cider-eval-buffer)
  (other-window 1)
  (cider-switch-to-current-repl-buffer))

(require 'evil)
(evil-leader/set-key-for-mode 'clojure-mode "ce" 'cider-eval-last-sexp)

(define-key clojure-mode-map (kbd "C-c C-z") 'cider-jack-in)
(define-key clojure-mode-map (kbd "A-r") 'clj-send-to-repl)
