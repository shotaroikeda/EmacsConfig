;; C/C++ tweaks

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)

(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Compiles and runs the C script that was created.
(defun compile-and-run-prim-c ()
  (interactive)
  (setq this-file-name (buffer-name)
		make-command (eval (concat "make -k " (file-name-sans-extension buffer-file-name)))
		run-command (eval (file-name-sans-extension buffer-file-name)))
  (if (get-buffer "*C-Output*") nil
	(get-buffer-create "*C-Output*"))
  (switch-to-buffer-other-window "*C-Output*")
  (erase-buffer)
  ;; Pretty print the output
  (insert "############################### ")
  (insert this-file-name)
  (insert " ###############################\n")
  (insert "Running " make-command "...\n")
  (insert "##############################################################\n\n")
  (insert (shell-command-to-string make-command))
  (insert "\n\n##############################################################\n\n")
  (insert "Running " run-command)
  (insert "\n\nOutput:\n")
  (insert "##############################################################\n\n")
  (insert (shell-command-to-string run-command))
  (insert "\n\n##############################################################\n")
  (insert "Finished running " run-command "\n\n"))

;; Have a minor mode to avoid conflict between Python, C, C++
(define-minor-mode c-interactive-minor-mode
  "A minor mode to allow really fast compiling, without conflicting with other modes"
  ;; initial toggle
  nil
  ;; keymap
  '(
	(,(kbd "A-r") . compile-and-run-prim-c))
  )
