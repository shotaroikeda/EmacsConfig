;; C/C++ tweaks
(require 'cc-mode)
(require 'cl)

(setq-default c-basic-offset 8
              tab-width 8
              indent-tabs-mode t)

;; Make sure that brackets get inserted with proper indentation
(sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
 
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(setq c-default-style "linux"
      c-basic-offset 8)

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
  (setq buffer-read-only nil)
  ;;switch to fundamental mode to allow buffer text insertion
  (fundamental-mode)
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
  (insert "Finished running " run-command "\n\n")
  (insert "Press q to quit or A-O to switch buffers\n")
  ;; switch to help mode to make the buffer have help mode keybinds
  (help-mode))

;; Have a mode-map to avoid conflicts
(define-key c-mode-map (kbd "A-r") 'compile-and-run-prim-c)

;; Compiles and runs the C++ script that was created
(defun compile-and-run-cpp ()
  (interactive)
  (setq this-file-name (buffer-name)
		make-command (eval (concat "clang++ " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out"))
		run-command (eval (concat (file-name-sans-extension buffer-file-name) ".out")))
  (if (get-buffer "*C++-Output*") nil
	(get-buffer-create "*C++-Output*"))
  (switch-to-buffer-other-window "*C++-Output*")
  (setq buffer-read-only nil)
  ;; switch to fundamental mode to allow buffer text insertion
  (fundamental-mode)
  (erase-buffer)
  ;; Pretty print the output
  (insert "############################### ")
  (insert this-file-name)
  (insert " ###############################\n")
  (insert "Running " make-command "...\n")
  (insert "##############################################################\n\n")
  (setq error-msg (eval(shell-command-to-string make-command)))
  (if (string= error-msg "") 
	(insert "Clang did not return any errors.\nCompiling is successful!")
	(insert error-msg))
  (insert "\n\n##############################################################\n\n")
  (insert "Running " run-command)
  (insert "\n\nOutput:\n")
  (insert "##############################################################\n\n")
  (insert (shell-command-to-string run-command))
  (insert "\n\n##############################################################\n")
  (insert "Finished running " run-command "\n\n")
  (insert "Press q to close or A-O to switch buffers\n")
  ;; switch to help mode to make the buffer have help mode keybinds
  (help-mode))

;; Have a mode-map to avoid conflicts
(define-key c++-mode-map (kbd "A-r") 'compile-and-run-cpp)

