(defun clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
	(comint-truncate-buffer)))

(defun configure ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; (defun fullscreen ()
;;        (interactive)
;;        (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;; 	    		 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(defun fullscreen (&optional f)
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
						 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
						 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)
						 ))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
	(while (/= arg 0)
	  (let ((this-win (window-buffer))
			(next-win (window-buffer (funcall selector))))
		(set-window-buffer (selected-window) next-win)
		(set-window-buffer (funcall selector) this-win)
		(select-window (funcall selector)))
	  (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun indent-buffer ()
  (interactive)
  (save-excursion
	(indent-region (point-min) (point-max) nil)))

(defun load-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

;; Screen Manipulation
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))
;; Magit Functions
(require 'magit)
(defun stage-and-commit ()
  "Requires Magit. Stages all current files in the project then commits them"
  (interactive)
  (magit-stage-all)
  (magit-commit))
