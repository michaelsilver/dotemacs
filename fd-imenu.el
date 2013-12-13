(define-derived-mode imenu-selection-mode fundamental-mode "imenu"
  "Major mode for imenu selection."
  (suppress-keymap imenu-selection-mode-map)
  (define-key imenu-selection-mode-map "j" 'next-line)
  (define-key imenu-selection-mode-map "k" 'previous-line)
  (define-key imenu-selection-mode-map "l" 'imenu-selection-select)
  (define-key imenu-selection-mode-map "\C-m" 'imenu-selection-select)
  (define-key imenu-selection-mode-map "h" 'kill-this-buffer)
  )


(defun imenu--position-entry (point buffer)
  "Get a string that tells compilation mode where the point in
  this buffer is."
  (with-current-buffer buffer
    (format "%s:%d: " (buffer-file-name) (line-number-at-pos point))))

(defvar imenu--selection-buffer " *imenu-select*")
(defvar imenu--target-buffer nil)
(defun imenu-make-selection-buffer (&optional index-alist)
  (interactive)
  (require 'which-func)
  (setq index-alist (if index-alist index-alist (imenu--make-index-alist)))
  (let ((cur (which-function)))
    (when (listp cur)
      (setq cur (car cur)))
    (setq imenu--target-buffer (current-buffer))
    (with-temp-buffer-window imenu--selection-buffer nil nil
			     (buffer-disable-undo)
			     (insert (format "Functions in '%s':\n" cur))
			     (dolist (x index-alist)
			       (when (and (markerp (cdr x)) (< 0 (cdr x)))
				(let ((p1 (point))
				      (p2 (progn (insert (imenu--position-entry (cdr x) imenu--target-buffer)) (point))))
				  (overlay-put (make-overlay p1 p2) 'invisible t)
				  (insert "\t" (car x) "\n"))))
			     (compilation-minor-mode t))))


(defun imenu-selection-select ()
  (interactive)
  (let ((sel (substring (thing-at-point 'line) 0 -1)))
    (bury-buffer)
    (switch-to-buffer imenu--target-buffer)
    (imenu sel)))

(global-set-key (kbd "C-x i") 'imenu-make-selection-buffer)

(provide 'fd-imenu)
