;; Miscelaneous settings
;;
;; Do not include anything that requires anything that isnt packaged
;; with emacs here.

(require 'fd-cookbook)

(let ((personal-el  "~/.emacs.d/personal.el"))
  (if (file-readable-p personal-el)
      (load-file personal-el)
    (load-file (my-expand-path "dummy-personal.el"))))

;; Server configuration
(require 'server)
(if (server-running-p)
    (message "Skipping server creation, one already exists")
  (server-start))

;; Configurations
(delete-selection-mode t)
(setq backup-directory-alist (list (cons "." (my-expand-path "backup/"))))
(set-input-method 'greek)
(toggle-input-method)
(setq scroll-step 1)
(global-set-key "\C-Z" 'revert-buffer)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-set-key (kbd"C-c d") 'duplicate-line-or-region)

(defun fd-new-buf-extension (dir &optional default path-len)
  "Return either the last two directories, or default"
  (if dir
      (let* ((path-len (or path-len 2))
	     (dirs (reverse (split-string dir "/" t)))
	     (top-dirs-raw (remove nil (subseq dirs 0 path-len)))
	     ;; If we do not have enough dirs append "" at the end so
	     ;; we get something that resembles an abs path.
	     (top-dirs (if (and (< (length top-dirs-raw) path-len)
				(not (equal "~" (car top-dirs-raw))))
			   (reverse (cons "" (reverse top-dirs-raw)))
			 top-dirs-raw)))
	(mapconcat (lambda (s) (format "%s/" s)) (reverse top-dirs) nil))
    default))

(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (when buffer-file-name
    (add-to-list 'mode-line-buffer-identification
		 '(:eval (fd-new-buf-extension default-directory "unlinked: ")))))
(add-hook 'find-file-hook 'add-mode-line-dirtrack)

;; If wou dont have the above path, i feel 'forward is best.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


(defun chmod+x-this ()
  "Add executable permissions to the current file."
  (interactive)
  (if buffer-file-name
      (let ((new-mode (logior #o111 (file-modes buffer-file-name))))
	(set-file-modes buffer-file-name new-mode))
    (message "No such file to make executable.")))

;; Rename files
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(cond ((get-buffer new-name)
	       (error "A buffer named '%s' already exists!" new-name))
	      (t
	       (rename-file filename new-name 1)
	       (rename-buffer new-name)
	       (set-visited-file-name new-name)
	       (set-buffer-modified-p nil)
	       (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(setq require-final-newline t)

(defun file-directory-name (fname)
  "Get the parent dir of the fname. If fname is a dir get the
parent."
  (replace-regexp-in-string "[^/]+$" ""
			    (directory-file-name fname)))

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(defadvice save-buffer (around save-buffer-as-root-around activate)
  "Use sudo to save the current buffer."
  (interactive "p")
  (if (and (buffer-file-name)
	   (file-accessible-directory-p
	    (file-directory-name (buffer-file-name)))
	   (not (file-writable-p (buffer-file-name)))
	   (not (string/starts-with buffer-file-name "/sudo")))
      (let ((buffer-file-name (format "/sudo::%s" buffer-file-name)))
	ad-do-it)
    ad-do-it))


(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
	(transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "C-M-p") 'move-text-up)
(global-set-key (kbd "C-M-n") 'move-text-down)

(global-set-key (kbd "C-c f d") 'find-dired)

(defalias 'yes-or-no-p 'y-or-n-p
  "Faster yes or no's")

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
	 (other (next-window))
	 (this-buffer (window-buffer this))
	 (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(global-unset-key (kbd "C-x TAB"))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "/")
	      'dired-isearch-filenames)))

(add-hook 'text-mode
	  (lambda ()
	    (define-key text-mode-map (kbd "M-n") 'forward-paragraph)))

(defun fd-read-var (var)
  (interactive "xVariable: ")
  var)

(defun fd-read-dir (dir)
  (interactive (list (ido-read-directory-name "Directory: ")))
  dir)

(defun fd-read-val (str)
  (interactive "sValue: ")
  str)

(defun fd-dir-local ()
  (interactive)
  (let* ((default-directory (call-interactively 'fd-read-dir))
	 (var (call-interactively 'fd-read-var))
	 (val (call-interactively 'fd-read-val)))
   (add-dir-local-variable major-mode var val)))

(provide 'fd-misc)
