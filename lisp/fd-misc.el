;; Miscelaneous settings
;;
;; Do not include anything that requires anything that isnt packaged
;; with emacs here.

(require 'fd-cookbook)
(require 's)
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

(global-set-key (kbd"C-c C-d") 'duplicate-line-or-region)
(global-set-key (kbd"C-c d") 'duplicate-line-or-region)

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

(defvar-local untabify-on-save nil
  "Automatically untabify the buffer before saving")
(defadvice save-buffer (around save-buffer-as-root-around activate)
  "Use sudo to save the current buffer."
  (interactive "p")
  (when untabify-on-save
    (save-excursion
      (mark-whole-buffer)
      (call-interactively 'untabify)))
  (if (and (buffer-file-name)
	   (file-accessible-directory-p
	    (file-directory-name (buffer-file-name)))
	   (not (file-writable-p (buffer-file-name)))
	   (not (string/starts-with buffer-file-name "/sudo")))
      (let ((buffer-file-name (format "/sudo::%s" buffer-file-name)))
	ad-do-it)
    ad-do-it))

(defun move-text-internal (arg)
  (let ((cc (current-column)))
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
	(forward-line (min (- arg 1) -1))
	(move-to-column cc))))))

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
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "C-M-n") 'move-text-down)

(global-set-key (kbd "C-c f d") 'find-dired)

(when (fboundp 'old-y-or-n-p)
  (defalias 'y-or-n-p 'old-y-or-n-p))

(defalias 'yes-or-no-p 'y-or-n-p
  "Faster yes or no's")

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this-window (selected-window))
	 (other-window (next-window))
	 (this-buffer (window-buffer this-window))
	 (other-buffer (window-buffer other-window)))
    (set-window-buffer other-window this-buffer)
    (set-window-buffer this-window other-buffer)
    (select-window other-window)))

(global-set-key (kbd "C-x TAB") 'swap-buffers-in-windows)

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


(defun shell-command-on-buffer (&optional cmd)
  "Asks for a command and executes it in inferior shell with current buffer
as input replacing the buffer with the output."
  (interactive (list (read-shell-command "Shell command on buffer: ")))
  (shell-command-on-region
   (point-min) (point-max) cmd nil t))

(defun kill-buffers-regex (rx)
  "kill buffers that match the rx."
  (interactive "sRegex for buffers: ")
  (dolist (b (buffer-list))
    (when (string-match rx (buffer-name b))
      (kill-buffer b))))

(defun kill-dired-buffers ()
  (interactive)
  (dolist (b (buffer-list))
    (when
	(with-current-buffer b (eq major-mode 'dired-mode))
      (kill-buffer b))))

(defun kill-file-buffers ()
  (interactive)
  (dolist (b (buffer-list))
    (when (buffer-file-name b)
      (kill-buffer b))))

(defun kill-fs-buffers ()
  "Kill all filesystem related buffers."
  (interactive)
  (kill-file-buffers)
  (kill-dired-buffers))

(defun bury-directory-buffers (dir)
  (interactive
   (list (ido-read-directory-name
          "Bury buffers under directory: ")))
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (equal default-directory dir)
        (bury-buffer)))))


(setq auto-save-timeout 0
      auto-save-interval 0)

(global-set-key (kbd "M-S-<up>")
		(lambda () (interactive) (enlarge-window 10)))
(global-set-key (kbd "M-S-<down>")
		(lambda () (interactive) (enlarge-window -10)))
(global-set-key (kbd "M-S-<left>")
		(lambda () (interactive) (enlarge-window -10 t)))
(global-set-key (kbd "M-S-<right>")
		(lambda () (interactive) (enlarge-window 10 t)))

(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)

(defun hide-carriage-return-characters ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(unless (boundp 'pcache-version-constant)
  (setq pcache-version-constant "fake-version"))

;;; BUFFER MANAGEMENT ;;;

(defmacro on-buffers (prompt func ignore-list reverse-buffer-list &rest attr)
  "Open by evaluating attr for each, ignoring IGNORE-LIST and
maybe reversing the result. Then applying func"
  `(let ((attribute-key-fn (lambda (b) (let ((res (with-current-buffer b ,@attr)))
                                         (if (symbolp res) (symbol-name res) res)))))
     (func-on-buffers ,func attribute-key-fn
                      (read-buffers-attribute ,prompt attribute-key-fn)
                      ,reverse-buffer-list)))

(defun read-buffers-attribute (prompt attribute-fn &optional ignore-list)
  "Read an attrubute of an open buffer generated by
  ATTRIBUTE-FN. IGNORE-LIST is a list of attributes to be ignored
  and PROMPT is the user prompt"
  (ido-completing-read prompt
                       (delete-if
                        (lambda (m) (member m ignore-list))
                        (delete-dups
                         (mapcar attribute-fn
                                 (buffer-list))))))

(defun func-on-buffers (func attribute-fn attr &optional reverse-buffer-list)
  "Buffers get attributes generated by ATTRIBUTE-FN which is a
function which takes buffer as a single argument. The ones that
match attr get FUNC which takes a buffer as a single argument run
on them.

REVERSE-BUFFER-LIST if non-nill reverses the order in which FUNC
is applied to the buffers."
  (dolist (b (delete-if-not
              (lambda (buf) (equal (funcall attribute-fn buf) attr))
              (if reverse-buffer-list
                  (reverse (buffer-list))
                (buffer-list))))
    (with-current-buffer b
      (funcall func b))))

(defun bury-buffers-expr (expr &optional prompt ignore-list)
  (interactive "xExpr to get attribute for buryal (current buffer is each buffer): ")
  (on-buffers (or prompt "Bury buffers with attribute: ")
              'kill-buffer ignore-list nil (eval expr)))

(defun unbury-buffers-expr (expr &optional prompt ignore-list)
  (interactive "xExpr to get attribute for unburial (current buffer is each buffer): ")
  (on-buffers (or prompt "Unbury buffers with attribute: ")
              'kill-buffer ignore-list t (eval expr)))

(defun kill-buffers-expr (expr &optional prompt ignore-list)
  (interactive "xExpr to get attribute (current buffer is each buffer): ")
  (on-buffers (or prompt "Kill buffers with attribute: ")
              'kill-buffer ignore-list nil (eval expr)))

(defmacro define-buffer-commands (postfix ignore-list &rest key)
  "Define {kill,unbury,bury}-buffers-<postfix> that run expr and
ignore buffers with."
  (dolist (prefix '("kill" "unbury" "bury"))
    `(set (intern (format "%s-buffers-%s" prefix postfix))
          (lambda ()
            (interactive)
            (call-interactively (intern (format "%s-buffers-expr" prefix))
                                key (format "%s buffers: " prefix) ignore-list)))))


;; PATH in emacs
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (car (reverse (split-string (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))))
    (setenv "PATH" path-from-shell)
    (setenv "EDITOR" "emacsclient")
    (setenv "EMACS" (s-trim (shell-command-to-string "echo $EMACS")))
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
(global-unset-key (kbd "M-`"))

(windmove-default-keybindings)
(setq windmove-wrap-around t)

(provide 'fd-misc)
