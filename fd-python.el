;; Python

;; Fix docstring paragraph filling
(add-hook 'python-mode-hook (lambda ()
			      (setq paragraph-start (concat paragraph-start "\\|\\s-*\"\"\".*$")
				    python-fill-docstring-style 'django)))

(require 'python)
;; (defun py-my-indent-region (&optional min max)
;;   "Stupidly clamp indentation to the closest multiple of 4 spaces."
;;   (interactive)
;;   (save-excursion
;;     (let ((top (or min (point-min)))
;; 	  (bottom (or max (point-max)))
;; 	  (line-move-visual nil))
;;       (goto-char top)
;;       (while (<= (point) bottom)
;; 	(indent-line-to
;; 	 (* 4 (round (/ (float (current-indentation)) 4))))
;; 	(next-line) (end-of-line)))))


(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(defun fd-venv-path (path)
  (interactive (list (ido-read-directory-name "Virtualenv root: ")))
  (add-dir-local-variable 'python-mode 'python-shell-virtualenv-path path))

(defun fd-python-project-root ()
  (locate-dominating-file (buffer-file-name) "setup.py"))

(defun fd-python-jump-to-test ()
  "Assume that we are in the same filename only with the test_
prefix."
  (find-file (format "%stests/test_%s.py"
		     (file-name-as-directory
		      (fd-python-project-root))
		     (file-name-base))))

(defun fd-python-jump-to-implementation ()
  (find-file (format "%s/%s/%s.py"
		     (fd-python-project-root)
		     (downcase
		      (file-name-nondirectory
		       (directory-file-name
			(fd-python-project-root))))
		     (replace-regexp-in-string "^test_" "" (file-name-base)))))

(defun fd-python-in-test-p ()
  "If the file is called 'test_<name>.py' we are in."
  (string-match-p "/test_[^/]*\\.py$" (buffer-file-name)))

(defun fd-python-jump-between-test-and-implementation ()
  (interactive)
  (if (fd-python-in-test-p)
      (fd-python-jump-to-implementation)
    (fd-python-jump-to-test)))

(defun fd-python-command ()
  "Calculate the string used to execute the inferior Python process."
  (let ((process-environment (python-shell-calculate-process-environment))
        (exec-path (python-shell-calculate-exec-path)))
    (executable-find python-shell-interpreter)))

(defun fd-python-run-tests ()
  (interactive)
  (let (compilation-directory
	compile-command
	(default-directory (fd-python-project-root)))
    (compile (format "%s setup.py test" (fd-python-command)) t)))


(defun fd-python-hook ()
  (define-key python-mode-map (kbd "C-c C-t")
    'fd-python-jump-between-test-and-implementation)
  (define-key python-mode-map (kbd "C-c M-t") 'fd-python-run-tests))

(add-hook 'python-mode-hook 'fd-python-hook)

(provide 'fd-python)
