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
(setq python-shell-virtualenv-path "~/bin/py")

(defun fd-venv-path (path)
  (interactive (list (ido-read-directory-name "Virtualenv root: ")))
  (add-dir-local-variable 'python-mode 'python-shell-virtualenv-path path))

(defun fd-python-project-root (&optional path)
  (locate-dominating-file (or path (buffer-file-name)) "setup.py"))

(defun fd-python-project-tests-dir (&optional root-dir)
  (concat (file-name-as-directory (or root-dir (fd-python-project-root)))
	  "tests"))

(defun fd-python-jump-to-test ()
  "Assume that we are in the same filename only with the test_
prefix."
  (find-file (format "%stest_%s.py"
		     (file-name-as-directory
		      (fd-python-project-tests-dir))
		     (file-name-base))))

(defun fd-python-source-dir (&optional root-dir)
  (format "%s/%s"
	  (or root-dir (fd-python-project-root))
	  (downcase
	   (file-name-nondirectory
	    (directory-file-name
	     (or root-dir (fd-python-project-root)))))))

(defun fd-python-jump-to-implementation ()
  "I assume I am in a test."
  (find-file (format "%s/%s.py"
		     (fd-python-source-dir)
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


(defun fd-python-current-class-path ()
  (save-excursion
    (let ((cls (progn (re-search-backward "^class *\\(Test.*\\)(" nil t)
		      (match-string-no-properties 1)))
	  (module (replace-regexp-in-string
		   "/" "."
		   (file-relative-name
		    (concat (file-name-directory (buffer-file-name))
			    (file-name-base (buffer-file-name)))
		    (fd-python-project-root)))))
      (if cls
	  (concat module "." cls)
	""))))

(defun modtime (f)
  (nth 5 (file-attributes f)))

(defun fd-python-in-project-p (path &optional prj-path)
  "Non nil if PATH is in project defined by PRJ_PATH. If PRJ_PATH
is nil check if path in any project."
  (or
   (file-exists-p (concat (file-name-as-directory path) "setup.py"))
   (if prj-path
       (let ((prj-root (fd-python-project-root prj-path)))
	 (when prj-root
	   (s-starts-with-p (expand-file-name prj-root)
			    (expand-file-name path))))
     (fd-python-project-root path))))

(defun fd-python-open-project-internal (dir)
  (dolist  (f (sort
	       (append (directory-files
			(fd-python-project-tests-dir dir) t ".*\.py$")
		       (directory-files
			(fd-python-source-dir dir) t ".*\.py$"))
	       (lambda (f1 f2) (time-less-p (modtime f1) (modtime f2)))))
    (find-file f)))

(defvar fd-python-projects-directory "~/Projects/CSAIL/Python/"
  "The directory where you have most of your python projects.")

(defun fd-python-open-project (dir)
  (interactive (list (ido-read-directory-name
		      "Python project root to open: "
		      fd-python-projects-directory)))
  (when (not (fd-python-in-project-p dir))
    (error "Not a python project"))

  (if (fd-python-in-project-p dir)
      (save-excursion
	(fd-python-open-project-internal dir))
    (fd-python-open-project-internal dir)))

(defun fd-python-close-project (dir)
  (interactive (list (ido-read-directory-name
		      "Python project root to close: ")))
  (when (not (fd-python-in-project-p dir))
    (error "Could not close project. Not a project directory"))

  (dolist (b (buffer-list))
    (when (and (buffer-file-name b)
	       (fd-python-in-project-p (buffer-file-name b)
				       dir))
      (kill-buffer b))))

(defvar fd-setup-test-cmd "test"
  "This might even 'nosetests'.")

(defun* fd-test-cmd (&rest module)
  (format "%s %s setup.py %s %s"
	  (fd-python-command)
	  (or module "")
	  fd-setup-test-cmd
	  (if (fd-python-in-test-p)
	      (format "-s %s" (fd-python-current-class-path))
	    "")))


(defun fd-python-run-tests (arg)
  "Run tests of current project, if prefix jump to test buffer or
file before running (thus you might run the current test only)."
  (interactive "P")
  (save-excursion
    (when (and (fd-python-in-test-p)
	       (not (fd-python-in-test-p)))
      (fd-python-jump-to-test))
    (let (compilation-directory
	  compile-command
	  (default-directory (fd-python-project-root)))
      (compile (fd-test-cmd) t))))

(defun fd-gud-pdb-test (&optional cmd)
  (interactive
   (list
    (read-shell-command "Command to run tests: "
			(fd-test-cmd "-m pdb"))))

  (let ((default-directory (fd-python-project-root)))
    (when (null default-directory) (error "Not in a python project."))
    (when (get-buffer "*gud-pdb*") (kill-buffer-ask (get-buffer "*gud-pdb*")))
    (pdb cmd)))

(defun fd-python-hook ()
  (define-key python-mode-map (kbd "C-c C-t")
    'fd-python-jump-between-test-and-implementation)
  (define-key python-mode-map (kbd "C-c M-t") 'fd-python-run-tests)
  (define-key python-mode-map (kbd "C-c C-e") 'python-shell-send-buffer)
  (local-unset-key (kbd "<backtab>")))

(add-hook 'python-mode-hook 'fd-python-hook)

(provide 'fd-python)
