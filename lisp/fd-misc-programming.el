;; Programming realted miscelaneous
(which-function-mode t)

(setq own-git-repo-regex-list '("origin[ \t]*git@github.com:fakedrake.*"
                                "origin[ \t]*git@github.com:codebendercc.*"))

(defun fd-git-remotes ()
  "Get or set-and-get git remotes. Caching function"
  (if (boundp 'fd-git-remotes) fd-git-remotes
    (setq-local fd-git-remotes (git--exec-string "remote" "-v"))))

(defun in-own-git-repo-p ()
  (some (lambda (x)
          (string-match
           x (fd-git-remotes)))
        own-git-repo-regex-list))

(defun in-git-repo-p ()
  (null
   (string-match
    "fatal: Not a git repository"
    (fd-git-remotes))))


(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace if you are not in a foreign
  project."
  (when (or (in-own-git-repo-p)
            (not (in-git-repo-p))
            (y-or-n-p "Foreign git origin. Delete trailing whitespaces? "))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'maybe-delete-trailing-whitespace)

(setq require-final-newline 'query)
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1))) ;; fix tabcompletion

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers


(add-hook 'term-mode-hook
          #'(lambda ()
              (setq autopair-dont-activate t) ;; for emacsen < 24
              (autopair-mode -1))             ;; for emacsen >= 24
          )

;; Indent buffer
(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))
(global-set-key "\C-x\\" 'indent-buffer)

;; Ediff
(setq ediff-split-window-function 'split-window-horizontally)

(defalias '>> 'rsh)
(defalias '<< 'lsh)

(defun github-clone (repo)
  (interactive "MProvide the repository line <username>/<repo>: ")
  (async-shell-command (format "git clone git@github.com:%s" repo)))

(global-set-key (kbd "C-j") 'default-indent-new-line)
(global-set-key (kbd "C-M-l") 'add-dir-local-variable)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(defvar projects-alist '(("wikipedia-base" . "/home/fakedrake/Projects/CSAIL/Python/WikipediaBase/PLAN.org")
                         ("overlay-parse" . "/home/fakedrake/Projects/CSAIL/Python/py/src/overlay-parse/overlay_parse/dates.py")
                         ("dotemacs" . "~/.emacs.d/fd-misc-programming.el")
                         ("csail-plan" . "~/Projects/CSAIL/PLAN.org")
                         ("wikipedia-mirror-futuna" . "/ssh:cperivol@futuna.csail.mit.edu:/scratch/cperivol/wikipedia-mirror/")
                         ("wikipedia-mirror-ashmore" . "/ssh:cperivol@ashmore.csail.mit.edu:/local/cperivol/wikipedia-mirror/")
                         ("wikipedia-mirror-tuvalu" . "/ssh:cperivol@tuvalu.csail.mit.edu:/local/cperivol/wikipedia-mirror/")
                         ("xilinx-zynq-bootstrap-purple" . "/ssh:cperivol@purple:/homes/cperivol/Projects/xilinx-zynq-bootstrap")
                         ("wikipedia-base-futuna" . "/ssh:cperivol@futuna.csail.mit.edu:/scratch/cperivol/wikipediabase/"))
  "An alist of projects and the most relatd file.")

(defun read-project-name ()
  "An abstraction to implement ways of deciding which should be
the default project"
  (ido-completing-read "Jump to project: " projects-alist))

(defun jump-to-project (project-name)
  "Jump to a characteristic file in a project found in
PROJECTS-ALIST."
  (interactive (list (read-project-name)))
  (find-file  (cdr (assoc project-name projects-alist))))

(global-set-key (kbd "C-x j p") 'jump-to-project)

(setq git--commit-buttonize-filenames-regex "^\t[^:]+: +\\(.*\\)")

(defmacro btn-cmd (&rest body)
  "Builds a function that runs the specified body and remembers
the point position in the commit buffer between calls of each
function created this way."
  `(lambda ()
     (interactive)
     (let ((commit-buffer (current-buffer)))
       (save-excursion
         ;; Make sure there is a mark
         (unless (boundp 'button-mark)
           (setq-local button-mark (make-marker))
           (set-marker button-mark (point-min)))

         (goto-char (marker-position button-mark))
         (let ((eval-val (progn ,@body)))
           (with-current-buffer-safe commit-buffer
             (set-marker button-mark (point)))
           eval-val)))))

(defun activate-previous-button ()
  (let ((btn (previous-button (point))))
    (if btn
        (progn
          (goto-char (button-end btn))
          (button-activate btn))
      (message "Before the first button"))))

(defun activate-next-button ()
  (let ((btn (next-button (point))))
    (if btn
        (progn
          (goto-char (button-end btn))
          (button-activate btn))
      (message "After last button"))))

(defun fd-compilation-git-commit-bindings ()
  (local-set-key (kbd "C-x `") (btn-cmd (activate-next-button)))
  (local-set-key (kbd "M-g n") (btn-cmd (activate-next-button)))
  (local-set-key (kbd "M-g p") (btn-cmd (activate-previous-button))))

(add-hook 'git-comment-hook 'fd-compilation-git-commit-bindings)
(add-hook 'sql-mode-hook (lambda () (setq-local untabify-on-save t)))
(provide 'fd-misc-programming)
