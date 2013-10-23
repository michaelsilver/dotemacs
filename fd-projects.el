;;; fd-projects.el
;; A file-centeric project system. Files and buffers are king,
;; projects are just a set of shared properties that our enhanced
;; files have.

;; TODO:
;;
;; Project Compilation: Have a wrapper for compile/recompile that will
;; be project aware.
;;
;; Project Matching: Given a buffer decide which of the know projects
;; is most probably the one or create one using git.
;;
;; Dynamic Project creation: we need to be able to create a
;; project from a git repo
;;
;; Project File: It will be aware of it's buffer if open, it will be
;; able to open, it will know a short version of it's name, it's
;; modification date and it will be extensible. Also this can be
;; cached using a local buffer variable.


;; Files should return absolute paths

(defun default-project-files (root)
  "Default files function."
  (directory-files root))

(defstruct (project
	    (:constructor nil)
	    (:constructor new-project (name root &key
					    (files 'default-project-files)
					    (regexp nil)
					    (recursive t))))
  name root files regexp recursive)

(defun project-file-list (pr)
  "Get the project files of a project."
  (let* ((df (project-files pr))
	 (root (project-root pr))
	 (all-files (funcall df root))
	 (regex (project-regexp pr))
	 (filtered-files (if regex
			     (remove-if-not
			      (lambda (x) (string-match regex x)) all-files)
			   all-files)))
    filtered-files))

(defvar projects-list (list (new-project "emacs" "~/.emacs.d/" :regexp "\\.el$"))
  "List of project structs.")

(defun projects-completion-alist (projects)
  "Get an alist suitable for completing read."
  (mapcar (lambda (x) (cons (project-name x) x))
	  projects))

(defun project-files-completion-alist (files)
  "Get a list of files to complete. Sort them by modification
  date."
  (mapcar (lambda (x) x)
	  files))

(defun projects-file-open (p)
  "Interactively find a project and open it."
  (interactive (let ((completions (projects-completion-alist projects-list)))
		 (list (cdr (assoc
			     (completing-read "Open project: "
					      completions
					      nil t) completions)))))
  (message (format "Project %s" (project-name p)))
  (find-file (completing-read "Open file of a project: "
			      (project-files-completion-alist (project-file-list p)))))

(provide 'fd-projects)
(defun test-project ()
  (let ((pr (new-project "emacs" "~/.emacs.d/" :regexp "\\.el$")))
    (project-file-list pr)))

(test-project)
