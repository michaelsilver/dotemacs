;; fd-projects.el

;; Files should return absolute paths
(defstruct project name root files)

(defvar projects-list nil
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
  (interactive (completing-read "Open project: "
				(projects-completion-alist project-list)
				nil t))
  (find-file (completing-read "Open file of a project: "
			      (project-files-completion-alist (project-files p)))))

(provide 'fd-projects)
