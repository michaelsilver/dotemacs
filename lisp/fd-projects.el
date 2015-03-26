;;; fd-projects.el --- Summary
;;; Commetary:
;; Open and bury projects intuitively.
;;
;; TODO:
;; - Prioritize open buffers over
;;; Code:
(require 'recentf)

(defvar project--commit-files-command
  "for i in  $(git rev-list HEAD~%d..HEAD); do git show --pretty='format:' --name-only  $i; done | sort | uniq -c | sort -n -r | sed 's/^ *[0-9]* *//'"
  "Command to be formated with the number of commits to be looked
  into.")

(defvar project--commit-depth 20
  "Number of commits to look into when looking for recently
  edited files")

(defvar project--max-files 5
  "Number of files to open for a project.")

(defun project-roots (&optional files)
  "Project roots of files if they are in a git repository. If
files is not provided use recent files."
  (delete-if-not
   (apply-partially 'string-prefix-p "/")
   (delete-dups
    (mapcar
     (lambda (d)
       (string-trim
        (shell-command-to-string
         (format "git -C %s rev-parse --show-toplevel" d))))
     (delete-dups
      (mapcar
       'file-directory-name
       (delete-if-not 'file-exists-p
                      (or files recentf-list))))))))

(defun project-buffer-in-project (project buffer)
  (projects-file-in-project
   (or (buffer-file-name buffer)
       (with-current-buffer buffer
         default-directory))))

(defun project-file-in-project (project file)
  "Check if buffer corresponds to the project"
  (= 0
     (call-process "git" nil nil nil "-C"
                   project
                   "ls-files"
                   file
                   "--error-unmatch")))

(defun project-common-files (project)
  "List interesting files in a project using
`project--comit-files-command' and `project--commit-depth'. These
are the filesystem files."
  (mapcar (apply-partially 'concat project "/")
          (split-string
           (shell-command-to-string
            (format "cd %s && %s | head -%d"
                    project
                    (format project--commit-files-command project--commit-depth)
                    project--max-files))
           "\n")))

(defun project-recent-files (project)
  "Recently opened files of project"
  (delete-if-not (apply-partially 'projects-file-in-project)
                 recentf-list))

(defun project-files (project)
  "A prioritized list of some of the files in project."
  (delete-dups (append
                (project-recent-files project)
                (project-common-files project))))

(defun project-read-root (pred)
  (let ((roots (project-roots)))
    (ido-completing-read pred roots nil t)))


(defun project-buffers (project)
  "A list of open buffers in the project. Ordered as they were in
buffer-list."
  (delete-if-not
   (apply-partially 'projects-buffer-in-project project)
   (buffer-list)))

(defun project-open (project)
  "Open project. Preserve the sequence of buffers in the buffer
list and prioritize recent buffers over unopened ones. Unopened
buffers are ones that have recent commits."
  (interactive
   (list (project-read-root "Open project: ")))
  (let ((bufs (project-buffers project)))
    ;; First get all the files we dont have, they will mess up the
    ;; buffer-list ordering
    (mapc 'find-file (reverse (project-files project)))
    ;; Restore ordering for our old buffers and put them at the top.
    (mapc 'switch-to-buffer (reverse bufs))))

(defun bury-project ()
  "Burry the current project."
  (interactive)
  (let ((root (car (project-roots))))
    (dolist (b (buffer-list))
      (when (file-in-directory-p (buffer-file-name b) root)
        (bury-buffer b)))))

(provide 'fd-projects)
;;; fd-projects.el ends here
