;; This is a cookbook I will fill with random elisp I write and might
;; need in the future but dont want loaded with emacs.

;; Helpers for sorting grep results
(defun file-size (filename)
  "Size of the file."
  (nth 7 (file-attributes filename)))

(defun grep-current-line-filesize ()
  ""
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((p1 (point)))
      (search-forward ":") (backward-char)
      (let ((fsize (file-size (buffer-substring p1 (point)))))
	(beginning-of-line)
	(insert (format "filesize:%d| " fsize))))))

(defun remove-grep-line-filesize ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp "^filesize:[0-9]+| " "")))
