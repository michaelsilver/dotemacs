(defun fd-sql-hook ()
  (define-key sql-mode-map (kbd "C-c <") 'decrease-left-margin)
  (define-key sql-mode-map (kbd "C-c >") 'increase-left-margin))

(add-hook 'sql-mode-hook 'fd-sql-hook)

(provide 'fd-sql)
