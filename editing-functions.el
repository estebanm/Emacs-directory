(defun kill-following-whitespace ()
  (interactive)
  (if (looking-at "[ \s]")
      (progn
        (delete-char 1)
        (kill-following-whitespace))))

(fset 'kill-until-start-of-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\260" 0 "%d")) arg)))
