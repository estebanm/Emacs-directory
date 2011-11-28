(defun kill-following-whitespace ()
  (interactive)
  (if (looking-at "[ \t]")
      (progn
        (delete-char 1)
        (kill-following-whitespace))))

(fset 'kill-until-start-of-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\260" 0 "%d")) arg)))

(fset 'clone-current-test
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 13 115 101 97 114 99 104 45 98 97 99 107 tab 45 tab return 94 32 32 125 44 return 67108896 14 24 13 115 101 97 114 99 104 45 102 111 114 tab 45 tab return 94 32 32 125 return 1 24 11 25 25] 0 "%d")) arg)))

(defvar isearch-initial-string nil)
(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))
(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise go
   back one character (when you're at the end of a line ending in
   a closed paren, it's annoying having to go one character
   backwards just so that this works)."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (backward-char))))
