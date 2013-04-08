(defun kill-following-whitespace ()
  (interactive)
  (if (looking-at "[ \t]")
      (progn
        (delete-char 1)
        (kill-following-whitespace))))

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

(defun hlu-make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
                 (not (file-executable-p buffer-file-name)))
        (set-file-modes buffer-file-name
                        (logior (file-modes buffer-file-name) #o100))
        (message (concat "Made " buffer-file-name " executable"))))))

(defun comment-and-duplicate-line ()
  "Copy current line to line below and comment current line."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties beg end))
         (column (current-column)))
      (comment-region beg end)
      (goto-char (line-end-position))
      (newline)
      (insert line)
      (move-to-column column)))

;; Stolen from http://whattheemacsd.com//editing-defuns.el-01.html
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))
