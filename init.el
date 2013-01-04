;; Load different files
(setq custom-config-base-dir (concat "~/src/Emacs-directory/")
      custom-config-extras-dir (concat custom-config-base-dir "extra/")
      custom-config-contrib-dir (concat custom-config-base-dir "contrib/"))
(add-to-list 'load-path custom-config-base-dir)
(add-to-list 'load-path custom-config-extras-dir)
(if (file-exists-p custom-config-extras-dir)
  (mapc #'load (directory-files custom-config-extras-dir nil ".*el$")))
(if (file-exists-p custom-config-contrib-dir)
  (mapc (lambda (dir) (add-to-list 'load-path dir))
        (directory-files custom-config-contrib-dir t "^[^.]")))

;; Own code
; Load own functions, bound to shortcuts below
(load-library "editing-functions")
; AsciiDoc major mode
(require 'asciidoc-mode)

;; Font and color theme
(set-default-font "Inconsolata-12")
(require 'color-theme-zenburn)
(color-theme-zenburn)

;; Misc options and minor modes
; automatically revert files that changed on disk when the open buffer
; has no unsaved changes
(global-auto-revert-mode 1)
; smex (better M-x behaviour)
(smex-initialize)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)
; Rainbow
(require 'rainbow-mode)
(rainbow-mode)
; Volatile highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)
; Ace jump
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
; Disable flyspell, not cool
(flyspell-mode -1)
; Autocomplete (see http://cx4a.org/software/auto-complete/manual.html)
;;; (add-to-list 'load-path "~/src/Emacs-directory/extra/auto-complete/")
;;; (require 'auto-complete-config)
;;; (add-to-list 'ac-dictionary-directories "~/src/Emacs-directory/extra/auto-complete/ac-dict")
;;; (ac-config-default)
; Pomodoro (M-x pomodoro-start)
(require 'pomodoro)
; Expand region
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(pending-delete-mode t)
; Buster mode
(require 'buster-mode)
; Make scripts executable on save
(add-hook 'after-save-hook 'hlu-make-script-executable)
; mark-multiple
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)
(global-set-key (kbd "C-M-*") 'mark-all-like-this)
; projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-ignored-directories '("build" "deps" "tmp" "node_modules" "mobile" "docs"))
(setq projectile-ignored-files '("*.log" "*.json"))
; Mouse sucks, load minibuffer-errors
(load-library "flymake-cursor")

;; Common shortcuts
(global-set-key "\C-ch" help-map)
(global-set-key "\C-h" 'backward-delete-char)
; (global-set-key "\M-\C-h" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-xj" '(lambda () (interactive) (join-line -1)))
(global-set-key "\C-x\C-j" '(lambda () (interactive) (join-line -1)))
; (global-set-key "\C-\M-t" 'transpose-lines)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
; I hate minimize
(global-set-key "\C-z" 'ignore)
(global-set-key "\C-x\C-z" 'ignore)
; Add C-c c t
(global-set-key "\C-cct" 'clone-current-test)
; Search for the word under the cursor (like * in VIM)
(global-set-key [?\C-8]  'isearch-forward-at-point)
(global-set-key (kbd "C-%")  'goto-match-paren)
; Key chords
(key-chord-mode 1)
; Use iy-go-to-char to gain some VIM POWA
(key-chord-define-global "qw" 'iy-go-to-char)
(key-chord-define-global "QW" 'iy-go-to-char-backward)
(key-chord-define-global "dw" 'kill-following-whitespace)
; d0 chord as C-u in bash (mimicking VIM keystrokes)
(key-chord-define-global "d0" 'kill-until-start-of-line)
; Change window with Ctrl-TAB
(global-set-key [\C-tab] 'other-window)

;; Expansion
; Complete lines like VIM
(global-set-key "\C-x\C-o" (make-hippie-expand-function
                               '(try-expand-line) t))
; Complete file names like VIM
(global-set-key "\C-x\C-t" (make-hippie-expand-function
                               '(try-complete-file-name
                                 try-complete-file-name-partially) t))


;; yasnippet
(setq yas/root-directory "~/src/Emacs-directory/snippets")
(yas/load-directory yas/root-directory)


;;; Major mode shenanigans ---------------------------------------------

;; Markdown mode for .text files
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.\\(text\\|markdown\\)" . markdown-mode) auto-mode-alist))

;; AsciiDoc mode
(setq auto-mode-alist
   (cons '("\\.\\(asciidoc\\|txt\\)" . asciidoc-mode) auto-mode-alist))

;; nXML configuration
(require `nxml-mode)
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))
(add-hook 'nxml-mode-hook '(lambda ()
                             (local-set-key [M-end] 'nxml-finish-element)))
; Autoclose tag on </
(setq nxml-slash-auto-complete-flag t)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
; Javascript refactoring (have to be loaded here, after loading js2-mode)
(require 'js2-refactor)
; jshint
(require 'flymake-node-jshint)
(add-hook 'js2-mode-hook (lambda () (flymake-mode t)))


;; Python
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
; Pyflakes ON, baby
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "flake8" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

; Clojure
(require 'clojure-mode)
(require 'clojure-test-mode)

;; Magit
(require 'magit)
(load-library "magit-config")
