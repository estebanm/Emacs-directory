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
(add-to-list 'load-path "~/src/emacs/asciidoc")
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
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
; Disable flyspell, not cool
(flyspell-mode -1)
; Autocomplete (see http://cx4a.org/software/auto-complete/manual.html)
(add-to-list 'load-path "~/src/Emacs-directory/extra/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/src/Emacs-directory/extra/auto-complete/ac-dict")
(ac-config-default)
; Pomodoro (M-x pomodoro-start)
(require 'pomodoro)

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
(key-chord-define-global "qq" 'iy-go-to-char)
(key-chord-define-global "QQ" 'iy-go-to-char-backward)
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
(add-hook 'nxml-mode-hook '(lambda nil
                             (local-set-key [M-end] 'nxml-finish-element)))
; Autoclose tag on </
(setq nxml-slash-auto-complete-flag t)

;; Javascript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-consistent-level-indent-inner-bracket-p t)
(setq js2-pretty-multiline-decl-indentation-p t)
(put 'narrow-to-region 'disabled nil)
