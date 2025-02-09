(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(load "~/.emacs.rc/rc.el")

(setq custom-file "~/.emacs.custom.el")
(setq make-backup-files nil)

;; My favorite fonts
(add-to-list 'default-frame-alist `(font . "Consolas-14"))
;; (add-to-list 'default-frame-alist `(font . "UnifontExMono-12"))
;; (add-to-list 'default-frame-alist `(font . "Iosevka Custom-14"))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(fringe-mode 0)
(ido-mode 1)
(ido-everywhere 1)
(setq inhibit-startup-screen t)
(setq dired-dwim-target t)
(setq display-line-numbers-type 1)
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(global-hl-line-mode 1)
(set-language-environment "UTF-8")
(setq-default indent-tabs-mode nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq default-directory "~/dev/")

(rc/require 'gruber-darker-theme)
(load-theme 'gruber-darker t)

(use-package mood-line

  ;; Enable mood-line
  :config
  (mood-line-mode)

  ;; Use pretty Fira Code-compatible glyphs
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

;; Duplicate line
(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
	(line (let ((s (thing-at-point 'line t)))
		(if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))
(global-set-key (kbd "C-,") 'rc/duplicate-line)

(rc/require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-to-list 'load-path "~/.emacs.local")
(require 'simpc-mode)
;; (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; Fasm-mode
(require 'fasm-mode)

;; C++ normal tabs hook
(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)

;; Go-mode
(rc/require 'go-mode)
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

;; Haskell-mode
(rc/require 'haskell-mode)

;; Smex
(rc/require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Move text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; Multiple cursors
(rc/require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; Magit
(rc/require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Disable stupid beep sounds on windows
(when (eq system-type 'windows-nt)
  (setq ring-bell-function 'ignore))
  ;; (setq visible-bell 1))


;; Expand region
(rc/require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-startup-indented 1)

;; Try to complete the file path
(global-set-key (kbd "C-M-/") 'my-expand-file-name-at-point)
(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))

;; Zig-mode
(rc/require 'zig-mode)
(setq zig-format-on-save nil)

;; Compilation mode ANSI colors hook
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (dired-toggle-read-only)
  (display-ansi-colors)
  (dired-toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Typescript mode
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(rc/require 'tide)

;; Cmake-mode
(rc/require 'cmake-mode)

(rc/require 'glsl-mode)
(defun glsl-mode-adjust-closing-bracket ()
  (setf (cdr (assoc 'arglist-close c-offsets-alist)) 0))
(add-hook 'glsl-mode-hook #'glsl-mode-adjust-closing-bracket)
  
(load-file custom-file)
