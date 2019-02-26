;;; init.el --- Alex Metzger's Emacs config
;;
;; Copyright (c) 2019 Alex Metzger
;;
;; Author: Alex Metzger <asm@asm.io>
;; URL: https://gitlab.com/ametzger/emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Alex Metzger's Emacs config, heavily influenced by bbatsov's
;; https://github.com/bbatsov/emacs.d/blob/master/init.el and
;; Atman50's https://github.com/Atman50/emacs-config

;;; License:

;; MIT License
;;
;; Copyright (c) 2019 Alex Metzger
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

;; packaging
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)




;; vanity
(setq user-full-name "Alex Metzger"
      user-mail-address "asm@asm.io")

;; emacs baseline + annoyances
(setq load-prefer-newer t)

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(setq large-file-warning-threshold 50000000)

(defconst asm/savefile-dir
  (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p asm/savefile-dir)
  (make-directory asm/savefile-dir))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(blink-cursor-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq initial-scratch-message (format ";; Hola!\n\n"))

(setq scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(defun asm/split-window-vertically ()
  "Open a new vertical window and switch to it."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun asm/split-window-horizontally ()
  "Open a new horizontal window and switch to it."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'asm/split-window-vertically)
(global-set-key (kbd "C-x 3") 'asm/split-window-horizontally)

(defun asm/toggle-window-split ()
  "Toggle how windows are split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c |") 'asm/toggle-window-split)

(setq help-window-select t)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

;; Bind C-S-SPC to mark the whole line (similar to
;; `evil-visual-line')
(defun asm/select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position))
  (forward-line))
(global-set-key (kbd "C-S-SPC") 'asm/select-current-line)

;; font config
(let ((font-size (if (eq system-type 'darwin)
                     16
                   13)))
  (set-frame-font (format "Operator Mono %d" font-size)))

(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#6d7a96" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#6d7a96" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "#81A1C1" :slant italic)))))

;; indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Newline at end of file
(setq require-final-newline t)

(delete-selection-mode t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x \\") #'align-regexp)

(defun asm/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") #'asm/switch-to-previous-buffer)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq frame-title-format nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; platform-specific
(if (eq system-type 'darwin)
      (progn (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
             (add-to-list 'default-frame-alist '(ns-appearance . dark))
             (setq-default ns-use-srgb-colorspace t
                           ns-use-proxy-icon nil)))

;;; built-in packages
;; disable version control, magit forever
(remove-hook 'find-file-hook 'vc-find-file-hook)
(setq vc-handled-backends nil)

(use-package paren
  :config
  (show-paren-mode +1))

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" asm/savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" asm/savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" asm/savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

;;; third-party

;; theme, modeline
(use-package doom-themes
  :ensure t
  :after (rainbow-delimiters)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nord t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-python-executable (expand-file-name "~/.pyenv/shims/python"))
  (setq doom-modeline-lsp nil)
  (setq doom-modeline-mu4e nil))

;; usability (abo-abo, bbatsov and tarsius are gods)
(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)
  (setq magit-repository-directories '("~/proj/")))

(use-package forge
  :ensure t
  :requires magit)

(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

(use-package ag
  :if (executable-find "ag")
  :ensure t)

(use-package ripgrep
  :if (executable-find "rg")
  :ensure t)

(use-package pt
  :ensure t)

(use-package projectile
  :ensure t
  :after (ivy)
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

;; universal code-related
(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package dash-at-point
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (add-to-list 'dash-at-point-mode-alist '(python-mode . "asmdj"))
  (global-set-key (kbd "s-d") 'dash-at-point))

(use-package zeal-at-point
  :ensure t
  :if (memq window-system '(x))
  :config
  (global-set-key (kbd "s-d") 'zeal-at-point))

(use-package flycheck
  :after pyenv-mode
  :ensure t
  :config
  (add-hook 'after-init-hook (lambda ()
                               (flymake-mode -1)
                               (global-flycheck-mode))))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package super-save
  :ensure t
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))

(use-package crux
  :ensure t
  :bind (("C-c d" . crux-duplicate-current-line-or-region)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.4)
  (which-key-mode +1))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-set-key (kbd "C-/") 'undo-tree-undo)
  (global-set-key (kbd "C-?") 'undo-tree-redo))

(use-package prescient
  :ensure t)

(use-package ivy-prescient
  :ensure t
  :requires (prescient ivy)
  :config
  (ivy-prescient-mode))

(defun asm/ivy-sort-by-length (_name candidates)
  "Sort `CANDIDATES' matching `_NAME' by length."
  (cl-sort (copy-sequence candidates)
           (lambda (f1 f2)
             (< (length f1) (length f2)))))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-sort-matches-functions-alist
        '((t)
          (counsel-find-file . asm/ivy-sort-by-length)
          (projectile-completing-read . asm/ivy-sort-by-length)))
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper)
  (global-set-key "\C-r" 'swiper))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key counsel-find-file-map (kbd "C-l") 'ivy-backward-delete-char))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1)

  ;; Have `kill-region' delete the current line if no region is active
  (defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2))))))

(use-package rainbow-delimiters
  :ensure t)

;; show hex colors with background, e.g. #0000ff
(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 100) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; ruby
(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode))

;; markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

;; python
(use-package pyenv-mode
  :ensure t
  :config
  (add-to-list 'exec-path "~/.pyenv/shims")
  (add-hook 'python-mode-hook 'pyenv-mode)
  (setq-default flycheck-python-pycompile-executable (expand-file-name
                                                      "~/.pyenv/shims/python")
                flycheck-python-flake8-executable (expand-file-name
                                                   "~/.pyenv/shims/flake8")
                flycheck-python-mypy-executable (expand-file-name
                                                 "~/.pyenv/shims/mypy")))

(use-package pyenv-mode-auto
  :ensure t)

(use-package pipenv
  :ensure t)

(defun asm/python-mode-hook ()
  "Initialize `python-mode'."

  ;; use flat imenu
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (subword-mode +1)
  (setq indent-tabs-mode nil))

(use-package python
  :mode ("\\.py'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq-default python-fill-docstring-style 'django)
  (add-hook 'python-mode-hook 'asm/python-mode-hook))

(use-package jedi
  :ensure t)

(use-package company-jedi
  :ensure t
  :after (company jedi)
  :config
  (add-to-list 'company-backends 'company-jedi)
  (setq-default company-jedi-python-bin "~/.pyenv/shims/python"))

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :config
  (add-to-list 'company-backends
               '(company-anaconda :with company-capf)))

(use-package blacken
  :ensure t
  :config
  (setq blacken-executable "~/.pyenv/shims/black")
  (define-key python-mode-map (kbd "C-c C-b") 'blacken-buffer))

(use-package flycheck-mypy
  :ensure t)

(use-package ein
  :ensure t
  :config
  (setq-default ein:complete-on-dot nil
                ein:completion-backend 'ein:use-company-backend
                ein:query-timeout 1000
                ein:default-url-or-port "http://localhost:8888"
                ein:worksheet-enable-undo 'full)
  (setq-default request--curl-cookie-jar (concat user-emacs-directory "request/curl-cookie-jar")))

(use-package json-mode
  :ensure t
  :config
  (define-key json-mode-map (kbd "C-c C-b") 'json-pretty-print-buffer))

(use-package yaml-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.html$"
  :config
  (setq-default web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-sql-indent-offset 2
                web-mode-enable-auto-indentation nil)
  (add-hook 'web-mode-hook (lambda () (web-mode-set-engine "django"))))

(use-package js2-mode
  :ensure t
  :mode "//.js$"
  :config
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "setTimeout" "clearTimeout" "setInterval"
                                         "clearInterval" "location" "console" "JSON"
                                         "jQuery" "$"))

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(bind-keys :map global-map
           :prefix-map asm/ctrl-z-prefix-map
           :prefix "C-z"
           ("r" . anzu-query-replace)
           ("e" . flycheck-list-errors)
           ("w" . ace-window)
           ("R" . anzu-query-replace-regexp)
           ("f" . projectile-find-file)
           ("F" . projectile-find-file-other-window)
           ("p" . projectile-switch-project)
           ("R" . anzu-query-replace-regexp)
           ("f" . projectile-find-file)
           ("s" . projectile-ag)
           ("i" . imenu)
           ("d" . dash-at-point))

;; load custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
