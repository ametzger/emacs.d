;;; init.el --- Alex Metzger's Emacs config
;;
;; Copyright (c) 2021 Alex Metzger
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
(setq package-enable-at-startup nil
      load-prefer-newer t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; emacs 27+ will throw "Warning (package): Unnecessary call to
;; ‘package-initialize’ in init file" error if this is called, but
;; older versions require it.
(when (version< emacs-version "27.0")
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default use-package-enable-imenu-support t
              use-package-verbose nil)
(require 'use-package)
;; (use-package auto-package-update
;;   :config
;;   (setq-default auto-package-update-delete-old-versions t
;;                 auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

;; (use-package paradox
;;   :ensure t
;;   :config
;;   (paradox-enable))

;; vanity
(setq user-full-name    "Alex Metzger"
      user-mail-address "asm@asm.io"
      user-login-name   "asm")

;; emacs baseline + annoyances
(setq custom-file (make-temp-file ""))

(setq save-interprogram-paste-before-kill t
      gc-cons-threshold 64000000)

(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold 200000)))

(setq large-file-warning-threshold 50000000)

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c k")
                (lambda ()
                  (interactive)
                  (kill-this-buffer)))
(global-set-key (kbd "C-c C-k")
                (lambda ()
                  (interactive)
                  (kill-this-buffer)
                  (asm/delete-windows-and-rebalance)))

(defconst asm/savefile-dir
  (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p asm/savefile-dir)
  (make-directory asm/savefile-dir))

;; Disable visual cruft
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(blink-cursor-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Disable menu bar on Linux and in terminal, enable on grapical OS X.
(defun asm/menubar-config (&optional frame)
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
    (if (and (display-graphic-p frame)
          (memq window-system '(mac ns)))
      1 0)))
(add-hook 'after-make-frame-functions 'asm/menubar-config)

(defun display-startup-echo-area-message ()
  (message "Howdy!"))

(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      initial-scratch-message (format "Welcome to Emacs %s (started %s, startup took %s)\n\n"
                                      emacs-version
                                      (current-time-string)
                                      (emacs-init-time))
      scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil
      frame-resize-pixelwise t
      initial-major-mode 'text-mode)

;; indent on RET
(global-set-key (kbd "RET") #'newline-and-indent)

;; TODO(asm,2019-03-21): these don't work correctly with multiple
;; monitors.
(defun asm/window-left ()
  (interactive)
  (let ((frame (selected-frame))
        (one-half-display-pixel-width (/ (display-pixel-width) 3)))
    (set-frame-width frame one-half-display-pixel-width nil 'pixelwise)
    (set-frame-height frame (display-pixel-height) nil 'pixelwise)
    (set-frame-position frame 0 0)))

;; leften the window when starting emacs
(add-hook 'after-init-hook #'asm/window-max)

(defun asm/window-max ()
  (interactive)
  (toggle-frame-maximized))

  ;; (let ((frame (selected-frame)))
  ;;   (set-frame-position frame 0 0)
  ;;   (set-frame-width frame (display-pixel-width) nil 'pixelwise)
  ;;   (set-frame-height frame (display-pixel-height) nil 'pixelwise)))

(defun asm/split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (balance-windows)
  (other-window 1))

(defun asm/split-window-horizontally ()
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1))

(defun asm/delete-windows-and-rebalance ()
  (interactive)
  (unless (one-window-p)
    (delete-window)
    (balance-windows)))

(global-set-key (kbd "C-x 2") #'asm/split-window-vertically)
(global-set-key (kbd "C-x 3") #'asm/split-window-horizontally)
(global-set-key (kbd "C-x 0") #'asm/delete-windows-and-rebalance)

(defun asm/toggle-window-split ()
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

(global-set-key (kbd "C-c |") #'asm/toggle-window-split)

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
(global-set-key (kbd "C-S-SPC") #'asm/select-current-line)

;; font config
(let ((font-name
       ;; "IBM Plex Mono Medium"
       "Operator Mono Medium"
       ;; "SF Mono"
       ;; "Go Mono"
       ;; "Cascadia Code"
       )
      (font-size (if (eq system-type 'darwin)
                     18
                   13)))
  (set-frame-font (format "%s %d" font-name font-size) t t))

;; italics for comments, keywords. Prettiest in Operator Mono.
(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#6d7a96" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#6d7a96" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "#81A1C1" :slant italic)))))

;; indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              sh-basic-offset 2)

(put 'upcase-region 'disabled nil)

(setq require-final-newline t)

(delete-selection-mode t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

(set-charset-priority 'unicode)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)
      locale-coding-system   'utf-8)

(defun asm/comment-sanely ()
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))

(global-set-key (kbd "M-;") #'asm/comment-sanely)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
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

;; platform-specific
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark))
      (setq-default ns-use-srgb-colorspace t
                    ns-use-proxy-icon nil)
      (global-set-key (kbd "s-n") #'make-frame-command)))

;;; built-in packages
;; disable version control, magit forever
(remove-hook 'find-file-hook 'vc-find-file-hook)
(setq vc-handled-backends ())

(use-package server
  :defer 2
  :init
  (server-mode t)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package paren
  :config
  (show-paren-mode +1))

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package abbrev
  :diminish abbrev-mode
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
  :disabled
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
        ;; keep home clean
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
  :bind
  ;; ("C-c C-j" . dired-jump)
  (:map dired-mode-map
        ("RET" . dired-find-alternate-file)
        ("^" . (lambda () (interactive) (find-alternate-file ".."))))

  :config
  ;; dired ls config, disabled for now. Using `ls-lisp' instead.
  ;; OS X uses BSD ls by default, `brew install coreutils` puts GNU ls
  ;; as gls. Use that for dired if it's present
  ;; (setq dired-listing-switches "-aBhl")
  ;; (let ((coreutils-ls-path (executable-find "gls")))
  ;;   (if (and (eq system-type 'darwin)
  ;;            coreutils-ls-path)
  ;;       (setq insert-directory-program coreutils-ls-path))))

  (require 'ls-lisp)
  (setq ls-lisp-dirs-first t
        ls-lisp-use-insert-directory-program nil)

  (put 'dired-find-alternate-file 'disabled nil)

  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t))

(use-package dired-x
  :after (dired)
  :config
  (progn
    (setq dired-omit-verbose nil)
    ;; toggle `dired-omit-mode' with C-x M-o
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
          "^\\.?#\\|^.DS_STORE$\\|^.projectile$\\|^.git$\\|^.CFUserTextEncoding$\\|^.Trash$\\|^__pycache__$")))

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

(use-package ibuffer
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

(use-package ispell
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name (executable-find "aspell")
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
          ispell-silently-savep t)))

(use-package flyspell
  :init
  (progn
    (setq-default flyspell-use-meta-tab nil)))

(defun asm/org-mode-hook ()
  (auto-fill-mode t)
  (visual-line-mode t)
  (flyspell-mode t))

(use-package org
  :ensure t
  :demand
  :mode ("\\.org\\'" . org-mode)
  :bind
  ("C-c l" . org-store-link)
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  (:map org-mode-map
        ("C-a"   . crux-move-beginning-of-line)
        ("<RET>" . org-return-indent))
  :config
  (progn
    (setq org-directory "~/org"
          org-default-notes-file (concat org-directory "/scratch.org")
          org-agenda-files (mapcar
                            (lambda (path)
                              (concat org-directory "/" path))
                            '("scratch.org"
                              "main.org"
                              "todo.org"))
          org-capture-templates '(("t" "Todo" entry
                                   (file+headline "~/org/todo.org" "Tasks")
                                   "* TODO %^{Description}
   :LOGBOOK:
   - Added: %U
   :END:")
                                  ("l" "Link" item (file "~/org/links.org")
                                   "[[%^{URL}][%^{Description}]] %?%U\n" :prepend t)
                                  ("n" "Note" item
                                   (file+headline "~/org/scratch.org" "Notes")
                                   "%? %U\n%a\n" :prepend t))
          org-use-speed-commands t
          org-return-follows-link t
          org-confirm-babel-evaluate nil)
    (add-hook 'org-mode-hook #'asm/org-mode-hook)

    ;; support windmove keybinds without breaking heading shift
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)

    ;; Re-enable "<s" and other expansions after org 9.2
    (require 'org-tempo)))

(use-package org-bullets
  :ensure t
  :after (org)
  :commands (org-bullets-mode)
  :hook (org-mode . org-bullets-mode))

(use-package org-journal
  :ensure t
  :disabled
  :defer 2
  :init
  (defun asm/org-journal-done ()
    "Simple convenience function.
    Saves the buffer of the current day's entry and kills the window
    Similar to org-capture like behavior"
    (interactive)
    (save-buffer)
    (kill-buffer-and-window))
  :custom
  (org-journal-dir (concat (file-name-as-directory org-directory) "journal"))
  (org-journal-file-format "%Y/%m/%Y%m%d")
  (org-journal-date-format "%A, %Y-%m-%d")
  (org-journal-enable-agenda-integration t)
  (org-journal-hide-entries-p nil)
  (org-journal-time-format "%R
   ")
  :bind
  (("C-c C-j" . org-journal-new-entry)
   :map org-journal-mode-map
   ("C-c C-c" . asm/org-journal-done)))

;; emacs tools
(use-package esup
  :ensure t
  :disabled
  :init
  (setq esup-user-init-file
        (file-truename "~/.emacs.d/init.el")))

(use-package keyfreq
  :ensure t
  :disabled
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package helpful
  :ensure t
  :bind
  (("C-h f"   . helpful-callable)
   ("C-h v"   . helpful-variable)
   ("C-h k"   . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F"   . helpful-function)
   ("C-h C"   . helpful-command)))

;; theme, modeline
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :after (all-the-icons)
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package doom-themes
  :ensure t
  :after (rainbow-delimiters)
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-neotree-file-icons t
        doom-nord-brighter-comments nil
        doom-nord-region-highlight 'frost
        doom-nord-padded-modeline t
        doom-solarized-light-brighter-comments nil
        doom-solarized-light-brighter-modeline nil
        doom-solarized-light-padded-modeline t)
  (let (
        ;; (active-theme 'doom-solarized-light)
        (active-theme 'doom-nord)
        )
    (load-theme active-theme t))
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-python-executable (expand-file-name "~/.pyenv/shims/python")
        doom-modeline-lsp nil
        doom-modeline-mu4e nil
        doom-modeline-irc nil
        doom-modeline-env-version nil))

;; emoji
(use-package company-emoji
  :ensure t
  :demand
  :after (company)
  :hook
  ((markdown-mode   . company-mode)
   (git-commit-mode . company-mode)))

(use-package emojify
  :ensure t
  :hook
  ((markdown-mode     . emojify-mode)
   (git-commit-mode   . emojify-mode)
   (magit-status-mode . emojify-mode)
   (magit-log-mode    . emojify-mode)))

;; usability
(use-package hungry-delete
  :disabled
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package avy
  :ensure t
  :bind (("s-."   . avy-goto-word-or-subword-1)
         ("s-,"   . avy-goto-char-timer)
         ("C-'"   . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :config
  (setq avy-background t))

(use-package editorconfig
  :disabled
  :ensure t
  :hook
  ((mardown-mode . editorconfig-mode)
   (python-mode  . editorconfig-mode)
   (web-mode     . editorconfig-mode)
   (js2-mode     . editorconfig-mode)
   (sh-mode      . editorconfig-mode)
   (ruby-mode    . editorconfig-mode))
  :config
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-attr-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-attr-value-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-code-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-css-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-markup-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-sql-indent-offset))
  (add-to-list 'editorconfig-indentation-alist '(web-mode js2-basic-offset))
  (add-to-list 'editorconfig-indentation-alist '(js-mode js-indent-level js2-basic-offset))
  (add-to-list 'editorconfig-indentation-alist '(js2-mode js-indent-level js2-basic-offset))
  (add-to-list 'editorconfig-indentation-alist '(js2-minor-mode js-indent-level js2-basic-offset))
  (add-to-list 'editorconfig-indentation-alist '(nginx-mode nginx-indent-level nginx-indent-level)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)
  (setq magit-repository-directories '(("~/proj/" . 2)))

  ;; use full-screen magit
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen)))

(use-package forge
  :ensure t
  :disabled
  :demand t
  :after magit
  :init
  (setq forge-topic-list-limit '(10 . 0)))

(defun asm/git-commit-hook ()
  (set (make-local-variable 'company-backends)
       '(company-emoji)))
(add-hook 'git-commit-mode-hook #'asm/git-commit-hook)

(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

(use-package browse-at-remote
  :ensure t
  :defer t
  :commands (browse-at-remote)
  :bind (("C-c g" . browse-at-remote-kill)
         ("C-c G" . browse-at-remote))
  :custom
  ;; Use full commit hashes for long-lived links
  (browse-at-remote-prefer-symbolic nil))

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package direnv
  :ensure t
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package ag
  :if (executable-find "ag")
  :ensure t)

(use-package ripgrep
  :if (executable-find "rg")
  :ensure t)

(defun asm/deadgrep-project-root ()
  (if (projectile-project-p)
      (projectile-project-root)
    default-directory))

(use-package deadgrep
  :ensure t
  :after (ripgrep projectile)
  ;; :init
  ;; (setq deadgrep-project-root-function #'asm/deadgrep-project-root)
  )

(use-package wgrep
  :ensure t)

(use-package pt
  :ensure t)

(use-package dumb-jump
  :ensure t
  :bind
  ("C-M-g" . dumb-jump-go)
  ("C-c j" . hydra-dumb-jump/body)
  :config
  (setq dumb-jump-selector 'ivy)
  (defhydra hydra-dumb-jump (:color blue
                                    :hint nil)
          "
[dumb-jump]        _j_ump        _b_ack        _p_review        _J_ump in other window        _q_uit        "

    ("j" dumb-jump-go)
    ("b" dumb-jump-back)
    ("p" dumb-jump-quick-look)
    ("J" dumb-jump-go-other-window)
    ("q" nil)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :after (ivy)
  :init
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  (:map projectile-command-map
        ("F" . projectile-find-file-other-window))
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t))

(defhydra hydra-projectile (:color teal
                            :hint nil)
     "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
     ("a"   projectile-ag)
     ("b"   projectile-switch-to-buffer)
     ("c"   projectile-invalidate-cache)
     ("d"   projectile-find-dir)
     ("s-f" projectile-find-file)
     ("ff"  projectile-find-file-dwim)
     ("fd"  projectile-find-file-in-directory)
     ("g"   ggtags-update-tags)
     ("s-g" ggtags-update-tags)
     ("i"   projectile-ibuffer)
     ("K"   projectile-kill-buffers)
     ("s-k" projectile-kill-buffers)
     ("m"   projectile-multi-occur)
     ("o"   projectile-multi-occur)
     ("s-p" projectile-switch-project "switch project")
     ("p"   projectile-switch-project)
     ("s"   projectile-switch-project)
     ("r"   projectile-recentf)
     ("x"   projectile-remove-known-project)
     ("X"   projectile-cleanup-known-projects)
     ("z"   projectile-cache-current-file)
     ("`"   hydra-projectile-other-window/body "other window")
     ("q"   nil "cancel" :color blue))

(use-package counsel-projectile
  :ensure t
  :bind
  ("C-c p SPC" . counsel-projectile)
  :init
  (global-set-key (kbd "C-c C-p") #'hydra-projectile/body))

; TODO: this is slow, see
; https://github.com/purcell/ibuffer-projectile/issues/11
(use-package ibuffer-projectile
  :ensure t
  :after (projectile ibuffer)
  :config
  (progn
    (defun asm/ibuffer-hook ()
      (ibuffer-projectile-set-filter-groups)
      ;; sort alphabetically then by major mode
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)
        (ibuffer-do-sort-by-major-mode)))

    (add-hook 'ibuffer-hook #'asm/ibuffer-hook)))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package browse-kill-ring
  :ensure t
  :bind
  ("C-M-y" . browse-kill-ring))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-;"           . mc/mark-all-like-this-dwim)
  ("C-c C-<"       . mc/mark-all-like-this)
  ("C->"           . mc/mark-next-like-this)
  ("C-<"           . mc/mark-previous-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)
  :config
  ;; unmap return when in multi-cursor
  (define-key mc/keymap (kbd "<return>") nil))

(use-package iedit
  :ensure t
  :bind (("C-c ;" . iedit-mode)))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill))

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
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
         ("s-z" . (lambda () (interactive) (zop-up-to-char -1)))))

;; imenu
;; recenter around selected imenu items
(defun asm/imenu-select-hook ()
  (recenter scroll-margin))
(add-hook 'imenu-after-jump-hook 'asm/imenu-select-hook)
;; always be scanning
(setq imenu-auto-rescan t
      imenu-auto-rescan-maxout (* 1024 1024)
      imenu--rescan-item '("" . -99))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package imenu-list
  :ensure t
  :defer t
  :functions (imenu-list-smart-toggle)
  :bind (("C-." . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t))

;; general code-related
;; semantic mode is slow, disable it
(semantic-mode -1)

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package dash-at-point
  :ensure t
  :disabled
  :if (memq window-system '(mac ns))
  :config
  (add-to-list 'dash-at-point-mode-alist '(python-mode . "asmdj"))
  (global-set-key (kbd "s-d") #'dash-at-point))

(use-package zeal-at-point
  :ensure t
  :disabled
  :if (memq window-system '(x))
  :config
  (global-set-key (kbd "s-d") #'zeal-at-point))

(use-package flycheck
  :after pyenv-mode
  :diminish flycheck-mode
  :ensure t
  :config
  (add-hook 'after-init-hook (lambda ()
                               (flymake-mode -1)
                               (global-flycheck-mode)))
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-global-modes '(not org-mode
                                   text-mode
                                   fundamental-mode
                                   ein:notebook-mode))
  (global-company-mode t))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (progn
    (setq yas-prompt-functions '(yas-ido-prompt
                                 yas-completing-prompt))
    (yas-reload-all)
    (yas-global-mode)
    (defhydra hydra-yas (:color blue
                                :hint nil)
      "
[yasnippet]        _i_nsert        _n_ew        _v_isit snippet file        _r_eload all        e_x_pand        _?_ list snippets        "
      ("i" yas-insert-snippet)
      ("n" yas-new-snippet)
      ("v" yas-visit-snippet-file)
      ("r" yas-reload-all)
      ("x" yas-expand)
      ("?" yas-describe-tables)
      ("q" nil "cancel" :color blue))
    (global-set-key (kbd "C-c y") #'hydra-yas/body)
    (advice-add 'company-complete-common :before
                (lambda () (setq my-company-point (point))))
    (advice-add 'company-complete-common :after
                (lambda ()
                  (when (equal my-company-point (point))
                    (yas-expand))))))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

(use-package super-save
  :disabled
  :ensure t
  :config
  (setq auto-save-default nil)
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))

(use-package crux
  :ensure t
  :bind (("C-c d"         . crux-duplicate-current-line-or-region)
         ("M-o"           . crux-smart-open-line)
         ("C-c n"         . crux-cleanup-buffer-or-region)
         ("C-c f"         . crux-recentf-find-file)
         ("C-M-z"         . crux-indent-defun)
         ("C-c e"         . crux-eval-and-replace)
         ("C-c w"         . crux-swap-windows)
         ("C-c D"         . crux-delete-file-and-buffer)
         ("C-c r"         . crux-rename-buffer-and-file)
         ("C-c TAB"       . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I"         . crux-find-user-init-file)
         ("s-r"           . crux-recentf-find-file)
         ("s-j"           . crux-top-join-line)
         ("C-^"           . crux-top-join-line)
         ("s-k"           . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o"           . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package diff-hl
  :ensure t
  :disabled
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.4)
  (which-key-mode +1))

(use-package cheatsheet
  :ensure t
  :disabled
  :bind
  (:map cheatsheet-mode-map
        ("q" . kill-buffer-and-window))
  :config
  (progn
    (cheatsheet-add :group 'Common
                    :key "C-z"
                    :description "Open shorties")))

(use-package discover-my-major
  :ensure t
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-set-key (kbd "C-/") #'undo-tree-undo)
  (global-set-key (kbd "C-?") #'undo-tree-redo)
  (global-set-key (kbd "C-c u") #'undo-tree-visualize)
  (global-undo-tree-mode))

(use-package prescient
  :ensure t)

(use-package ivy-prescient
  :ensure t
  :after (prescient ivy)
  :config
  (ivy-prescient-mode))

(defun asm/ivy-sort-by-length (_name candidates)
  (cl-sort (copy-sequence candidates)
           (lambda (f1 f2)
             (< (length f1) (length f2)))))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :demand
  :config
  (setq ivy-count-format ""
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-sort-matches-functions-alist '((t)
                                           (counsel-find-file . asm/ivy-sort-by-length)
                                           (projectile-completing-read . asm/ivy-sort-by-length)
                                           )
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t
        ivy-format-function 'ivy-format-function-arrow)
  (set-face-attribute 'ivy-current-match nil :foreground "#242832")
  (ivy-mode 1)
  :bind
  ("C-c C-r" . ivy-resume))

(use-package swiper
  :ensure t
  :bind
  ("C-s"   . swiper)
  ("C-r"   . swiper)
  ("C-S-s" . isearch-forward)
  ("C-S-r" . isearch-backwards))

(defun asm/contextual-switch-buffer ()
  "Switch to projectile buffers if in a counsel project,
  otherwise do a normal `counsel-switch-buffer'."
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-switch-to-buffer)
    (counsel-switch-buffer)))

(use-package counsel
  :ensure t
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x b"   . asm/contextual-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-c i"   . counsel-imenu)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)
   :map counsel-find-file-map
   ("C-l" . ivy-backward-delete-char)))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") #'ace-window)
  (global-set-key [remap other-window] #'ace-window))

(use-package neotree
  :ensure t
  :defer t
  :commands (neotree-toggle)
  :bind (("C-c t" . neotree-toggle))
  :init
  (progn
    (setq neo-smart-open t
          neo-dont-be-alone t)
    (add-hook 'neotree-mode-hook
              (lambda ()
                (setq-local mode-line-format nil)
                (local-set-key (kbd "C-s") #'isearch-forward)
                (local-set-key (kbd "C-r") #'isearch-backward)))))

; TODO: this is poorly configured, it's annoying to fight it when a
; buffer accidentally crosses project boundaries.
(use-package perspective
  :ensure t
  :config
  (persp-mode)

  ;; switch to perspective when exiting ibuffer
  (defun asm/persp-ibuffer-visit-buffer ()
    (interactive)
    (let ((buf (ibuffer-current-buffer t))
          (persp-name (string-remove-prefix "Projectile:" (get-text-property
                       (line-beginning-position) 'ibuffer-filter-group))))
      (persp-switch persp-name)
      (switch-to-buffer buf)))

  (define-key ibuffer-mode-map (kbd "RET") 'asm/persp-ibuffer-visit-buffer))

(use-package persp-projectile
  :ensure t
  :after (perspective)
  :bind
  ("C-c x" . hydra-persp/body)
  :config
  (defhydra hydra-persp (:columns 4
                         :color blue)
    "Perspective"
    ("a" persp-add-buffer "Add Buffer")
    ("i" persp-import "Import")
    ("c" persp-kill "Close")
    ("n" persp-next "Next")
    ("p" persp-prev "Prev")
    ("k" persp-remove-buffer "Kill Buffer")
    ("r" persp-rename "Rename")
    ("A" persp-set-buffer "Set Buffer")
    ("s" persp-switch "Switch")
    ("C-x" persp-switch-last "Switch Last")
    ("b" persp-switch-to-buffer "Switch to Buffer")
    ("P" projectile-persp-switch-project "Switch Project")
    ("q" nil "Quit")))

(use-package zoom
  :disabled
  :init
  (zoom-mode +1)
  :config
  (setq zoom-size '(0.618 . 0.618)))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode +1)

  (defadvice kill-region (before smart-cut activate compile)
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2))))))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :disabled
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 100) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (setq sp-highlight-pair-overlay nil)
  (smartparens-global-mode t)
  (require 'smartparens-config)
  :config
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-pair "'" nil :unless '(sp-point-after-word-p))
  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-a" . sp-backward-down-sexp)
   ("C-S-d" . sp-beginning-of-sexp)
   ("C-S-a" . sp-end-of-sexp)
   ("C-M-e" . sp-up-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s"   . sp-splice-sexp)
   ("M-r"   . sp-splice-sexp-killing-around)
   ("C-)"   . sp-forward-slurp-sexp)
   ("C-}"   . sp-forward-barf-sexp)
   ("C-("   . sp-backward-slurp-sexp)
   ("C-{"   . sp-backward-barf-sexp)
   ("M-S"   . sp-split-sexp)
   ("M-J"   . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

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

;; LSP
(use-package eglot
  :ensure t
  :disabled
  :config
  (setq eglot-ignored-server-capabilites '(:documentHighlightProvider)
        eglot--mode-line-format "LSP"
        eglot-workspace-configuration '((pyls.configurationSources . ["flake8"])))
  :hook ((python-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c h" . eglot-help-at-point)))

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
                                                 "~/.pyenv/shims/mypy")
                flycheck-flake8rc ".flake8"))

(use-package pyenv-mode-auto
  :ensure t)

(use-package pipenv
  :ensure t)

(defun asm/python-mode-hook ()
  ;; use flat imenu
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (subword-mode +1)
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'company-backends)
       '(company-jedi
         company-anaconda)))

(use-package python
  :mode ("\\.py'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq-default python-fill-docstring-style 'django)
  (add-hook 'python-mode-hook 'asm/python-mode-hook)
  (unbind-key "C-c C-j" python-mode-map))

(use-package jedi
  :ensure t)

(use-package company-jedi
  :ensure t
  :after (company jedi)
  :config
  (setq-default company-jedi-python-bin "~/.pyenv/shims/python"))

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode))

(use-package blacken
  :ensure t
  :hook
  ((python-mode . blacken-mode))
  :config
  (setq blacken-executable "~/.pyenv/shims/black")
  (define-key python-mode-map (kbd "C-c C-b") 'blacken-buffer))

(use-package py-isort
  :ensure t
  :config
  (setq py-isort-options '("--lines=100" "--multi-line=3" "--trailing-comma")))

(defun asm/toggle-isort ()
  "Toggle isort before-save-hook."
  (interactive)
  (if (member 'py-isort-before-save before-save-hook)
      (progn
        (remove-hook 'before-save-hook 'py-isort-before-save)
        (message "isort disabled"))
    (progn
      (add-hook 'before-save-hook 'py-isort-before-save)
      (message "isort enabled"))))

(use-package flycheck-mypy
  :ensure t)

;; TODO: ein-poly fucks all kinds of shit up, known working version is
;; ein-20190611.1229. The new version seems to fuck around with
;; smartparens and other important modes.
(use-package ein
  :ensure t
  :commands (ein:login)
  :init
  (setq ein:complete-on-dot -1
        ein:completion-backend 'ein:use-none-backend
        ein:query-timeout 1000
        ein:default-url-or-port "http://localhost:8888"
        ein:worksheet-enable-undo 'full
        ein:notebook-modes '(ein:notebook-python-mode ein:notebook-plain-mode))
  :config
  (cond
   ((eq system-type 'darwin)
    (setq-default ein:console-args
                  '("--gui=osx" "--matplotlib=osx" "--colors=Linux")))
   ((eq system-type 'gnu/linux)
    (setq-default ein:console-args
                  '("--gui=gtk3" "--matplotlib=gtk3" "--colors=Linux"))))

  ;; Not sure if this is necessary - seems to make ein work on OS X.
  (setq-default request--curl-cookie-jar (concat user-emacs-directory
                                                 "request/curl-cookie-jar"))

  (add-hook 'ein:notebook-mode-hook
            (lambda ()
              (visual-line-mode +1)
              (whitespace-mode -1)
              (company-mode nil)
              (flycheck-mode nil)
              (undo-tree-mode nil)
              ;; (bind-key "C-/" 'undo-tree-undo)
              (bind-key "C-a" 'crux-move-beginning-of-line)
              )))

;; golang
(use-package go-projectile
  :ensure t)

(use-package go-mode
  :ensure t
  :defer t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (add-hook 'go-mode-hook 'electric-pair-mode)
  (defun asm/go-mode-hook ()
    ;; call gofmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-to-list 'exec-path "~/proj/go/bin")
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go vet"))
    (local-set-key (kbd "C-c C-c") 'compile)
    (set (make-local-variable 'company-backends) '(company-go))

    (setenv "GOPATH" (expand-file-name "~/proj/go")))

  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'asm/go-mode-hook)
  :bind
  (:map go-mode-map
        ("C-c g a" . go-imports-insert-import)
        ("C-c g p" . go-direx-pop-to-buffer)
        ("C-c g b" . go-direx-switch-to-buffer)
        ("C-c g i" . go-impl)
        ("C-c g f" . go-fill-struct)
        ("C-c g r" . go-rename)
        ("C-c g l" . go-imports-reload-packages-list)
        ("C-c g t" . go-tag-add)
        ("C-c g v" . go-tag-remove)
        ("C-c t g" . go-gen-test-dwim)
        ("C-c t a" . go-gen-test-all)
        ("C-c t e" . go-gen-test-exported)
        ("C-c t f" . go-test-current-file)
        ("C-c t t" . go-test-current-test)
        ("C-c t p" . go-test-current-project)
        ("C-c t b" . go-test-current-benchmark)
        ("C-c t x" . go-run)))

(use-package company-go
  :ensure t
  :after go
  :config
  (setq tab-width 4)
  (setq company-go-gocode-command (expand-file-name "~/proj/go/bin/gocode"))
  ;; (setq company-go-insert-arguments -1)
  (setq company-go-show-annotation t)
  :bind (:map go-mode-map
              ("M-." . godef-jump)))

(use-package go-eldoc
  :ensure t
  :after go
  :hook
  (go-mode . go-eldoc-setup))

(use-package go-guru
  :ensure t
  :after go
  :hook
  (go-mode . go-guru-hl-identifier-mode))

(use-package gorepl-mode
  :ensure t
  :after go
  :hook
  (go-mode . gorepl-mode))

;; rust
(use-package rust-mode
  :ensure t
  :defer t
  :mode "\\.rs$"
  :config
  (setq rust-format-on-save t))

(use-package racer
  :ensure t
  :after rust-mode
  :config
  (setq racer-rust-src-path (expand-file-name "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
        racer-cmd (expand-file-name "~/.cargo/bin/racer")))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'electric-pair-mode)

;; misc languages
(use-package fish-mode
  :ensure t
  :defer t
  :mode "\\.fish$")

(use-package counsel-jq
  :ensure t
  :defer 0.4)

(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json$"
  :bind
  (:map json-mode-map
        ("C-c C-b" . json-pretty-print-buffer)
        ("C-c C-j" . counsel-jq))
  :init
  (setq js-indent-level 2))


(use-package yaml-mode
  :ensure t
  :defer t
  :mode "\\.yaml$")

(use-package toml-mode
  :ensure t
  :defer t
  :mode (("\\.toml$" . toml-mode)
         ("Pipfile$" . toml-mode)))

(use-package web-mode
  :ensure t
  :defer t
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
  :defer t
  :mode "\\.js$"
  :config
  (setq-default js2-basic-indent 4
                js2-basic-offset 4
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "setTimeout" "clearTimeout" "setInterval"
                                         "clearInterval" "location" "console" "JSON"
                                         "jQuery" "$"))

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(use-package coffee-mode
  :ensure t
  :disabled
  :mode
  ("\\.coffee\\'" . coffee-mode)
  :init
  (setq coffee-tab-width 2))


(use-package typescript-mode
  :ensure t
  :mode
  ("\\.ts\\'" . typescript-mode)
  :init
  (setq typescript-indent-level 2))

(use-package terraform-mode
  :ensure t
  :mode "\\.tf$"
  :hook
  (terraform-mode . company-mode)
  :config
  (set-face-foreground terraform--resource-name-face "#B58DAE")
  (set-face-foreground terraform--resource-type-face "#B58DAE"))

(use-package company-terraform
  :ensure t)

(defun asm/terraform-mode-hook ()
  (subword-mode +1)
  (terraform-format-on-save-mode t)
  (set (make-local-variable 'company-backends)
       '(company-terraform)))
(add-hook 'terraform-mode-hook #'asm/terraform-mode-hook)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package nginx-mode
  :ensure t
  :defer t
  :init (setq nginx-indent-level 2))

(use-package company-nginx
  :ensure t
  :after (company))

(use-package crontab-mode
  :ensure t
  :mode "crontab.*")

(use-package jinja2-mode
  :ensure t
  :mode ((".*\\.jinja" . jinja2-mode)
         (".*\\.jinja2" . jinja2-mode)))

(use-package clojure-mode
  :ensure t)

(use-package restclient
  :ensure t
  :defer t
  :commands (restclient-mode)
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode))

(defun asm/open-init-file ()
  (interactive)
  (projectile-persp-switch-project "~/proj/emacs.d")
  (find-file (expand-file-name "~/proj/emacs.d/init.el")))

(defun asm/empty-buffer ()
  (interactive)
  (command-execute 'asm/split-window-horizontally)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    buf))

(defun asm/org-open-file ()
  (interactive)
  (let ((file-to-open
         (read-file-name
          "Open org file: "
          (expand-file-name "~/org/"))))
    (find-file file-to-open)))

(defun asm/yank-filename ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                           default-directory
                         (replace-regexp-in-string ".*/proj/" "" (buffer-file-name)))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(global-set-key
 (kbd "C-z")
 (defhydra ctrl-z-hydra (:color blue
                         :columns 4)
   "Shorties"
   ("b" asm/empty-buffer "empty buffer")
   ("e" flycheck-list-errors "list errors")
   ("f" asm/yank-filename "yank filename")
   ("i" asm/open-init-file "open init")
   ("l" counsel-bookmark "bookmarks")
   ("n" ein:login "EIN")
   ("o" asm/org-open-file "find org file")
   ("p" projectile-persp-switch-project "open project")
   ("r" anzu-query-replace-regexp "regex replace")
   ("s" counsel-rg "ripgrep")
   ("w" ace-window "ace window")
   ("C-s" deadgrep "deadgrep")
   ("q" nil "quit")))

;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
