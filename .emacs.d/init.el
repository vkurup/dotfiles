;;; init.el --- Vinod's .emacs.d -*- lexical-binding: t; -*-

;; Author: Vinod Kurup <vinod@kurup.com>
;; URL: https://github.com/vkurup/dotfiles

;;; Commentary:
;; I started with the Emacs starter kit (from technomancy), and then piled on things
;; over the decades. Lots stolen from
;; https://github.com/bbatsov/emacs.d/blob/master/init.el

;;; Code:
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata if the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;; vinod customizations
(setq user-full-name "Vinod Kurup"
      user-mail-address "vinod@kurup.com")
;; set calendar's location (for sunrise sunset)
(setq calendar-latitude 35.9162
      calendar-longitude -79.0999
      calendar-location-name "Chapel Hill, NC")

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; auto-fill in org and text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; maximize the initial frame automatically
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; nano emacs style
(setq default-frame-alist
      (append (list
               '(font . "Roboto Mono:style=Regular:size=20")
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))
;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
;; Vertical window divider
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; display line numbers https://www.emacswiki.org/emacs/LineNumbers#h5o-1
(global-display-line-numbers-mode)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defconst vkurup-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p vkurup-savefile-dir)
  (make-directory vkurup-savefile-dir))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 88 characters
(setq-default fill-column 88)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;; built-in packages
(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

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
  (setq save-place-file (expand-file-name "saveplace" vkurup-savefile-dir))
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
        savehist-file (expand-file-name "savehist" vkurup-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" vkurup-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15)
  (recentf-mode +1))

(use-package windmove
  :config
  ;; use super + arrow keys to switch between visible buffers
  (windmove-default-keybindings 'super))

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

  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package rainbow-delimiters
  :ensure t)

(use-package coffee-mode)

(use-package lisp-mode
  :config
  (defun visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

;;; third-party packages
;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))
(use-package poet-theme
  :ensure t
  :config
  (load-theme 'poet t)
  (add-hook 'text-mode-hook
            (lambda () (variable-pitch-mode 1))))
;; (load-theme 'adwaita t)
;; (load-theme 'zenburn t)
;; (load-theme 'wombat t)
;; (load-theme 'misterioso t)

(use-package vterm
  :ensure t)

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package diminish
  :ensure t
  :config
  (diminish 'flyspell-mode)
  (diminish 'flyspell-prog-mode)
  (diminish 'eldoc-mode))

(use-package jq-mode
  :ensure t)

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ;; syntax highlighting works for .hurl
         ("\\.hurl\\'" . restclient-mode))
  :config
  (setq restclient-same-buffer-response t))

(use-package restclient-jq
  :ensure t)


(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-char-2))
  :config
  (setq avy-background t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq ediff-split-window-function (quote split-window-horizontally))
  (setq ediff-diff-options "-w"))

;; FIXME
(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

(use-package rg
  :ensure t
  :init
  (rg-enable-default-bindings))

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/src/" "~/dev/"))
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching t)
  (projectile-mode +1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules" )
  (add-to-list 'projectile-globally-ignored-directories "submodules"))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode))
  (diminish 'elisp-slime-nav-mode))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (diminish 'paredit-mode "()"))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :init
  (global-anzu-mode +1))

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

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  (diminish 'rainbow-mode))

(use-package whitespace
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 120) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :ensure t
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package flycheck-joker
  :ensure t)

(use-package clj-refactor
  :ensure t)
(use-package clojure-mode
  :ensure t
  :config
  (setq clojure-toplevel-inside-comment-form t)
  (setq clojure-indent-style 'align-arguments)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (require 'flycheck-clj-kondo)
  (clj-refactor-mode 1)
  (yas-minor-mode 1))

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'lsp)
  (add-hook 'clojurec-mode-hook 'lsp)
  (add-hook 'clojurescript-mode-hook 'lsp)
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 1024 1024))
  (setq treemacs-space-between-root-nodes nil)
  (setq company-minimum-prefix-length 1)
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-completion-enable nil)
  (setq lsp-clojure-custom-server-command '("bash" "-c" "/opt/homebrew/bin/clojure-lsp")))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode #'subword-mode))

(use-package erlang
  :ensure t
  :config
  (when (eq system-type 'windows-nt)
    (setq erlang-root-dir "C:/Program Files/erl7.2")
    (add-to-list 'exec-path "C:/Program Files/erl7.2/bin")))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'subword-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'haskell-doc-mode))

(use-package web-mode
  :ensure t
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :preface)

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

(use-package yaml-mode
  :ensure t)

(use-package cask-mode
  :ensure t)

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package selectrum
  :ensure t
  :bind
  ;; Make M-backspace delete more than a character
  (:map minibuffer-local-map
        ("M-<backspace>" . selectrum-backward-kill-sexp))
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode)
  (diminish 'company-mode))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package flyspell
  :ensure t
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eldev
  :ensure t)

(use-package super-save
  :ensure t
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1)
  (diminish 'super-save-mode))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1)
  (diminish 'which-key-mode))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1)
  (diminish 'undo-tree-mode))

;; (use-package ivy
;;   :ensure t
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   (global-set-key (kbd "<f6>") 'ivy-resume))
;;
;; (use-package swiper
;;   :ensure t
;;   :config
;;   (global-set-key "\C-s" 'swiper))
;;
;; (use-package counsel
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;   (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;   (global-set-key (kbd "C-c g") 'counsel-git)
;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   (global-set-key (kbd "C-c a") 'counsel-ag)
;;   (global-set-key (kbd "C-x l") 'counsel-locate)
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

;; super useful for demos
(use-package keycast
  :ensure t)

(use-package gif-screencast
  :ensure t
  :config
  ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
  (setq gif-screencast-args '("-x"))
  ;; Optional: Used to crop the capture to the Emacs frame.
  (setq gif-screencast-cropping-program "mogrify")
  ;; Optional: Required to crop captured images.
  (setq gif-screencast-capture-format "ppm"))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1)
  (diminish 'volatile-highlights-mode))

(use-package yasnippet
  :ensure t
  :config
  ;; (yas-global-mode)
  )

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :after yasnippet
;;   :config (yasnippet-snippets-initialize))

;; https://github.com/sabof/org-bullets
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

;; macOS modifier keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; ESK things
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-o") 'other-window)
;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; You can keep system- or user-specific customizations here
(setq esk-system-config (concat user-emacs-directory system-name ".el")
      esk-user-config (concat user-emacs-directory user-login-name ".el")
      esk-user-dir (concat user-emacs-directory user-login-name)
      shared-lib-dir (concat user-emacs-directory "lib"))
(when (file-exists-p esk-system-config) (load esk-system-config))
(when (file-exists-p esk-user-config) (load esk-user-config))
(add-to-list 'load-path shared-lib-dir)
(add-to-list 'load-path esk-user-dir)
(when (file-exists-p esk-user-dir)
  (mapc 'load (directory-files esk-user-dir nil "^[^#].*el$")))

(setq create-lockfiles nil
      visible-bell t
      inhibit-startup-screen t
      initial-scratch-message nil
      indicate-empty-lines nil
      cursor-in-non-selected-windows nil
      sentence-end-double-space nil
      shift-select-mode nil
      backup-inhibited t
      kill-whole-line t        ; ctrl-k kills whole line if at col 0
      diff-switches "-u")

(defun esk-untabify-buffer ()
  "Untabify the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun esk-indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun esk-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (esk-indent-buffer)
  (esk-untabify-buffer)
  (delete-trailing-whitespace))

(defun esk-sudo-edit (&optional arg)
  "Open a file ARG using sudo."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun esk-insert-date ()
  "Insert a 'time-stamp' according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (remove-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'cider-format-buffer t t)
;; (remove-hook 'before-save-hook 'cider-format-buffer)

;; ;; work with ubuntu clipboard
;; (setq interprogram-paste-function 'x-selection-value)

;; ;; map RET to newline-and-indent
;; (define-key global-map (kbd "RET") 'newline-and-indent)

;; elpy
(use-package elpy
  :ensure t
  :bind (:map elpy-mode-map
              ("<M-left>" . nil)
              ("<M-right>" . nil)
              ("<M-S-left>" . elpy-nav-indent-shift-left)
              ("<M-S-right>" . elpy-nav-indent-shift-right)
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark))
  :config
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python3")
  ;; use flycheck instead of flymake
  ;; https://github.com/jorgenschaefer/elpy/issues/304
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (elpy-enable))

;; ;; elixir
;; ;; https://elixirforum.com/t/emacs-elixir-setup-configuration-wiki/19196
;; ;; using lsp-mode
;; (use-package lsp-mode
;;   :ensure t
;;   :bind ("C-c h" . lsp-describe-thing-at-point)
;;   :commands lsp
;;   :diminish lsp-mode
;;   :hook
;;   (elixir-mode . lsp)
;;   :init
;;   (add-to-list 'exec-path "~/dev/elixir-ls/release"))

;; (use-package company-lsp
;;   :ensure t
;;   :bind
;;   ("M-SPC" . company-complete)
;;   :config
;;   (push 'company-lsp company-backends)
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 1))

(use-package tide :ensure t)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript - tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; ;; js2
;; (add-hook 'js2-mode-hook 'flycheck-mode)
;; (require 'tide)
;; (add-hook 'js2-mode-hook #'setup-tide-mode)
;; ;; configure javascript-tide checker to run after your default javascript checker
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;; ;; https://emacs.stackexchange.com/a/33544/289
;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
;; (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
;;   "Workaround sgml-mode and follow airbnb component style."
;;   (save-excursion
;;     (beginning-of-line)
;;     (if (looking-at-p "^ +\/?> *$")
;;         (delete-char sgml-basic-offset))))

;; ;; disable jshint since we prefer eslint checking
;; ;; disable json-jsonlist checking for json files
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(javascript-jshint
;;                         json-jsonlist)))

;; ;; use eslint with web-mode for jsx files
;; (flycheck-add-mode 'javascript-eslint 'web-mode)

;; ;; customize flycheck temp file prefix
;; (setq-default flycheck-temp-prefix ".flycheck")

;; ;; for better jsx syntax-highlighting in web-mode
;; ;; - courtesy of Patrick @halbtuerke
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))

;; ;; beancount
;; ;; custom copied https://github.com/beancount/beancount-mode
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
;; (add-hook 'beancount-mode-hook
;;           (lambda () (local-set-key (kbd "C-c C-k") 'vk-beancount-copy-transaction-at-point)))
(define-key beancount-mode-map (kbd "C-c C-k") 'vk-beancount-copy-transaction-at-point)
(use-package ledger-mode
  :ensure t)

(defun vk-beancount-copy-transaction-at-point ()
  "Copy the transaction at point and paste it to end of file."
  (interactive)
  (beancount-goto-transaction-begin)
  (setq pt (point))
  ;; (beancount-goto-transaction-end)
  (forward-line)
  (while (not (looking-at-p "[[:blank:]]*$"))
    (forward-line))
  (copy-region-as-kill pt (point))
  (goto-char (point-max))
  (insert "\n")
  (yank)
  (backward-char)
  (beancount-goto-transaction-begin)
  ;; replace date with current date
  (delete-char 10)
  (insert (format-time-string "%Y-%m-%d"))
  ;; replace * with txn
  (forward-char)
  (when (looking-at-p "*")
    (delete-char 1)
    (insert "txn")))

;; org mode
;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/gtd.org"))
(setq org-capture-templates
      '(("t" "Todo [inbox]" entry
         (file+headline "~/org/gtd.org" "Autofocus")
         "* TODO %i%?\n%a\n%T")))
(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/tickler.org" :maxlevel . 2)))
(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)" "WAITING(w)")))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(define-key global-map [f8] (lambda () (interactive) (org-capture nil "t")))
(setq org-log-done (quote time))
(setq org-agenda-show-log t)
(setq org-return-follows-link t)
(setq org-startup-indented t)
(setq org-agenda-start-on-weekday nil) ; show agenda starting today
(setq org-use-speed-commands t)
(setq org-archive-location (concat org-directory "archive/%s_archive::"))

(defun gtd ()
  "Open my todo list."
  (interactive)
  (find-file (concat org-directory "gtd.org")))

;; (defun vk-slugify (title)
;;   "Convert a normal TITLE string to something that can be used in a blog slug."
;;   (replace-regexp-in-string "[\\., ]+" "-"
;;                             (replace-regexp-in-string "['\?]" ""
;;                                                       (downcase title))))

;; (defun vk-blogpost (title)
;;   "Create a new blog post titled TITLE."
;;   (interactive "sPost Title: ")
;;   (let ((slug (vk-slugify title)))
;;     (find-file (concat "~/dev/kurup.org/content/post/"
;;                        (format-time-string "%Y-%m-%d")
;;                        "-" slug ".markdown"))
;;     (insert "---\n")
;;     (insert "date: " (format-time-string "%Y-%m-%dT%H:%M:%S") "\n")
;;     (insert "title: " title "\n")
;;     (insert "categories: \n")
;;     (insert "---\n\n")))


(global-set-key [(control x) (control r)] 'esk-sudo-edit)

;; erc
(require 'secrets)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#tripython" "#emacs")))

;; ;; (progn
;; ;;   (erc
;; ;;    :server "irc.freenode.net"
;; ;;    :port 6667
;; ;;    :nick "vkurup"
;; ;;    :password erc-freenode-password))

;; ;; http://emacsredux.com/blog/2013/03/29/terminal-at-your-fingertips/
;; (defun visit-term-buffer ()
;;   "Create or visit a terminal buffer."
;;   (interactive)
;;   (if (not (get-buffer "*ansi-term*"))
;;       (progn
;;         (split-window-sensibly (selected-window))
;;         (other-window 1)
;;         (ansi-term (getenv "SHELL")))
;;     (switch-to-buffer-other-window "*ansi-term*")))
;; (global-set-key (kbd "C-c t") 'visit-term-buffer)

;; show image dimensions in modeline
;; http://emacs.stackexchange.com/a/7693/289
(defun show-image-dimensions-in-mode-line ()
  (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
         (width (car image-dimensions))
         (height (cdr image-dimensions)))
    (setq mode-line-buffer-identification
          (format "%s %dx%d" (propertized-buffer-identification "%12b") width height))))
(add-hook 'image-mode-hook #'show-image-dimensions-in-mode-line)

;;; workaround until Emacs 29
;;; https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; emacsclient
(server-start)

(defun vk-guideline-pdf-to-csv ()
  "Convert line from Guideline PDF statement to beancount transaction."
  (interactive)
  (let ((company "Kevel")
        (year "2021"))
    (beginning-of-line)
    (insert year "-")
    (forward-char 2)
    (insert "-")
    (forward-char 3)
    (insert "txn \"")
    (let ((beg (point)))
      (forward-word)
      (let* ((end (point))
             (trans-type (buffer-substring beg end)))
        (forward-char)
        (let ((beg (point)))
          (forward-word)
          (let* ((end (point))
                 (symbol (buffer-substring beg end)))
            (insert "\"\n  Assets:Investments:Guideline:" company ":" symbol)
            (insert "  ")
            (forward-char)
            (if (string-equal (upcase trans-type) "SELL")
                (insert " -"))
            (forward-word 2)
            (insert " " symbol " {}\n  ")
            (insert "Assets:Investments:Guideline:" company ":Cash    ")))
        (let* ((beg (point)))
          (end-of-line)
          (search-backward " ")
          (delete-region beg (point)))
        (delete-char 2)
        (if (string-equal (upcase trans-type) "SELL")
            (delete-char 1)
          (insert "-"))
        (end-of-line)
        (insert " USD\n")
        (if (string-equal (upcase trans-type) "SELL")
            (insert "  Income:CapitalGains:NT\n"))
        (forward-char)))))

(setq revert-without-query '(".*"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-indent-style 'align-arguments)
 '(custom-safe-themes
   '("2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" default))
 '(js-indent-level 4)
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git\\'" "[/\\\\]\\.github\\'" "[/\\\\]\\.circleci\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.yarn\\'" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.shadow-cljs\\'" "[/\\\\]\\.babel_cache\\'" "[/\\\\]\\.cpcache\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'" "[/\\\\]_opam\\'" "[/\\\\]_build\\'" "[/\\\\]\\.direnv\\'" "[/\\\\]screenshots\\'" "[/\\\\]logs\\'"))
 '(package-selected-packages
   '(restclient-jq jq-mode tide coffee-mode forge popper lsp-mode zop-to-char yaml-mode which-key web-mode vterm volatile-highlights use-package undo-tree super-save smex selectrum-prescient rg restclient rainbow-mode rainbow-delimiters projectile poet-theme move-text markdown-mode marginalia magit ledger-mode keycast inf-ruby inf-clojure imenu-anywhere hl-todo haskell-mode git-timemachine gif-screencast flycheck-joker flycheck-eldev flycheck-clj-kondo expand-region exec-path-from-shell erlang elpy elixir-mode elisp-slime-nav easy-kill direnv diminish diff-hl crux clj-refactor cask-mode anzu adoc-mode ace-window))
 '(safe-local-variable-values '((use-inf-clojure . t) (inf-clojure-buffer . "geir-repl")))
 '(sh-basic-offset 2)
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
