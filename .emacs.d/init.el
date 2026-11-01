;;; init.el --- Vinod's .emacs.d -*- lexical-binding: t; -*-

;; Author: Vinod Kurup <vinod@kurup.com>
;; URL: https://github.com/vkurup/dotfiles

;;; Commentary:
;; I started with the Emacs starter kit (from technomancy), and then piled on things
;; over the decades. Lots stolen from
;; https://github.com/bbatsov/emacs.d/blob/master/init.el

;;; Code:

;;; Package management

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

(use-package diminish
  :ensure t
  :config
  (diminish 'flyspell-mode)
  (diminish 'flyspell-prog-mode)
  (diminish 'eldoc-mode))

;;; Environment (PATH, direnv)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

;;; Personal info

;; vinod customizations
(setq user-full-name "Vinod Kurup"
      user-mail-address "vinod@kurup.com")

;;; General settings

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB).
;; lsp-mode recommends this much.
(setq gc-cons-threshold (* 100 1024 1024))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defconst vkurup-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p vkurup-savefile-dir)
  (make-directory vkurup-savefile-dir))

;; store autosave files in the tmp dir (backups are off, see backup-inhibited)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(setq revert-without-query '(".*"))

(setq create-lockfiles nil
      inhibit-startup-screen t
      initial-scratch-message nil
      indicate-empty-lines nil
      cursor-in-non-selected-windows nil
      sentence-end-double-space nil
      shift-select-mode nil
      backup-inhibited t
      kill-whole-line t        ; ctrl-k kills whole line if at col 0
      diff-switches "-u")

(put 'narrow-to-region 'disabled nil)

;;; Appearance

(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)

;; maximize the initial frame automatically
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; frame: font, no scroll/tool/menu bars, padding around the edges
(setq default-frame-alist
      (append (list
               (cons 'font (if (eq system-type 'darwin)
                               "Roboto Mono:style=Regular:size=20"
                             "DejaVu Sans Mono:style=Regular:size=20"))
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))
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

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; display line numbers https://www.emacswiki.org/emacs/LineNumbers#h5o-1
(global-display-line-numbers-mode)

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package poet-theme
  :ensure t
  :config
  (load-theme 'poet t)
  ;; Proportional font for prose only. Lots of non-prose modes (yaml,
  ;; ...) derive from text-mode, so list the prose ones explicitly.
  ;; Plain text-mode covers magit commit messages too.
  (defun vk-variable-pitch-for-prose ()
    "Turn on `variable-pitch-mode' in prose buffers."
    (when (or (eq major-mode 'text-mode)
              (derived-mode-p 'org-mode 'markdown-mode 'adoc-mode))
      (variable-pitch-mode 1)))
  (add-hook 'text-mode-hook #'vk-variable-pitch-for-prose))

;; show image dimensions in modeline
;; http://emacs.stackexchange.com/a/7693/289
(defun show-image-dimensions-in-mode-line ()
  (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
         (width (car image-dimensions))
         (height (cdr image-dimensions)))
    (setq mode-line-buffer-identification
          (format "%s %dx%d" (propertized-buffer-identification "%12b") width height))))
(add-hook 'image-mode-hook #'show-image-dimensions-in-mode-line)

;;; Global keybindings

;; macOS modifier keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-o") 'other-window)
;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

(global-set-key [(control x) (control r)] 'crux-sudo-edit)

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1)
  (diminish 'which-key-mode))

;;; Projects and search

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/src/" "~/dev/"))
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching t)
  (projectile-mode +1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules" )
  (add-to-list 'projectile-globally-ignored-directories "submodules"))

;; rg's default bindings put rg-menu on C-c s, but crux (in Editing,
;; below) takes that key, so this section has to come first.
(use-package rg
  :ensure t
  :init
  (rg-enable-default-bindings))

;;; Editing

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance
(setq js-indent-level 4)
(setq sh-basic-offset 2)

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 88 characters
(setq-default fill-column 88)

;; auto-fill in org and text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; delete the selection with a keypress
(delete-selection-mode t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

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

(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package whitespace
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 120) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1)
  (diminish 'undo-tree-mode))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1)
  (diminish 'volatile-highlights-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  (diminish 'rainbow-mode))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :init
  (global-anzu-mode +1))

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-char-2))
  :config
  (setq avy-background t))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("C-c n" . crux-cleanup-buffer-or-region) ; indent, untabify, trim whitespace
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
         ("C-c s" . crux-ispell-word-then-abbrev))
  :config
  ;; C-c n with no region cleans up the whole buffer
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify))

(defun esk-insert-date ()
  "Insert a 'time-stamp' according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;;; Spelling and syntax checking

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

;;; Completion

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

(use-package yasnippet
  :ensure t)

;;; Files, buffers and windows

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

(use-package super-save
  :ensure t
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1)
  (diminish 'super-save-mode))

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

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package windmove
  :config
  ;; switch between visible buffers: super+arrows on Mac, shift+arrows
  ;; elsewhere (GNOME grabs super+arrows)
  (windmove-default-keybindings (if (eq system-type 'darwin) 'super 'shift)))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

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

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

;;; Version control

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

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; Terminals and LLM agents

(use-package vterm
  :ensure t)

;; LLM agents in an Emacs buffer via the Agent Client Protocol (ACP).
;; Needs the adapter on PATH (npm i -g @agentclientprotocol/claude-agent-acp)
;; and a one-time `claude' login outside Emacs. See NOTES.org.
(use-package agent-shell
  :ensure t
  :bind (("C-c A" . agent-shell))
  :config
  ;; use the Claude Code subscription login, not an API key
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  ;; inherit Emacs's environment so asdf/mise shims, direnv etc. are on PATH
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))
  ;; start in Auto mode, like the claude CLI: Claude decides which actions
  ;; are safe and only asks about risky ones (the default, "Manual", asks
  ;; before every change). Switch per session with C-c C-m.
  (setq agent-shell-anthropic-default-session-mode-id "auto")
  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config)))

;;; Programming languages

;;;; Emacs Lisp

(use-package rainbow-delimiters
  :ensure t)

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

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode))
  (diminish 'elisp-slime-nav-mode))

(use-package paredit
  :ensure t
  :hook
  ((clojure-mode . paredit-mode))
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (diminish 'paredit-mode "()"))

(use-package cask-mode
  :ensure t)

;;;; Clojure

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
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (require 'flycheck-clj-kondo))

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'lsp)
  (add-hook 'clojurec-mode-hook 'lsp)
  (add-hook 'clojurescript-mode-hook 'lsp)
  (setq read-process-output-max (* 1024 1024))
  (setq treemacs-space-between-root-nodes nil)
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-completion-enable nil)
  (setq lsp-clojure-custom-server-command
        (list "bash" "-c" (or (executable-find "clojure-lsp") "clojure-lsp")))
  ;; don't watch these (on top of lsp-mode's defaults)
  (dolist (dir '("[/\\\\]screenshots\\'" "[/\\\\]logs\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories dir)))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;;;; Python

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

;;;; Ruby

(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :ensure t
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode))

;;;; Elixir and Haskell

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode-hook #'subword-mode))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'subword-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'haskell-doc-mode))

;;;; Web, JavaScript and TypeScript

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
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode)))

(use-package coffee-mode
  :ensure t)

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
;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'web-mode-hook
          (lambda ()
            (when (member (file-name-extension (or buffer-file-name ""))
                          '("ts" "tsx"))
              (setup-tide-mode))))
;; enable typescript - tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;;;; Data formats and HTTP

(use-package yaml-mode
  :ensure t)

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

;;; Writing (Markdown, AsciiDoc)

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

;;; Local files: lib/, per-machine and per-user config

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

;;; Org mode

;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/gtd.org" "~/org/trips.org"))
(setq org-capture-templates
      '(("t" "Todo [inbox]" entry
         (file+headline "~/org/gtd.org" "Autofocus")
         "* TODO %i%?\n%a\n%T")))
(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/tickler.org" :maxlevel . 2)))
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(p)" "|" "DONE(d)" "CANCELLED(c)" "WAITING(w)")))
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

;;; Finance (beancount)

;; custom copied https://github.com/beancount/beancount-mode
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
(define-key beancount-mode-map (kbd "C-c C-k") 'vk-beancount-copy-transaction-at-point)

(defun vk-beancount-copy-transaction-at-point ()
  "Copy the transaction at point and paste it to end of file."
  (interactive)
  (beancount-goto-transaction-begin)
  (let ((pt (point)))
    (forward-line)
    (while (not (looking-at-p "[[:blank:]]*$"))
      (forward-line))
    (copy-region-as-kill pt (point)))
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

(defun vk-guideline-pdf-to-csv ()
  "Convert line from Guideline PDF statement to beancount transaction.

Copy PDF into transactions.beancount buffer. Make lines look like:
12/27 Sell VTABX 0.010 $22.12 -$0.09 - -$0.13 -$0.22
Adjust company and year in this function, if needed.

Action can be Buy, Sell, or Reinvest"
  (interactive)
  (let ((company "Kevel")
        (year "2025"))
    (beginning-of-line)
    (insert year "-")
    (forward-char 2)

    ;; replace "/" with "-"
    (insert "-")
    (delete-forward-char 1)

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

;;; Demos

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

;;; Server and Custom

;; emacsclient
(server-start)

;; Settings saved from M-x customize (and package-selected-packages, which
;; changes on every install) go in custom.el. It's git-ignored because it's
;; different on each machine; put settings I want everywhere in this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;;; init.el ends here
