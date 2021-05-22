;; -*- lexical-binding: t -*-
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(anzu
                      avy
                      cider
                      clojure-mode
                      clojure-mode-extra-font-locking
                      company
                      counsel
                      direnv
                      dockerfile-mode
                      elixir-mode
                      elixir-yasnippets
                      elpy
                      exec-path-from-shell
                      flycheck
                      ivy
                      js2-mode
                      json-mode
                      lsp-mode
                      magit
                      markdown-mode
                      org
                      paredit
                      pyenv-mode
                      projectile
                      rainbow-delimiters
                      rainbow-mode
                      smex
                      tagedit
                      tide
                      use-package
                      web-mode
                      yaml-mode
                      zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

; https://github.com/abo-abo/avy
(global-set-key (kbd "C-:") 'avy-goto-char-2)

; https://github.com/justbur/emacs-which-key
(which-key-mode)

; https://github.com/sabof/org-bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-undo-tree-mode)

;; macOS modifier keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; reload file if it changed on disk
(global-auto-revert-mode t)

;; Display the column number.
(column-number-mode t)

;; ESK things
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "") 'isearch-backward-regexp)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-o") 'other-window)
;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(progn
  ;; Turn off mouse interface early in startup to avoid momentary display
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

  ;; You can keep system- or user-specific customizations here
  (setq esk-system-config (concat user-emacs-directory system-name ".el")
        esk-user-config (concat user-emacs-directory user-login-name ".el")
        esk-user-dir (concat user-emacs-directory user-login-name))

  (add-to-list 'load-path esk-user-dir)

  (setq shared-lib-dir (concat user-emacs-directory "lib"))
  (add-to-list 'load-path shared-lib-dir)

  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)

  (when (file-exists-p esk-system-config) (load esk-system-config))
  (when (file-exists-p esk-user-config) (load esk-user-config))
  (when (file-exists-p esk-user-dir)
    (mapc 'load (directory-files esk-user-dir nil "^[^#].*el$"))))

;; clojure for the brave and true
;; No need for ~ files when editing
(setq create-lockfiles nil)
;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(load "setup-clojure.el")
(load "setup-js.el")

;; direnv
(direnv-mode)

;; ivy/swiper/counsel
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))
(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))
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
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; tried putting this in ivy config, but it got overridden here, so...
  (setq ivy-initial-inputs-alist nil)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq visible-bell t
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      indicate-empty-lines nil
      cursor-in-non-selected-windows nil
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      backup-inhibited t
      diff-switches "-u")

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
;; Highlight current line
(global-hl-line-mode 1)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(random t) ;; Seed the random-number generator

(defun esk-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun esk-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun esk-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (esk-indent-buffer)
  (esk-untabify-buffer)
  (delete-trailing-whitespace))

(defun esk-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun esk-lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun esk-insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(yas-global-mode 1)
(global-anzu-mode +1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (remove-hook 'before-save-hook 'delete-trailing-whitespace)

;; projectile settings
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-enable-caching t)

;; work with ubuntu clipboard
(setq interprogram-paste-function 'x-selection-value)

;; map RET to newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :bind (:map elpy-mode-map
              ("<M-left>" . nil)
              ("<M-right>" . nil)
              ("<M-S-left>" . elpy-nav-indent-shift-left)
              ("<M-S-right>" . elpy-nav-indent-shift-right)
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark))
  :config
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python3"))

;; elixir
;; https://elixirforum.com/t/emacs-elixir-setup-configuration-wiki/19196
;; using lsp-mode
(use-package lsp-mode
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  :init
  (add-to-list 'exec-path "~/dev/elixir-ls/release"))

(use-package company-lsp
  :bind
  ("M-SPC" . company-complete)
  :config
  (push 'company-lsp company-backends)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; ruby
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\|Vagrantfile\\)\\'" . ruby-mode))

;; scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[px]$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))


;; js2
(add-hook 'js2-mode-hook 'flycheck-mode)
(require 'tide)
(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;; https://emacs.stackexchange.com/a/33544/289
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround sgml-mode and follow airbnb component style."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))


(add-hook 'after-init-hook 'global-company-mode)

;; http://www.flycheck.org/manual/latest/index.html
(global-flycheck-mode t)

;; disable jshint since we prefer eslint checking
;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint
                        json-jsonlist)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
;; Inherit environment variables from Shell.
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("PATH"))))

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; make moving around windows easier
(require 'windmove)
(windmove-default-keybindings)
(setq org-replace-disputed-keys t)

(add-hook 'css-mode-hook 'rainbow-mode)
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))

;; For running tests easily
(global-set-key [f6] 'recompile)

;; beancount
;; custom copied https://github.com/beancount/beancount-mode
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

;; org mode
;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/inbox.org"
                         "~/org/gtd.org"
                         "~/org/tickler.org"))
(setq org-capture-templates
      '(("t" "Todo [inbox]" entry
         (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %i%?\n  %a")
        ("T" "Tickler" entry
         (file+headline "~/org/tickler.org" "Tickler")
         "* %i%? \n %U")))
(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/tickler.org" :maxlevel . 2)))
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(define-key global-map [f8] (lambda () (interactive) (org-capture nil "t")))
;; ;;(define-key global-map [f9] 'remember-region)
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

(defun vk-slugify (title)
  "Convert a normal TITLE string to something that can be used in a blog slug."
  (replace-regexp-in-string "[\\., ]+" "-"
                            (replace-regexp-in-string "['\?]" ""
                                                      (downcase title))))

(defun vk-blogpost (title)
  "Create a new blog post titled TITLE."
  (interactive "sPost Title: ")
  (let ((slug (vk-slugify title)))
    (find-file (concat "~/dev/kurup.org/content/post/"
                       (format-time-string "%Y-%m-%d")
                       "-" slug ".markdown"))
    (insert "---\n")
    (insert "date: " (format-time-string "%Y-%m-%dT%H:%M:%S") "\n")
    (insert "title: " title "\n")
    (insert "categories: \n")
    (insert "---\n\n")))

;; vinod customizations
(setq user-mail-address "vinod@kurup.com")
;; set calendar's location (for sunrise sunset)
(setq calendar-latitude 35.9162)
(setq calendar-longitude -79.0999)
(setq calendar-location-name "Chapel Hill, NC")

(setq-default kill-whole-line t)        ; ctrl-k kills whole line if at col 0

;; Delete old backup versions silently
(setq delete-old-versions t)

;; (load-theme 'adwaita t)
(load-theme 'zenburn t)
;; (load-theme 'wombat t)

;; nano-emacs https://github.com/rougier/nano-emacs
(require 'nano-layout)
(require 'nano-faces)
(nano-faces)
(require 'nano-modeline)

(global-set-key [(control x) (control r)] 'esk-sudo-edit)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

(setq-default default-tab-width 4
              indent-tabs-mode nil)
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;; erc
(require 'secrets)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#tripython" "#emacs")))

;; (progn
;;   (erc
;;    :server "irc.freenode.net"
;;    :port 6667
;;    :nick "vkurup"
;;    :password erc-freenode-password))

;; http://emacsredux.com/blog/2013/03/29/terminal-at-your-fingertips/
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))
(global-set-key (kbd "C-c t") 'visit-term-buffer)

;; http://emacsredux.com/blog/2013/03/29/automatic-electric-indentation/
(electric-indent-mode 0)

;; use flycheck instead of flymake
;; https://github.com/jorgenschaefer/elpy/issues/304
(when (require 'flycheck nil t)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; show image dimensions in modeline
;; http://emacs.stackexchange.com/a/7693/289
(defun show-image-dimensions-in-mode-line ()
  (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
         (width (car image-dimensions))
         (height (cdr image-dimensions)))
    (setq mode-line-buffer-identification
          (format "%s %dx%d" (propertized-buffer-identification "%12b") width height))))

(add-hook 'image-mode-hook #'show-image-dimensions-in-mode-line)

(define-coding-system-alias 'UTF-8 'utf-8)

;; emacsclient
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(compilation-always-kill t)
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("de538b2d1282b23ca41ac5d8b69c033b911521fe27b5e1f59783d2eb20384e1f" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "f61972772958e166cda8aaf0eba700aad4faa0b4101cee319e894e7a747645c9" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults))
 '(elpy-test-runner 'elpy-test-django-runner)
 '(erc-autojoin-mode t)
 '(erc-enable-logging 'erc-log-all-but-server-buffers)
 '(erc-log-channels-directory "~/.erc/logs")
 '(erc-log-insert-log-on-open t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-track-position-in-mode-line t)
 '(fci-rule-color "#383838")
 '(fill-column 88)
 '(js-indent-level 2)
 '(js2-auto-indent-p t)
 '(js2-enter-indents-newline t)
 '(js2-strict-missing-semi-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(magit-pull-arguments nil)
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   '(cider undo-tree org-bullets which-key try elixir-yasnippets elixir-mode rjsx-mode dockerfile-mode dash-functional lsp-mode counsel less-css-mode forge tide pyenv-mode avy use-package json-mode zenburn-theme yaml-mode web-mode smex rainbow-mode projectile markdown-mode js2-mode flycheck erc-hl-nicks elpy elfeed anzu))
 '(python-check-command "flake8")
 '(rst-compile-toolsets
   '((html "rst2html.py" ".html" nil)
     (latex "rst2latex.py" ".tex" nil)
     (newlatex "rst2newlatex" ".tex" nil)
     (pseudoxml "rst2pseudoxml.py" ".xml" nil)
     (xml "rst2xml.py" ".xml" nil)
     (pdf "rst2pdf" ".pdf" nil)
     (s5 "rst2s5.py" ".html" nil)))
 '(safe-local-variable-values '((use-inf-clojure . t) (inf-clojure-buffer . "geir-repl")))
 '(temporary-file-directory (concat user-emacs-directory "tmp"))
 '(web-mode-auto-close-style 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-style-padding 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq revert-without-query '(".*"))
;; "Take text formatted like '0411 VTIAX 1.322 23.48' and turn it into a transaction"
(fset 'vk-guideline-buy
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 50 48 50 48 45 right right 45 right right 32 116 120 110 right 67108896 134217830 134217847 134217826 34 66 85 89 32 25 right 13 65 115 115 101 116 115 58 73 110 118 101 115 116 109 101 110 116 115 58 71 117 105 100 101 108 105 110 101 58 25 32 32 4 4 4 4 4 4 134217848 115 101 97 13 32 left 32 25 32 123 125 13 65 115 115 101 116 115 58 73 110 118 101 115 116 109 101 110 116 115 58 71 117 105 100 101 108 105 110 101 58 67 97 115 104 32 32 45 5 32 85 83 68 13 6 5] 0 "%d")) arg)))
