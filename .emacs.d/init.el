(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      projectile
                      autopair
                      yasnippet
                      markdown-mode
                      pony-mode
                      fuzzy
                      haskell-mode
                      rainbow-mode
                      twittering-mode
                      auto-complete)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(autopair-global-mode)
(projectile-global-mode)
(yas-global-mode 1)

;; open a shell
(shell)

;; autocomplete
(require 'auto-complete-config)
(setq ac-dictionary-files (list (concat user-emacs-directory ".dict")))
(ac-config-default)
;; hack to fix ac-sources after pycomplete.el breaks it
(add-hook 'python-mode-hook
          '(lambda ()
             (setq ac-sources '(ac-source-pycomplete
                                ac-source-abbrev
                                ac-source-dictionary
                                ac-source-words-in-same-mode-buffers))))

;; python-mode
(setq py-install-directory (concat esk-user-dir "/python-mode/"))
(add-to-list 'load-path py-install-directory)
;; this will show method signatures while typing
(setq py-set-complete-keymap-p t)
(setq py-use-current-dir-when-execute-p t)
(require 'python-mode)
;; activate the virtualenv where Pymacs is located
(virtualenv-workon "vkinvest")

(defun load-pycomplete ()
  "Load and initialize pycomplete."
  (interactive)
  (let* ((pyshell (py-choose-shell))
         (path (getenv "PYTHONPATH")))
    (setenv "PYTHONPATH" (concat
                          (expand-file-name py-install-directory) "completion"
                          (if path (concat path-separator path))))
    (if (py-install-directory-check)
        (progn
          (setenv "PYMACS_PYTHON" (if (string-match "IP" pyshell)
                                      "python"
                                    pyshell))
          (autoload 'pymacs-apply "pymacs")
          (autoload 'pymacs-call "pymacs")
          (autoload 'pymacs-eval "pymacs")
          (autoload 'pymacs-exec "pymacs")
          (autoload 'pymacs-load "pymacs")
          (load (concat py-install-directory "completion/pycomplete.el") nil t)
          (add-hook 'python-mode-hook 'py-complete-initialize))
      (error "`py-install-directory' not set, see INSTALL"))))
(eval-after-load 'pymacs '(load-pycomplete))

;; pyflakes flymake integration
;; http://stackoverflow.com/a/1257306/347942
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook 'flymake-mode)

(add-hook 'css-mode-hook 'rainbow-mode)

;; ledger
;; make cleared items green, uncleared pink
(add-hook 'ledger-mode-hook
          (lambda ()
            (highlight-lines-matching-regexp "^..\\(..\\)?/..?/..?[        ]+[^\\*]" (quote hi-pink))
            (highlight-lines-matching-regexp "^..\\(..\\)?/..?/..?[        ]+\\*" (quote hi-green))))

(defun ledger ()
  "Open my ledger file and go to today"
  (interactive)
  (find-file "~/Dropbox/ledger.dat")
  (ledger-find-slot (current-time)))

(defun vk-copy-ledger-entry-to-bottom ()
  "Copy the current transaction to the bottom of the ledger"
  (interactive)
  (re-search-backward "^[12][09]")
  (let ((beg (point)))
    (forward-char)
    (re-search-forward "^[12][09]")
    (beginning-of-line)
    (copy-region-as-kill beg (point))
    (goto-char (point-max))
    (yank '(non nil list))
    (forward-word)
    (forward-char)))

;; org mode
(require 'org-install)
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "todo.org"))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map [f8] (lambda () (interactive) (org-capture nil "t")))
;;(define-key global-map [f9] 'remember-region)
(setq org-log-done t)
(setq org-agenda-show-log t)
(setq org-return-follows-link t)
(setq org-startup-indented t)
(setq org-agenda-start-on-weekday nil) ; show agenda starting today
(setq org-use-speed-commands t)
(setq org-archive-location (concat org-directory "archive/%s_archive::"))

(defun gtd ()
  "Open my todo list"
  (interactive)
  (find-file (concat org-directory "gtd.org")))

(fset 'vk-process-movie-list
      [?\C-a down ?\C-s ?2 ?0 ?1 ?1 left left left left ?\C-  ?\C-s ?  ?\C-s left ?\M-w right ?\C-y ?- left left left backspace ?- left left left backspace ?- right right right right right right ?\C-  ?\C-e ?\C-w ?. ?a ?v ?i left left left left ?\C-x ?o ?m ?p ?l ?a ?y ?e ?r ?  ?\C-y return ?\C-x ?o])

(defun vk-slugify (title)
  "Convert a normal Title string to something that can be used in a blog slug."
  (replace-regexp-in-string "[\\. ]+" "-"
                            (replace-regexp-in-string "'" ""
                                                      (downcase title))))

(defun vk-blogpost (title)
  "Create a new blog post."
  (interactive "sPost Title: ")
  (let ((slug (vk-slugify title)))
    (find-file (concat "~/dev/kurup.org/source/_posts/"
                       (format-time-string "%Y-%m-%d")
                       "-" slug ".markdown"))
    (insert "---\n")
    (insert "layout: post\n")
    (insert "date: " (format-time-string "%Y/%m/%d %H:%M:%S") "\n")
    (insert "title: " title "\n")
    (insert "comments: true\n")
    (insert "categories: \n")
    (insert "---\n\n")))

;; twitter mode
;; http://www.emacswiki.org/emacs/TwitteringMode
(require 'twittering-mode)
(setq twittering-use-master-password t)


;; vinod customizations
(setq user-mail-address "vinod@kurup.com")
;; set calendar's location (for sunrise sunset)
(setq calendar-latitude 35.9162)
(setq calendar-longitude -79.0999)
(setq calendar-location-name "Chapel Hill, NC")

(setq-default kill-whole-line t)        ; ctrl-k kills whole line if at col 0
(menu-bar-mode)

;; Delete old backup versions silently
(setq delete-old-versions t)

(load-theme 'adwaita t)

(global-set-key [(control x) (control r)] 'esk-sudo-edit)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; thanks johnw: https://gist.github.com/1198329
(defun find-grep-in-project (command-args)
  (interactive
   (progn
     (list (read-shell-command "Run find (like this): "
                               '("git ls-files -z | xargs -0 egrep -nH -e " . 41)
                               'grep-find-history))))
  (when command-args
    (let ((null-device nil)) ; see grep
      (grep command-args))))

;; octave mode for .m files
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(setq-default default-tab-width 4)
(set-default-font "Consolas-12")

;; emacsclient
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "~/src/android-sdk-linux_x86")
 '(browse-url-browser-function (quote browse-url-chromium))
 '(browse-url-generic-program "chromium-browser")
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files (quote ("~/org/gtd.org")))
 '(org-capture-templates (quote (("j" "Journal Entry" entry (file "~/org/notes.org") "* %T %?") ("t" "Create Task" entry (file+headline "~/Dropbox/org/gtd.org" "Inbox") "* TODO %^{Description} %^g
%?
Added: %U") ("d" "Diary" entry (file+headline "~/Dropbox/org/notes.org" "Diary") "* %T
Kavi's favorite: %^{Kavi's favorite}
Anika's favorite: %^{Anika's favorite}

%?"))))
 '(org-velocity-allow-regexps t)
 '(org-velocity-always-use-bucket t)
 '(org-velocity-bucket "~/Dropbox/org/bucket.org")
 '(org-velocity-create-method (quote capture))
 '(org-velocity-max-depth 2)
 '(org-velocity-search-method (quote phrase))
 '(pony-server-host "0.0.0.0")
 '(temporary-file-directory (concat user-emacs-directory "tmp")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
