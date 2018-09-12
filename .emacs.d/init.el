(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(anzu
                      autopair
                      company
                      elfeed
                      elpy
                      erc-hl-nicks
                      flycheck
                      js2-mode
                      json-mode
                      ledger-mode
                      magit
                      markdown-mode
                      org
                      projectile
                      rainbow-mode
                      smex
                      web-mode
                      yaml-mode
                      zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ESK things: Keep?
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c g") 'magit-status)

(progn
  ;; Turn off mouse interface early in startup to avoid momentary display
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

  ;; You can keep system- or user-specific customizations here
  (setq esk-system-config (concat user-emacs-directory system-name ".el")
        esk-user-config (concat user-emacs-directory user-login-name ".el")
        esk-user-dir (concat user-emacs-directory user-login-name))

  (add-to-list 'load-path esk-user-dir)

  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)

  (when (file-exists-p esk-system-config) (load esk-system-config))
  (when (file-exists-p esk-user-config) (load esk-user-config))
  (when (file-exists-p esk-user-dir)
    (mapc 'load (directory-files esk-user-dir nil "^[^#].*el$"))))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory "~/.emacs.d/oddmuse"
      save-place-file "~/.emacs.d/places"
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      diff-switches "-u")

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

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
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
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

;;; Disable autopair-global-mode in calc-mode
;;; https://github.com/capitaomorte/autopair/issues/17
(add-hook 'calc-mode-hook
           #'(lambda ()
               (autopair-mode -1)))
(autopair-global-mode)
(ido-mode 1)
(ido-everywhere 1)
(projectile-global-mode)
(yas-global-mode 1)
(global-anzu-mode +1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (remove-hook 'before-save-hook 'delete-trailing-whitespace)

;; projectile settings
(setq projectile-enable-caching t)

;; work with ubuntu clipboard
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; map RET to newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

(elpy-enable)

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
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(add-hook 'web-mode-hook
           #'(lambda ()
               (autopair-mode -1)
               (auto-fill-mode -1)))

;; js2
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'flycheck-mode)

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))

;; For running tests easily
(global-set-key [f6] 'recompile)

;; ledger
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(add-hook 'ledger-mode-hook
          #'(lambda ()
              (autopair-mode -1)
              (auto-fill-mode -1)))

;; org mode
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "todo.org"))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
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
  (replace-regexp-in-string "[\\., ]+" "-"
                            (replace-regexp-in-string "['\?]" ""
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

;; (load-theme 'adwaita t)
(load-theme 'zenburn t)
;; (load-theme 'wombat t)

(global-set-key [(control x) (control r)] 'esk-sudo-edit)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

(setq-default default-tab-width 4)
(set-frame-font "Inconsolata-12")
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;; erc
(require 'secrets)
(add-hook 'erc-mode-hook #'(lambda () (autopair-mode -1)))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#ledger" "#vumi-libya" "#tripython" "#rapidsms" "#elpy" "#emacs-elpy")
        ("caktusgroup.com" "#caktus" "#libya" "#radiology" "#rsvp" "#oberlin")))

;; (progn
;;   ;; (erc-tls
;;   ;;  :server "chat.caktusgroup.com"
;;   ;;  :port 6697
;;   ;;  :nick "vkurup"
;;   ;;  :password erc-password)
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
(add-hook 'term-mode-hook
          (lambda ()
            (autopair-mode -1)) ;; for emacsen >= 24
          )
(global-set-key (kbd "C-c t") 'visit-term-buffer)

;; http://emacsredux.com/blog/2013/03/29/automatic-electric-indentation/
(electric-indent-mode 0)

;; use flycheck instead of flymake
;; https://github.com/jorgenschaefer/elpy/issues/304
(when (require 'flycheck nil t)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; elfeed
(global-set-key (kbd "C-x w") 'elfeed)

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
 '(compilation-always-kill t)
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(custom-safe-themes
   (quote
    ("de538b2d1282b23ca41ac5d8b69c033b911521fe27b5e1f59783d2eb20384e1f" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "f61972772958e166cda8aaf0eba700aad4faa0b4101cee319e894e7a747645c9" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(elfeed-feeds
   (quote
    ("https://www.archlinux.org/feeds/news/" "http://www.caktusgroup.com/feeds/main/" "http://feeds.feedburner.com/CoderWeeklyArchiveFeed" "http://www.stephanboyer.com/rss" "http://feeds.feedburner.com/extracheese" "http://www.daemonology.net/hn-daily/index.rss" "http://opensource.com/health/feed" "http://blogs.hospitalmedicine.org/SHMClinicalBlog/?feed=rss2" "http://psung.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/jazzychadblog" "https://julien.danjou.info/blog/index.xml" "http://www.learningclojure.com/feeds/posts/default" "http://pipes.yahoo.com/pipes/pipe.run?_id=3PHwctj52xGg02vB6kjTQA&_render=rss" "http://mmcgrana.github.io/atom.xml" "http://minimallinux.com/feed/" "http://feeds.pheedo.com/OcwWeb/rss/new/mit-newocwscholarcourses" "http://allbleedingstops.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/MuddyCalves" "http://www.newsobserver.com/static/includes/most_popular-daily.rss" "http://www.tbray.org/ongoing/ongoing.atom" "http://blog.pinboard.in/feed/" "https://pragprog.com/magazines.opds" "http://primarilypictures.tumblr.com/rss" "http://yosefk.com/blog/feed" "http://feeds.feedburner.com/relevance-blog" "http://feeds.feedburner.com/ryanwaggoner" "http://kennedychina.blogspot.com/feeds/posts/default" "http://blog.fogus.me/feed/" "http://blogs.hospitalmedicine.org/hm10blog/feed/" "http://technomancy.us/feed/atom.xml" "http://blackstag.com/blog.rss" "http://changelog.complete.org/feed" "http://feeds2.feedburner.com/thecodemill/dawn-patrol" "https://github.com/blog.atom" "http://feeds.feedburner.com/blogspot/MKuf" "http://www.zefrank.com/theshow/replay/?feed=rss2" "http://blog.wikinvest.com/feed/atom/index.html" "http://blog.thinglabs.com/rss" "http://www.kurup.org/thinkup/crawler/rss.php?un=vinod@kurup.com&as=ca68922cda7604acbc7e71441f5e1f94" "http://feeds.feedburner.com/VirtuousCode" "http://womensbestkeptsecrets.com/" "http://blog.xkcd.com/feed/" "http://blog.yorba.org/feed" "http://feeds.raganwald.com/raganwald" "http://scribbling.net/feed/" "http://seriouspony.com/blog?format=rss" "http://www.fredtrotter.com/feed/" "http://s200161356.onlinehome.us/SHMClinicalBlog/?feed=rss2" "http://feeds.feedburner.com/WachtersWorld" "http://feeds.feedburner.com/43folders" "http://feeds.feedburner.com/jarkkolaine/sSkp" "http://subhasisarchana.blogspot.com/feeds/posts/default" "http://borkwarellc.wordpress.com/feed/" "http://feeds.feedburner.com/kurup/MGsB" "http://varkeyblog.com/cgi-sys/suspendedpage.cgi?feed=atom" "http://feeds.feedburner.com/vkurup" "http://stiglerdiet.com/feeds/all.atom.xml" "http://bc.tech.coop/blog/rss.xml" "http://briancarper.net/feed" "http://www.defmacro.org/" "http://blog.disqus.com/rss" "http://emacs-fu.blogspot.com/feeds/posts/default" "http://genehack.org/feed/rss/index.xml" "http://feeds.feedburner.com/newartisanscom" "http://feeds.feedburner.com/PindsBlog" "http://groups.google.com/group/ledger-cli/atom_v1_0_msgs.xml?num=50" "http://torvalds-family.blogspot.com/feeds/posts/default" "http://weeklyreddit.appspot.com/rss/linux" "http://feeds.feedburner.com/emacsblog" "http://firehose.diveintomark.org/atom.xml" "http://feeds.mekk.waw.pl/MekksBlog?format=xml" "http://emacs.wordpress.com/feed/" "https://blog.nearlyfreespeech.net/feed/atom/?_=5977" "http://feeds.feedburner.com/OfficialGmailBlog" "http://feeds.feedburner.com/PaulGrahamUnofficialRssFeed" "http://blogs.law.harvard.edu/philg/feed/atom/" "http://weeklyreddit.appspot.com/rss/programming" "http://feeds.postrank.com/4adcf2b26442f84ebcf631f42516c1ac?level=best" "http://www.aaronsw.com/weblog/index.xml" "http://feeds.feedburner.com/sachac" "http://user/04689935238818494850/state/com.google/broadcast" "http://spyced.blogspot.com/feeds/posts/default" "http://steve-yegge.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/tom-preston-werner" "http://withoutane.com/feed/" "http://www.pixelbeat.org/feed/rss2.xml" "http://www.garann.com/dev/feed/" "http://gigasquidsoftware.com/atom.xml" "http://aphyr.com/posts.atom" "http://user/14474885413983863081/state/com.google/broadcast" "http://rubick.com:8002/blogger/rss/rss/rss.xml" "http://feeds.feedburner.com/MarkAufflicksWeblog" "http://www.solutiongrove.com/blogger/rss/rss.xml" "http://feeds.dailylit.com/feeds/subs/fc826fec3370831ba4c1f2eee7a65535" "http://philipsung.blogspot.com/feeds/posts/default" "http://www.xkcd.com/rss.xml" "http://feeds.feedburner.com/zefrank" "http://feeds.kottke.org/main" "http://feeds.feedburner.com/AAIIComputerizedInvestingIssueUpdate" "http://feeds.feedburner.com/AAIIJournalIssueUpdate" "http://feeds.feedburner.com/AAIIModelPortfolioUpdate" "http://feeds.feedburner.com/AAIIStockScreensUpdate" "http://feeds.feedburner.com/GoogleFinanceBlog" "http://carpedurham.com/feed/atom/" "http://feeds2.feedburner.com/EclecticGlobOfTangentialVerbosity" "http://nullprogram.com/feed/" "http://www.terminally-incoherent.com/blog/feed/" "http://greengeckobay.blogspot.com/feeds/posts/default")))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-test-runner (quote elpy-test-django-runner))
 '(erc-autojoin-mode t)
 '(erc-enable-logging (quote erc-log-all-but-server-buffers))
 '(erc-log-channels-directory "~/.erc/logs")
 '(erc-log-insert-log-on-open t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-track-position-in-mode-line t)
 '(fci-rule-color "#383838")
 '(fill-column 100)
 '(js2-auto-indent-p t)
 '(js2-enter-indents-newline t)
 '(js2-strict-missing-semi-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-pull-arguments nil)
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files (quote ("~/Documents/caktus.org")))
 '(org-capture-templates
   (quote
    (("j" "Journal Entry" entry
      (file "~/org/notes.org")
      "* %T %?")
     ("t" "Create Task" entry
      (file+headline "~/Dropbox/org/gtd.org" "Inbox")
      "* TODO %^{Description} %^g
%?
Added: %U")
     ("d" "Diary" entry
      (file+headline "~/Dropbox/org/notes.org" "Diary")
      "* %T
Kavi's favorite: %^{Kavi's favorite}
Anika's favorite: %^{Anika's favorite}

%?"))))
 '(org-completion-use-ido t)
 '(org-velocity-allow-regexps t)
 '(org-velocity-always-use-bucket t)
 '(org-velocity-bucket "~/Dropbox/org/bucket.org")
 '(org-velocity-create-method (quote capture))
 '(org-velocity-max-depth 2)
 '(org-velocity-search-method (quote phrase))
 '(package-selected-packages
   (quote
    (json-mode zenburn-theme yaml-mode web-mode swiper smex rainbow-mode projectile markdown-mode magit ledger-mode js2-mode flycheck erc-hl-nicks elpy elfeed autopair anzu)))
 '(python-check-command "flake8")
 '(rst-compile-toolsets
   (quote
    ((html "rst2html.py" ".html" nil)
     (latex "rst2latex.py" ".tex" nil)
     (newlatex "rst2newlatex" ".tex" nil)
     (pseudoxml "rst2pseudoxml.py" ".xml" nil)
     (xml "rst2xml.py" ".xml" nil)
     (pdf "rst2pdf" ".pdf" nil)
     (s5 "rst2s5.py" ".html" nil))))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (setenv "DJANGO_SETTINGS_MODULE" "cts.settings.local"))
     (eval progn
           (setenv "DJANGO_SETTINGS_MODULE" "mdumaker.settings.local"))
     (eval progn
           (setenv "DJANGO_SETTINGS_MODULE" "ncvoter.local_settings"))
     (eval progn
           (setenv "DJANGO_SETTINGS_MODULE" "inddex.settings.dev"))
     (eval progn
           (setenv "DJANGO_SETTINGS_MODULE" "libya_elections.settings.local")))))
 '(temporary-file-directory (concat user-emacs-directory "tmp"))
 '(web-mode-code-indent-offset 4)
 '(web-mode-css-indent-offset 4)
 '(web-mode-enable-comment-keywords t)
 '(web-mode-enable-part-face t)
 '(web-mode-extra-python-keywords t)
 '(web-mode-markup-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
