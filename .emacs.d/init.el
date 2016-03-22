(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      anzu
                      autopair
                      company
                      elfeed
                      elpy
                      erc-hl-nicks
                      flycheck
                      js2-mode
                      markdown-mode
                      org
                      projectile
                      rainbow-mode
                      web-mode
                      yaml-mode
                      zenburn-theme
                      zencoding-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
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
(setq flycheck-jshintrc "~/.emacs.d/.jshintrc")

;; make moving around windows easier
(require 'windmove)
(windmove-default-keybindings 'super)
(setq org-replace-disputed-keys t)

(add-hook 'css-mode-hook 'rainbow-mode)

;; For running tests easily
(global-set-key [f6] 'recompile)

;; ledger
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory user-login-name "/ledger/")))
(load "ldg-new")
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

;; emacsclient
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "~/src/android-sdk-linux_x86")
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(browse-url-generic-program "chromium-browser")
 '(compilation-always-kill t)
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(custom-safe-themes (quote ("de538b2d1282b23ca41ac5d8b69c033b911521fe27b5e1f59783d2eb20384e1f" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "f61972772958e166cda8aaf0eba700aad4faa0b4101cee319e894e7a747645c9" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(elfeed-feeds (quote ("https://www.archlinux.org/feeds/news/" "http://www.caktusgroup.com/feeds/main/" "http://feeds.feedburner.com/CoderWeeklyArchiveFeed" "http://www.stephanboyer.com/rss" "http://feeds.feedburner.com/extracheese" "http://www.daemonology.net/hn-daily/index.rss" "http://opensource.com/health/feed" "http://blogs.hospitalmedicine.org/SHMClinicalBlog/?feed=rss2" "http://psung.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/jazzychadblog" "https://julien.danjou.info/blog/index.xml" "http://www.learningclojure.com/feeds/posts/default" "http://pipes.yahoo.com/pipes/pipe.run?_id=3PHwctj52xGg02vB6kjTQA&_render=rss" "http://mmcgrana.github.io/atom.xml" "http://minimallinux.com/feed/" "http://feeds.pheedo.com/OcwWeb/rss/new/mit-newocwscholarcourses" "http://allbleedingstops.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/MuddyCalves" "http://www.newsobserver.com/static/includes/most_popular-daily.rss" "http://www.tbray.org/ongoing/ongoing.atom" "http://blog.pinboard.in/feed/" "https://pragprog.com/magazines.opds" "http://primarilypictures.tumblr.com/rss" "http://yosefk.com/blog/feed" "http://feeds.feedburner.com/relevance-blog" "http://feeds.feedburner.com/ryanwaggoner" "http://kennedychina.blogspot.com/feeds/posts/default" "http://blog.fogus.me/feed/" "http://blogs.hospitalmedicine.org/hm10blog/feed/" "http://technomancy.us/feed/atom.xml" "http://blackstag.com/blog.rss" "http://changelog.complete.org/feed" "http://feeds2.feedburner.com/thecodemill/dawn-patrol" "https://github.com/blog.atom" "http://feeds.feedburner.com/blogspot/MKuf" "http://www.zefrank.com/theshow/replay/?feed=rss2" "http://blog.wikinvest.com/feed/atom/index.html" "http://blog.thinglabs.com/rss" "http://www.kurup.org/thinkup/crawler/rss.php?un=vinod@kurup.com&as=ca68922cda7604acbc7e71441f5e1f94" "http://feeds.feedburner.com/VirtuousCode" "http://womensbestkeptsecrets.com/" "http://blog.xkcd.com/feed/" "http://blog.yorba.org/feed" "http://feeds.raganwald.com/raganwald" "http://scribbling.net/feed/" "http://seriouspony.com/blog?format=rss" "http://www.fredtrotter.com/feed/" "http://s200161356.onlinehome.us/SHMClinicalBlog/?feed=rss2" "http://feeds.feedburner.com/WachtersWorld" "http://feeds.feedburner.com/43folders" "http://feeds.feedburner.com/jarkkolaine/sSkp" "http://subhasisarchana.blogspot.com/feeds/posts/default" "http://borkwarellc.wordpress.com/feed/" "http://feeds.feedburner.com/kurup/MGsB" "http://varkeyblog.com/cgi-sys/suspendedpage.cgi?feed=atom" "http://feeds.feedburner.com/vkurup" "http://stiglerdiet.com/feeds/all.atom.xml" "http://bc.tech.coop/blog/rss.xml" "http://briancarper.net/feed" "http://www.defmacro.org/" "http://blog.disqus.com/rss" "http://emacs-fu.blogspot.com/feeds/posts/default" "http://genehack.org/feed/rss/index.xml" "http://feeds.feedburner.com/newartisanscom" "http://feeds.feedburner.com/PindsBlog" "http://groups.google.com/group/ledger-cli/atom_v1_0_msgs.xml?num=50" "http://torvalds-family.blogspot.com/feeds/posts/default" "http://weeklyreddit.appspot.com/rss/linux" "http://feeds.feedburner.com/emacsblog" "http://firehose.diveintomark.org/atom.xml" "http://feeds.mekk.waw.pl/MekksBlog?format=xml" "http://emacs.wordpress.com/feed/" "https://blog.nearlyfreespeech.net/feed/atom/?_=5977" "http://feeds.feedburner.com/OfficialGmailBlog" "http://feeds.feedburner.com/PaulGrahamUnofficialRssFeed" "http://blogs.law.harvard.edu/philg/feed/atom/" "http://weeklyreddit.appspot.com/rss/programming" "http://feeds.postrank.com/4adcf2b26442f84ebcf631f42516c1ac?level=best" "http://www.aaronsw.com/weblog/index.xml" "http://feeds.feedburner.com/sachac" "http://user/04689935238818494850/state/com.google/broadcast" "http://spyced.blogspot.com/feeds/posts/default" "http://steve-yegge.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/tom-preston-werner" "http://withoutane.com/feed/" "http://www.pixelbeat.org/feed/rss2.xml" "http://www.garann.com/dev/feed/" "http://gigasquidsoftware.com/atom.xml" "http://aphyr.com/posts.atom" "http://user/14474885413983863081/state/com.google/broadcast" "http://rubick.com:8002/blogger/rss/rss/rss.xml" "http://feeds.feedburner.com/MarkAufflicksWeblog" "http://www.solutiongrove.com/blogger/rss/rss.xml" "http://feeds.dailylit.com/feeds/subs/fc826fec3370831ba4c1f2eee7a65535" "http://philipsung.blogspot.com/feeds/posts/default" "http://www.xkcd.com/rss.xml" "http://feeds.feedburner.com/zefrank" "http://feeds.kottke.org/main" "http://feeds.feedburner.com/AAIIComputerizedInvestingIssueUpdate" "http://feeds.feedburner.com/AAIIJournalIssueUpdate" "http://feeds.feedburner.com/AAIIModelPortfolioUpdate" "http://feeds.feedburner.com/AAIIStockScreensUpdate" "http://feeds.feedburner.com/GoogleFinanceBlog" "http://carpedurham.com/feed/atom/" "http://feeds2.feedburner.com/EclecticGlobOfTangentialVerbosity" "http://nullprogram.com/feed/" "http://www.terminally-incoherent.com/blog/feed/" "http://greengeckobay.blogspot.com/feeds/posts/default")))
 '(elpy-modules (quote (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
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
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files (quote ("~/Documents/caktus.org")))
 '(org-capture-templates (quote (("j" "Journal Entry" entry (file "~/org/notes.org") "* %T %?") ("t" "Create Task" entry (file+headline "~/Dropbox/org/gtd.org" "Inbox") "* TODO %^{Description} %^g
%?
Added: %U") ("d" "Diary" entry (file+headline "~/Dropbox/org/notes.org" "Diary") "* %T
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
 '(pony-server-host "0.0.0.0")
 '(python-check-command "flake8")
 '(rst-compile-toolsets (quote ((html "rst2html.py" ".html" nil) (latex "rst2latex.py" ".tex" nil) (newlatex "rst2newlatex" ".tex" nil) (pseudoxml "rst2pseudoxml.py" ".xml" nil) (xml "rst2xml.py" ".xml" nil) (pdf "rst2pdf" ".pdf" nil) (s5 "rst2s5.py" ".html" nil))))
 '(safe-local-variable-values (quote ((eval progn (setenv "DJANGO_SETTINGS_MODULE" "duckling.settings.local")) (eval progn (setenv "DJANGO_SETTINGS_MODULE" "cts.settings.local")) (eval progn (setenv "DJANGO_SETTINGS_MODULE" "rescue_id.settings.local") (setenv "PATH" (concat (getenv "PATH") ":/home/vkurup/dev/rescue_id/node_modules/.bin")) (setq exec-path (append exec-path (quote ("/home/vkurup/dev/rescue_id/node_modules/.bin"))))) (eval progn (setenv "DJANGO_SETTINGS_MODULE" "rescue_id.settings.local")) (eval progn (setenv "DJANGO_SETTINGS_MODULE" "ucare4life.settings.local")) (eval progn (setenv "DJANGO_SETTINGS_MODULE" "service_info.settings.local") (setenv "PATH" (concat (getenv "PATH") ":/home/vkurup/dev/serviceinfo/node_modules/.bin")) (setq exec-path (append exec-path (quote ("/home/vkurup/dev/serviceinfo/node_modules/.bin"))))) (eval progn (setenv "DJANGO_SETTINGS_MODULE" "service_info.settings.local") (setenv "PATH" (concat (getenv "PATH") ":/home/vkurup/dev/serviceinfo/node_modules/.bin"))) (test-case-name . hnec\.tests) (eval progn (setenv "DJANGO_SETTINGS_MODULE" "service_info.settings.local")) (eval progn (setenv "DJANGO_SETTINGS_MODULE" "dr_tea.settings.local")) (eval progn (setenv "DJANGO_SETTINGS_MODULE" "rescuesms.settings.local")) (eval progn (setenv "DJANGO_SETTINGS_MODULE" "libya_elections.settings.local")) (project-venv-name . "superlists") (project-venv-name . "pythontdd") (project-venv-name . "rescuesms") (project-venv-name . "reporting-api") (project-venv-name . "dr-tea") (project-venv-name . "oberlin") (project-venv-name . "libya-elections") (project-venv-name . "rsvp") (encoding . utf-8) (whitespace-line-column . 80) (lexical-binding . t))))
 '(temporary-file-directory (concat user-emacs-directory "tmp"))
 '(vc-annotate-background "#2b2b2b")
 '(vc-annotate-color-map (quote ((20 . "#bc8383") (40 . "#cc9393") (60 . "#dfaf8f") (80 . "#d0bf8f") (100 . "#e0cf9f") (120 . "#f0dfaf") (140 . "#5f7f5f") (160 . "#7f9f7f") (180 . "#8fb28f") (200 . "#9fc59f") (220 . "#afd8af") (240 . "#bfebbf") (260 . "#93e0e3") (280 . "#6ca0a3") (300 . "#7cb8bb") (320 . "#8cd0d3") (340 . "#94bff3") (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3")
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-comment-keywords t)
 '(web-mode-enable-part-face t)
 '(web-mode-extra-python-keywords t)
 '(web-mode-markup-indent-offset 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-font-xact-highlight-face ((t (:background "black")))))
