;;; custom-test.el --- Custom settings live outside init.el -*- lexical-binding: t; -*-

;;; Commentary:
;; M-x customize and package installs save to custom.el, which is
;; git-ignored because it differs per machine.  Settings I want on every
;; machine are plain setq's in init.el, so check they still apply.

;;; Code:
(require 'test-helper)

(ert-deftest custom/saves-go-to-ignored-custom-el ()
  (should (equal custom-file (expand-file-name "custom.el" vk-test-emacs-dir)))
  (let ((default-directory vk-test-emacs-dir))
    (should (zerop (call-process "git" nil nil nil
                                 "check-ignore" "-q" "custom.el")))))

(ert-deftest custom/init-el-has-no-custom-block ()
  ;; if this fails, Emacs wrote Custom settings back into init.el
  (with-temp-buffer
    (insert-file-contents (expand-file-name "init.el" vk-test-emacs-dir))
    (should-not (search-forward "(custom-set-variables" nil t))))

(ert-deftest custom/shared-settings-apply ()
  (should (equal js-indent-level 4))
  (should (equal sh-basic-offset 2))
  (should (equal org-agenda-files '("~/org/gtd.org" "~/org/trips.org")))
  (require 'clojure-mode)
  (should (eq clojure-indent-style 'align-arguments)))

(ert-deftest custom/lsp-ignores-my-dirs-plus-defaults ()
  (require 'lsp-mode)
  (dolist (dir '("[/\\\\]screenshots\\'" "[/\\\\]logs\\'"   ; mine
                 "[/\\\\]node_modules"))                    ; a default
    (should (member dir lsp-file-watch-ignored-directories))))

;;; custom-test.el ends here
