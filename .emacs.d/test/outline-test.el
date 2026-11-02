;;; outline-test.el --- Fold ;;; sections in elisp files -*- lexical-binding: t; -*-

;;; Commentary:
;; In Emacs Lisp files, TAB on a ;;; heading folds that section and S-TAB
;; folds them all, like magit.  TAB anywhere else still indents.

;;; Code:
(require 'test-helper)

(defconst vk-test-outline-file
  ";;; One\n(setq a 1)\n\n;;; Two\n(setq b\n2)\n"
  "Two sections; the second has a line that needs indenting.")

(defun vk-test-hidden-p (text)
  "Non-nil if the line containing TEXT is folded away."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (outline-invisible-p)))

(ert-deftest outline/tab-on-heading-folds-that-section ()
  (vk-with-temp-file ".el" vk-test-outline-file
    (should outline-minor-mode)
    (goto-char (point-min))
    (vk-press "TAB")
    (should (vk-test-hidden-p "(setq a 1)"))
    (should-not (vk-test-hidden-p "(setq b"))))

(ert-deftest outline/shift-tab-folds-everything ()
  (vk-with-temp-file ".el" vk-test-outline-file
    (vk-press "<backtab>")
    (should (vk-test-hidden-p "(setq a 1)"))
    (should (vk-test-hidden-p "(setq b"))))

(ert-deftest outline/tab-elsewhere-still-indents ()
  (vk-with-temp-file ".el" vk-test-outline-file
    (goto-char (point-min))
    (search-forward "\n2)")
    (vk-press "TAB")
    ;; lined up under the `b' of (setq b
    (should (string-match-p "\n      2)" (buffer-string)))))

;;; outline-test.el ends here
