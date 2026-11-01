;;; fonts-test.el --- Proportional font for prose, monospace for the rest -*- lexical-binding: t; -*-

;;; Commentary:
;; poet-theme looks best with a proportional (variable-pitch) font, but
;; only prose should get one.  Files where columns or indentation matter
;; must stay monospace, even when their mode derives from text-mode.

;;; Code:
(require 'test-helper)

(defun vk-test-variable-pitch-p (ext)
  "Non-nil if visiting a file ending in EXT gets a proportional font."
  (vk-with-temp-file ext "x\n"
    (bound-and-true-p buffer-face-mode)))

(ert-deftest fonts/prose-is-proportional ()
  (dolist (ext '(".txt" ".org" ".md" ".markdown" ".adoc"))
    ;; comparing lists so a failure names the file type
    (should (equal (list ext t)
                   (list ext (and (vk-test-variable-pitch-p ext) t))))))

(ert-deftest fonts/commit-messages-are-proportional ()
  ;; magit opens commit messages in plain text-mode
  (with-temp-buffer
    (text-mode)
    (should (bound-and-true-p buffer-face-mode))))

(ert-deftest fonts/columns-and-indentation-stay-monospace ()
  (dolist (ext '(".yaml" ".yml" ".beancount" ".http" ".py" ".el"))
    (should-not (and (vk-test-variable-pitch-p ext) ext))))

;;; fonts-test.el ends here
