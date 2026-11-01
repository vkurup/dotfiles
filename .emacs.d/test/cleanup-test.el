;;; cleanup-test.el --- C-c n tidies the whole buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; C-c n (crux-cleanup-buffer-or-region) replaced my old esk-cleanup-buffer:
;; with no region it re-indents, untabifies and trims trailing whitespace
;; in the whole buffer, wherever point is.

;;; Code:
(require 'test-helper)

(ert-deftest cleanup/whole-buffer-without-a-region ()
  (vk-with-temp-file ".el" "(defun f ()   \n\t\t(foo)\n      (bar))\n"
    ;; point at the end and no region, so crux's advice is what makes
    ;; this reach the top of the buffer
    (goto-char (point-max))
    (deactivate-mark)
    (vk-press "C-c n")
    (should (equal (buffer-string) "(defun f ()\n  (foo)\n  (bar))\n"))))

;;; cleanup-test.el ends here
