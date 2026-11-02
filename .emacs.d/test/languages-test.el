;;; languages-test.el --- Language helpers turn on when I open a file -*- lexical-binding: t; -*-

;;; Commentary:
;; Most language packages load only when I open a matching file, so check
;; their helper modes still turn on.  paredit used to work in elisp only
;; because clj-refactor happened to load it at startup.

;;; Code:
(require 'test-helper)

(defun vk-test-modes-on (ext contents)
  "Open a file ending in EXT holding CONTENTS; return its minor modes.
Doesn't start a language server: `lsp' is stubbed and shows up as `lsp'."
  (let (lsp-called)
    (cl-letf (((symbol-function 'lsp) (lambda (&rest _) (setq lsp-called t))))
      (vk-with-temp-file ext contents
        (append (and lsp-called '(lsp)) local-minor-modes)))))

(ert-deftest languages/elisp-gets-paredit ()
  ;; In a fresh Emacs: in this one, other tests may have loaded paredit
  ;; already, which would hide the bug.
  (let ((test-dir (expand-file-name "test" vk-test-emacs-dir)))
    (should (equal "paredit-mode"
                   (car (last (split-string
                               (with-output-to-string
                                 (call-process
                                  "emacs" nil standard-output nil "--batch"
                                  "-L" test-dir "-l" "test-helper"
                                  "--eval" "(vk-with-temp-file \".el\" \"(f)\\n\" (princ (if paredit-mode \"paredit-mode\" \"none\")))"))
                               "\n" t)))))))

(ert-deftest languages/clojure-gets-its-helpers ()
  (let ((modes (vk-test-modes-on ".clj" "(ns x)\n")))
    (dolist (mode '(paredit-mode clj-refactor-mode subword-mode
                    rainbow-delimiters-mode yas-minor-mode lsp))
      (should (equal (list mode t) (list mode (and (memq mode modes) t)))))))

(ert-deftest languages/ruby-gets-inf-ruby ()
  (should (memq 'inf-ruby-minor-mode (vk-test-modes-on ".rb" "def f\nend\n"))))

;;; languages-test.el ends here
