;;; typescript-test.el --- TypeScript files get web-mode and tide -*- lexical-binding: t; -*-

;;; Commentary:
;; typescript-mode isn't installed, so both .ts and .tsx go to web-mode,
;; which turns on tide for them.  tide itself is stubbed out here so the
;; tests don't start a TypeScript server.

;;; Code:
(require 'test-helper)

(defun vk-test-tide-started-p (ext)
  "Non-nil if visiting a file ending in EXT opens web-mode and starts tide."
  (let (started)
    (cl-letf (((symbol-function 'setup-tide-mode)
               (lambda () (setq started t))))
      (vk-with-temp-file ext "const x: number = 1;\n"
        (and (eq major-mode 'web-mode) started)))))

(ert-deftest typescript/ts-and-tsx-open-in-web-mode-with-tide ()
  (dolist (ext '(".ts" ".tsx"))
    (should (equal (list ext t)
                   (list ext (and (vk-test-tide-started-p ext) t))))))

(ert-deftest typescript/other-web-mode-files-skip-tide ()
  (should-not (vk-test-tide-started-p ".html")))

;;; typescript-test.el ends here
