;;; test-helper.el --- Load my real init.el for regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by test/run-tests.sh before the *-test.el files.  It loads the
;; real init.el (so tests exercise my actual config and keybindings), but
;; first switches off the parts that touch the outside world:
;; - no emacs server (it would fight the Emacs I'm actually using)
;; - no secrets (decrypting would pop up a GPG prompt)
;; - nothing saved on exit (savehist, recentf, prescient, ...)

;;; Code:
(require 'ert)
(require 'cl-lib)

(defconst vk-test-emacs-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "The .emacs.d directory these tests belong to.")

(setq user-emacs-directory vk-test-emacs-dir)

(advice-add 'server-start :override #'ignore)
(provide 'secrets)
(advice-add 'load :before-until
            (lambda (file &rest _) (string-match-p "secrets" file)))

(load (expand-file-name "init.el" vk-test-emacs-dir) nil t)

(setq kill-emacs-hook nil)

(defmacro vk-with-temp-file (ext contents &rest body)
  "Visit a temp file ending in EXT holding CONTENTS, run BODY in its buffer.
The file is visited with `find-file', so the major mode, hooks and
keybindings are the ones I'd get opening a real file.  Cleaned up after."
  (declare (indent 2))
  `(let ((file (make-temp-file "vk-test-" nil ,ext ,contents)))
     (unwind-protect
         (with-current-buffer (find-file-noselect file)
           (unwind-protect (progn ,@body)
             (set-buffer-modified-p nil)
             (kill-buffer)))
       (delete-file file))))

(defun vk-press (keys)
  "Run the command bound to KEYS (e.g. \"C-c C-k\") in the current buffer."
  (let ((cmd (key-binding (kbd keys))))
    (unless (commandp cmd)
      (error "%s is not bound to a command here (got %S)" keys cmd))
    (call-interactively cmd)))

(provide 'test-helper)
;;; test-helper.el ends here
