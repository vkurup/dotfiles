;;; beancount-test.el --- My beancount workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; The thing I do most: open transactions.beancount, search for a payee,
;; press C-c C-k to copy that transaction to the bottom of the file with
;; today's date, then edit it.

;;; Code:
(require 'test-helper)

(defconst vk-test-transactions
  "2019-02-12 * \"Duke Energy\" \"Electric bill\"
  Expenses:Home:Electric                                  84.12 USD
    ofx_fitid: \"123456\"
  Assets:Banking:Checking

2026-10-30 txn \"Weaver Street Market\" \"Groceries\"
  Expenses:Food:Groceries                                 42.17 USD
  Liabilities:CreditCards:Visa

2026-10-31 txn \"Cosmic Cantina\"
  Expenses:Food:Restaurants                               18.50 USD
  Liabilities:CreditCards:Visa
"
  "Made-up transactions shaped like the real file (which ends without a
blank line after the last transaction).")

(defun vk-test-today ()
  (format-time-string "%Y-%m-%d"))

(defun vk-test-copy-payee (payee)
  "Do what I do: search for PAYEE, then press C-c C-k."
  (goto-char (point-min))
  (search-forward payee)
  (vk-press "C-c C-k"))

(ert-deftest beancount/opens-in-beancount-mode ()
  (vk-with-temp-file ".beancount" vk-test-transactions
    (should (eq major-mode 'beancount-mode))
    ;; columns of amounts need a monospace font (see also fonts-test.el)
    (should-not (bound-and-true-p buffer-face-mode))))

(ert-deftest beancount/copy-txn-to-bottom-with-todays-date ()
  (vk-with-temp-file ".beancount" vk-test-transactions
    (vk-test-copy-payee "Weaver Street")
    (should (equal (buffer-string)
                   (concat vk-test-transactions "\n"
                           (vk-test-today) " txn \"Weaver Street Market\" \"Groceries\"
  Expenses:Food:Groceries                                 42.17 USD
  Liabilities:CreditCards:Visa
")))
    ;; point is left on the new transaction, ready to edit
    (should (looking-back (concat "^" (vk-test-today) " ") (line-beginning-position)))
    (should (= (line-number-at-pos) (- (count-lines (point-min) (point-max)) 2)))))

(ert-deftest beancount/copy-turns-star-flag-into-txn-and-keeps-metadata ()
  (vk-with-temp-file ".beancount" vk-test-transactions
    (vk-test-copy-payee "Duke Energy")
    (should (string-suffix-p
             (concat "\n\n" (vk-test-today) " txn \"Duke Energy\" \"Electric bill\"
  Expenses:Home:Electric                                  84.12 USD
    ofx_fitid: \"123456\"
  Assets:Banking:Checking
")
             (buffer-string)))))

(ert-deftest beancount/copy-works-from-a-posting-line ()
  ;; a search can land on an account or amount instead of the payee
  (vk-with-temp-file ".beancount" vk-test-transactions
    (vk-test-copy-payee "18.50")
    (should (string-suffix-p
             (concat "\n\n" (vk-test-today) " txn \"Cosmic Cantina\"
  Expenses:Food:Restaurants                               18.50 USD
  Liabilities:CreditCards:Visa
")
             (buffer-string)))))

(ert-deftest beancount/saving-only-adds-the-new-transaction ()
  ;; before-save hooks (whitespace-cleanup etc.) must not touch the rest
  ;; of the file
  (vk-with-temp-file ".beancount" vk-test-transactions
    (vk-test-copy-payee "Weaver Street")
    (let ((expected (buffer-string))
          (file buffer-file-name))
      (save-buffer)
      (should (equal (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     expected)))))

;;; beancount-test.el ends here
