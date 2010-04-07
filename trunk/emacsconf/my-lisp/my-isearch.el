;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; my-isearch.el --- 
;; Time-stamp: <2010-01-29 17:29:15 julian>
;; Created: 2010 Julian Qian
;; Version: $Id: my-isearch.el,v 0.0 2010/01/29 09:26:38 julian Exp $

;; 

;;; Code:
;; (eval-when-compile (require 'cl))

(setq isearch-allow-scroll t)

;; word case unsensitive
(setq-default case-fold-search t)

;; After press 'C-s' to begin incremental search, press 'M-i' to replace
;; the searching content.
(defun isearch-query-replace-current ()
  "Replace current searching string."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search)
        (from-string isearch-string))
    (if (string= from-string "")
        (isearch-update)
      (if (not isearch-success)
          (progn
            (message "Search string not found")
            (sleep-for 0.5)
            (isearch-update))
        (progn
          (isearch-done)
          (goto-char (min (point) isearch-other-end)))
        (perform-replace
         from-string
         (read-from-minibuffer
          (format "Query replace %s with: " from-string)
          "" nil nil query-replace-to-history-variable from-string t)
         t                                ; query flag
         isearch-regexp
         nil)))))

(defun toggle-case-fold-search-when-search ()
  "Call `toggle-case-fold-search' when run isearch."
  (interactive)
  (toggle-case-fold-search)
  (let ((str isearch-string))
    (goto-char isearch-opoint)
    (isearch-done)
    (let ((isearch-command
           (if isearch-forward
               (if isearch-regexp 'isearch-forward-regexp 'isearch-forward)
             (if isearch-regexp 'isearch-backward-regexp 'isearch-backward))))
      (call-interactively isearch-command))
    (isearch-yank-string str)))

(apply-define-key
 isearch-mode-map
 `(("M-i" isearch-query-replace-current-sb)
   ("M-k" isearch-clean)
   ("M-g" isearch-cancel)
   ("M-u" isearch-toggle-word)
   ("M-y" isearch-yank-line)
   ("C-y" isearch-yank-kill)
   ("M-H" isearch-help-map)
   ("M-h" isearch-del-char)))

(defun isearch-clean ()
  "Clean string in `iserch-mode'."
  (interactive)
  (goto-char isearch-opoint)
  (let ((isearch-command
         (if isearch-forward
             (if isearch-regexp 'isearch-forward-regexp 'isearch-forward)
           (if isearch-regexp 'isearch-backward-regexp 'isearch-backward))))
    (call-interactively isearch-command)))


(provide 'my-isearch)

;;; my-isearch.el ends here
