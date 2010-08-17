;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; recentf-cfg.el ---
;; Time-stamp: <2010-06-02 16:11:42 Wednesday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: recentf-cfg.el,v 0.0 2010/04/21 11:17:52 jqian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

;;; recent finded buffers
(setq recentf-max-saved-items nil)
(setq recentf-save-file (concat my-temp-dir "emacs.recentf"))

(recentf-mode t)

(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
         (tocpl (mapcar (function
                         (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
         (prompt (append '("File name: ") tocpl))
         (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-ignore-representation fname tocpl)))))
(global-set-key (kbd "C-x C-o") 'recentf-open-files-compl)

;; Also store recent opened directories besides files
(defun recentf-add-dir ()
  "Add directory name to recentf file list."
  (recentf-add-file dired-directory))
(add-hook 'dired-mode-hook 'recentf-add-dir)


(provide 'recentf-cfg)

;;; recentf-cfg.el ends here
