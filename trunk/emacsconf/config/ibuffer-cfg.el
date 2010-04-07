;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; ibuffer-cfg.el ---
;; Time-stamp: <2010-03-11 10:58:29 Thursday by julian>
;; Created: 2010 Julian Qian
;; Version: $Id: ibuffer-cfg.el,v 0.0 2010/03/11 02:57:52 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (setq ibuffer-filter-groups
                  '(
                    ("*buffer*" (name . "\\*.*\\*"))
                    ("TAGS" (name . "^TAGS\\(<[0-9]+>\\)?$"))
                    ("dired" (mode . dired-mode))))))

(defface ibuffer-sql-face '((t (:foreground "Goldenrod"))) "Ibuffer sql face")
(defface ibuffer-text-face '((t (:foreground "SkyBlue"))) "Ibuffer text face")
(defface ibuffer-readonly-face '((t (:foreground "Blue" :weight bold))) "Ibuffer read only face")
(defface ibuffer-lisp-face '((t (:foreground "gold"))) "Ibuffer lisp face")

(setq
 ibuffer-fontification-alist
 '( ;; Sql-mode buffers
   (5  (string-match ".sql$" (buffer-name)) ibuffer-sql-face)
   (10 (eq major-mode 'text-mode) ibuffer-text-face)
   (15 (eq major-mode 'emacs-lisp-mode) ibuffer-lisp-face)
   (20 (string-match "^*" (buffer-name)) ibuffer-readonly-face)))

(setq ibuffer-formats '((mark modified read-only " " (name 21 21) " "
                              (size 6 -1 :right) " " (mode 25 25 :center)
                              ;;                         " " (process 8 -1)
                              " " filename)
                        (mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")

(provide 'ibuffer-cfg)

;;; ibuffer-cfg.el ends here
