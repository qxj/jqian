;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; desktop-cfg.el ---
;; Time-stamp: <2010-03-11 10:07:00 Thursday by julian>
;; Created: 2010 Julian Qian
;; Version: $Id: desktop-cfg.el,v 0.0 2010/01/28 10:27:56 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))


;;;  Saving the buffer list : M-x desktop-save
(desktop-save-mode 1)
(setq desktop-base-file-name "emacs.desktop")
(setq desktop-path (list my-temp-dir))
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
;;; Do not save to desktop
(setq desktop-buffers-not-to-save
     (concat "\\(" "\\.log\\|\\.diary\\|\\.elc" "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;; if error occurred, no matter it!
;; (condition-case nil
;;     (desktop-read)
;;   (error nil))


(provide 'desktop-cfg)

;;; desktop-cfg.el ends here
