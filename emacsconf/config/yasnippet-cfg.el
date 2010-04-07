;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; yasnippet-cfg.el --- 
;; Time-stamp: <2010-02-01 15:04:46 Monday by julian>
;; Created: 2010 Julian Qian
;; Version: $Id: yasnippet-cfg.el,v 0.0 2010/01/28 10:28:42 julian Exp $

;; 

;;; Code:
;; (eval-when-compile (require 'cl))

;;; Yet Another Snippet -  pluskid@newsmth
(require 'yasnippet) ;; not yasnippet-bundle
(setq yas/root-directory my-snippet-dir)
(yas/load-directory yas/root-directory)
(yas/initialize)     ;; enable yas/minor-mode globally
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))


;; FOR `auto-complete-mode', so disable default yasnippet expand action
(if (fboundp 'auto-complete-mode)
    (progn
      (setq yas/trigger-key nil)
      (define-key yas/keymap (kbd "M-j") 'yas/next-field-or-maybe-expand)
      (define-key yas/keymap (kbd "M-k") 'yas/prev-field)))


(provide 'yasnippet-cfg)

;;; yasnippet-cfg.el ends here
