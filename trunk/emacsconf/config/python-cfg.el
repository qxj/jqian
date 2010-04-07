;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; python-cfg.el ---
;; Time-stamp: <2010-03-11 17:47:34 Thursday by julian>
;; Created: 2010 Julian Qian
;; Version: $Id: python-cfg.el,v 0.0 2010/03/10 12:03:12 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

(define-prefix-command 'python-mode-map)

(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun my-python-mode-hook ()
;; (setq tab-width 2)
  (local-set-key [return] 'newline-and-indent)
  (setq indent-tabs-mode t)
  (auto-fill-mode 1)
  (turn-on-eldoc-mode)

  (define-key python-mode-map "\"" 'electric-pair)
  (define-key python-mode-map "\'" 'electric-pair)
  (define-key python-mode-map "(" 'electric-pair)
  (define-key python-mode-map "[" 'electric-pair)
  (define-key python-mode-map "{" 'electric-pair)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)


(provide 'python-cfg)

;;; python-cfg.el ends here
