;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; perl-cfg.el ---
;; Time-stamp: <2010-03-10 19:58:44 Wednesday by julian>
;; Created: 2010 Julian Qian
;; Version: $Id: perl-cfg.el,v 0.0 2010/03/10 11:57:14 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl progs" t)

(defun my-perl-mode-hook ()
  ;; (setq tab-width 2)
  (local-set-key [return] 'newline-and-indent)
  (setq indent-tabs-mode nil)
  (line-number-mode 1)
  (column-number-mode 1)
  (abbrev-mode 1)
  (turn-on-eldoc-mode)
  (cperl-mode)
  (setq compile-command (concat "perl -cw " buffer-file-name)))
(add-hook 'perl-mode-hook 'my-perl-mode-hook)

(setq cperl-auto-newline-after-colon t)
(setq cperl-autoindent-on-semi t)
(setq cperl-extra-newline-before-brace nil)
(setq cperl-indent-parens-as-block t)

(provide 'perl-cfg)

;;; perl-cfg.el ends here
