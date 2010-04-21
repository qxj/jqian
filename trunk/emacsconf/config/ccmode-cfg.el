;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; ccmode-cfg.el --- enhancement for c/c++ development
;; Time-stamp: <2010-04-21 19:31:12 Wednesday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: dev-cfg.el,v 0.0 2010/01/28 10:17:11 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

;;{{{ help to switch between source and header files
(require 'sourcepair)
;; (define-key c-mode-map (kbd "C-c s") 'sourcepair-load)
;; (define-key c++-mode-map (kbd "C-c s") 'sourcepair-load)
(setq sourcepair-source-path '( "." "../src"))
(setq sourcepair-header-path user-head-file-dir)
(setq sourcepair-recurse-ignore '("CVS" "bin" "lib" "Obj" "Debug" "Release" ".svn"))
;;}}}

;;{{{ find out all header files that .cpp includes
(require 'c-includes)
(setq c-includes-binding t)
(setq c-includes-path ffap-c-path)
;;}}}

;;{{{ ebrowse
(require 'ebrowse)
(add-to-list 'auto-mode-alist '("BROWSE\\.*" . ebrowse-tree-mode))
;;}}}

;;{{{ show function name where cursor stay
(require 'which-func)
(which-func-mode 1)
(setq which-func-unknown "unknown")
;;}}}



(require 'cc-mode)
(require 'c-eldoc)

;; highlight possible warnings in .c files
(global-cwarn-mode t)

(setq-default comment-column 72)
(setq compilation-scroll-output t)

;; customisation of cc-mode
(defun my-c-mode-common-hook ()
  ;; style customization
  (c-set-offset 'member-init-intro '++)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (c-set-offset 'substatement-open 0)
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  ;; (c-toggle-auto-hungry-state 0) ;; auto return after semicolon
  ;; minor modes
  (auto-fill-mode 1)
  (c-turn-on-eldoc-mode)
  ;; local keys
  (local-set-key [return] 'newline-and-indent)
  ;; (local-set-key [delete] 'delete-char)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;; (add-hook 'c-mode-common-hook 'my-common-hook)
;; (add-hook 'c-mode-common-hook 'my-show-prog-keywords)

(setq-default c-default-style (quote ((java-mode . "java") (other . "gnu"))))

(font-lock-add-keywords 'c-mode '(("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1
                                   font-lock-function-name-face)) t)

;;; auto align and indent
;; (defun my-indent-or-complete ()
;;   (interactive)
;;   (if (looking-at "\\>")
;;       (hippie-expand nil)
;;     (indent-for-tab-command)))

;; (add-hook 'c-mode-common-hook
;;           (function (lambda ()
;;            ;;(define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
;;            ;;(define-key c-mode-base-map [(control m)] 'align-newline-and-indent)
;;                       (define-key c-mode-base-map [(control m)] 'newline-and-indent))))

;;; dev-cfg.el ends here
