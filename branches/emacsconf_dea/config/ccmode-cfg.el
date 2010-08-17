;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; ccmode-cfg.el --- enhancement for c/c++ development
;; Time-stamp: <2010-06-30 13:37:58 Wednesday by jqian>
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

(setq-default comment-column 40)
(setq compilation-scroll-output t)

(defun my-quick-compile ()
  "A quick compile funciton for C++"
  (interactive)
  (compile (concat "g++ -g -pg " (buffer-name (current-buffer)))))

(defconst my-c-style
  ;; Always indent c/c++ sources, never insert tabs
  '((c-basic-offset             . 4)
    (c-tab-always-indent        . t)
    ;; Offset for line only comments
    (c-comment-only-line-offset . 0)
    ;; Controls the insertion of newlines before and after braces.
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    ;; Controls the insertion of newlines before and after certain colons.
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    ;; List of various C/C++/ObjC constructs to "clean up".
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    ;; Association list of syntactic element symbols and indentation offsets.
    (c-offsets-alist            . ((topmost-intro . 0)
                                   (arglist-close . c-lineup-arglist)
                                   (substatement  . +)
                                   (substatement-open . 0)
                                   (case-label        . +)
                                   (block-open        . 0)
                                   (access-label      . -)
                                   (label             . -)
                                   (inclass           . +)
                                   (inline-open       . 0)
                                   ;; (c-echo-syntactic-information-p . t)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    )
  "My C/C++/ObjC Programming Style")

;; customisation of cc-mode
(defun my-c-mode-common-hook ()
  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '++)
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  ;; (c-toggle-auto-hungry-state 1)

  (c-set-offset 'substatement-open 0)
  ;; (c-set-style "bsd")
  (c-add-style "Personal" my-c-style t)

  ;; (c-toggle-auto-hungry-state 0) ;; auto return after semicolon
  ;; minor modes
  (auto-fill-mode 1)
  (c-turn-on-eldoc-mode)
  ;; local keys
  (define-key c-mode-base-map (kbd "<return>") 'newline-and-indent)
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
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
