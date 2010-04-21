;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; common-cfg.el ---
;; Time-stamp: <2010-04-21 20:00:03 Wednesday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: common-cfg.el,v 0.0 2010/03/10 10:47:21 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

;; Customized keywords
(dolist (mode '(c-mode c++-mode java-mode lisp-mode emacs-lisp-mode lisp-interaction-mode sh-mode sgml-mode))
  (font-lock-add-keywords mode
                          '(("\\<\\(FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face prepend)
                            ("\\<\\(DONE\\):" 1 font-lock-doc-face t)
                            ;; highlight too long lines
                            ("^[^\n]\\{120\\}\\(.*\\)$" 1 font-lock-warning-face t)
                            ("(\\|)" . beautiful-blue-face)
                            ("\\[\\|]" . yellow-face)
                            ("<\\|>" . cyan-face)
                            ("{\\|}" . green-face))))

(set-default 'auto-mode-alist
             (append '(("\\.\\(php\\|inc\\)\\'" . php-mode)
                       ("\\(Makefile\\|\\.mak\\)" . makefile-mode)
                       ("\\.p[lm]\\'" . cperl-mode)
                       ("\\.\\(cl\\|li?sp\\)\\'" . lisp-mode)
                       ("\\.css\\'" . css-mode)
                       ("\\.html\\'" . html-mode)
                       ("\\.js\\'" . js2-mode)
                       ("\\.\\(jpg\\|png\\|gif\\)\\'" . image-mode))
                     auto-mode-alist))

(add-hook 'Info-mode-hook
          (lambda ()
            (define-key Info-mode-map '[down] 'pager-row-down)
            (define-key Info-mode-map (kbd "M-n") 'pager-row-down)
            (define-key Info-mode-map '[up] 'pager-row-up)))

(add-hook 'Man-mode-hook
          (lambda ()
            (define-key Man-mode-map '[down] 'pager-row-down)
            (define-key Man-mode-map (kbd "M-n") 'pager-row-down)
            (define-key Man-mode-map (kbd "M-p") 'pager-row-up)
            (define-key Man-mode-map '[up] 'pager-row-up)))

;; clean trailing whitespaces automatically
(defun my-trailing-whitespace-hook ()
  (when (member major-mode (list 'c++-mode
                                 'c-mode
                                 'emacs-lisp-mode))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-trailing-whitespace-hook)

;; untabify some modes
(defun my-untabify-hook ()
  (when (member major-mode (list 'c++-mode
                                 'c-mode
                                 'emacs-lisp-mode))
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'my-untabify-hook)

(provide 'common-cfg)

;;; common-cfg.el ends here
