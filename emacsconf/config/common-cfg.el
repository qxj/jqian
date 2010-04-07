;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; common-cfg.el ---
;; Time-stamp: <2010-03-11 17:44:00 Thursday by julian>
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

;; clean trailing whitespaces automatically
(setq my-trailing-whitespace-modes '(c++-mode
                                     c-mode
                                     haskell-mode
                                     emacs-lisp-mode
                                     lisp-mode
                                     scheme-mode
                                     erlang-mode))

(defun my-trailing-whitespace-hook ()
  (when (member major-mode my-trailing-whitespace-modes)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-trailing-whitespace-hook)

;; untabify some modes
(setq my-untabify-modes '(haskell-mode
                          emacs-lisp-mode
                          lisp-mode
                          scheme-mode
                          erlang-mode))
(defun my-untabify-hook ()
  (when (member major-mode my-untabify-modes)
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'my-untabify-hook)

(provide 'common-cfg)

;;; common-cfg.el ends here
