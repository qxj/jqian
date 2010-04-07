;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; common-cfg.el ---
;; Time-stamp: <2010-03-11 11:44:54 Thursday by julian>
;; Created: 2010 Julian Qian
;; Version: $Id: webdev-cfg.el,v 0.0 2010/03/11 12:13:14 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

;;; css-mode
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)

;;; javascript
(autoload 'js2-mode "js2-mode" nil t)
(setq js2-use-font-lock-faces t)
(setq js2-indent-on-enter-key t)
(setq js2-basic-offset 2)

;;; webdev-cfg.el ends here
