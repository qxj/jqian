;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; assist-cfg.el --- some assistant functions
;; Time-stamp: <2010-07-13 17:05:54 Tuesday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: assit-cfg.el,v 0.0 2010/04/21 11:20:42 jqian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))


;;{{{ fold mode
;;; (setq fold-mode-prefix-key "\C-c\C-o")
;;; (setq fold-autoclose-other-folds nil)
;;; (require 'fold nil t)
;;; (when (featurep 'fold)
;;;   (add-hook 'find-file-hook 'fold-find-file-hook t))
;;}}}

;;{{{ browser kill ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
;;}}}

;;; {{{ template
(require 'template)
(template-initialize)

(setq template-default-directories (list my-template-dir))
(add-to-list 'template-find-file-commands 'ido-exit-minibuffer)

;; (defvar last-template nil "Recently used template file")
;; (defun my-template-expand-template (template)
;;   "Expand template file"
;;   (interactive
;;    (list
;;     (read-file-name
;;      (if last-template (format "Please specify templete file(defautly %s): " last-template) "Please specify template file: ")
;;      (concat my-emacs-path "templates") last-template t)))
;;   (template-expand-template template)
;;   (setq last-template template))
;; (dolist (map (list emacs-lisp-mode-map
;;                    c-mode-base-map
;;                    makefile-mode-map
;;                    makefile-automake-mode-map
;;                    sh-mode-map
;;                    text-mode-map))
;;   (define-key map (kbd "C-c T") 'my-template-expand-template)
;;   (define-key map (kbd "C-c C-t") 'template-expand-template))
;;; }}}

;;{{{ display SPC and TAB charactors
(require 'blank-mode)
;;}}}

;;{{{ A visual table editor, very cool
(autoload 'table-insert "table" "WYGIWYS table editor")
;;}}}

;;{{{ ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;}}}

;;{{{ winner mode: restore the last windows arrangement
(winner-mode 1)
;;}}}

;;{{{ uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;;}}}

;;{{{ kill buffers every 24 hr
;; (require 'midnight)
;;}}}

;;{{{ find file at point
(require 'ffap)
(setq ffap-c-path (append ffap-c-path system-head-file-dir user-head-file-dir))
;;}}}


(provide 'assit-cfg)

;;; assit-cfg.el ends here
