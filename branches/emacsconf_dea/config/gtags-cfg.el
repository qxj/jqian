;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; gtags-cfg.el ---
;; Time-stamp: <2010-04-21 19:30:25 Wednesday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: gtags-cfg.el,v 0.0 2010/03/09 09:12:27 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))


;;{{{ gnu global
(autoload 'gtags-mode "gtags" "" t)

(dolist (map (list c-mode-hook c++-mode-hook))
  (add-hook 'map '(lambda () (gtags-mode 1))))
;;}}}

;;{{{ key bind for gtags-mode
(gtags-mode 1)
(define-prefix-command 'my-gtags-map)
(global-set-key (kbd "C-c g") 'my-gtags-map)
(define-key gtags-mode-map (kbd "C-c g v") 'gtags-visit-rootdir)
(define-key gtags-mode-map (kbd "C-c g t") 'gtags-find-tag)
(define-key gtags-mode-map (kbd "C-c g o") 'gtags-find-tag-other-window)
(define-key gtags-mode-map (kbd "C-c g r") 'gtags-find-rtag)
(define-key gtags-mode-map (kbd "C-c g s") 'gtags-find-symbol)
(define-key gtags-mode-map (kbd "C-c g p") 'gtags-find-pattern)
(define-key gtags-mode-map (kbd "C-c g g") 'gtags-find-with-grep)
(define-key gtags-mode-map (kbd "C-c g i") 'gtags-find-with-idutils)
(define-key gtags-mode-map (kbd "C-c g f") 'gtags-find-file)
(define-key gtags-mode-map (kbd "C-c g a") 'gtags-parse-file)
(define-key gtags-mode-map (kbd "C-c g b") 'yp-gtags-append)
;;}}}

(defun yp-gtags-append ()
  (interactive)
  (if gtags-mode
      (progn
        (message "start to global -u")
        (start-process "gtags-name" "*gtags-var*" "global" "-u"))))


(provide 'gtags-cfg)

;;; gtags-cfg.el ends here
