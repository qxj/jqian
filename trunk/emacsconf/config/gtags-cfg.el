;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; gtags-cfg.el ---
;; Time-stamp: <2010-03-11 16:34:27 Thursday by julian>
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
(dolist (map (list
              c-mode-base-map
              php-mode-map
              python-mode-map))
  (apply-define-key
   map
   `(
     ("C-c g v" gtags-visit-rootdir)  ;; search directory
     ("C-c g t" gtags-find-tag)  ;; function define
     ("C-c g o" gtags-find-tag-other-window)
     ("C-c g r" gtags-find-rtag)  ;; function references
     ("C-c g s" gtags-find-symbol)  ;; symbol define
     ("C-c g p" gtags-find-pattern)
     ("C-c g g" gtags-find-with-grep)
     ("C-c g i" gtags-find-with-idutils)
     ("C-c g f" gtags-find-file)  ;; find file in project
     ("C-c g a" gtags-parse-file)  ;; list defines
     ("C-c g b" yp-gtags-append)  ;; update TAGS file
     )))
;;}}}

(defun yp-gtags-append ()
  (interactive)
  (if gtags-mode
      (progn
        (message "start to global -u")
        (start-process "gtags-name" "*gtags-var*" "global" "-u"))))


(provide 'gtags-cfg)

;;; gtags-cfg.el ends here
