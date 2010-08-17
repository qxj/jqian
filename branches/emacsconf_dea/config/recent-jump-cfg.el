;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; recent-jump-cfg.el ---
;; Time-stamp: <2010-05-11 11:12:36 Tuesday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: recent-jump-cfg.el,v 0.0 2010/01/29 10:01:02 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

(require 'recent-jump)
(require 'recent-jump-small)
;; (setq recent-jump-threshold 4)
;; (setq recent-jump-ring-length 10)

(setq rj-mode-line-format nil)
(setq rjs-mode-line-format nil)

(recent-jump-mode)
(recent-jump-small-mode)


;; (global-set-key (kbd "M-9") 'recent-jump-jump-backward)
;; (global-set-key (kbd "M-0") 'recent-jump-jump-forward)

(let ((map global-map)
      (key-pairs
       `(("<M-left>"   recent-jump-backward)
         ("<M-right>"   recent-jump-forward)
         ("C-M-9" recent-jump-small-backward)
         ("C-M-0" recent-jump-small-forward))))
  (apply-define-key map key-pairs))

(provide 'recent-jump-cfg)

;;; recent-jump-cfg.el ends here
