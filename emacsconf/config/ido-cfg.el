;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; ido-cfg.el --- fast switch buffers
;; Time-stamp: <2010-03-11 17:34:46 Thursday by julian>
;; Created: 2010 Julian Qian
;; Version: $Id: ido-cfg.el,v 0.0 2010/03/10 11:33:48 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))


(require 'ido)
(ido-mode 1)
;; use normal find-file function for ftp files
(setq ido-slow-ftp-host-regexps '(".*"))
;; don't search for other directorys
(setq ido-work-directory-list-ignore-regexps '(".*"))
(setq ido-save-directory-list-file (concat my-temp-dir "emacs.ido-last"))

(setq ido-ignore-buffers
      '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
        "^\\*Shell*" "^\\*CEDET" "^\\*Customize" "^\\*Ibuffer" "^\\*.*Log\\*$"
        "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-" "^\\*Kill"
        "^\\*Backtrace" "^\\*grep"
        "_region_" " output\\*$" "^TAGS$" "^\*Ido")
      ido-ignore-directories
      '("\\`auto/" "\\.prv/" "\\`CVS/" "\\`\\.\\./" "\\`\\./")
      ido-ignore-files
      '("\\`auto/" "\\.prv/" "_region_" "\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./"))

(provide 'ido-cfg)

;;; ido-cfg.el ends here
