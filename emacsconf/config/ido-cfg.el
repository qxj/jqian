;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; ido-cfg.el --- fast switch buffers
;; Time-stamp: <2010-06-18 14:19:46 Friday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: ido-cfg.el,v 0.0 2010/03/10 11:33:48 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))


(require 'ido)
(ido-mode 1)

(setq ido-enable-regexp t
      ido-everywhere t)

;; use normal find-file function for ftp files
(setq ido-slow-ftp-host-regexps '(".*"))
;; don't search for other directorys
(setq ido-work-directory-list-ignore-regexps '(".*"))
(setq ido-save-directory-list-file (concat my-temp-dir "emacs.ido-last"))

(setq ido-ignore-buffers
      '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
        "^\\*Shell*" "^\\*CEDET" "^\\*Customize" "^\\*Ibuffer" "^\\*.*Log\\*$"
        "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-" "^\\*Kill"
        "^\\*Backtrace" "^\\*grep" "^\\*Bookmark" "\\-preprocessed\\*"
        "_region_" " output\\*$" "^TAGS$" "^\\*Ido" "^\\*GTAGS" "^\\*Minibuf")
      ido-ignore-directories
      '("\\`auto/" "\\.prv/" "\\`CVS/" "\\`\\.\\./" "\\`\\./" "^\\.")
      ido-ignore-files
      '("\\`auto/" "\\.prv/" "_region_" "\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "^\\."))

;; visit with dired also push the diretory to `ido-work-directory-list'
(defadvice ido-file-internal (after ido-dired-add-work-directory)
  (when (eq ido-exit 'dired)
    (ido-record-work-directory (expand-file-name default-directory))))
(ad-activate 'ido-file-internal)
;; push the most used directory to `ido-work-directory-list'
(mapc (lambda (dir)
        (add-to-list 'ido-work-directory-list
                     (expand-file-name dir)))
      '("~/.emacs.d/"
        "~/.emacs.d/config/"
        "~/projects/"
        "~/temp/"
        "~/"))

(provide 'ido-cfg)

;;; ido-cfg.el ends here
