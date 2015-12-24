;; -*- mode: Emacs-Lisp -*-

;; python programming environment

;;; scripts setting
(deh-package python
  :config
  ;;; Start an inferior python process, wordaround for eldoc error
  ;; (let ((python-cmd (if (executable-find python-shell-interpreter)
  ;;                       (python-shell-parse-command))))
  ;;   (if python-cmd (run-python python-cmd t nil)))

  (deh-add-hook python-mode-hook
    (my/prog-mode-hook)
    ;; Avoid mysterious "Arithmetric error", bug:
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=15975
    (setq python-indent-offset 4)
    (when (boundp 'rope-completions) (ac-ropemacs-initialize))
    (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p nil t)
    )

  )

(deh-package doctest-mode)

;; # Preparation:
;; 1. $ sudo pip install jedi epc sexpdata
;; 2. emacs-epc, emacs-deferred, emacs-ctable (copy .el into `load-path`)
;; 3. download and uncompress emacs-jedi, then `make requirements` (depends on virtualenv)
;;
;; http://tkf.github.io/emacs-jedi/released/#install
;;
(deh-package jedi
  :disabled
  :commands (jedi:setup jedi-direx:pop-to-buffer)
  :init
  (setq jedi:setup-keys t             ;set it before jedi loaded
        jedi:complete-on-dot t
        jedi:tooltip-method nil
        jedi:use-shortcuts t
        jedi:install-imenu t)
  (add-hook 'python-mode-hook 'jedi:setup))

;;# Preparation:
;;
;; 1. install pymacs, rope, ropemode, ropemacs one by one.
;; 2. put pymacs.el (pymacs) into `load-path'.
;; 3. check whether pymacs is working.
;;
;; http://pymacs.progiciels-bpi.ca/pymacs.html#check-if-pymacs-would-work
;;
;;# Useful features of ropemacs:
;;
;; 1. full code completion of modules/classes/methods (M-/)
;; 2. instant documentation for element under cursor (C-c d)
;; 3. jump to modules/classes/methods definition (C-c g)
;; 4. refactor (rename: C-c r r)
;; 5. list all occurences of a name in your entire project
;;
;; http://rope.sourceforge.net/ropemacs.html
;;
(deh-package pymacs
  :disabled
  :commands (pymacs-apply pymacs-call pymacs-eval pymacs-exec pymacs-load)
  :config
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-confirm-saving nil
        ropemacs-enable-autoimport t)
  (ropemacs-mode t)
  ;; (deh-package pycomplete)
  )

;;# Preparation:
;; 1. $ sudo pip install virtualenv ipython autopep8 flake8 jedi
;;
;; http://segmentfault.com/a/1190000004165173
;;
(deh-package elpy
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)

  (elpy-enable)
  (elpy-use-ipython)
  )

(deh-package py-autopep8
  :if (executable-find "autopep8")
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(deh-package ein
  :commands (ein:notebooklist-open
             ein:notebooklist-new-notebook
             ein:junk-new)
  )
