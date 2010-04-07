;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; 2-cygwin.el --- cygwin functions
;; Time-stamp: <2008-09-28 15:50:21 lancer>

;; This assumes that Cygwin is installed in C:\cygwin (the
;; default) and that C:\cygwin\bin is not already in your
;; Windows Path (it generally should not be).
(setq exec-path (cons "d:/cygwin/bin" exec-path))
(setenv "PATH" (concat "d:\\cygwin\\bin;" (getenv "PATH")))
(setenv "PATH" (concat "d:\\cygwin\\usr\\bin;" (getenv "PATH")))
(setenv "PATH" (concat "d:\\cygwin\\usr\\local\\bin;" (getenv "PATH")))
(setenv "MANPATH" "d:/cygwin/user/man;d:/cygwin/usr/local/man")
(setenv "INFOPATH" "d:/cygwin/user/local/info;d:/cygwin/usr/share/info")
(setq Man-header-file-path (quote ("d:/cygwin/usr/include" "d:/cygwin/usr/local/include")))

(setenv "SHELL" "d:\\cygwin\\bin\\bash.exe")

(setq explicit-shell-file-name "bash.exe")

;; ###### ELISP FUNCTIONS ######
(add-hook 'shell-mode-hook 'wcy-shell-mode-hook-func)
;; auto kill shell buffer, when exit
(defun wcy-shell-mode-hook-func  ()
  (set-process-sentinel (get-buffer-process (current-buffer))
;                       'wcy-shell-mode-kill-buffer-on-exit)
                        #'wcy-shell-mode-kill-buffer-on-exit)
  )
(defun wcy-shell-mode-kill-buffer-on-exit (process state)
  (kill-buffer (current-buffer)))

;; auto rename shell name, to open more shell window
(defun wcy-shell-mode-auto-rename-buffer (text)
  (if (eq major-mode 'shell-mode)
      (rename-buffer  (concat "shell: " default-directory) t)))

;;;_ , 处理 cygwin 的文件目录。使用 w3m 时要用。
(defun ywb-convert-to-cygwin-path (path)
  (concat "file:///cygdrive/" (substring path 0 1) (substring path 2)))
(defun ywb-convert-cygwin-path (path)
  (setq path (substring path 17))
  (concat (substring path 0 1) ":" (substring path 1)))

;; Let Emacs recognize Cygwin's path names, such as /cygdrive/c
;; (require 'cygwin-mount)
;; (cygwin-mount-activate)

