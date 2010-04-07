;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; cygwin-cfg.el --- cygwin functions
;; Time-stamp: <2010-01-28 15:48:26 julian>

;; This assumes that Cygwin is installed in C:\cygwin (the
;; default) and that C:\cygwin\bin is not already in your
;; Windows Path (it generally should not be).
(setq exec-path (cons (concat my-cygwin-dir "bin") exec-path))
(setenv "PATH" (concat (concat my-cygwin-dir "bin;") (getenv "PATH")))
(setenv "PATH" (concat (concat my-cygwin-dir "usr/bin;") (getenv "PATH")))
(setenv "PATH" (concat (concat my-cygwin-dir "usr/local/bin;") (getenv "PATH")))
(setq Man-header-file-path (quote ((concat my-cygwin-dir "usr/include")
                                   (concat my-cygwin-dir "usr/local/include"))))

(setenv "SHELL" (concat my-cygwin-dir "bin/bash.exe"))

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

