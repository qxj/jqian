;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; php-cfg.el --- php-mode settings
;; Time-stamp: <2010-01-28 16:30:08 julian>

;;_+ php-mode
(require 'php-mode)

(if mswin
    (progn
      (define-key php-mode-map
        [menu-bar php php-debug]
        '("PHP Debug" . php-debug))

      (defun php-debug ()
        (interactive)
        (shell-command
         (concat "D:/WebEngine/php-5.2.1-Win32/php.exe -l \""
                 (buffer-file-name)
                 "\""))
        )

      (define-key php-mode-map
        [menu-bar php php-run]
        '("Run PHP" . php-run))

      (defun php-run ()
        (interactive)
        (shell-command
         (concat "D:/WebEngine/php-5.2.1-Win32/php.exe -q \""
                 (buffer-file-name)
                 "\"")))

      (defun my-php-mode()
        ;; 将回车代替C-j的功能，换行的同时对齐
        (define-key php-mode-map [return] 'newline-and-indent)
        (define-key php-mode-map [(control c) (r)] 'php-run)
        (define-key php-mode-map [(control c) (d)] 'php-debug)
        (interactive)
        ;; 设置php程序的对齐风格
        (c-set-style "K&R")
        ;; 自动模式，在此种模式下当你键入{时，会自动根据你设置的对齐风格对齐
        ;;(c-toggle-auto-state)
        ;; 此模式下，当按Backspace时会删除最多的空格
        (c-toggle-hungry-state)
        ;; TAB键的宽度设置为4
        (setq c-basic-offset 4)
        ;; 在菜单中加入当前Buffer的函数索引
        (imenu-add-menubar-index)
        ;; 在状态条上显示当前光标在哪个函数体内部
        (which-function-mode))

      (add-hook 'php-mode-hook 'my-php-mode)))
