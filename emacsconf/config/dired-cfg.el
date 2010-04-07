;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; dired-cfg.el --- dired mode
;; Time-stamp: <2010-01-29 17:00:25 julian>

;; Setting for dired
(setq dired-recursive-copies 'always)
;(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)

;; C-x C-j open the directory of current buffer
(global-set-key (kbd "C-x C-j")
                (lambda ()
                  (interactive)
                  (if (buffer-file-name)
                      (dired default-directory))))

;;{{{ dired-x & wdired
;; dired-x
(require 'dired-x)
;; dired-single
(require 'dired-single)
(add-hook 'dired-load-hook 'wcy-dired-load-hook)
(add-hook 'dired-mode-hook 'wcy-dired-mode-hook)
(defun wcy-dired-mode-hook  ()
  ;; Set buffer-local variables here.  For example:
  (dired-omit-mode 1)
  (setq dired-omit-files-p t)
  ;; jun
  (setq dired-backup-overwrite 'always)
  (setq dired-listing-switches "-Aol")
  (setq dired-no-confirm
      '(byte-compile chgrp chmod chown compress copy delete hardlink load move print shell symlink uncompress))
  (define-key dired-mode-map (kbd "RET") 'joc-dired-single-buffer)
  (define-key dired-mode-map (kbd "^") '(lambda () (interactive) (joc-dired-single-buffer "..")))
  (define-key dired-mode-map (kbd "<M-up>") '(lambda () (interactive) (joc-dired-single-buffer "..")))
  ;;

  
  (define-key dired-mode-map (kbd "[" ) 'backward-page)
  (define-key dired-mode-map (kbd "]" ) 'forward-page)
;;   (define-key dired-mode-map (kbd "<M-up>" ) 'dired-up-directory)
;;   (define-key dired-mode-map (kbd "ESC <up>" ) 'dired-up-directory)
  (define-key dired-mode-map (kbd "<down-mouse-1>" ) 'ignore)
  (define-key dired-mode-map (kbd "<double-mouse-1>" ) 'wcy-dired-mouse-find-file)
  (substitute-key-definition 'dired-find-file 'wcy-dired-find-file dired-mode-map)
  (substitute-key-definition 'dired-mouse-find-file-other-window 'wcy-dired-mouse-find-file-other-window dired-mode-map)
  (substitute-key-definition 'dired-next-line 'dired-next-file-line dired-mode-map)
  (substitute-key-definition 'dired-previous-line 'dired-previous-file-line dired-mode-map)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.[^.]*$")
  ;; remove .pdf .dvi from the default value.
  (setq dired-omit-extensions '("CVS/" ".o" "~" ".bin" ".lbin"
                                ".fasl" ".ufsl" ".a" ".ln" ".blg"
                                ".bbl" ".elc" ".lof" ".glo" ".idx"
                                ".lot" ".fmt" ".tfm" ".class" ".fas" ".lib" ".x86f"
                                ".sparcf" ".lo" ".la" ".toc" ".log" ".aux" ".cp" ".fn" ".ky" ".pg"
                                ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs"
                                ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky"
                                ".kys" ".pg" ".pgs" ".tp"
                                ".tps" ".vr" ".vrs"))
  (define-key dired-mode-map (kbd "/")  'dired-omit-expunge)
  ;; sort something
  (make-local-variable  'dired-sort-map)
  (setq dired-sort-map (make-sparse-keymap))
  (define-key dired-mode-map "s" dired-sort-map)
  (define-key dired-sort-map "s" '(lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
  (define-key dired-sort-map "x" '(lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
  (define-key dired-sort-map "t" '(lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
  (define-key dired-sort-map "n" '(lambda () "sort by Name" (interactive) (dired-sort-other (concat dired-listing-switches
                                                                                                    ""))))

  ;; hide mode user group information colummn
  nil)
;;; make the execuatable file with different color
(defun wcy-dired-load-hook ()  
  (load "dired-x")
  (setq dired-font-lock-keywords
        (append dired-font-lock-keywords
                (list (list dired-re-exe
                            '(".+" (dired-move-to-filename) nil (0 font-lock-comment-face)))
                      (cons dired-re-perms font-lock-constant-face)
                      ))))

;; list the directoryies first, from emacswiki
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header  
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'sof/dired-sort)

 
;; don't like that dired-next/previous-line moves to "not useful" lines, like the empty ones between inserted subdirectores, so
;; I came up with this simple defun that suit my needs better.

 (defun dired-next-file-line ()
   "Moves to the next dired line that have a file or directory name on
 it"
   (interactive)
   (call-interactively 'dired-next-line)
   (if (not (dired-move-to-filename))
       (dired-next-file-line)))

(defun dired-previous-file-line ()
   "Moves to the previous dired line that have a file or directory name
 on it"
   (interactive)
   (call-interactively 'dired-previous-line)
   (if (not (dired-move-to-filename))
       (dired-previous-file-line)))

;; color dir & files
;; (eval-after-load "dired" '(require 'dired+))
;; (eval-after-load "dired-x" '(require 'dired+))
;; (eval-after-load "dired" '(require 'dired-sort-menu))
;; (eval-after-load "dired-x" '(require 'dired-sort-menu))

(defvar wcy-dired-find-file-cmds-alist
  `(("\\.xls$" "start" file-name)
    ("\\.doc$" "start" file-name)
    ("\\.pdf$" "start" file-name)
    ("\\.rar$" "start" file-name)
    ("\\.zip$" "start" file-name)
    ("\\.ppt$" "start" file-name))
  "第一个字符串表示过滤文件的正则表达式。 表示那些文件不用
emacs 打开。而是用外部程序打开。

后面的内容对于 windows 系统没有什么
用，在 windows 系统下会根据注册的文件类型打开文件。

在 Unix 系统下， 后面的元素指明的打开某一个文件的方式。
符号 file-name 表示文件的全名。" )

(defun wcy-open-file-via-extension(&optional file-name)
  (interactive "fOpen file:")
  (let ((cmd (cdr (find-if (lambda (x)
                             (string-match (car x) file-name))
                           wcy-dired-find-file-cmds-alist))))
    (if (not cmd)
        nil
      (setq cmd
            (mapcar (lambda (s)
                      (if (eq s 'file-name)
                          file-name
                        s))
                    cmd))
      (cond
       ((eq system-type 'windows-nt)
        (w32-shell-execute 'open file-name))
       (t
        (let* ((process (apply 'start-process-shell-command
                               "no name"
                               (get-buffer-create "*open file*")
                               cmd))
               (status (process-status process)))
          (princ (apply 'concat "start process: " cmd))))))))


(defun wcy-dired-find-file()
  (interactive)
  (let* ((file-name (file-name-sans-versions (dired-get-filename) t)))
    (when (not (and (file-exists-p file-name)
                    (file-regular-p file-name)
                    (wcy-open-file-via-extension file-name)))
      (dired-find-file))))
(defun wcy-dired-mouse-find-file-other-window (event)
  "In dired, visit the file or directory name you click on."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))))
    (when (not (wcy-open-file-via-extension file))
      (select-window (posn-window (event-end event)))
      (find-file-other-window (file-name-sans-versions file t)))))
(defun wcy-dired-mouse-find-file (event)
  "In dired, visit the file or directory name you click on."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))
        (when (not (wcy-open-file-via-extension file))
          (select-window (posn-window (event-end event)))
          (dired-find-file))))))
(defun wcy-t-test(e)
  (interactive "e")
  (print "ok"))

(defvar wcy-dired-mode-hide-column-regex
  "^\\s-\\{2\\}[drwx-]\\{10\\}\\s-+[0-9]+\\s-+\\sw+\\s-+\\sw+"
  "doc")
(defun wcy-dired-mode-hide-column ()
  (interactive)
  (when (eq major-mode 'dired-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward wcy-dired-mode-hide-column-regex nil t nil)
          (let ((o (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put o 'invisible t)
            (overlay-put o 'priority 0)
            ;;(overlay-put o 'face '(foreground-color . "red"))
            (overlay-put o 'id 'wcy-dired-mode-hide-column)))))))

(defun wcy-dired-mode-show-column ()
  (interactive)
  (when (eq major-mode 'dired-mode)
    (mapc (lambda (o)
            (if (eq (overlay-get o 'id) 'wcy-dired-mode-hide-column)
                (delete-overlay o)))
          (overlays-in (point-min) (point-max)))))

;; wdired
(require 'wdired)
(autoload 'wdired-change-to-wdired-mode "wdired")
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; (define-key dired-mode-map
;;   [menu-bar immediate wdired-change-to-wdired-mode]
;;   '("Edit File Names" . wdired-change-to-wdired-mode))

(defun my-dired-long-lines ()
  (toggle-truncate-lines t))
(add-hook 'dired-after-readin-hook 'my-dired-long-lines)


;;;;;; Dired Association
;;This allows "X" in dired to open the file using the explorer settings.      
;;From TBABIN(at)nortelnetworks.com
;;ToDo: adapt mswindows-shell-execute() for XEmacs or use tinyurl shell exec
;; (when (and (string-match "GNU" (emacs-version))
;;            (string= window-system "w32"))
;;   (defun dired-custom-execute-file (&optional arg)
;;     (interactive "P")
;;     (mapcar #'(lambda (file)
;;                 (w32-shell-execute "open" (convert-standard-filename file)))
;;             (dired-get-marked-files nil arg)))
;;   (defun dired-custom-dired-mode-hook ()
;;     (define-key dired-mode-map "X" 'dired-custom-execute-file))
;;   (add-hook 'dired-mode-hook 'dired-custom-dired-mode-hook))
;; 以下是当你在 dired buffer 里按 ! 时的情景。

;; 这个和 shell-command 不同之处在于，它是后台异步生成进程。可以避免生成有点烦人
;; 的 output buffer. (似乎前面有人问过)
;; (defun my-start-process-shell-command (cmd)
;;   "Don't create a separate output buffer."
;;   (start-process-shell-command cmd nil cmd))

;; ;; redefine this function to disable output buffer.
;; (defun dired-run-shell-command (command)
;;   (let ((handler
;;          (find-file-name-handler (directory-file-name default-directory)
;;                                  'shell-command)))
;;     (if handler (apply handler 'shell-command (list command))
;;       (my-start-process-shell-command command)))
;;   ;; Return nil for sake of nconc in dired-bunch-files.
;;   nil)

;; 有点像一些文件管理器里的双击功能。我们要做的是：! RET.
;; (setq dired-guess-shell-alist-user
;;       `((,(regexp-opt
;;            '(".mp3" ".ogg" ".wav" ".avi" ".mpg" ".dat" ".wma" ".asf" ".rmvb"))
;;          (progn (emms-add-file (dired-get-filename))
;;                 (keyboard-quit)))

;;         (,(regexp-opt
;;            '(".gif" ".png" ".bmp" ".jpg" ".tif"))
;;          "xzgv")

;;         (".htm[l]?" "firefox")
;;         (".dvi" "xdvi")
;;         (".rar" "unrar x")
;;         (".pdf" "xpdf")
;;         (".ppt" "openoffice")
;;         ))

;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (setq dired-guess-shell-alist-user
;;                   (list (list "\\.ps\\'" "gsview32")
;;                         (list "\\.pdf\\'" "gsview32")
;;                         (list "\\.ppt\\'" "ppt")
;;                         (list "\\.doc\\'" "word")
;;                         (list "\\.chm\\'" "hh")
;;                         (list "\\.mht\\'" "ie")
;;                         (list "\\.htm*" "ie")
;;                         (list "\\.mp3" "foobar2000")
;;                         (list "\\.rm\\'" "mplayer")
;;                         (list "\\.rmvb\\'" "mplayer")
;;                         (list "\\.avi\\'" "mplayer")))))
;;}}}
