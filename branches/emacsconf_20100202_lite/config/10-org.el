;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; 10-org.el --- ilisp settings
;; Time-stamp: <2010-02-02 06:08:35 jqian>

;;基本设置
(require 'org)
(require 'org-mouse)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map  "\C-cb" 'org-iswitchb)

;;(setq org-agenda-include-diary t) ;;包含日历纪念日
;;(setq org-log-done t)
(setq org-log-done 'note) ;;完成任务备注
(setq org-log-done 'time) ;;时间戳

(setq org-hide-leading-stars t)

(defcustom org-export-html-style
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"wheer.css\">" ""
  :group 'org-export-html
  :type 'string)

;;TODO设置（相当于汉化）
  (setq  org-todo-keywords
         '((sequence  "计划(t)"  "等待(w@/!)" "开始(s!)" "|" "放弃(c@/!)" "完成(d!)")))
  (setq org-archive-tag "资料库"
        org-closed-string "任务关闭："
        org-comment-string "备注"
        org-deadline-string "截止时间："
        org-quote-string "引用"
        org-scheduled-string "计划时间："
        org-time-stamp-custom-formats (quote ("<%Y年%m月%d日 %A>" . "<%Y年%m月%d日 %A %H:%M>"))
        org-todo-interpretation (quote sequence))

;; OrgMode & Remember
(unless (fboundp 'remember-mode)
  (add-to-list 'load-path "~/.emacs.d/site-lisp/remember-2.0"))

(require 'remember)
;;; (org-remember-insinuate)
(setq  org-directory  "~/org/")
(setq  org-default-notes-file  (concat  org-directory  "/notes.org"))
(define-key  global-map  "\C-cr"  'org-remember)
(setq  org-remember-templates
 '(("工作计划"  ?t  "* 计划 %^{标题} %^g\n       %?     %i\n" "~/org/Task.org"  "新任务")
   ("私人安排"  ?g  "* %^{标题} %^g\n       %?     %i\n   引用自：%a" "~/org/Personal.org"  "新安排")
   ("日记"  ?d  "* %u %^{主题}\n  %?\n   %i\n\n     引用自：%a" "~/org/Journal.org")
   ("学习"  ?x  "* %u %^{主题}\n  %?\n   %i\n\n     引用自：%a" "~/org/Study.org")
   ("项目"  ?s  "* %^{标题}\n  %?\n   %i\n\n     引用自：%a" "~/org/Project.org"  "新资料")))

;; If you are, like me, missing the function org-remember-insinuate, try the following
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;;test
;;(require 'org-export-freemind-install)

;;方便查看
(setq org-agenda-sorting-strategy
  '((agenda priority-down time-up)
    (todo priority-down category-keep)
    (tags priority-down category-keep)))
(setq org-todo-keyword-faces
      '(("等待" . (:foreground "gray" :weight bold))))
