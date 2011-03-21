;; -*- coding: utf-8 mode: emacs-lisp -*-
;;

;; press ^ in *group buffer*, list all servers
;; press / o in *summary buffer*, list all old messages

(setq gnus-startup-file "~/Gnus/newsrc"
      gnus-default-directory "~/Gnus"
      gnus-home-directory "~/Gnus"
      gnus-dribble-directory "~/Gnus"
      gnus-directory "~/Gnus/News"
      gnus-article-save-directory "~/Gnus/News"
      gnus-kill-files-directory "~/Gnus/News/trash"
      gnus-agent-directory "~/Gnus/News/agent"
      gnus-cache-directory "~/Gnus/News/cache"
      gnus-cache-active-file "~/Gnus/News/cache/active"
      message-directory "~/Gnus/Mail"
      message-auto-save-directory "~/Gnus/Mail/drafts"
      mail-source-directory "~/Gnus/Mail/incoming"
      mail-archive-file-name "~/Gnus/outgoing"
      nnmail-message-id-cache-file "~/Gnus/nnmail-cache"
      nnml-newsgroups-file "~/Gnus/Mail/newsgroup"
      nntp-marks-directory "~/Gnus/News/marks"
      mml-default-directory "~/Gnus/Attachment")

;;; Backends

(setq gnus-interactive-exit nil)

(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-topics
      gnus-subscribe-options-newsgroup-method 'gnus-subscribe-topics)

(setq gnus-nov-is-evil nil
      gnus-asynchronous t
      gnus-fetch-old-headers 'some
      gnus-use-cross-reference nil)

;; Newsgroup
(setq gnus-select-method '(nntp "localhost")) ; for leafnode

;; (setq gnus-select-method '(nntp "news.cn99.com"))

(setq gnus-default-subscribed-newsgroups
      '("cn.bbs.comp.emacs"
        "cn.bbs.comp.linux"
        "cn.bbs.comp.lang.python"
        "gnu.emacs.help"
        ))

;; Use `B' in Group buffer to subscribe to some groups from a different
;; newsgroup server.

;; nntp: news.gmane.org, aioe.cjb.net, news.mozilla.org

(setq gnus-secondary-select-methods
      `((nnml "")
        ;; (nnfolder "")
        (nntp "news.newsfan.net")

        ;; gmail
        (nnimap "gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnimap-authinfo-file "~/.authinfo"))))

;; Store encrypted gmail login info in ~/.authinfo.gpg, eg:
;; machine imap.gmail.com login xxx@gmail.com password yyy port 993
;; machine smtp.gmail.com login xxx@gmail.com password yyy port 587



;;; Receiving Mails

(setq mail-sources
      `(
        ;; pop3 mail setting
        ;; (pop :server "pop3.example.com"
        ;;      :user "username@example.com"
        ;;      :port "110"
        ;;      :password "password"
        ;;      )
        ;; local mail pool
        ;; (file :path "~/Gnus/incoming")
        ))

(eval-after-load "gnus"
  '(progn
     ;; When idle 2 minutes, then check news every 3 minutes.
     (if gnus-plugged
         (gnus-demon-add-handler 'xwl-gnus-group-get-new-news 30 10)
       (gnus-demon-add-handler 'xwl-gnus-group-get-new-news 30 nil))

     ;; FIXME: can i remove this?
     (load "rs-gnus-summary.el")

     ))

(setq my-gnus-important-groups
      '("nnimap+imap.gmail.com:important.now"
        "nnfolder:important.now"
        ))

(defun my-gnus-group-get-new-news ()
  (interactive)
  (if gnus-plugged
      ;; Only get news for groups with a level lower than 4.  This is
      ;; because some levels' updating takes too long time.
      (gnus-group-get-new-news 3)
    (gnus-read-active-file)
    (gnus-get-unread-articles)
    (gnus-group-list-groups))

  (let ((new (apply '+
                    (mapcar (lambda (i)
                              (let ((n (gnus-group-unread i)))
                                (if (numberp n) n 0)))
                            my-gnus-important-groups))))
    (if (zerop new)
        (setq my-mail-notify-string "")
      (setq my-mail-notify-string (format "Mail(%d)" new))
      (my-notify "Gnus" my-mail-notify-string)
      )
    (force-mode-line-update)))


;;; Sending Mails

;; 1) internal

;; starttls.el, pop3.el, starttls/gnutls-bin
;; (require 'starttls)
;; (setq mail-user-agent 'gnus-user-agent)

;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-auth-credentials "~/.authinfo.gpg"
;;       smtpmail-local-domain system-name)

;; 2) msmtp
;; http://www.emacswiki.org/emacs/GnusMSMTP
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp")


;;; Message Mode

(unless (eq system-type 'windows-nt)
  (add-hook 'message-send-hook 'ispell-message)
  (add-hook 'mail-send-hook  'ispell-message)
  (add-hook 'mh-before-send-letter-hook 'ispell-message)
  )

(setq message-sendmail-envelope-from 'header)

(eval-after-load "message"
  '(progn
     (define-key message-mode-map (kbd "ESC TAB") 'bbdb-complete-name)
     (define-key message-mode-map (kbd "<backtab>") 'bbdb-complete-name)))

;; set outgoing coding
(setq mm-coding-system-priorities '(utf-8 gb2312 gbk gb18030 iso-8859-1))

;; One can mail the *Group* buffer, select different posting styles
;; according to group name at point for matching. So better avoiding
;; composing mail by `C-x m', instead, `m' at a proper group line in
;; *Group* buffer.

(global-unset-key (kbd "C-x m"))

;; attachment
(defalias 'mail-header-encode-parameter 'rfc2047-encode-parameter)


;;; Chinese Stuffs

(define-coding-system-alias 'x-gbk 'gb18030)

(eval-after-load "gnus"
  '(progn
     (setq gnus-default-charset 'gbk)
     (setq gnus-group-name-charset-group-alist
           '(("nnrss.*" . utf-8)        ; `G R'
             ("news\\.newsfan\\.net" . gbk)
             (".*" . gb18030)))
     (setq gnus-newsgroup-ignored-charsets
           '(unknown-8bit x-unknown x-gbk gb18030))

     (add-to-list 'gnus-group-charset-alist '("nnrss.*" utf-8))
     (add-to-list 'gnus-group-charset-alist
                  '("\\(^\\|:\\)cn\\>\\|\\<chinese\\>" gbk))

     (setq gnus-summary-show-article-charset-alist
           '((1 . utf-8)
             (2 . big5)
             (3 . gb18030)
             (4 . gbk)
             (5 . gn2312)
             (6 . utf-7)))))


;;; Essential: It's all about groups!

;; 1. List Groups, Nonlist Groups
;; 2. Group Parameters
;; 3. Posting Style Based On Group
;; 4. Split Received Mails Into Groups
;; 5. Expire Groups

;;;; 1. List Groups, Nonlist Groups, Important Groups

;; '((group . to-list) ...)
(setq my-list-table
      (mapcar
       (lambda (i)
         (cons (replace-regexp-in-string "@.*" "" i) i))
       '(
         "bug-gnu-emacs@gnu.org"
         ;; to remove

         ;; "cocoa-dev@lists.apple.com"
         "pongba@googlegroups.com"
         "python-cn@googlegroups.com"
         )))


;;;; 2. Gnus Parameters
(setq gnus-gcc-mark-as-read t)		; mark Gcc mail as read

(setq gnus-parameters
      `(,@(mapcar
           (lambda (i)
             `(,(car i)
               (to-list . ,(cdr i))
               (gcc-self . t)))
           my-list-table)

        ("nnimap+imap.gmail.com.*"
         (gcc-self . t))

        (".*important.*"
         (gcc-self . t))

        ("trash"
         (total-expire . t))))


;;;; 3. Posting Style
(setq mail-signature t)

(setq gnus-posting-styles
      `((".*"
         (name ,user-full-name)
         ,(if (file-exists-p "~/Gnus/xface.png")
              '(face (gnus-convert-png-to-face "~/Gnus/xface.png")))
         (x-url (getenv "WWW_HOME"))
         (organization ,system-name)
         (signature ,(concat "Best Regards,\n" user-full-name))
         (eval (setq message-sendmail-extra-arguments '("-a" "gmail"))))
        (,(regexp-opt (mapcar 'car my-list-table))
         )
        ;; cn.*
        (,(regexp-opt '("cn.fan" "cn.bbs"))
         (address ,(concat "NO.SPAM" user-mail-address)) ; avoid spam
         (eval (list
                (setq message-sendmail-extra-arguments '("-a" "gmail"))
                (setq mm-coding-system-priorities '(gb2312 gbk gb18030 utf-8))))
         ;;(body "")
         )
        ))

;; Keep company mail at the end.
(if (and (fboundp 'my-company-email) (fboundp 'my-company-posting-style))
    (add-to-list 'gnus-posting-styles
                 `(,(regexp-opt (replace-regexp-in-string "@.*" "" my-company-email))
                   ,@(my-company-posting-style)
                   (eval (setq message-sendmail-extra-arguments '("-a" "company"))))))


;;;; 4. Split Received Mails
(setq mail-source-delete-incoming t)

(setq gnus-message-archive-group nil)

(setq gnus-outgoing-message-group
      '(nnml "archive"
             (nnml-directory   "~/Gnus/Mail/archive")
             (nnml-active-file "~/Gnus/Mail/archive/active")
             (nnml-get-new-mail nil)
             (nnml-inhibit-expiry t)))

;; A function that selects a reasonable group for Gcc'ing this mail.
;;(defun MySendedMail ()
;;  (cond ((and gnus-newsgroup-name
;;              (not (message-news-p))
;;              (stringp gnus-newsgroup-name))
;;         gnus-newsgroup-name)
;;        (t ted-default-gcc-group)))
;;;; Use it.
;;(setq gnus-outgoing-message-group "nnml:SendMails")

(setq gnus-outgoing-message-group
      '(lambda nil
         (if (message-news-p)
             "nnml:SendNews"
           "nnml:SendMails")))

(setq nnmail-split-fancy
      `(|
        (from ,(concat ".*"
                       (regexp-opt '("@localhost"
                                     "Cron Daemon"))
                       ".*")
              "local")
        ,@(mapcar (lambda (i)
                    `(any ,(cdr i) ,(car i)))
                  `(,@my-list-table))
        (: my-split-mailing-lists)
        ;; (to ,user-mail-address (: my-notify-important))
        (from ".*@\\(mails.pku.edu.cn\\|pku.org.cn\\)" "pku")
        ;; (any ".*@gmail.com" "gmail")
        (to ".*@newsmth.*" "newsmth")
        ;; maybe junk
        ))

(setq nnmail-treat-duplicates 'delete
      nnmail-crosspost nil
      nnmail-split-methods 'nnmail-split-fancy
      nnmail-split-fancy-match-partial-words t)

(setq nnimap-split-inbox '("INBOX")
      nnimap-split-rule 'nnmail-split-fancy)

(defun my-split-mailing-lists ()
  "e.g., foo@googlegroups.com -> foo, digest from xwl's setting."
  (let ((re (concat "^List-Post:.*<mailto:\\([-a-zA-Z._]+\\)@"
                    (regexp-opt '("googlegroups.com"
                                  "lists.apple.com"
                                  "lists.sourceforge.net"
                                  ;; "gnu.org"
                                  )))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward re nil t)
        (match-string 1)))))

(defun my-notify-important ()
  ;;   (my-shell-command-asynchronously
  ;;     "zenity --info --text \"You've Got Mail \!\" --title \"Gnus\"")
  "important.now")


;;;; 5. Expire Groups

(setq nnmail-expiry-wait-function
      (lambda (group)
        (cond
         ;; email
         ((string-match "nnimap.*" group)
          'never)
         ((string-match "nnml.*" group)
          'never)
         ;; list
         ((string-match (regexp-opt
                         (mapcar 'car `(,@my-list-table)))
                        group)
          14)
         ;; trash
         ((or (string= "trash" group)
              (string= "cc-trash" group))
          3)
         (t 'never))))

;; (setq nnmail-fancy-expiry-targets
;;       '(("Newsgroups" ".+" "")))


;;; Group

(setq gnus-activate-level 5)

(setq gnus-permanently-visible-groups '"nn*")
;; (setq gnus-permanently-visible-groups
;;       (regexp-opt `("blog-life"
;;                     "blog-tech"
;;                     "important.now"
;;                     )))

(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(add-hook
 'gnus-group-mode-hook
 '(lambda nil
    (local-unset-key (kbd "q"))
    (local-unset-key (kbd "Q"))))


;;; Summary
;; Make mails sent by myself display my name instead of "=>blahblah" in
;; the summary buffer.
(setq gnus-ignored-from-addresses nil)

;; date view
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")
        (604800               . "%a %H:%M") ; this week
        ((gnus-seconds-month) . "%d")
        ((gnus-seconds-year)  . "%m/%d")
        (t                    . "%Y/%m/%d")))

;; Note!  Do `^, g' to update changes by `nnmail-extra-headers'! See
;; info for more.
(setq gnus-extra-headers '(Content-Type To Newsgroups))
(setq nnmail-extra-headers gnus-extra-headers)

(defalias 'gnus-user-format-function-ct 'rs-gnus-summary-line-content-type)

(defun gnus-user-format-function-from (head)
  "Trim `From:' to 20 bytes."
  (let* ((re "[\" ]")
         (from
          (replace-regexp-in-string
           (format "^%s+\\|%s+$" re re)
           ""
           (replace-regexp-in-string
            "<.*>" "" (gnus-header-from head)))))
    (when (> (length from) 20)
      (setq from (concat (substring from 0 18) "..")))
    (format "%-20s" from)))

;;(setq gnus-summary-line-format "%U%R%z%-6d  %5k  %-20f%B%s\n")
(setq gnus-summary-line-format ":%U%R %B %s %-60=|%4L |%-20,20f |%&user-date; \n")
;; (setq gnus-summary-line-format "%U%R%z%10&user-date; %u&ct; %5k  %4i  %u&from; %B(%t) %s\n")

(setq gnus-summary-same-subject ""
      gnus-sum-thread-tree-indent "  "
      gnus-sum-thread-tree-single-indent "◎ "
      gnus-sum-thread-tree-root "● "
      gnus-sum-thread-tree-false-root "☆"
      gnus-sum-thread-tree-vertical "│"
      gnus-sum-thread-tree-leaf-with-other "├─"
      gnus-sum-thread-tree-single-leaf "\\")

;; vi
(defun xwl-vi-like-hook ()
  (local-set-key (kbd "k") 'previous-line)
  (local-set-key (kbd "j") 'next-line)
  (local-set-key (kbd "l") 'forward-char)
  (local-set-key (kbd "h") 'backward-char))

(defun xwl-gnus-summary-mode-hook ()
  (xwl-vi-like-hook)

  (define-key gnus-summary-mode-map (kbd "p") 'gnus-summary-prev-same-subject)
  (define-key gnus-summary-mode-map (kbd "n") 'gnus-summary-next-same-subject)
  (define-key gnus-summary-mode-map (kbd "q") 'delete-other-windows)
  (define-key gnus-summary-mode-map (kbd "Q") 'gnus-summary-exit)

  (define-key gnus-summary-mode-map (kbd ",") 'gnus-summary-prev-thread)
  (define-key gnus-summary-mode-map (kbd ".") 'gnus-summary-next-thread)

  (define-key gnus-summary-mode-map (kbd "<") 'scroll-other-window-down)
  (define-key gnus-summary-mode-map (kbd ">") 'scroll-other-window)
  (define-key gnus-summary-mode-map (kbd "/ n") 'gnus-summary-insert-new-articles)

  (define-key gnus-summary-mode-map (kbd "r") (lambda () (interactive)
                                                (gnus-summary-show-article)
                                                (other-window 1)))

  (define-key gnus-summary-mode-map (kbd "RET") (lambda () (interactive)
                                                  (gnus-summary-show-article)
                                                  (other-window 1)))

  (define-key gnus-summary-mode-map (kbd "C-o") nil))

;; (add-hook 'gnus-summary-mode-hook 'xwl-gnus-summary-mode-hook)

(add-hook 'gnus-summary-prepared-hook 'gnus-summary-hide-all-threads)

;; save/copy some articles?
;;
;; - `B c': copy article to some group
;; - `*': put it in the cache, and use `Y c' to show it later
;; - `M-*': delete cached articles
(setq gnus-use-cache 'passive)


;;; Article

(setq gnus-visible-headers
      (concat "^\\("
              (regexp-opt
               '("From" "To" "CC" "Subject" "Date"
                 "User-Agent" "X-Mailer" "X-Newsreader"
                 "NNTP-Posting-Host"
                 "Organization"
                 ;; "Content-Type" "Content-Transfer-Encoding"
                 "Newsgroups"))
              "\\):"))

;; Use 'C-u g' to view mail source, W w, W Q
(add-hook 'gnus-article-prepare-hook 'gnus-article-fill-long-lines)

(defun xwl-gnus-article-show-ip ()
  "Show author's ip info in newsgroups."
  (save-excursion
    (message-narrow-to-headers)
    (when (search-forward-regexp
           "NNTP-Posting-Host: \\([0-9.]+\\)" nil t) ; a-zA-Z
      (end-of-line)
      (insert-and-inherit " (")
      (insert-and-inherit
       (car
        (split-string
         (shell-command-to-string
          (concat "pyip.py " (match-string-no-properties 1)))
         "\n")))
      (insert-and-inherit ")"))))

(setq message-yank-prefix nil)
;; "> ")


;;; Scoring

(add-hook 'message-sent-hook 'gnus-score-followup-thread)

(add-hook 'gnus-summary-exit-hook 'gnus-group-save-newsrc)
(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
(add-hook 'gnus-summary-exit-hook 'gnus-group-sort-groups-by-rank)


;;; MIME

(setq mm-default-directory "~/Downloads")

;; Use "symbol link files" for attached files, instead of making copies.
(setq gnus-gcc-externalize-attachments 'all)

;; See `~/.mailcap' about actions based on MIME.


;; general setting
(setq gnus-agent t                                 ; 开启代理功能, 以支持离线浏览
      gnus-inhibit-startup-message t               ; 关闭启动时的画面
      gnus-novice-user nil                         ; 关闭新手设置, 不进行确认
      gnus-expert-user t                           ; 不询问用户
      gnus-show-threads t                          ; 显示邮件线索
      gnus-use-dribble-file nil                    ; 不创建恢复文件
      gnus-always-read-dribble-file nil            ; 不读取恢复文件
      gnus-large-newsgroup 100                     ; 设置大容量的新闻组默认显示的大小
      gnus-large-ephemeral-newsgroup nil           ; 同上, 只不过对于短暂的新闻组
      gnus-summary-ignore-duplicates t             ; 忽略具有相同ID的消息
      gnus-treat-fill-long-lines t                 ; 如果有很长的行, 不提示
      message-confirm-send t                       ; 防止误发邮件, 发邮件前需要确认
      message-from-style 'angles                   ; From 头的显示风格
      message-syntax-checks '((sender . disabled)) ; 语法检查
      nnmail-expiry-wait 7                         ; 邮件自动删除的期限 (单位: 天)
      nnmairix-allowfast-default t                 ; 加快进入搜索结果的组
      gnus-summary-display-while-building 100)

;;
;; View setting
;;
(setq mm-text-html-renderer 'w3m
      mm-inline-large-images t
      mm-attachment-override-types '("image/.*"))
(auto-image-file-mode 1)

;; summary view setting
(setq gnus-summary-gather-subject-limit 'fuzzy)
(defun gnus-user-format-function-a (header)
  (let ((myself (concat "<" my-mail ">"))
        (references (mail-header-references header))
        (message-id (mail-header-id header)))
    (if (or (and (stringp references)
                 (string-match myself references))
            (and (stringp message-id)
                 (string-match myself message-id)))
        "X" "│")))

;; 用 Supercite 显示多种多样的引文形式
(setq sc-attrib-selection-list nil
      sc-auto-fill-region-p nil
      sc-blank-lines-after-headers 1
      sc-citation-delimiter-regexp "[>]+\\|\\(: \\)+"
      sc-cite-blank-lines-p nil
      sc-confirm-always-p nil
      sc-electric-references-p nil
      sc-fixup-whitespace-p t
      sc-nested-citation-p nil
      sc-preferred-header-style 4
      sc-use-only-preference-p nil)

;; threads
(setq gnus-use-trees t                  ; 联系老的标题
      gnus-tree-minimize-window nil     ; 用最小窗口显示
      gnus-generate-tree-function 'gnus-generate-horizontal-tree ; 生成水平树
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ; 聚集函数根据标题聚集
      )

;;
;; 时间显示
;;
(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local) ;将邮件的发出时间转换为本地时间
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)   ;跟踪组的时间轴
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)              ;新闻组分组

;; 排序
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)                               ;时间的逆序
        (not gnus-thread-sort-by-number)))                           ;跟踪的数量的逆序

;; 自动跳到第一个没有阅读的组
(add-hook 'gnus-switch-on-after-hook 'gnus-group-first-unread-group) ;gnus切换时
(add-hook 'gnus-summary-exit-hook 'gnus-group-first-unread-group)    ;退出Summary时

;; 自动更新新消息，功能不错，但在我的机器上会很慢...
;; (add-hook 'gnus-summary-exit-hook 'gnus-notify+)        ;退出summary模式后
;; (add-hook 'gnus-group-catchup-group-hook 'gnus-notify+) ;当清理当前组后
;; (add-hook 'mail-notify-pre-hook 'gnus-notify+)          ;更新邮件时

;; 斑纹化
(setq gnus-summary-stripe-regexp        ;设置斑纹化匹配的正则表达式
      (concat "^[^"
              gnus-sum-thread-tree-vertical
              "]*"))

;; 其他的一些设置
;;

;;
;;不喜欢 Summary buffer 和 Article buffer 的版面，如何改变？或者三个
;;窗口的显示？
;;可以通过调用函数 gnus-add-configuration 来控制窗口的配置。语法有点
;;复杂，不过在手册 "Windows Layout" 中解释得很清楚，一些比较流行的例
;;子：
;;用 35% 的 Summary 比 65% 的 Article 替换原来的 25% 比 75%（其中的
;;1.0 意思是“占满剩余空间”）：
;;
;;(gnus-add-configuration '(article (vertical 1.0 (summary .35 point) (article 1.0))))
;;
;;三个窗口显示。左边是 Group buffer，右上是 Summary buffer，右下是
;;Article buffer：
;;

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 35
                         (group 1.0))
               (vertical 1.0
                         (summary 0.35 point)
                         (article 1.0)))))
(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 35
                         (group 1.0))
               (vertical 1.0
                         (summary 1.0 point)))))


;;Article Buffer设置

;;
;;写消息时如何打开自动折行 (word-wrap) ？
;;
(add-hook 'message-mode-hook (lambda ()
                               (setq fill-column 80)
                               (turn-on-auto-fill)))

;;引用设置：不要原来的签名，引用全文
(setq message-cite-function 'message-cite-original-without-signature)
(add-hook 'mail-citation-hook 'sc-cite-original)

;;
;;压缩保存的邮件
;;
(setq nnml-use-compressed-files t)

;;
;;开启记分
;;
(setq gnus-use-adaptive-scoring t
      gnus-save-score t
      gnus-score-find-score-files-function
      '(gnus-score-find-hierarchical gnus-score-find-bnews bbdb/gnus-score))
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

(defvar gnus-default-adaptive-score-alist
  '((gnus-kill-file-mark (from -10))
    (gnus-unread-mark)
    (gnus-read-mark (from 10) (subject 30))
    (gnus-catchup-mark (subject -10))
    (gnus-killed-mark (from -1) (subject -30))
    (gnus-del-mark (from -2) (subject -15))
    (gnus-ticked-mark (from 10))
    (gnus-dormant-mark (from 5))))

(setq gnus-confirm-mail-reply-to-news t
      message-kill-buffer-on-exit t
      message-elide-ellipsis "[...]\n"
      )

;;
;; 如何存档有趣的消息？我将这个函数绑定在了F6键上
;;
;; 例如在 gnu.emacs.gnus 中，你偶然发现一个有趣的消息，想要存档，有好几种
;; 方法。第一种，也是最简单的，另存为文件`O f'。但是，从 Gnus 访问这样的
;; 存档文件并不方便。把 Frank Haun <pille3003@fhaun.de> 的这个代码
;; 片断放入 ~/.gnus：
;;
(defun my-archive-article (&optional n)
  "Copies one or more article(s) to a corresponding `nnml:' group, e.g.
     `gnus.ding' goes to `nnml:1.gnus.ding'. And `nnml:List-gnus.ding' goes
     to `nnml:1.List-gnus-ding'.

     Use process marks or mark a region in the summary buffer to archive
     more then one article."
  (interactive "P")
  (let ((archive-name
         (format "nnml:archives.%s"
                 (if (featurep 'xemacs)
                     (replace-in-string gnus-newsgroup-name "^.*:" "")
                   (replace-regexp-in-string "^.*:" "" gnus-newsgroup-name)))))
    (gnus-summary-copy-article n archive-name)))
;;
;; 此时，可以在 summary buffer 中用 `M-x my-archive-article' 把光标处的文
;; 章存档到一个 nnml 组（当然，可以改为你想要的其他后端）。
;;

;; 另一种保存帖子的方法：
;;
;; 看到有价值的帖子，只要按下`*'键，这篇帖子就会被拷贝到本地的cache中保存
;; 起来，即使服务器那边删除了帖子，也没关系了。如果不想要了，用`Meta-*'就
;; 可以把帖子从缓存中删掉。
;;
;; (setq gnus-use-cache 'passive)

;;
;; 多窗口处理
;;
;;
;; From: Katsumi Yamaoka <yamaoka@jpl.org>
;; Subject: Re: multiple message frames
;; To: ding@gnus.org
;; Newsgroups: gnus.ding
;; Date: Thu, 11 Sep 2003 16:06:37 +0900
;;
;; Hi,
;;
;; This is the revised version of the "multiple message frames" suit.
;; It makes it possible to open multiple message frames and delete
;; each frame automatically after sending or killing it.  You can use
;; it by simply putting it in your .gnus.el file.  Enjoy!
;;

(let* ((default
         ;; Winodw layout for normal message frames.
         '(vertical
           ((user-position . t)
            ;;(left . -1) (top . 1)
            (width . 80) (height . 40))
           (message 1.0 point)))
       (bug
        ;; Window layout for message frames reporting bugs.
        ;; Note that multiple gnus-bug frames are not supported.
        '(vertical
          ((user-position . t)
           ;;(left . -1) (top . 1)
           (width . 80) (height . 40))
          (if gnus-bug-create-help-buffer '("*Gnus Help Bug*" 0.5))
          ("*Gnus Bug*" 1.0 point)))
       (config
        `(frame
          1.0
          (if (buffer-live-p gnus-summary-buffer)
              (if (get-buffer gnus-article-buffer)
                  (car (cdr (assq 'article gnus-buffer-configuration)))
                (car (cdr (assq 'summary gnus-buffer-configuration))))
            (car (cdr (assq 'group gnus-buffer-configuration))))
          ,default))
       (settings '(compose-bounce forward mail-bounce message post
                                  reply reply-yank)))
  (while settings
    (gnus-add-configuration (list (car settings) config))
    (setq settings (cdr settings)))
  (setcdr (nthcdr 2 (setq config (copy-sequence config))) (list bug))
  (gnus-add-configuration (list 'bug config)))

(add-hook
 'gnus-configure-windows-hook
 (lambda nil
   (if (eq major-mode 'message-mode)
       (let* ((message-frame (selected-frame))
              (delete-frame-function
               `(lambda nil
                  (if (and
                       ;; Uncomment the following line if other windows
                       ;; in message frames are supposed to be important.
                       ;;(eq (selected-window) (next-window))
                       (eq (selected-frame) ,message-frame))
                      (delete-frame ,message-frame)))))
         (setq gnus-frame-list (delq message-frame gnus-frame-list)
               message-exit-actions `((funcall ,delete-frame-function))
               message-postpone-actions message-exit-actions)
         (if (or (featurep 'xemacs)
                 (< emacs-major-version 21))
             (make-local-hook 'kill-buffer-hook))
         (add-hook 'kill-buffer-hook `,delete-frame-function t t)))))

;; Don't popup a message frame when sending a queued message.
(add-hook
 'gnus-message-setup-hook
 (lambda nil
   (if (or (memq this-command '(gnus-draft-send-message
                                gnus-draft-send-all-messages
                                gnus-group-send-queue))
           (and (featurep 'gnus-delay)
                (save-excursion
                  (save-restriction
                    (widen)
                    (message-narrow-to-headers)
                    (re-search-forward
                     (concat "^" (regexp-quote gnus-delay-header)
                             ":\\s-+")
                     nil t)))))
       (let ((config (copy-sequence gnus-buffer-configuration)))
         (set (make-local-variable 'gnus-buffer-configuration)
              (cons '(forward (vertical 1.0 (message 1.0 point)))
                    (delq (assq 'forward config) config)))
         (set (make-local-variable 'gnus-configure-windows-hook)
              nil)))))

;; REF: http://www.ibm.com/developerworks/cn/linux/l-cn-emacsgnus/

;;(gnus-compile)
;;(provide 'init-gnus)
