;; -*- coding: utf-8 mode: emacs-lisp -*-
;;
;; 1-先说说我的系统：
;; ubuntu 9.10 默认中文环境zh_CN.UTF-8,输入法Ibus
;; Emacs23.1 + w3w + gnus + erc + org-mode
;; 最后配置了stunnel4用于处理ssl连接，这个主要是为了对付gmail，对我来说更为实际的是
;; stunnel4在windows和linux下都能用。

;; 2-收拾stunnel4：
;; 这个程序在ubuntu9.xx上好像有些问题，看到老外在论坛上直骂“biche!”。主要的毛病就是
;; 不能作为服务自动启动，每次都要手动，很烦人。我也没有解决，只能也骂一句：他奶奶的！
;; 配置stunnel4需要设置两个文件：
;; (1) /etc/stunnel/stunnel.conf
;; 这是我的配置文件，你可以参考：

;; ; Sample stunnel configuration file by Michal Trojnara 2002-2009
;; ; Some options used here may not be adequate for your particular configuration
;; ; Please make sure you understand them (especially the effect of the chroot jail)

;; ; Certificate/key is needed in server mode and optional in client mode
;; ;;cert = /etc/ssl/certs/stunnel.pem
;; ;key = /etc/ssl/certs/stunnel.pem

;; ; Protocol version (all, SSLv2, SSLv3, TLSv1)
;; sslVersion = SSLv3

;; ; Some security enhancements for UNIX systems - comment them out on Win32
;; chroot = /var/lib/stunnel4/
;; setuid = stunnel4
;; setgid = stunnel4
;; ; PID is created inside the chroot jail
;; pid = /stunnel4.pid

;; ; Some performance tunings
;; socket = l:TCP_NODELAY=1
;; socket = r:TCP_NODELAY=1
;; ;compression = rle

;; ; Workaround for Eudora bug
;; ;options = DONT_INSERT_EMPTY_FRAGMENTS

;; ; Authentication stuff
;; ;verify = 2
;; ; Don't forget to c_rehash CApath
;; ; CApath is located inside chroot jail
;; ;CApath = /certs
;; ; It's often easier to use CAfile
;; ;CAfile = /etc/stunnel/certs.pem
;; ; Don't forget to c_rehash CRLpath
;; ; CRLpath is located inside chroot jail
;; ;CRLpath = /crls
;; ; Alternatively you can use CRLfile
;; ;CRLfile = /etc/stunnel/crls.pem

;; ; Some debugging stuff useful for troubleshooting
;; debug = 7
;; output = /var/log/stunnel4/stunnel.log

;; ; Use it for client mode
;; client = yes

;; ; Service-level configuration

;; [pop3s]
;; accept  = 995
;; connect = 110

;; [imaps]
;; accept  = 993
;; connect = 143

;; [ssmtp]
;; accept  = 465
;; connect = 25

;; ; ----------------------------------------------------
;; ; gmail 的设置
;; ; accept表示stunnel的接受端口, connect表示stunnel的发送端口
;; ; 可以简单的把它理解为一个转接口：
;; ; 一端接上自己的机器，一端连接到gmail服务器

;; [gmail-pop3s]
;; accept  = 127.0.0.1:9959
;; connect = pop.gmail.com:995

;; [gmail-imaps]
;; accept  = 127.0.0.1:9939
;; connect = imap.gmail.com:993

;; [gmail-ssmtp]
;; accept  = 127.0.0.1:4659
;; connect = smtp.gmail.com:465

;; ;[https]
;; ;accept  = 443
;; ;connect = 80
;; ;TIMEOUTclose = 0

;; ; vim:ft=dosini

;; (2) /etc/default/stunnel4
;; 这个文件更简单，不同的系统可能不同，ubuntu上是有的，用来控制是否随系统启动
;; 似乎有点鸡肋！

;; # /etc/default/stunnel
;; # Julien LEMOINE <speedblue@debian.org>
;; # September 2003

;; # Change to one to enable stunnel automatic startup
;; ENABLED=1
;; FILES="/etc/stunnel/*.conf"
;; OPTIONS=""

;; # Change to one to enable ppp restart scripts
;; PPP_RESTART=0

;; (3) 我不太理解Certificate key的工作机制，在(1)的配置中已经禁用了，但在调式的
;; 过程中似乎必须要生成一个stunnel.pem。我参照/usr/share/doc/stunnel4/readme.debian
;; 操作了一下，可以使用了。
;; 要使用管理员权限，这是具体过程：
;; # cd /etc/stunnel
;; # openssl req -new -x509 -nodes -days 365 -out stunnel.pem -keyout stunnel.pem
;; # chmod 600 stunnel.pem
;; # dd if=/dev/urandom of=temp_file count=2
;; # openssl dhparam -rand temp_file 512 >> stunnel.pem
;; # ln -sf stunnel.pem `openssl x509 -noout -hash < stunnel.pem`.0
;; 最后做个链接：
;; # cd /etc/ssl/certs
;; # ln -s /etc/stunnel/stunnel.pem stunnel.pem
;; 以上操作我一点也不理解，纯粹是照抄！
;; 配置完成后，运行stunnel4服务:
;; # /etc/init.d/stunnel4 restart

;; (4)配置gnus
;; 我是在.emacs里调用mygnus.el来分离配置文件的，你可以直接写在.emacs里
;; 在我的配置文件中有个变量 emacsHome是我的emacs主目录(在移动硬盘上)，你
;; 修改成自己的地址。
;; 这个配置文件包含了很多人的内容，比较杂，可以作为一个参考根据个人喜好剪裁...

;; (require 'gnus)
;; gnus-notify+从这里下载：http://www.emacswiki.org/emacs/gnus-notify%2B.el
;; (require 'gnus-notify+)

;; 存储设置
;; concat emacsHome "xx/xx/xx") --用来计算我自己的Gnus存档目录，emacsHome变量
;; 是我自己预先设置的，完全可以不用，直接写成 "xx/xx/xx"

(setq home-directory "~/")

(setq gnus-startup-file                 ; 初始文件
      (concat home-directory "Gnus/newsrc")
      gnus-default-directory            ; 默认目录
      (concat home-directory "Gnus")
      gnus-home-directory               ; 主目录
      (concat home-directory "Gnus")
      gnus-dribble-directory            ; 恢复目录
      (concat home-directory "Gnus")
      gnus-directory                    ; 新闻组的存储目录
      (concat home-directory "Gnus/News")
      gnus-article-save-directory       ; 文章保存目录
      (concat home-directory "Gnus/News")
      gnus-kill-files-directory         ; 文件删除目录
      (concat home-directory "Gnus/News/trash")
      gnus-agent-directory              ; 代理目录
      (concat home-directory "Gnus/News/agent")
      gnus-cache-directory              ; 缓存目录
      (concat home-directory "Gnus/News/cache")
      gnus-cache-active-file            ; 缓存激活文件
      (concat home-directory "Gnus/News/cache/active")
      message-directory                 ; 邮件的存储目录
      (concat home-directory "Gnus/Mail")
      message-auto-save-directory       ; 自动保存的目录
      (concat home-directory "Gnus/Mail/drafts")
      mail-source-directory             ; 邮件的源目录
      (concat home-directory "Gnus/Mail/incoming")
      nnmail-message-id-cache-file      ; nnmail的消息ID缓存
      (concat home-directory "Gnus/nnmail-cache")
      nnml-newsgroups-file              ; 邮件新闻组解释文件
      (concat home-directory "Gnus/Mail/newsgroup")
      nntp-marks-directory              ; nntp组存储目录
      (concat home-directory "Gnus/News/marks")
      mml-default-directory             ; 附件的存储位置
      (concat home-directory "Gnus/Attachment"))
;;
;; 关闭默认的archive 这个方法不好控制
;;
(setq gnus-message-archive-group nil)

;; 设置存档目录
(setq gnus-outgoing-message-group
      '(nnml "archive"
             (nnml-directory   (concat home-directory "Gnus/Mail/archive"))
             (nnml-active-file (concat home-directory "Gnus/Mail/archive/active"))
             (nnml-get-new-mail nil)
             (nnml-inhibit-expiry t)))

;;;; 一个老外的例子，可以参考
;;;;A function that selects a reasonable group for Gcc'ing this mail.
;;(defun MySendedMail ()
;;  (cond ((and gnus-newsgroup-name
;;              (not (message-news-p))
;;              (stringp gnus-newsgroup-name))
;;         gnus-newsgroup-name)
;;        (t ted-default-gcc-group)))
;;;; Use it.
;;(setq gnus-outgoing-message-group "nnml:SendMails")

(defun MySended ()
  (if (message-news-p)
      "nnml:SendNews"
    "nnml:SendMails"))
(setq gnus-outgoing-message-group 'MySended)

;; 新闻组
;;cn99
(setq gnus-select-method '(nntp "news.cn99.com"))

;;雅科\新帆
(add-to-list 'gnus-secondary-select-methods '(nntp "news.newsfan.net"))
;;(add-to-list 'gnus-secondary-select-methods '(nntp "news.yaako.com"))

;;首先我们设置POP3服务器：
;; Gmail

;;告诉gnus如何存放接收来的邮件，gnus把这个叫做backend，
;;最常用的方式是nnfolder，另外还有nnmbox, nnml等其它几种方式，我们
;;选择其中一种就可以了：

;; 采用stunnel作为服务器处理ssl链接
(setq mail-sources
      '(
        ;; gmail setting
        (imap :server "127.0.0.1" ; stunnel 为本地服务
              :user 'my-gmail-user ; 用户名
              :port 9939          ; stunnel 本地端口
              ;; :stream ssl
              :password 'my-gmail-passwd
              )
        ;; pop3 mail setting
        ;; (pop :server "pop3.example.com"   ; 在这里设置pop3服务器
        ;;      :user "username@example.com" ; 用户名
        ;;      :port "110"
        ;;      :password "password"
        ;;      )
        ))

;;告诉gnus如何存放接收来的邮件，gnus把这个叫做backend，最常用的方式
;;是nnfolder，另外还有nnmbox, nnml等其它几种方式，我们选择其中一种就可以了：
(setq gnus-secondary-select-methods '((nnml "")))

;;然后我们设置SMTP服务器，采用smtp方式发送邮件需要一个小程序
;;smtpmail.el, 这个程序现在已被纳入了官方的Emacs，如果你用的是最新的
;;CVS Emacs，比如 Emacs22, Emacs23等，就已经包含了这个程序。你可以检
;;查一下emacs的安装目录中 lisp/mail/ 目录下有没有这个文件，如果没有
;;的话，就只好自己下载、安装了。现在我们看看如何设置：

;;发送邮件，参考 http://www.emacswiki.org/emacs/GnusGmail
;;;; 采用stunnel处理ssl连接
(setq message-send-mail-function 'smtpmail-send-it
      ;;smtpmail-starttls-credentials '(("127.0.0.1" 4659 nil nil))
      smtpmail-auth-credentials '(("127.0.0.1" 4659 my-gmail-user my-gmail-passwd))
      smtpmail-default-smtp-server "127.0.0.1"
      smtpmail-smtp-server "127.0.0.1"
      smtpmail-smtp-service 4659
      ;;smtpmail-local-domain "your-pc-name"
      )

;;
;;现在我们可以选择几个自己喜欢的新闻组，作为默认的新闻组：
;;
(setq gnus-default-subscribed-newsgroups
      '(
        "cn.bbs.comp.emacs"
        "cn.bbs.comp.linux"
        "cn.bbs.comp.lang.python"
        "gnu.emacs.help"
        ))

;;
;; 邮件分类,使用 nnmail-split-fancy方法更为灵活
;;

(setq nnmail-treat-duplicates 'delete ; 如果由重复，删除！
      nnmail-crosspost nil ; 同一个邮件不要到多个组
      nnmail-split-methods 'nnmail-split-fancy ; 这个分类方法比较灵活
      nnmail-split-fancy-match-partial-words t ; 单词部分匹配也算成功匹配
      nnmail-split-fancy
      '(| ; 根据 mailing list 分类
        (to my-gmail-user "To-me")
        (any ".*@gmail.com" "gmail")
        "misc")) ;; 这里或许是 junk 了

;;
;;总是显示mail组，如何显示所有组呢？
;;
(setq gnus-permanently-visible-groups '"nn*")


;;--------------------------------------------------------------------------------------
;; 一些常规设置
;;
;;(gnus-agentize)                                  ; 旧格式，已废弃！
(setq gnus-agent t                                 ; 开启代理功能, 以支持离线浏览
      gnus-inhibit-startup-message t               ; 关闭启动时的画面
      gnus-novice-user nil                         ; 关闭新手设置, 不进行确认
      gnus-expert-user t                           ; 不询问用户
      gnus-show-threads t                          ; 显示邮件线索
      gnus-interactive-exit nil                    ; 退出时不进行交互式询问
      gnus-use-dribble-file nil                    ; 不创建恢复文件
      gnus-always-read-dribble-file nil            ; 不读取恢复文件
      gnus-asynchronous t                          ; 异步操作
      gnus-large-newsgroup 100                     ; 设置大容量的新闻组默认显示的大小
      gnus-large-ephemeral-newsgroup nil           ; 同上, 只不过对于短暂的新闻组
      gnus-summary-ignore-duplicates t             ; 忽略具有相同ID的消息
      gnus-treat-fill-long-lines t                 ; 如果有很长的行, 不提示
      message-confirm-send t                       ; 防止误发邮件, 发邮件前需要确认
      message-kill-buffer-on-exit t                ; 设置发送邮件后删除buffer
      message-from-style 'angles                   ; From 头的显示风格
      message-syntax-checks '((sender . disabled)) ; 语法检查
      nnmail-expiry-wait 7                         ; 邮件自动删除的期限 (单位: 天)
      nnmairix-allowfast-default t                 ; 加快进入搜索结果的组
      gnus-summary-display-while-building 100)

;;
;; 显示设置
;;
(setq mm-text-html-renderer 'w3m        ; 用W3M显示HTML格式的邮件
      mm-inline-large-images t          ; 显示内置图片
      mm-attachment-override-types '("image/.*")) ; Inline images?
(auto-image-file-mode)                                ;自动加载图片

;; 概要显示设置
(setq gnus-summary-gather-subject-limit 'fuzzy) ; 聚集题目用模糊算法
(setq gnus-summary-line-format "%4P %U%R%z%O %{%5k%} %{%14&user-date;%}   %{%-20,20n%} %{%ua%} %B %(%I%-60,60s%)\n")
(defun gnus-user-format-function-a (header) ; 用户的格式函数 `%ua'
  (let ((myself (concat "<" my-mail ">"))
        (references (mail-header-references header))
        (message-id (mail-header-id header)))
    (if (or (and (stringp references)
                 (string-match myself references))
            (and (stringp message-id)
                 (string-match myself message-id)))
        "X" "│")))

(setq gnus-user-date-format-alist             ; 用户的格式列表 `user-date'
      '(((gnus-seconds-today) . "TD %H:%M")   ; 当天
        (604800 . "W%w %H:%M")                ; 七天之内
        ((gnus-seconds-month) . "%d %H:%M")   ; 当月
        ((gnus-seconds-year) . "%m-%d %H:%M") ; 今年
        (t . "%y-%m-%d %H:%M")))              ; 其他

;; 线程的可视化外观, `%B'
(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-indent "    ")
(setq gnus-sum-thread-tree-single-indent "◎ ")
(setq gnus-sum-thread-tree-root "● ")
(setq gnus-sum-thread-tree-false-root "☆")
(setq gnus-sum-thread-tree-vertical "│")
(setq gnus-sum-thread-tree-leaf-with-other "├─ ")
(setq gnus-sum-thread-tree-single-leaf "t─ ")


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

;; 线程设置
(setq gnus-use-trees t                  ; 联系老的标题
      gnus-tree-minimize-window nil     ; 用最小窗口显示
      gnus-fetch-old-headers 'some      ; 抓取老的标题以联系线程
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
      '(
        (not gnus-thread-sort-by-date)                               ;时间的逆序
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

;;------------------------------------------------------------------------------------
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

;;
;;不喜欢 Summary buffer 的样子，如何调整？
;;那么你需要和变量 gnus-summary-line-format 玩一玩，它得值是一个符号
;;串，比如作者，日期，主题等。手册 "Summary Buffer Lines" 中有可用的
;;符号列表和常被忘记的节点 "Formatting Variables" 和它的子节点。其中
;;有很多有用的东西，像指定光标和制表符的位置等。
;;
;;从 5.10.0 起，Gnus 新提供了一些很不错的标志符，例如，%B 可以形成一
;;个线索树，%&user-date 根据帖子给出时间细节。例子如下：
;;
(setq gnus-summary-line-format ":%U%R %B %s %-60=|%4L |%-20,20f |%&user-date; \n")

;;Article Buffer设置
;;设定要显示的头消息格式
(setq gnus-visible-headers
      "^\\(^To:\\|^CC:\\|^From:\\|^Subject:\\|^Date:\\|^Followup-To:
\\|^X-Newsreader:\\|^User-Agent:\\|^X-Mailer:
\\|Line:\\|Lines:\\|Content-Type:\\|NNTP-Posting-Host\\)")

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
;;如果开启了主题视图，只看未读邮件是令人讨厌的，在 ~/.gnus 里面加如这行：
;;
(setq gnus-fetch-old-headers 'some)
;;
;; topic mode 参考这里：(info "(gnus)Group Topics")
;;
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'mail-citation-hook 'sc-cite-original)

;;
;;压缩保存的邮件
;;
(setq nnml-use-compressed-files t)

;;
;;开启记分
;;
(setq gnus-use-adaptive-scoring t)
(setq gnus-save-score t)
(add-hook 'mail-citation-hook 'sc-cite-original)
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

(setq gnus-score-find-score-files-function
      '(gnus-score-find-hierarchical gnus-score-find-bnews bbdb/gnus-score)
      gnus-use-adaptive-scoring t)

;;;
(setq gnus-confirm-mail-reply-to-news t
      message-kill-buffer-on-exit t
      message-elide-ellipsis "[...]\n"
      )

;;
;; 如何存档有趣的消息？我将这个函数绑定在了F6键上
;;
;; 例如在 gnu.emacs.gnus 中，你偶然发现一个有趣的消息，想要存档，有好几种
;; 方法。第一种，也是最简单的，另存为文件`O f'。但是，从 Gnus 访问这样的
;; 存档文件并不方便。把 Frank Haun &lt;pille3003@fhaun.de&gt; 的这个代码
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
;; 当然，也可以使用缓冲：
;;
(setq gnus-use-cache t)

;;
;; 这样，你只需设置 tick 或者 dormant 标记来保存，在缓冲中设置已读标记可
;; 以删除（文章）。
;;
;; 另一种保存帖子的方法：
;;
;; 看到有价值的帖子，只要按下`*'键，这篇帖子就会被拷贝到本地的cache中保存
;; 起来，即使服务器那边删除了帖子，也没关系了。如果不想要了，用`Meta-*'就
;; 可以把帖子从缓存中删掉。
;;
(setq gnus-use-cache 'passive)

;;
;; 中文！中文！永远都是头痛的事儿...Emacs23终于解决的这个问题 :-)
;;
;; 设置编码，这个是改变了整个emacs的编码！太恐怖了
;;(set-language-environment 'Chinese-GBK)
(setq gnus-default-charset 'gbk)
(add-to-list 'gnus-group-charset-alist
             '("\\(^\\|:\\)cn\\>\\|\\<chinese\\>" gbk))
(setq gnus-summary-show-article-charset-alist
      '((1 . utf-8)
        (2 . big5)
        (3 . gb18030)
        (4 . gbk)
        (5 . gn2312)
        (6 . utf-7)))

(setq gnus-group-name-charset-group-alist
      '(("\\.com\\.cn:" . gbk)
        ("news\\.newsfan\\.net" . gbk)))

(setq gnus-group-name-charset-method-alist
      '(((nntp "news.cn99.net") . gbk)))

(setq gnus-group-name-charset-method-alist
      '(((nntp "news.newsfan.net") . gbk)))

(setq gnus-newsgroup-ignored-charsets
      '(unknown-8bit x-unknown x-gbk gb18030))

;; 显示编码格式
(add-hook 'gnus-startup-hook
          '(lambda ()
             (setq gnus-visible-headers
                   (concat "^User-Agent:\\|^Content-Type:\\|"
                           "Content-Transfer-Encoding:\\|"
                           "^X-mailer:\\|^X-Newsreader:\\|^X-Sender:\\|"
                           gnus-visible-headers))))

;;设置发送风格
(setq gnus-posting-styles
      '(
        ;; all
        (".*"
         (name my-real-name)
         (address (concat "NO.SPAM" my-gmail-user))
         (face (gnus-convert-png-to-face (concat home-directory "Gnus/xface.png")))
         (organization my-laptop-name)
         (signature (concat "
Best Regards,
" my-real-name))
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 gb2312 gbk gb18030 utf-8)))
         ;;(body "")
         )
        ;;cn.bbs.com
        ("^cn\\.bbs\\.comp"
         (name my-real-name)
         (address (concat "NO.SPAM" my-gmail-user))
         (face (gnus-convert-png-to-face (concat home-directory "Gnus/xface.png")))
         (organization my-laptop-name)
         (signature "")
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 gb2312 gbk gb18030 utf-8)))
         ;;(body "")
         )
        ))


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

;;(gnus-compile)                          ;编译一些选项, 加快速度
;;(provide 'init-gnus)