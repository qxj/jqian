;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

;;; incoming: fetchmail -> procmail -> gnus
;;# two kind mail backend: mbox and maildir
;;;; mbox -> nnmail
;;# ~/.procmailrc
;; MAILDIR=$HOME/Mail
;; DEFAULT=$MAILDIR/inbox/
;; :0:
;; inbox.spool
;;# ~/.gnus.el
;; (setq mail-sources '((directory :path "~/Mail" :suffix ".spool")))
;;;; maildir -> nnmaildir
;;# ~/.procmailrc
;; MAILDIR=$HOME/Mail
;; DEFAULT=$MAILDIR/inbox/
;; :0:
;; ${DEFAULT}
;;# ~/.gnus.el
;; (setq mail-sources '((maildir :path "~/Mail/" :subdirs ("cur" "new"))))
;; (setq gnus-secondary-select-methods '((nnmaildir "" (directory "~/Mail/"))))
;;;; gnus
;; press "^" (gnus-enter-server-mode), then enter "{nnmaildir:}", then
;; press "u" in front of "inbox" to subscribe it.
;;; outgoing: gnus -> msmtp
;;# ~/.gnus.el
;; (setq message-send-mail-function 'message-send-mail-with-sendmail
;;       sendmail-program "msmtp")
;;# ~/.msmtprc
;; account gmail
;; host smtp.gmail.com
;; domain smtp.gmail.com
;; tls on
;; tls_certcheck off
;; tls_starttls on
;; auth on
;; user junist@gmail.com
;; password password
;; port 587
;; logfile /home/jqian/msmtp.log
;;; imap
;;# ~/.gnus.el
;; (setq gnus-secondary-select-methods
;;       `((nnimap "gmail"
;;                 (nnimap-address "imap.gmail.com")
;;                 (nnimap-server-port 993)
;;                 (nnimap-stream ssl)
;;                 (nnimap-authinfo-file "~/.authinfo"))))
;;# ~/.authinfo.gpg
;; machine imap.gmail.com login xxx@gmail.com password yyy port 993
;; machine smtp.gmail.com login xxx@gmail.com password yyy port 587

;;; my setting
(setq gnus-select-method '(nnmaildir "" (directory "~/Mail/")))

;; (setq gnus-secondary-select-methods '((nntp "localhost") ; leafnode
;;                                       (nnmaildir "" (directory "~/Mail/"))))

;;;; Outgoing messages
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp")
(setq gnus-outgoing-message-group
      '(nnml "archive"
             (nnml-directory   "~/Mail/archive")
             (nnml-active-file "~/Mail/archive/active")
             (nnml-get-new-mail nil)
             (nnml-inhibit-expiry t)))
;;;; Setting
(setq gnus-interactive-exit nil
      gnus-subscribe-newsgroup-method 'gnus-subscribe-topics
      gnus-subscribe-options-newsgroup-method 'gnus-subscribe-topics
      gnus-fetch-old-headers t          ; 'some
      gnus-nov-is-evil nil
      gnus-use-cross-reference nil
      gnus-gcc-mark-as-read t)
;;;; Gnus Parameters
(setq gnus-parameters
      `((".*inbox.*"
         (gcc-self . t))
        (".*important.*"
         (gcc-self . t))
        ("trash"
         (total-expire . t))))
;;;; Posting styles
(setq gnus-posting-styles
      `((".*"
         (name ,user-full-name)
         ,(if (file-exists-p "~/xface.png")
              '(face (gnus-convert-png-to-face "~/xface.png")))
         (x-url (getenv "WWW_HOME"))
         (organization ,system-name)
         (signature ,(concat "Best Regards,\n" user-full-name))
         (eval (setq message-sendmail-extra-arguments '("-a" "gmail"))))
        ;; cn.*
        (,(regexp-opt '("cn.fan" "cn.bbs"))
         (address ,(concat "NO.SPAM" user-mail-address)) ; avoid spam
         (eval (list
                (setq message-sendmail-extra-arguments '("-a" "gmail"))
                (setq mm-coding-system-priorities '(gb2312 gbk gb18030 utf-8))))
         ;;(body "")
         )
        ))
;;;; Splitting received messages
(setq nnmail-treat-duplicates 'delete
      nnmail-crosspost nil
      nnmail-split-fancy-match-partial-words t
      nnmail-split-methods 'nnmail-split-fancy
      nnmail-split-fancy
      `(|
        (from ,(concat ".*"
                       (regexp-opt '("@localhost"
                                     "Cron Daemon"))
                       ".*")
              "local")
        ;; (: my-split-mailing-lists)
        ;; (to ,user-mail-address (: my-notify-important))
        (from ".*@\\(mails.pku.edu.cn\\|pku.org.cn\\)" "pku")
        ;; (any ".*@gmail.com" "gmail")
        (to ".*@newsmth.*" "newsmth")
        ;; maybe junk
        ))
;;;; Expire setting
(setq nnmail-expiry-wait-function
      (lambda (group)
        (cond
         ((string-match "nnimap.*" group)
          'never)
         ((string-match "nnml.*" group)
          'never)
         ((or (string= "trash" group)
              (string= "cc-trash" group))
          3)
         (t 'never))))
;;;; Group setting
(setq gnus-activate-level 5
      gnus-permanently-visible-groups '"nn*")
;;;; Sumary setting
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")
        (604800               . "%a %H:%M") ; this week
        ((gnus-seconds-month) . "%d")
        ((gnus-seconds-year)  . "%m/%d")
        (t                    . "%Y/%m/%d")))
(setq gnus-summary-line-format ":%U%R %B %s %-60=|%4L |%-20,20f |%&user-date; \n"
      gnus-summary-same-subject ""
      gnus-sum-thread-tree-indent "  "
      gnus-sum-thread-tree-single-indent "◎ "
      gnus-sum-thread-tree-root "● "
      gnus-sum-thread-tree-false-root "☆"
      gnus-sum-thread-tree-vertical "│"
      gnus-sum-thread-tree-leaf-with-other "├─"
      gnus-sum-thread-tree-single-leaf "\\")

(eval-after-load "gnus"
  '(deh-define-key gnus-summary-mode-map
     ((kbd "p") . 'gnus-summary-prev-same-subject)
     ((kbd "n") . 'gnus-summary-next-same-subject)
     ((kbd "q") . 'delete-other-windows)
     ((kbd "Q") . 'gnus-summary-exit)
     ((kbd ",") . 'gnus-summary-prev-thread)
     ((kbd ".") . 'gnus-summary-next-thread)
     ((kbd "<") . 'scroll-other-window-down)
     ((kbd ">") . 'scroll-other-window)
     ((kbd "/ n") . 'gnus-summary-insert-new-articles)
     ((kbd "r") . (lambda () (interactive) (gnus-summary-show-article) (other-window 1)))
     ((kbd "RET") . (lambda () (interactive) (gnus-summary-show-article) (other-window 1)))
     ((kbd "C-o") . nil)
     ;; move
     ((kbd "k") . 'previous-line)
     ((kbd "j") . 'next-line)
     ((kbd "l") . 'forward-char)
     ((kbd "h") . 'backward-char)))

(add-hook 'gnus-summary-prepared-hook 'gnus-summary-hide-all-threads)
;;;; Article setting
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
(add-hook 'gnus-article-prepare-hook 'gnus-article-fill-long-lines)
;;;; sorting
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(add-hook 'gnus-summary-exit-hook 'gnus-group-save-newsrc)
(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
(add-hook 'gnus-summary-exit-hook 'gnus-group-sort-groups-by-rank)
;;;; MIME
(setq mm-default-directory "~/Downloads")
(setq gnus-gcc-externalize-attachments 'all)

;;;; other
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

(defun my-toggle-gnus ()
  (interactive)
  (if (or (string-match "*Group*" (buffer-name))
          (string-match "*Summary" (buffer-name)))
      (bury-buffer)
    (let ((list (buffer-list)))
      (while list
        (if (or (string-match "*Group*" (buffer-name (car list)))
                (string-match "*Summary" (buffer-name (car list))))
            (progn
              (switch-to-buffer (car list))
              (setq list nil))
          (setq list (cdr list))))
      (unless (or (string-match "*Group*" (buffer-name))
                  (string-match "*Summary" (buffer-name)))
        (call-interactively 'gnus)))))


;; (gnus-compile)



