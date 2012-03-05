;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; 73-twitter.el ---
;; Time-stamp: <Julian Qian 2012-02-22 23:10:39>
;; Created: 2011 Julian Qian
;; Version: $Id: 73-twitter.el,v 0.0 2011/05/19 08:29:04 jqian Exp $

;;


(deh-require 'twittering-mode
  (setq twittering-use-master-password t
        twittering-allow-insecure-server-cert t
        twittering-oauth-use-ssl nil
        twittering-use-ssl nil)

  (twittering-enable-unread-status-notifier)
  ;; (setq-default twittering-icon-mode t)

  (setq twittering-initial-timeline-spec-string ":home"
        twittering-api-host "twip/override-mode-url" ; without "http://"!
        twittering-api-search-host twittering-api-host
        twittering-username "jqian"
        twittering-password "password"
        twittering-auth-method 'basic
        )

  ;; frequently used keybinds
  (deh-define-key twittering-mode-map
    ("u" . 'twittering-update-status-interactive) ; send a new tweet
    ("R" . 'twittering-reply-to-user)             ; reply
    ("\C-cr" . 'twittering-organic-retweet)       ; reply and RT
    ((kbd "C-c RET") . 'twittering-retweet)       ; offical RT
    ("V" . 'twittering-visit-timeline)            ; timeline
    ("F" . 'twittering-favorite)                  ; favorite tweet
    ("g" . 'twittering-current-timeline)          ; fetch timeline
    )

  (defun my-toggle-twittering ()
    "Switch to a twittering buffer or return to the previous buffer."
    (interactive)
    (if (derived-mode-p 'twittering-mode)
        (while (derived-mode-p 'twittering-mode)
          (bury-buffer))
      (let ((list (buffer-list)))
        (while list
          (if (with-current-buffer (car list)
                (derived-mode-p 'twittering-mode))
              (progn
                (switch-to-buffer (car list))
                (setq list nil))
            (setq list (cdr list))))
        (unless (derived-mode-p 'twittering-mode)
          (call-interactively 'twit)))))
  )

;;; 73-twitter.el ends here
