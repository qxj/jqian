;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; 73-twitter.el ---
;; Time-stamp: <Julian Qian 2015-03-30 00:31:34>
;; Created: 2011 Julian Qian
;; Version: $Id: 73-twitter.el,v 0.0 2011/05/19 08:29:04 jqian Exp $

;;


(deh-package twittering-mode
  :config
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
  (bind-keys
   :map twittering-mode-map
    ("u"  . twittering-update-status-interactive) ; send a new tweet
    ("R"  . twittering-reply-to-user)             ; reply
    ("C-c r"  . twittering-organic-retweet)       ; reply and RT
    ("C-c RET"  . twittering-retweet)       ; offical RT
    ("V"  . twittering-visit-timeline)            ; timeline
    ("F"  . twittering-favorite)                  ; favorite tweet
    ("g"  . twittering-current-timeline)          ; fetch timeline
    )

  (define-mode-toggle "twittering" twit
    (derived-mode-p 'twittering-mode))
  )

;;; 73-twitter.el ends here