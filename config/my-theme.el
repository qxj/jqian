;; -*- coding: utf-8 -*-

(deh-package fit-frame
  ;; avoid conflicting to winsav.el
  ;; (add-hook 'after-make-frame-functions 'fit-frame)
  )

(deh-section theme
  ;; (set-cursor-color "red")
  (setq custom-theme-directory (expand-file-name "theme" my-startup-dir))
  (load-theme 'zenburn :no-confirm))
