
(deh-package sql
  :config
  (setq sql-product 'mysql)
  (eval-after-load "sql"
    '(load-library "sql-indent")))
