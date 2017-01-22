# jqian
Automatically exported from code.google.com/p/jqian

## myconf

    svn co https://github.com/qxj/jqian/trunk/myconf

OR (needs git 1.7.0+)

    git init <repo>
    cd <repo>
    git remote add origin https://github.com/qxj/jqian.git
    git config core.sparsecheckout true
    echo "myconf/*" >> .git/info/sparse-checkout
    git pull origin master

## emacsconf.cask

    svn co https://github.com/qxj/jqian/branches/emacsconf.cask
    
OR (needs git 1.7.10+)

    git clone -b emacsconf.cask --single-branch https://github.com/qxj/jqian.git emacsconf.cask
