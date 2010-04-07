#!/bin/sh

tmpdir=yasnippet

svn checkout http://yasnippet.googlecode.com/svn/trunk/ yasnippet

mv $tmpdir/yasnippet.el ../load-path/
mv $tmpdir/snippets ..
rm -rf $tmpdir
