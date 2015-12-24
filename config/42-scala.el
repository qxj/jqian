;; -*- mode: Emacs-Lisp -*-

;; scala programming environment

(deh-package scala-mode2)

(deh-package ensime
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))


;;
;; ## Installing ENSIME sbt-plugin
;;
;; We need to generate a config file for our project. To do that, first we need
;; to install the ensime-sbt plugin first.
;;
;; 1. Edit ~/.sbt/0.13/plugins/plugins.sbt with:
;;
;;     resolvers += Resolver.sonatypeRepo("snapshots")
;;     addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.6")
;;
;; 2. Add this file .sbt/0.13/ensime.sbt:
;;
;;     import org.ensime.Imports._
;;     EnsimeKeys.compilerArgs in Compile := (scalacOptions in Compile).value ++ Seq("-Ywarn-dead-code", "-Ywarn-shadowing")
;;
;; ## Generating the ENSIME file
;;
;; The server part of ENSIME needs to generate an .ensime file. We need to
;; generate this before starting ENSIME for our project.
;;
;; We can generate the file by entering the directory and generating the ENSIME
;; file automatically:
;;
;;     $ cd ~/development/our-project
;;     $ sbt gen-ensime
;;
;; ## Reference
;;
;; - http://www.47deg.com/blog/scala-development-with-emacs
;; - http://www.troikatech.com/blog/2014/11/26/ensime-and-emacs-as-a-scala-ide
;;
