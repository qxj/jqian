<?php
// index.php --- Time-stamp: <2010-09-30 14:51:03 星期四 by lancer>
// Copyright 2010 Julian Qian
// Author: lancer@yitai01
// Version: $Id: index.php,v 0.0 2010/09/28 08:56:24 lancer Exp $
// Keywords: 

session_start();

  // Validate remote server

  // JSON RPC
include_once "jsonRPCServer.php";
include_once "ehomeRPC.php";

$server = new EhomeServer();
jsonRPCServer::handle($server) or print "no request";

?>