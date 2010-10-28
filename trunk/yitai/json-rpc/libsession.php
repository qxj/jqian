<?php
// libsession.php --- Time-stamp: <2010-09-30 14:46:02 星期四 by lancer>
// Copyright 2010 Julian Qian
// Author: lancer@yitai01
// Version: $Id: libsession.php,v 0.0 2010/09/30 04:53:00 lancer Exp $
// Keywords: 

  /*
   * TABLE STRUCTURE
   *   CREATE TABLE IF NOT EXISTS `session_db' (
   *     `sessionid' varchar(32) NOT NULL,
   *     `data' blob,
   *     `up_time' int(10) DEFAULT NULL,
   *     PRIMARY KEY (`uid')
   *     ) ENGINE=InnoDB DEFAULT CHARSET=latin1;
   */ 
class SessionDB {
    private $dbname = "sessiondb";
    private $data = array();
    private $sessionid;
    private $dblink;
    private $min_to_expire = 60;

    public function __construct($host, $user, $pass, $database){
        if(! ($this->dblink = mysql_pconnect($host, $user, $pass))) {
            throw new Exception("Cannot connect to session db");
        }
        if(! mysql_select_db($database, $this->dblink)){
            throw new Exception("Cannot select session db");
        }
        mysql_set_charset('utf8_general_ci');

        $this->sessionid = md5($_SERVER["REMOTE_ADDR"] .
                               $_SERVER["REMOTE_HOST"].
                               $_SERVER["HTTP_USER_AGENT"]);

        $sql = "select data, up_time from ".$this->dbname." where sessionid='{$this->sessionid}'";
            ;
        $ret = mysql_query($sql, $this->dblink);
        if($ret !== FALSE){
            $num = mysql_num_rows($ret);
            if($num){
                $data = mysql_result($ret, 0, 0);
                $up_time = mysql_result($ret, 0, 1);
                // previous session exists
                if($up_time <= $this->expire_time()){
                    $this->data = array();
                }else{
                    $this->data = unserialize($data);
                }                
            }else{
                $this->data = array();
                $data = serialize($data);
                $sql = "insert into ".$this->dbname." (data, up_time, sessionid) values ('".$data."', UNIX_TIMESTAMP(), '{$this->sessionid}')";
                mysql_query($sql, $this->dblink);
            }
        }else{
            throw new Exception("Session set failed: ". $sql);
        }        
    }

    public function set($key, $value){
        $this->data[$key] = $value;
    }

    public function get($key){
        if(array_key_exists($key, $this->data)){
            return $this->data[$key];
        }else{
            return false;
        }        
    }

    private function expire_time(){
        return time() - 60* $this->min_to_expire;
    }
    
    public function __destruct(){
        // flush session into sessiondb
        $data = serialize($this->data);
        $sql = "update ".$this->dbname." set data='".$data."', up_time=UNIX_TIMESTAMP() where sessionid='{$this->sessionid}'";
        mysql_query($sql, $this->dblink);
        
        // clean expired sessions
        $sql = "delete from ".$this->dbname." where up_time <= ".$this->expire_time();
        mysql_query($sql, $this->dblink);        
        
        mysql_close($this->dblink);
    }
  }

?>