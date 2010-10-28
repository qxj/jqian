<?php
// libdb.php --- Time-stamp: <2010-09-30 11:50:57 星期四 by lancer>
// Copyright 2010 Julian Qian
// Author: lancer@yitai01
// Version: $Id: libdb.php,v 0.0 2010/09/30 01:57:15 lancer Exp $
// Keywords: 

// Samples:
// $db = new Database("localhost", "username", "password", "database");
//  Sample for update/insert/delete query
// $db->connect();
// $db->query("update db .. ");
// $db->close();
//  Sample for select query all rows return
// $db->connect();
// $db->fetch_all("select db .. ");
// $db->close();
//  Sample for select only first row return
// $db->connect();
// $db->fetch_first("select db .. ");
// $db->close();

class Database {
    private $server = "localhost";
    private $user = "";
    private $pass = "";
    private $database = "";

    private $link_id;
    private $query_id;
    private $affected_rows;

    public function __construct($server, $user, $pass, $database){
        $this->server = $server;
        $this->user = $user;
        $this->pass = $pass;
        $this->database = $database;
    }

    public function connect($new_link = false){
        $this->link_id = @mysql_pconnect($this->server,
                                         $this->user,
                                         $this->pass,
                                         $new_link);

        if (! $this->link_id){
            throw new Exception("Could not connect to server: <strong>" . $this->server. "</strong>");
        }

        if (! @mysql_select_db($this->database, $this->link_id)){
            throw new Exception("Could not open database: <strong>" . $this->database. "</strong>");
        }

        unset($this->server,
              $this->user,
              $this->pass,
              $this->database);
    }

    public function close() {
        if( ! @mysql_close($this->link_id)){
            throw new Exception("Connection close failed.");
        }
    }

    // for "INSERT/UPDATE/DELETE ...", will return TRUE/FALSE
    // for "SELECT ..", will return resource_handle/FALSE
    public function query($sql) {
        // do query
        $this->query_id = @mysql_query($sql, $this->link_id);
        if (!$this->query_id) {
            throw new Exception("<b>MySQL Query failed:</b> $sql");
        }
        // $this->affected_rows = @mysql_affected_rows($this->link_id);
        return $this->query_id;
    }

    // for "SELECT ...", no need to call query()
    public function fetch_all($sql) {
        $query_id = $this->query($sql);
        $out = array();
        while ($row = $this->_fetch_array($query_id)){
            $out[] = $row;
        }
        $this->_free_result($query_id);
        
        return $out;
    }

    // for "SELECT ...", only return first row
    public function fetch_first($sql) {
        $query_id = $this->query($sql);
        $out = $this->_fetch_array($query_id);
        $this->_free_result($query_id);
        return $out;
    }

    public function query_num_rows(){
        $query_id = $this->query($sql);
        $num = mysql_num_row($query_id);
        $this->_free_result($query_id);
        return $num;
    }

    // private methods
    private function _escape($string) {
        if(get_magic_quotes_runtime()) $string = stripslashes($string);
        return @mysql_real_escape_string($string,$this->link_id);
    }

    private function _fetch_array($query_id = -1) {
        // retrieve row
        if ($query_id != -1) {
            $this->query_id = $query_id;
        }
        if (isset($this->query_id)) {
            $record = @mysql_fetch_assoc($this->query_id);
        }else{
            throw new Exception("Invalid query_id: <b>". $this->query_id.
                                "</b>. Records could not be fetched.");
        }
        return $record;
    }

    private function _free_result($query_id=-1) {
        if ($query_id!=-1) {
            $this->query_id = $query_id;
        }
        if($this->query_id != 0 && !@mysql_free_result($this->query_id)) {
            throw new Exception("Result ID: <b>". $this->query_id.
                                "</b> could not be freed.");
        }
    }

}


?>