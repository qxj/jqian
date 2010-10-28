<?php
// ehomeRPC.php --- Time-stamp: <2010-10-28 13:34:25 星期四 by lancer>
// Copyright 2010 Julian Qian
// Author: lancer@yitai01
// Version: $Id: ehomerpc.php,v 0.0 2010/09/30 02:15:03 lancer Exp $
// Keywords: 

require_once "libdb.php";
require_once "libsession.php";
require_once "config.php";

class EhomeServer {
    private $_uid;
    private $_db;
    private $_session;
    private $_alarm_types;
    private $_max_phones = 10;
    private $_max_devices = 10;

    public function __construct(){
        $this->_db = new Database(DBHOST, DBUSER, DBPASS, DBNAME);
        $this->_session = new SessionDB(DBHOST, DBUSER, DBPASS, DBNAME);
        $this->_uid = $this->_session->get("uid");
        $this->_alarm_types = array("wurgency", "wdoor", "winfra", "wwindow",
                                    "whelp", "wsmoke", "wgas", "wglass",
                                    "wwater", "wdefen");
    }

    public function test(){
        return "test connection!";
    }
    
    // Login
    // paramter: $username, $password
    // return: true
    public function login($username, $password){
        $this->_db->connect();
        $sql = "select uid, passwd, locked from user where username='$username' limit 1";
        $user = $this->_db->fetch_first($sql);
        $this->_db->close();
        
        if($user) {
            if($password == $user["passwd"] && $user["locked"] == 0){
                // authorized OK!
                $this->_session->set("uid", $user["uid"]);
                return true;
            }
        }
        $this->_session->set("uid", -1);
        return false;
    }
    // Get device list
    // return: array(array("device"=>"11094",
    //                     "address" =>"some place"),
    //              ...)
    public function getDeviceList(){
        $this->_db->connect();
        $sql = "select devid,addr from device where uid=". $this->_uid;
        $ret = $this->_db->fetch_all($sql);
        $this->_db->close();
        $devices = array();
        foreach($ret as $item){
            array_push($devices, array("device" => $item["devid"],
                                       "address" => $item["addr"]));
        }
        return $devices;
    }

    // Get phones binded to device
    // paramter: $device
    // return: array(array("phone"=>"18611101110",
    //                     "alarm-types"=>"urgency,help,defen"),
    //               ...)
    public function getBindPhones($device){
        $sql = "select * from phone where devid='$device' and uid=". $this->_uid;
        $this->_db->connect();
        $ret = $this->_db->fetch_all($sql);
        $this->_db->close();
        $phones = array();
        foreach($ret as $item){
            $types = array();
            foreach($item as $k=>$v){
                if(in_array($k, $this->_alarm_types)){
                    if($v){
                        array_push($types, substr($k, 1));
                    }
                }
            }
            array_push($phones, array("phone" => $item["mobile"],
                                      "alarm-types" => implode(",", $types)));
        }
        return $phones;
    }

    // $alarm_types = array("urgency"=>1,"door"=>0)
    public function setBindPhone($device, $phone, $alarm_types){
        
        $sql = "select pid from phone where devid='$device' and mobile='$phone' and uid=".$this->_uid;
        $this->_db->connect();
        $row = $this->_db->fetch_first($sql);
        $sql = "";
        if( empty($row) ){
            // check max binded phones
            $sql = "select count(pid) cnt from phone where devid='$device' and uid=".$this->_uid;
            $row = $this->_db->fetch_first($sql);
            $cnt = $row["cnt"];
            if($cnt >= $this->_max_phones){
                return false;
            }

            $sql1 = array();
            $sql2 = array();
            foreach($alarm_types as $k => $v){
                array_push($sql1, "w".$k); // urgency => wurgency
                array_push($sql2, $v);
            }
            $sql_1 = implode(",", $sql1);
            $sql_2 = implode(",", $sql2);

            if($sql_1) $sql_1 = ",". $sql_1;
            if($sql_2) $sql_2 = ",". $sql_2;
            
            $sql = "insert into phone (devid, mobile, uid".$sql_1.") values (".$device.",'".$phone."',".$this->_uid.$sql_2.");";
        } else {

            $sqls = array();
            foreach($alarm_types as $k => $v){
                array_push($sqls, "w".$k."=".$v);
            }
            $sql_1 = implode(",", $sqls);
            
            $pid = $row["pid"];
            if($sql_1){
                $sql = "update phone set ".$sql_1." where pid=$pid";
            }
        }
        $this->_db->query($sql);
        $this->_db->close();

        return true;
    }

    public function delBindPhone($device, $phone){
        $sql = "delete from phone where devid=$device and mobile='$phone' and uid=".$this->_uid;
        $this->_db->connect();
        $this->_db->query($sql);
        $this->_db->close();
        return true;
    }
    
    // Watch camera video
    public function getVideoUri(){
        return "not implement yet";
    }
    // Check defense status
    public function getDeviceStatus($device){
        include_once "/var/tmp/common_header.php";
        include_once "/home/www/yitai/system/application/language/english/yitai_lang.php";
        $this->_db->connect();
        $sql = "select stat, up_time from status where devid='$device' order by up_time desc limit 20";
        $ret = array();
        $index = 0;
        foreach($this->_db->fetch_all($sql) as $row){
            $stat = strtolower($stat_def[$row["stat"]]);
            $ret[$index]["status"] = $lang[$stat];
            $ret[$index]["up_time"] = $row["up_time"];
            $index++;
        }
        $this->_db->close();
        return $ret;
    }
    // Defense control
    public function setDeviceDefence($device, $defend){
        switch($defend){
            case true:
                $cmd = "bf";
                break;
            case false:
                $cmd = "cf";
                break;
            default:
                return false;
        }
        define('EHOME_APP', "/home/lancer/projects/fhomeyzd/fhomed");
        $run = EHOME_APP." -a $device,$cmd";

        /*
        error_log("\nsetDeviceDefence: ".
                  var_export($run,1)
                  , 3, "/tmp/php.log");
        */
        
        $ret = @exec($run);
        if($ret == 1){
            return true;
        }
        return false;
    }
    // Modify user profile
    public function getUserProfile(){
        $sql = "select u.*,l.name lname from user u left join location l on u.location=l.lid where uid=". $this->_uid;
        $this->_db->connect();
        $row = $this->_db->fetch_first($sql);
        $this->_db->close();
        return array("username" => $row["username"],
                     "realname" => $row["realname"],
                     "email" => $row["email"],
                     "phone" => $row["mobile"],
                     "address" => $row["addr"],
                     "location" => $row["lname"]);
    }

    public function setUserProfile($password,$email,$phone,$address){
        $st = array();
        if(!empty($password)){
            $st[] = "passwd='$password'";
        }
        if(!empty($email)){
            $st[] = "email='$email'";
        }
        if(!empty($phone)){
            $st[] = "mobile='$phone'";
        }
        if(!empty($address)){
            $st[] = "addr='$address'";
        }
        $sql = "update user set ".implode(", ", $st)." where uid=". $this->_uid;
        $this->_db->connect();
        $ret = $this->_db->query($sql);
        $this->_db->close();
        if($ret){
            return true;
        }
        return false;
    }
    
    // Push waring message
    public function checkAlarmMessage(){
        $this->_db->connect();
        $sql = "select devid from device where uid=". $this->_uid;
        $ret = $this->_db->fetch_all($sql);
        $devlist = array();
        foreach($ret as $k){
            $devlist[] = $k["devid"];
        }
        $sql = "select stat,devid,up_time from status where devid IN (".implode(",",$devlist).") and stat IN (5,6,7,27,14,15,16,17,18) and up_time > UNIX_TIMESTAMP() - 900";
        $ret = $this->_db->fetch_all($sql);
        $alarms = array();
        $devids = array();
        foreach($ret as $k){
            if(in_array($k["devid"], $devids)){
                continue;               // unique devid
            }else{
                array_push($devids, $k["devid"]);
            }
            array_push($alarms, array("device"=>$k["devid"],
                                      "alarm"=>$this->statusMessage_($k["stat"]),
                                      "up_time"=>$k["up_time"]));
        }
        $this->_db->close();
        return $alarms;
    }

    private function statusMessage_($stat){
        $msg = "";
        switch($stat){
            case 5:
                $msg = "urgency";
                break;
            case 6:
                $msg = "door";
                break;
            case 7:
                $msg = "infra";
                break;
            case 27:
                $msg = "window";
                break;
            case 14:
                $msg = "help";
                break;
            case 15:
                $msg = "smoke";
                break;
            case 16:
                $msg = "gas";
                break;
            case 17:
                $msg = "glass";
                break;
            case 18:
                $msg = "water";
                break;
        }
        return $msg;
    }
}


?>