<?php
/*
Plugin Name: Sync Photos
Plugin URI: http://blog.cong.co/sync-photos.html
Description: Sync photos from photo provider to your web server, such as GFWed picasaweb :(
Version: 1.0
Author: Julian Qian
Author URI: http://cong.co
*/
?>
<?php
/*  Copyright 2010  Julian Qian  (email : junist@gmail.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/
?>
<?php
  /* Usage:
   * Copy this file to directory `wp-content/plugins/', and active it in your
   * wordpress plugin management page. NO MORE OTHER STEPS.
   */

  /* Features:
   * 1. DONE: crawl every picasaweb links from posts' content
   * 2. DONE: all photos fetched from picasaweb into local disk on web server
   * 3. DONE: integrate cached picasaweb photos into wordpress gallery
   * 4. TODO: use proxy to fetch picasaweb if web server locates inside GFW
   * 5. TODO: support flickr.com
   */
?>
<?php
// error_reporting(E_ALL);
/** WordPress Image Administration API */
require_once(ABSPATH . 'wp-admin/includes/image.php');

// filter content? or footer?
add_filter('the_content',     array('SyncPhotos', 'sync'), 1);
add_filter('the_content_rss', array('SyncPhotos', 'sync'), 1);
add_filter('the_excerpt_rss', array('SyncPhotos', 'sync'), 1);

class PicasaWeb {
	var $pattern = "|(<a\s+href=\"http://picasaweb\.google\.com/[^\"]+\">)?<img\s+src=\"(http://lh[0-9]\.ggpht\.[^\"]+)\"(\s+alt=\"([^\"]*)\")?\s*/?>(</a>)?|si";
	var $subdir = "/picasaweb";   // wp-content/uploads/$this->subdir
	var $url_list = array();
	var $threads = 5;
	var $timeout = 10;             // seconds

	function log($msg){
		// echo __( "<b>PicasaWeb</b>: ". $msg. "<br />\n" );
		// flush();
	}
	
	function sync($content){
		// $content = preg_replace($this->wrap_pattern, "$1", $content);
		preg_match_all($this->pattern, $content, $match, PREG_SET_ORDER);
		$this->fillUrls($match);
		$this->fetchAllPhotos();
		return preg_replace_callback($this->pattern,
									 array(&$this, 'replacePhoto'),
									 $content);
	}

	function replacePhoto($match){
		$url_key = $this->checksum($match[2]);
		$local_url = $this->url_list[$url_key]["local"];
		$url = empty($local_url) ? $this->url_list[$url_key]["url"] : $local_url;
		$alt = $this->url_list[$url_key]["alt"];
		return "<a href=\"$url\" title=\"$alt\"><img src=\"$url\" alt=\"$alt\"></a>";
	}

	function checksum($string){
		return crc32($string);
	}

	function fillUrls($matches){
		foreach($matches as $match){
			// store url checksum as keys can avoid dup links
			$url = $match[2];
			$alt = $match[4] ? $match[4] : substr($url, strrpos($url, "/") + 1);
			$this->url_list[$this->checksum($url)] = array("url" => $url,
														   "alt" => $alt,
														   "local" => "");
			$this->log("Add url: ". $url);
		}
	}

	function fetchAllPhotos(){
		// check local cache
		$number_to_fetch = 0;
		foreach($this->url_list as $key => $url){
			if($local_url = $this->existCachePhoto($url["url"])){
				$this->url_list[$key]["local"] = $local_url;
			}else{
				++ $number_to_fetch;
			}
		}

		if($number_to_fetch){
			// local file path will be return from 1st arg: url_list
			$this->log("Begin to fetch photos.");
			$this->crawlPhotos($this->url_list,
							   $this->threads,
							   $this->timeout);
		}
	}

	// urls = array(url => cache url,
	//              ...)
	function crawlPhotos(&$urls = array(), $threads = 5, $timeout = 30){
		// Urls to download
		// $urls[] = "http://lh4.ggpht.com/_AogbBxxzmC0/S0L-nh3jF3I/AAAAAAAAJK4/f_fgUjy23CE/s800/avatar_poster_2.jpg";

		$mcurl = curl_multi_init();
		$threadsRunning = 0;
		$urls_id = 0;
		
		reset($urls); // start again first item
		$url_item = current($urls);
		for(;;) {
			// Fill up the slots
			while($threadsRunning < $threads &&
				  $url_item !== FALSE){
				// if not cached, run a curl job
				if(empty($url_item["local"])){
					$this->log("URL item: ". $url_item["url"]);
					
					$ch = curl_init();
					curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
					curl_setopt($ch, CURLOPT_BINARYTRANSFER, 1);
					curl_setopt($ch, CURLOPT_TIMEOUT, $timeout);
					curl_setopt($ch, CURLOPT_URL, $url_item["url"]);	// url
					curl_multi_add_handle($mcurl, $ch);
					$threadsRunning++;
				}
				
				$url_item = next($urls);
			}
			// Check if done
			if ($threadsRunning == 0 && $url_item === FALSE)
				break;
			// Let mcurl do it's thing
			curl_multi_select($mcurl);
			while(($mcRes = curl_multi_exec($mcurl, $mcActive)) == CURLM_CALL_MULTI_PERFORM) usleep(100000);
			if($mcRes != CURLM_OK) break;
			while($done = curl_multi_info_read($mcurl)) {
				$ch = $done['handle'];
				$done_url = curl_getinfo($ch, CURLINFO_EFFECTIVE_URL);
				$done_content = curl_multi_getcontent($ch);
				if(curl_errno($ch) == 0) {
					$urls[$this->checksum($done_url)]["local"] =
						$this->storePhotoToCache($done_content, $done_url);
					$this->log("Succeed to cache url: ".$done_url);
				} else {
					$this->log("Link <a href='$done_url'>$done_url</a> failed: ".curl_error($ch)."\n");
				}
				curl_multi_remove_handle($mcurl, $ch);
				curl_close($ch);
				$threadsRunning--;
			}
		}
		curl_multi_close($mcurl);
		$this->log( 'Done.' ); 
	}

	// return local filename
	function storePhotoToCache($content, $url){
		$filename = $this->fileName($url);
		$title = $this->url_list[$this->checksum($url)]["alt"];
		// wp_upload_bits() return: array( 'file' => $new_file, 'url' => $url, 'error' => false );
		$newfile = $this->wp_upload_bits($filename, $content);
		
		$filepath = $newfile["file"];
		$filetype = wp_check_filetype($filepath);
		$photo = array(
			"post_title" => $title,
			"post_content" => $filename,
			"post_status" => "inherit",
			"post_parent" => 0,
			"post_mime_type" => $filetype["type"],
			"guid" => $newfile["url"]);
		$postid = wp_insert_attachment($photo, $filepath);
		if( !is_wp_error($postid)){
			wp_update_attachment_metadata( $postid, wp_generate_attachment_metadata( $postid, $filepath ) );
		}
		// TODO: convert local path to url
		return $newfile["url"];
	}

	function existCachePhoto($url){
		$file = $this->filePath($url);
		if(file_exists($file["file"])){
			return $file["url"];
		}
		return FALSE;
	}

	function wp_upload_dir(){
		$siteurl = get_option( 'siteurl' );
		$upload_path = get_option( 'upload_path' );
		$upload_path = trim($upload_path);
		if ( empty($upload_path) ) {
			$dir = WP_CONTENT_DIR . '/uploads';
		} else {
			$dir = $upload_path;
			if ( 'wp-content/uploads' == $upload_path ) {
				$dir = WP_CONTENT_DIR . '/uploads';
			} elseif ( 0 !== strpos($dir, ABSPATH) ) {
				// $dir is absolute, $upload_path is (maybe) relative to ABSPATH
				$dir = path_join( ABSPATH, $dir );
			}
		}

		if ( !$url = get_option( 'upload_url_path' ) ) {
			if ( empty($upload_path) || ( 'wp-content/uploads' == $upload_path ) || ( $upload_path == $dir ) )
				$url = WP_CONTENT_URL . '/uploads';
			else
				$url = trailingslashit( $siteurl ) . $upload_path;
		}

		if ( defined('UPLOADS') ) {
			$dir = ABSPATH . UPLOADS;
			$url = trailingslashit( $siteurl ) . UPLOADS;
		}

		$bdir = $dir;
		$burl = $url;

		$subdir = $this->subdir;

		$dir .= $subdir;
		$url .= $subdir;

		$uploads = apply_filters( 'upload_dir', array( 'path' => $dir, 'url' => $url, 'subdir' => $subdir, 'basedir' => $bdir, 'baseurl' => $burl, 'error' => false ) );

		// Make sure we have an uploads dir
		if ( ! wp_mkdir_p( $uploads['path'] ) ) {
			return false;
		}

		return $uploads;		
	}
	
	// TODO: bind to wordpress album path
	// act the same as wp_upload_bits()
	function filePath($url){
		$upload = $this->wp_upload_dir();
		$filename = $this->fileName($url);
		return array("file" => $upload["path"]."/".$filename,
					 "url" => $upload["url"]."/".$filename);
	}

	function fileName($url){
		// return urlencode(substr($url, strrpos($url, "/") + 1));
		$filename = substr($url, strrpos($url, "/") + 1);
		return urldecode($filename);
		// $suffix = strtolower(substr($url, strrpos($url, ".")));
		// return crc32($url) . $suffix;
	}

	function wp_upload_bits( $filename, $bits ) {

		$wp_filetype = wp_check_filetype( $filename );

		$upload = $this->wp_upload_dir();
		
		$new_file = $upload['path'] . "/$filename";
		if ( ! wp_mkdir_p( dirname( $new_file ) ) ) {
			return false;
		}

		$ifp = @ fopen( $new_file, 'wb' );
		if ( ! $ifp )
			return false;
		
		@fwrite( $ifp, $bits );
		fclose( $ifp );
		// Set correct file permissions
		$stat = @ stat( dirname( $new_file ) );
		$perms = $stat['mode'] & 0007777;
		$perms = $perms & 0000666;
		@ chmod( $new_file, $perms );

		// Compute the URL
		$url = $upload['url'] . "/$filename";

		return array( 'file' => $new_file, 'url' => $url, 'error' => false );
	}
}

class SyncPhotos {
	static function sync($content){
		$pw = &new PicasaWeb();
		return $pw->sync($content);
	}
}

if(0):
	$content = "hahah test aa\n\"http://lh4.ggpht.com/_AogbBxxzmC0/S0L-nh3jF3I/AAAAAAAAJK4/f_fgUjy23CE/s800/avatar_poster_2.jpg\"\ntest here\"http://lh6.ggpht.com/_AogbBxxzmC0/SVZQUMjLw6I/AAAAAAAAG4w/85qHprPr6rQ/s800/IMG_5390_ting_web.jpg\"";
echo "<pre>Begin:...\n";
$pw = &new PicasaWeb();
$parsed = $pw->sync($content);
echo "End</pre>";
endif;
?>
