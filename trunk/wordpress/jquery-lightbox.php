<?php
/*
Plugin Name: jQuery Lightbox (Balupton)
Plugin URI: http://blog.cong.co/jquery-lightbox.html
Description: Used to overlay images on the current page. Original jQuery Lightbox by <a href="http://plugins.jquery.com/project/jquerylightbox_bal" title="jQuery Lightbox">Balupton</a>. Hacked Pedro Lamas's jquery lightbox plugin.
Version: 0.1
Author: Julian Qian
Author URI: http://cong.co
*/
/*
 Usage:
  Download jquery-light from here:
  http://github.com/balupton/jquery-lightbox/downloads
  Uncompress jquery-lightbox package into a directory, and put this PHP script
  into it, and this is your jquery-light plugin for wordpress.
  Copy this directory into your wordpress plugin directory, and active it from
  wordpress plugin page.
*/

if (!class_exists("jQueryLightbox")) {
  class jQueryLightbox {
    function jQueryLightbox()
    {
      if (is_admin() || !function_exists('plugins_url')) return;
      
      global $wp_version;
      
      $path = plugins_url('/jquery-lightbox/scripts/');
      
      if (version_compare($wp_version, '2.8', '<')) {
        wp_deregister_script('jquery');
        wp_enqueue_script('jquery');
      }
      wp_enqueue_script('jquery-lightbox', $path.'jquery.lightbox.min.js', array('jquery'));
      // wp_enqueue_script('jquery-lightbox-plugin', $path.'jquery.lightbox.plugin.min.js', array('jquery', 'jquery-lightbox'), '1.0');
      
      add_filter('the_content', array(&$this, 'FixLink'), 99);
      add_filter('the_excerpt', array(&$this, 'FixLink'), 99);
    }
    
    function FixLink($content) {
        global $post;
        $pattern        = "/(<a(?![^>]*?rel=['\"]lightbox.*)[^>]*?href=['\"][^'\"]+?\.(?:bmp|gif|jpg|jpeg|png)['\"][^\>]*)>/i";
        $replacement    = '$1 rel="lightbox['.$post->ID.']">';
        $content = preg_replace($pattern, $replacement, $content);
        return $content;        
    }
  }
}

if (class_exists("jQueryLightbox")) {
  //$jQueryLightbox = new jQueryLightbox();
  add_action('plugins_loaded', create_function('', 'global $jQueryLightbox; $jQueryLightbox = new jQueryLightbox();'));
}
?>
