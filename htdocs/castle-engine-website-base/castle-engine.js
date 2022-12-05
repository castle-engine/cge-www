'use strict';

function cge_update_download_size()
{
  var thumbnails = jQuery('.thumbnails');
  if (thumbnails.length != 0) {
    var window_width = jQuery(window).width();
    var download = jQuery('.download');
    if (window_width > 600) {
      download.css('max-width', (window_width - 250) + 'px');
      //console.log('Max size of download: ', download.css('max-width'));
    } else {
      // in this case, CSS makes .thumbnails take full page width
      download.css('max-width', '');
      //console.log('Max size of download cleared');
    }
  }
}

jQuery(window).resize(function() {
  cge_update_download_size();
});

jQuery(document).ready(function() {
  cge_update_download_size();
});

jQuery("#toggle-details").click(function() {
  jQuery("#details").toggle(50);
});

/* Using passive event listeners can improve scrolling performance, according to
   https://web.dev/uses-passive-event-listeners/
   (linked from Lighthouse).
   Solution for jQuery from
   https://stackoverflow.com/questions/60357083/does-not-use-passive-listeners-to-improve-scrolling-performance-lighthouse-repo
*/

jQuery.event.special.touchstart = {
    setup: function( _, ns, handle ) {
        this.addEventListener("touchstart", handle, { passive: !ns.includes("noPreventDefault") });
    }
};
jQuery.event.special.touchmove = {
    setup: function( _, ns, handle ) {
        this.addEventListener("touchmove", handle, { passive: !ns.includes("noPreventDefault") });
    }
};
jQuery.event.special.wheel = {
    setup: function( _, ns, handle ){
        this.addEventListener("wheel", handle, { passive: true });
    }
};
jQuery.event.special.mousewheel = {
    setup: function( _, ns, handle ){
        this.addEventListener("mousewheel", handle, { passive: true });
    }
};
