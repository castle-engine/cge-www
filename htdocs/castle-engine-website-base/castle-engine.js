'use strict';

/* Update .download width to fit alongside the column
   with .thumbnails. No longer needed.
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
*/

/* Simple JS to show/hide something, used by convert-output.php now. */
jQuery("#toggle-details").click(function() {
  jQuery("#details").toggle(50);
});

/* Download boxes (cge_download_engine, cge_download_application
   in castle_engine_functions.php) and convert-output.php:

   - On phones and tablets, reveal .cge-mobile-only notes
     (desktop downloads are useless there, point to mobile alternatives).

   - After a download link is clicked, reveal .download-thanks panel
     with a Patreon link, like convert-output.php does after a conversion.

   Matomo events (category "Download") let us measure both. */

function cge_is_phone_or_tablet()
{
  if (navigator.userAgentData && navigator.userAgentData.mobile) {
    return true;
  }
  return /Android|iPhone|iPad|iPod/i.test(navigator.userAgent) ||
    // iPadOS Safari by default reports itself as macOS
    (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1);
}

function cge_track_event(category, action, name)
{
  window._paq = window._paq || [];
  window._paq.push(['trackEvent', category, action, name]);
}

jQuery(function() {
  if (cge_is_phone_or_tablet()) {
    jQuery('.cge-mobile-only').each(function() {
      this.hidden = false;
      cge_track_event('Download', 'Mobile note shown', jQuery(this).data('product') || '');
    });
  }

  jQuery('.cge-share-link').on('click', function() {
    var button = jQuery(this);
    var url = window.location.href.split('#')[0];
    cge_track_event('Download', 'Mobile share link', url);
    if (navigator.share) {
      navigator.share({ title: document.title, url: url }).catch(function() {});
    } else if (navigator.clipboard) {
      navigator.clipboard.writeText(url).then(function() {
        button.text('Link copied, send it to yourself');
      });
    }
  });

  // Real downloads only: not "Source Code (GitHub)" or "FreeBSD (compile)" buttons.
  jQuery('.download .download_platform a').filter(function() {
    return /\/releases\/download\/|gateway\.scarf\.sh/.test(this.href);
  }).on('click', function() {
    var box = jQuery(this).closest('.download');
    var thanks = box.find('.download-thanks');
    if (thanks.length === 0 || !thanks.prop('hidden')) {
      return;
    }
    thanks.prop('hidden', false);
    box.find('.download-patreon-static').prop('hidden', true);
    cge_track_event('Download', 'Thanks shown', thanks.data('product'));
    // Let the browser start the download first.
    window.setTimeout(function() {
      thanks[0].scrollIntoView({ behavior: 'smooth', block: 'nearest' });
    }, 300);
  });

  jQuery('.download-thanks-close').on('click', function() {
    var thanks = jQuery(this).closest('.download-thanks');
    thanks.prop('hidden', true);
    thanks.closest('.download').find('.download-patreon-static').prop('hidden', false);
  });

  jQuery('.download-thanks .btn-patreon-brand').on('click', function() {
    cge_track_event('Download', 'Patreon after download',
      jQuery(this).closest('.download-thanks').data('product'));
  });
});
