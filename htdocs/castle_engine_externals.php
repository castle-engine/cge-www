<?php

/* Functions to integrate our pages with external websites widgets,
   like Flattr, G+, Facebook. */

function _castle_disable_externals()
{
  return (
    CASTLE_ENVIRONMENT == 'offline' ||
    CASTLE_ENVIRONMENT == 'development' ||
    HTML_VALIDATION ||
    CASTLE_GENERATE_OFFLINE);
}

function _castle_disable_stats()
{
  return (
    CASTLE_ENVIRONMENT != 'production' ||
    HTML_VALIDATION ||
    CASTLE_GENERATE_OFFLINE);
}

/* Google ------------------------------------------------------------------- */

function echo_piwik_tracking()
{
  if (_castle_disable_stats()) return;

  /* Note: only one piwik.js should be included,
     so don't report to multiple Piwik installations.
     This Piwik code must be synched with
     ../../papers/compositing_shaders_doc/xsl/html_piwik.xsl
     ../../vrml_engine_doc/xsl/html_piwik.xsl
     ../../castle_game_engine/doc/pasdoc/footer.html
  */
  ?>

<!-- Matomo -->
<script type="text/javascript">
  var _paq = _paq || [];
  /* tracker methods like "setCustomDimension" should be called before "trackPageView" */
  _paq.push(["setDomains", ["*.castle-engine.sourceforge.net","*.castle-engine.io","*.castle-engine.io","*.castle-engine.sourceforge.io"]]);
  _paq.push(['trackPageView']);
  _paq.push(['enableLinkTracking']);
  (function() {
    var u="//castle-engine.io/piwik/";
    _paq.push(['setTrackerUrl', u+'piwik.php']);
    _paq.push(['setSiteId', '1']);
    var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
    g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'piwik.js'; s.parentNode.insertBefore(g,s);
  })();
</script>
<noscript><p><img src="//castle-engine.io/piwik/piwik.php?idsite=1&rec=1" style="border:0;" alt="" /></p></noscript>
<!-- End Matomo Code -->

<?php
}

function echo_google_analytics_tracking()
{
  if (_castle_disable_stats()) return;

?>
<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src="https://www.googletagmanager.com/gtag/js?id=UA-61279460-1"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-61279460-1');
</script>
<?php
}

/* Facebook ------------------------------------------------------------------

   See http://developers.facebook.com/docs/reference/plugins/like/
*/

function facebook_header()
{
  if (HTML_VALIDATION) return '';
  return '
    <meta property="fb:admins" content="100000327755900" />
    <meta property="og:title" content="Castle Game Engine" />
    <meta property="og:type" content="game" />
    <meta property="og:image" content="https://castle-engine.io/images/castle_game_engine_icon.png" />
    <meta property="og:url" content="https://castle-engine.io/" />
    <meta property="og:description" content="' . htmlspecialchars(META_DESCRIPTION) . '" />
  ';
}

function facebook_button()
{
  if (_castle_disable_externals()) return '';

  /* Facebook docs say to put this somewhere at the beginning of <body>,
     but that's actually bad for us, facebook may be slow...
     And also we don't use facebook_button() on many pages.
     So put this header at facebook_button(), when it is called for 1st time.
     We assume you do echo this on a single page more than once. */

  $header = '
<div id="fb-root"></div>
<script>(function(d, s, id) {
  var js, fjs = d.getElementsByTagName(s)[0];
  if (d.getElementById(id)) return;
  js = d.createElement(s); js.id = id;
  js.src = "//connect.facebook.net/en_US/all.js#xfbml=1";
  fjs.parentNode.insertBefore(js, fjs);
}(document, \'script\', \'facebook-jssdk\'));</script>
';

  return $header .
//    '<div class="fb-like" data-href="https://castle-engine.io/" data-layout="box_count" data-action="like" data-show-faces="true" data-share="true"></div>';
      '<div class="fb-like-box" data-href="https://www.facebook.com/castleengine" data-colorscheme="light" data-show-faces="true" data-header="true" data-stream="true" data-show-border="true"></div>';
}

/* Paypal -------------------------------------------------------------------- */

function paypal_button($with_logos = true)
{
  if (_castle_disable_externals()) return '';

  return ($with_logos ?
    '<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
<input type="hidden" name="cmd" value="_s-xclick">
<input type="hidden" name="encrypted" value="-----BEGIN PKCS7-----MIIHbwYJKoZIhvcNAQcEoIIHYDCCB1wCAQExggEwMIIBLAIBADCBlDCBjjELMAkGA1UEBhMCVVMxCzAJBgNVBAgTAkNBMRYwFAYDVQQHEw1Nb3VudGFpbiBWaWV3MRQwEgYDVQQKEwtQYXlQYWwgSW5jLjETMBEGA1UECxQKbGl2ZV9jZXJ0czERMA8GA1UEAxQIbGl2ZV9hcGkxHDAaBgkqhkiG9w0BCQEWDXJlQHBheXBhbC5jb20CAQAwDQYJKoZIhvcNAQEBBQAEgYBUrC/iQ/1hnqmvmrZk2dQOTnMsJDWLh2ZLwzmArofct+Cu8hPWWA8+JbPXvfHE7mRcA0imvi6d2mqOCXbn9wi/E0M3bTZfP+qIk1Ei59FUWXpKhD3mhWD9GsDBHECjTfkMMN4OB2IWCFlIwwSWofcgR7a8OnF62Lw9BZwSXxuHlzELMAkGBSsOAwIaBQAwgewGCSqGSIb3DQEHATAUBggqhkiG9w0DBwQIH+lRGl+f3CaAgcjL2X9xe5xglpsFurJjkuK6mbemvbGRXp1HOfAn3+IbFrJyyEoBJI4LMSDkkRt3j2+3DZeE7zOz1ApuaIMN6ROXsks7mRtCnklklABNfOWzw+T8++L9CMuZ2vDKcyYcn4SYSNHh32iaS+IFLAuZShLYuQBMzGR6ljYO/C+3vCM0wLCB4B0OPkDMetfhpluE8GA8yLHMjutXw9wHC+K+KRDHRc5wtaO3mKgOL2rqJ9QGlsRhlZYrJHaOUjEpWc0jhjCC6kc8/+fTfaCCA4cwggODMIIC7KADAgECAgEAMA0GCSqGSIb3DQEBBQUAMIGOMQswCQYDVQQGEwJVUzELMAkGA1UECBMCQ0ExFjAUBgNVBAcTDU1vdW50YWluIFZpZXcxFDASBgNVBAoTC1BheVBhbCBJbmMuMRMwEQYDVQQLFApsaXZlX2NlcnRzMREwDwYDVQQDFAhsaXZlX2FwaTEcMBoGCSqGSIb3DQEJARYNcmVAcGF5cGFsLmNvbTAeFw0wNDAyMTMxMDEzMTVaFw0zNTAyMTMxMDEzMTVaMIGOMQswCQYDVQQGEwJVUzELMAkGA1UECBMCQ0ExFjAUBgNVBAcTDU1vdW50YWluIFZpZXcxFDASBgNVBAoTC1BheVBhbCBJbmMuMRMwEQYDVQQLFApsaXZlX2NlcnRzMREwDwYDVQQDFAhsaXZlX2FwaTEcMBoGCSqGSIb3DQEJARYNcmVAcGF5cGFsLmNvbTCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAwUdO3fxEzEtcnI7ZKZL412XvZPugoni7i7D7prCe0AtaHTc97CYgm7NsAtJyxNLixmhLV8pyIEaiHXWAh8fPKW+R017+EmXrr9EaquPmsVvTywAAE1PMNOKqo2kl4Gxiz9zZqIajOm1fZGWcGS0f5JQ2kBqNbvbg2/Za+GJ/qwUCAwEAAaOB7jCB6zAdBgNVHQ4EFgQUlp98u8ZvF71ZP1LXChvsENZklGswgbsGA1UdIwSBszCBsIAUlp98u8ZvF71ZP1LXChvsENZklGuhgZSkgZEwgY4xCzAJBgNVBAYTAlVTMQswCQYDVQQIEwJDQTEWMBQGA1UEBxMNTW91bnRhaW4gVmlldzEUMBIGA1UEChMLUGF5UGFsIEluYy4xEzARBgNVBAsUCmxpdmVfY2VydHMxETAPBgNVBAMUCGxpdmVfYXBpMRwwGgYJKoZIhvcNAQkBFg1yZUBwYXlwYWwuY29tggEAMAwGA1UdEwQFMAMBAf8wDQYJKoZIhvcNAQEFBQADgYEAgV86VpqAWuXvX6Oro4qJ1tYVIT5DgWpE692Ag422H7yRIr/9j/iKG4Thia/Oflx4TdL+IFJBAyPK9v6zZNZtBgPBynXb048hsP16l2vi0k5Q2JKiPDsEfBhGI+HnxLXEaUWAcVfCsQFvd2A1sxRr67ip5y2wwBelUecP3AjJ+YcxggGaMIIBlgIBATCBlDCBjjELMAkGA1UEBhMCVVMxCzAJBgNVBAgTAkNBMRYwFAYDVQQHEw1Nb3VudGFpbiBWaWV3MRQwEgYDVQQKEwtQYXlQYWwgSW5jLjETMBEGA1UECxQKbGl2ZV9jZXJ0czERMA8GA1UEAxQIbGl2ZV9hcGkxHDAaBgkqhkiG9w0BCQEWDXJlQHBheXBhbC5jb20CAQAwCQYFKw4DAhoFAKBdMBgGCSqGSIb3DQEJAzELBgkqhkiG9w0BBwEwHAYJKoZIhvcNAQkFMQ8XDTEyMDcwMTAxNTEyMVowIwYJKoZIhvcNAQkEMRYEFG25HNmKfz1QT6xntpViL5uoj08eMA0GCSqGSIb3DQEBAQUABIGACWaFHeSnfqk8wIGTyBlapjakm/gsl9lyNbMohKrM1f1wR/nRioCWig54+eB+Str6ghe3aZpC2MD+FvlnlFd/+1sbVisNk11MY7BIBjbQpGG6htDGqdDFwHWYUxgSWRQp2xz4mHP965TvqJNK8ww39lRhf6iHARgLxa5bwo4snXI=-----END PKCS7-----
">
<input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif" style="border: 0" name="submit" alt="PayPal - The safer, easier way to pay online!">
<img alt="" style="border: 0" src="https://www.paypalobjects.com/pl_PL/i/scr/pixel.gif" width="1" height="1">
</form>
' : '<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
<input type="hidden" name="cmd" value="_s-xclick">
<input type="hidden" name="encrypted" value="-----BEGIN PKCS7-----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-----END PKCS7-----
">
<input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" style="border: 0" name="submit" alt="PayPal - The safer, easier way to pay online!">
<img alt="" style="border: 0" src="https://www.paypalobjects.com/pl_PL/i/scr/pixel.gif" width="1" height="1">
</form>' );
}

/* Twitter ------------------------------------------------------------------- */

function twitter_widget()
{
  if (_castle_disable_externals()) return '';
  return '
<a class="twitter-timeline" href="https://twitter.com/castleengine" data-widget-id="506585522525859840">Tweety na temat @castleengine</a>
<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");</script>
';
}

/* YouTube ------------------------------------------------------------------- */

function youtube_subscribe()
{
  if (_castle_disable_externals()) return '';
  /* Using https://developers.google.com/youtube/youtube_subscribe_button */
  return '
<script src="https://apis.google.com/js/platform.js"></script>
<div class="g-ytsubscribe" data-channelid="UCq9jJ5ivIXC5VEWiUAfxBxw" data-layout="full" data-count="default"></div>
';
}

/* Disqus -------------------------------------------------------------------- */

// function disqus_form()
// {
//   if (_castle_disable_externals()) return '';

//   global $page_basename, $page_title, $this_page_name, $disqus_form_already_done;

//   /* only show it once. This allows to insert this earlier for news */
//   if (!empty($disqus_form_already_done)) return '';
//   $disqus_form_already_done = true;

//   /* From https://castle-engine.disqus.com/admin/settings/universalcode/

//      For Disqus, use the final website URL, even when looking at localhost
//      or http://michalis.ii.uni.wroc.pl/cge-www-master/ version. */

//   return "
//     <div id=\"disqus_thread\"></div>
//     <script type=\"text/javascript\">
//       var disqus_shortname = 'castle-engine'; // required: replace example with your forum shortname
//       var disqus_identifier = '" . htmlspecialchars($page_basename) . "';
//       var disqus_title = '" . htmlspecialchars($page_title) . "';
//       var disqus_url = '" . CASTLE_PROD_URL . $this_page_name . "';
//       (function() {
//           var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
//           dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
//           (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
//       })();
//     </script>
//     <noscript>Please enable JavaScript to view the <a href=\"https://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>";
// }

/* DuckDuckGo ---------------------------------------------------------------- */

// Generated with https://duckduckgo.com/search_box
function search_box()
{
//  if (_castle_disable_externals()) return '';
  return <<<EOD
<iframe src="https://duckduckgo.com/search.html?width=\&site=castle-engine.io&prefill=Search Castle Game Engine" style="overflow:hidden;margin:0;padding:0;width:NaNpx;height:40px;" frameborder="0"></iframe>
EOD;
}
