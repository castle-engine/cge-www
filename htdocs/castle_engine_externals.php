<?php

/* Functions to integrate our pages with external websites widgets,
   like Flattr, G+, Facebook. */

/* Flattr -------------------------------------------------------------------- */

function flattr_header()
{
  if (CASTLE_OFFLINE) return '';
  return '<script type="text/javascript">
/* <![CDATA[ */
    (function() {
        var s = document.createElement(\'script\'), t = document.getElementsByTagName(\'script\')[0];
        s.type = \'text/javascript\';
        s.async = true;
        s.src = \'http://api.flattr.com/js/0.6/load.js?mode=auto\';
        t.parentNode.insertBefore(s, t);
    })();
/* ]]> */
</script>';
}

function flattr_button($align = true)
{
  if (CASTLE_OFFLINE) return '';
  $result = '';
  if ($align) $result .= '<div style="float: right; margin: 1em;">';
  $result .= '
    <a class="FlattrButton" style="display:none;" href="http://castle-engine.sourceforge.net/"></a>
    <noscript><a href="http://flattr.com/thing/398312/Castle-Game-Engine" target="_blank">
    <img src="http://api.flattr.com/button/flattr-badge-large.png" alt="Flattr this" title="Flattr this" border="0" /></a></noscript>';
  if ($align) $result .= '</div>';
  return $result;
}

/* Google+ ------------------------------------------------------------------- */

function googleplus_header()
{
  if (CASTLE_OFFLINE) return '';
  return '<script type="text/javascript" src="https://apis.google.com/js/plusone.js"></script>
<link href="https://plus.google.com/101185352355602218697" rel="publisher" />';
}

function googleplus_badge()
{
  if (CASTLE_OFFLINE || HTML_VALIDATION) return '';
  /* Instead of +1 button, it's better to use a "badge",
     https://developers.google.com/+/web/badge/ .
     This allos Google to link our normal page back to our G+ page. */
  return '<div class="g-plus" data-href="https://plus.google.com/101185352355602218697"></div>';
  //return '<g:plusone size="tall"></g:plusone>';
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
    <meta property="og:image" content="http://castle-engine.sourceforge.net/images/castle_game_engine_icon.png" />
    <meta property="og:url" content="http://castle-engine.sourceforge.net/" />
    <meta property="og:description" content="An open-source 3D game engine, developed in modern Object Pascal, with excellent support for VRML X3D and many graphic effects. Also hosting view3dscene, our browser for VRML/X3D, Collada and other 3D models." />
  ';
}

function facebook_button()
{
  if (CASTLE_OFFLINE || HTML_VALIDATION) return '';


  /* Facebook docs say to put this somewhere at the beginning of <body>,
     but that's actually bad for us, facebook may be slow...
     And also we don't use facebook_button() on many pages.
     So put this header at facebook_button(), when it is called for 1st time. */
  global $castle_engine_was_facebook_button;
  if (empty($castle_engine_was_facebook_button))
  {
    $header = '
<div id="fb-root"></div>
<script>(function(d, s, id) {
  var js, fjs = d.getElementsByTagName(s)[0];
  if (d.getElementById(id)) return;
  js = d.createElement(s); js.id = id;
  js.src = "//connect.facebook.net/pl_PL/all.js#xfbml=1";
  fjs.parentNode.insertBefore(js, fjs);
}(document, \'script\', \'facebook-jssdk\'));</script>
';
    $castle_engine_was_facebook_button = true;
  } else
    $header = '';

  return $header .
    '<div class="fb-like" data-send="false" data-layout="box_count" data-width="50" data-show-faces="true"></div>';
}

/* Paypal -------------------------------------------------------------------- */

function paypal_button($with_logos = true)
{
  // Ok to show also when CASTLE_OFFLINE
  if (HTML_VALIDATION) return '';

  return ($with_logos ?
    '<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
<input type="hidden" name="cmd" value="_s-xclick">
<input type="hidden" name="encrypted" value="-----BEGIN PKCS7-----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-----END PKCS7-----
">
<input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif" border="0" name="submit" alt="PayPal - The safer, easier way to pay online!">
<img alt="" border="0" src="https://www.paypalobjects.com/pl_PL/i/scr/pixel.gif" width="1" height="1">
</form>
' : '<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
<input type="hidden" name="cmd" value="_s-xclick">
<input type="hidden" name="encrypted" value="-----BEGIN PKCS7-----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-----END PKCS7-----
">
<input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" border="0" name="submit" alt="PayPal - The safer, easier way to pay online!">
<img alt="" border="0" src="https://www.paypalobjects.com/pl_PL/i/scr/pixel.gif" width="1" height="1">
</form>' );
}
