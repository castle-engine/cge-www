<?php
  require_once 'vrmlengine_functions.php';
  vrmlengine_header('Forum');

  echo pretty_heading($page_title, NULL, 'Ask for help, report bugs, discuss features');
?>

<p><span style="font-weight: bold"><a href="<?php echo FORUM_URL; ?>">Go to our forum.</a></span> You can post without registering, or you can register with your <a href="https://sourceforge.net/">SourceForge</a> account.
Any questions related to our VRML engine (and related programs like
view3dscene) are welcome.
</p>

<p>Alternatively, if you prefer to send questions through email,
you can subscribe and post to our <?php echo MAILING_LIST_LINK; ?>.</p>

<p>You can also submit
<a href="<?php echo BUGS_TRACKER_URL; ?>">bugs</a>,
<a href="<?php echo FEATURE_REQUESTS_TRACKER_URL; ?>">feature requests</a>,
<a href="<?php echo PATCHES_TRACKER_URL; ?>">patches</a>
to appropriate tracker.</p>

<p>If you really want to contact the author directly,
<?php echo michalis_mailto('send email to Michalis Kamburelis'); ?>.</p>

<?php /*
<i>And one more thing : if the bug concerns one of my OpenGL programs,
remember to attach to your bug report output of the
< ?php echo a_href_page("glinformation","glinformation") ? > program.</i> */ ?>

<?php vrmlengine_footer(); ?>
