<?php
require_once "castle_engine_functions.php";
castle_header("Privacy Policy", array(
  'path' => array('talk', 'privacy_policy')
));
echo pretty_heading($page_title);
?>

<h2>Mobile applications</h2>

<p>Mobile applications released through <a href="https://play.google.com/store/apps/dev?id=8315026166336791468">our Google Play account</a> do not feature any analytics or ads unless explicitly stated in their description. They do not collect any data from you.

<p>Note though:

<ul>
  <li>
    <p>Applications using <i>Google Play Games</i> integration use standard  <a href="https://developers.google.com/games/services/">Google Play Games Services</a> mechanisms to login and associate game information (achievements, leaderboards, savegames) with your account.
</ul>

<h2>Website analytics</h2>

<p>We use website analytics to improve your experience on our webpages: to detect erors (e.g. broken links) and to decide how to best present our content (e.g. what content is most useful to you). We use it only internally. The decisions we make upon these statistics are performed by humans (i.e. they are not automated, we do not do "profiling" to serve different users different content based on how they behave).

<p>To be completely open, we want to tell you exactly what technologies we use:

<ol>
  <li><p><a href="https://matomo.org/">Matomo</a>. Self-hosted (the data is on our servers, not any 3rd party). The data (like IP addresses) is immediately anonymized (e.g. we store only first 2 bytes from IPv4 address). i.e. we don't store non-anonymized data in our database at all. The logs are removed after 180 days anyway.

  <li><p><a href="https://www.google.com/analytics/">Google Analytics</a>. The data is only stored on Google Analytics servers, and removed after 26 months.

  <li><p><a href="https://jetpack.com/support/wordpress-com-stats/">Wordpress stats</a>. The data is only stored by Wordpress.com servers. The logs are retained by Wordpress.com for 28 days.

</ol>

We do not share the gathered data with any 3rd-party. We only store the data on Google Analytics and Wordpress.com servers, as is necessary, and they guarantee that noone else should access this data.

<h2>Questions</h2>

<p>If you have any questions, contact us:
<a href="https://castle-engine.io/talk.php">publicly (Discord, forum...)</a>
or send an email to <?php echo michalis_mailto('Michalis Kamburelis'); ?>.

<?php
castle_footer();
?>
