<?php
  require_once "vrmlengine_functions.php";
  vrmlengine_header('OpenAL (3D Sound)', NULL, array('other'));

  $toc = new TableOfContents(
    array(
      new TocItem('Installing OpenAL', 'install'),
      new TocItem('Command-line options respected by all my programs that use OpenAL',
        'options'),
    )
  );
?>

<?php echo pretty_heading($page_title, NULL); ?>

<p><a href="http://openal.org/">OpenAL</a>
is a cool cross-platform library for 3D sound.
All the sound played by our engine goes through it.</p>

Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

Generally, you can install OpenAL from many sources, you can even
get optimized OpenAL drivers from your sound card manufacturer.
Section below describes how to install OpenAL in the
<i>simplest and most often case</i>.

<dl>
  <dt>Linux users:</dt>
  <dd><p>Install appropriate package for your Linux distribution.
    Look for a package named like <tt>libopenal*</tt>.</p>

    <p>My games work with both common OpenAL implementations:
    <i>OpenAL Sample Implementation</i> (the original implementation,
    from Loki, unmaintained) and
    <a href="http://kcat.strangesoft.net/openal.html">OpenAL Soft</a>
    (the newer, maintained version). Modern distributions (at least Debian
    and Ubuntu) contain <i>OpenAL Soft</i>.</p>
    </dd>

  <dt>FreeBSD users:</dt>
  <dd><p>Simple <tt>pkg_add -r openal</tt> command should do
    the trick.</p></dd>

  <dt>Mac OS X users:</dt>
  <dd><p>New Mac OS X installs have OpenAL already installed.
    If not, you can download and run
    <a href="http://connect.creativelabs.com/openal/Downloads/Forms/AllItems.aspx">Creative
    free OpenAL installer for Mac OS X</a>. (Choose <i>OpenAL_Installer_OSX</i>,
    that is <i>OpenAL Installer for MacOS X</i>.)
    </p></dd>

  <dt>Windows users:</dt>
  <dd><p>OpenAL libraries (DLL files) should already be included in all
    binary packages of my games under Windows. So you don't have to do anything.</p>

    <p>However, you're free to remove appropriate DLL files
    (<tt>OpenAL32.dll</tt> and <tt>wrap_oal.dll</tt>) provided in my archives
    and install
    OpenAL yourself. This is especially handy if you
    want to use other OpenAL implementation than the one from Creative.
    OpenAL libraries that are provided by default come from
    <a href="http://connect.creativelabs.com/openal/Downloads/Forms/AllItems.aspx">Creative
    free OpenAL installer</a> (choose <i>oalinst</i>,
    that is <i>OpenAL Installer for Windows</i>).</dd>
</dl>

<p>You can always compile from sources. See
<a href="http://kcat.strangesoft.net/openal.html">OpenAL Soft</a> or
<a href="http://connect.creativelabs.com/openal/OpenAL%20Wiki/Source%20Code.aspx">Creative
source code</a> instructions.

<?php echo $toc->html_section(); ?>

<dl class="command_line_options_list">
  <dt>--no-sound</dt>
  <dd><p>Don't output any sound, even if OpenAL is available and seems perfectly working.
    Our programs will not even try to initialize OpenAL in this case.
    Note that you <i>don't have</i> to use this option if you don't have
    OpenAL installed. This parameter is useful only if you
    have OpenAL, but you don't want to use it for
    whatever reason (e.g. because your cat sleeps on the
    chair beside you and you don't want to wake him up).</dd>

  <dt>--audio-device DEVICE-NAME</dt>
  <dd><p>Use given <tt>DEVICE-NAME</tt> for sound output. Usually, the default
    one should be the best.</p>

    <p>To list the available devices use the <tt>--help</tt> command-line option
    (at the <tt>--audio-device</tt> description you will see a list of devices detected).
    In <?php echo a_href_page('The Castle', 'castle'); ?>
    you can also see and choose them by the <i>Sound output device</i> menu.
    In <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
    you have comfortable menu <i>File -&gt; Preferences -&gt; Sound Device</i>.</p>

    <dl>
      <dt>Windows users:</dt>
      <dd><p>You may see two devices with newer OpenAL implementation:
        "<i>Generic Hardware</i>" (uses <i>DirectSound3D</i>)
        and "<i>Generic Software</i>" (uses <i>DirectSound</i>).
        As far as I know, "<i>Generic Software</i>" can only support
        stereo sound (2 channels, not more). On the other hand, it sometimes
        sounds better than "<i>Generic Hardware</i>".</p>

        <p>So be sure to try both devices and choose the better one.</p>
      </dd>

      <dt>Linux and FreeBSD users with OpenAL-Soft (newer) implementation:</dt>

      <dd><p>The default device selected on Linux should usually be ALSA,
        and should work perfectly on modern systems.</p>

        <p>A useful device is <i>"Wave File Writer"</i>, to record
        3D sound to file. Note that you have to specify output filename
        in config file, otherwise it will not work. For example,
        make a file <tt>$HOME/.alsoftrc</tt> with two lines:</p>

<pre>
[wave]
file = /tmp/output.wav
</pre>

        <p>For more configuration options, look into <tt>/etc/openal/alsoft.conf</tt>
        that should be installed with openal-soft package. It contains
        many comments about the possible options.</p>
      </dd>

      <dt>Linux and FreeBSD users with OpenAL Sample (older) implementation:</dt>
      <dd><p>The following devices are available:

        <table class="thin_borders">
          <tr><th>OpenAL Name</th>
              <th>Device</th>
              <th>Notes</th></tr>
          <tr><td><tt>native</tt></td>
              <td>operating system native</td>
              <td>The default device.</td></tr>
          <tr><td><tt>sdl</tt></td>
              <td>Simple DirectMedia Layer backend</td></tr>
          <tr><td><tt>arts</tt></td>
              <td>aRTs backend</td>
              <td>Note that it seems terribly unstable. Use at your own risk.</td>
              </tr>
          <tr><td><tt>esd</tt></td>
              <td>Esound daemon backend</td></tr>
          <tr><td><tt>alsa</tt></td>
              <td>ALSA backend</td>
              <td><p>While it works OK,
                it tries to acquire the exclusive ownership
                over sound device. So, unlike most other programs that use ALSA,
                OpenAL programs by default <i>cannot</i> play sound when some
                other program already plays sound. For example,
                you cannot run <tt>rhyrthmbox</tt> and listen to your own chosen
                music while playing my games.

                <p>The workaround is to run my games through
                <tt>aoss</tt> wrapper.</td>
              </tr>
          <tr><td><tt>waveout</tt></td>
              <td>WAVE file output</td>
              <td><p>A file named like <tt>openal-0.wav</tt> will be generated
                that contains recorded sound. </p>

                <p>Note that OpenAL
                generates somewhat buggy WAV file, and e.g. gstreamer-based
                programs may not want to play it. My programs will play it,
                but only because I made a workaround specifically for this.
                I found that it's usually a good workaround
                to convert the WAV file with sox,
                like <tt>sox openal-0.wav newopenal-0.wav</tt>.
                Such WAV file generated by sox should be readable everywhere.
                I reported this problem to Debian OpenAL package
                (probably a problem is general and fix will be generally usable
                too) <a href="http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=435754">here</a>.</p>
              </td>
              </tr>
          <tr><td><tt>null</tt></td>
              <td>no output</td></tr>
        </table>

        <p>In the simplest case, to use some other device, e.g. <tt>alsa</tt>,
        just run program with command-line option like:

        <pre>  --audio-device "'(( devices '(alsa) ))"</pre>

        (Yes, the first argument's character is an apostrophe,
        and the whole argument is quoted to avoid being
        splitted by shell. No, it's not Lisp, but it was
        designed to look similar.)

        <p>Then if you will find that e.g. <tt>alsa</tt> device
        sounds best and you want my programs (and all
        other programs that use OpenAL) to always use
        <tt>alsa</tt> device, you can create file
        <tt>/etc/openalrc</tt> or <tt>$HOME/.openalrc</tt> with
        line

        <pre>  ( define devices '(alsa) )</pre>
      </dd>
    </dl>
  </dd>
</dl>

<?php
  vrmlengine_footer();
?>
