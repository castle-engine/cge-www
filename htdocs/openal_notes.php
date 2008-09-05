<?php
  require "vrmlengine_functions.php";
  common_header('Notes related to all my programs using OpenAL', LANG_EN);

  $toc = new TableOfContents(
    array(
      new TocItem('Installing OpenAL', 'install'),
      new TocItem('Command-line options respected by all my programs that use OpenAL',
        'options'),
      new TocItem('FreeBSD specific notes', 'freebsd')
    )
  );
?>

<h2>Notes related to all my programs using OpenAL</h2>

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
    Under <i>Debian etch</i> this is package <tt>libopenal0a</tt>,
    under <i>Debian lenny</i> <tt>libopenal1</tt>. For other distros,
    look for something similar, with <tt>openal</tt> in their name.
    </p>

    <p>My games work with both common OpenAL implementations:
    <i>OpenAL Sample Implementation</i> (the original implementation,
    from Loki, unmaintained) and
    <a href="http://kcat.strangesoft.net/openal.html">OpenAL Soft</a>
    (the newer, maintained version).</p>
    </dd>

  <dt>FreeBSD users:</dt>
  <dd><p>Simple <tt>pkg_add -r openal</tt> command should do
    the trick.</p></dd>

  <dt>Mac OS X users:</dt>
  <dd><p>Newer Mac OS X seems to have this automatically installed.
    If not, you can download and run
    <a href="http://connect.creativelabs.com/openal/Downloads/Forms/AllItems.aspx">Creative
    free OpenAL installer for Mac OS X</a>. (Choose <i>OpenAL_Installer_OSX</i>,
    that is <i>OpenAL Installer for MacOS X</i>.)
    </p></dd>

  <dt>Windows users:</dt>
  <dd><p>OpenAL libraries (DLL files) should already be included in all
    binary packages of my games under Windows. So you don't have to do anything.

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
  <dd><p>Don't use OpenAL, even if it's available. The program
    will not even try to initialize OpenAL. Note that you
    <i>don't have</i> to use this option if you don't have
    OpenAL installed. This parameter is useful only if you
    have OpenAL, but you don't want to use it for
    whatever reason (e.g. because your cat sleeps on the
    chair beside you and you don't want to wake him up).</dd>

  <dt>--audio-device OPENAL-DEVICE-NAME</dt>
  <dd><p>Use given <tt>OPENAL-DEVICE-NAME</tt>. Details follow:

    <p>Since the default OpenAL implementations
    are purely in software, various OpenAL devices often
    mean that various software "backends" will be used.

    <p>There is no sure way to determine which OpenAL
    device will sound and work best, be fastest etc.
    You just have to try them all to be sure
    that you get all you can from OpenAL.

    <dl>
      <dt>Linux and FreeBSD users with OpenAL SI implementation:</dt>
      <dd><p>Usual OpenAL implementation will have the following
        devices available:

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

      <dt>Windows users:</dt>
      <dd><p>Newer Creative's OpenAL implementation has two devices:
        <tt>Generic Hardware</tt> (uses <tt>DirectSound3D</tt>)
        and <tt>Generic Software</tt> (uses <tt>DirectSound</tt>).

        <p>Like with Unices, it may be worth checking every
        available device to find which one suits you best.
        Call my programs with command-line options like
        <pre>
  --audio-device "Generic Hardware"
  --audio-device "Generic Software"
</pre>
      </dd>
    </dl>
  </dd>

  <dt>--print-audio-devices</dt>
  <dd><p>Query OpenAL for a list of available devices. Prints
    all allowed arguments for <tt>--audio-device</tt>
    option given above. Note that not all OpenAL
    implementations support it.</dd>
</dl>

<?php echo $toc->html_section(); ?>

<p><b>In FreeBSD 5.x</b> I had some serious problems getting OpenAL to
work smoothly within my OpenGL programs.
It was reported on openal mailing list, but this was probably the kind of problem
that results from a combination of various things &mdash;
like my particular sound card, maybe combined with NVidia OpenGL using
some significant CPU time etc. So nothing was solved.
Only <tt>waveout</tt> and <tt>null</tt> OpenAL devices worked correctly.
So if you experience problems, you should probably live without
OpenAL sound under FreeBSD. Run my programs with <tt>--no-sound</tt> option.</p>

<p><b>In FreeBSD 6.x</b> I was glad to notice that OpenAL works smoothly.</p>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("openal_notes", TRUE);
  };

  common_footer();
?>
