<?php
  require_once 'vrmlengine_functions.php';

  vrmlengine_header("glViewImage",
    "glViewImage - a small image viewer using OpenGL. " .
    "Can handle various image formats (PNG, JPEG, BMP, PPM, RGBE). " .
    "Has some nice features that allow you to scale images and test " .
    "are they &quot;tileable&quot;.",
    array('all_programs'));

  $toc = new TableOfContents(
    array(
      new TocItem('Download', 'download'),
      new TocItem('Features', 'features'),
      new TocItem('Running', 'run'),
      new TocItem('Special keys', 'keys'),
      new TocItem('Command-line options', 'command_line_options'),
      new TocItem('Image saving notes', 'saving'),
      new TocItem(DEPENDS, 'depends'),
    )
  );
  $toc->echo_numbers = true;
?>

<?php
  echo pretty_heading("glViewImage", VERSION_GLVIEWIMAGE);
  echo vrmlengine_thumbs(array(
    array('filename' => 'glviewimage_welcome.png', 'titlealt' => 'Screenshot from &quot;glViewImage&quot;'),
    array('filename' => 'glviewimage_dds.png', 'titlealt' => 'Screenshot from &quot;glViewImage&quot;'),
  ));
 ?>

<p><tt>glViewImage</tt> is an image viewer, converter and even
a very limited image editor.</p>

<?php
  echo $toc->html_toc();
  echo $toc->html_section();
?>

<?php echo_standard_program_download('glViewImage', 'glviewimage',
  VERSION_GLVIEWIMAGE,  $std_releases_post_1_8_0); ?>

<p><?php echo S_INSTALLATION_INSTRUCTIONS_SHORT; ?></p>
<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?></p>

<?php echo $toc->html_section(); ?>

<p><tt>glViewImage</tt> was originally developed to test and demonstrate
the power of image handling inside our VRML engine, but it became quite usable
utility on it's own. Viewed image can be quickly scaled and moved,
you can browse images list, you can test how image looks when tiled
(to test is it good for a texture or desktop wallpaper),
you can see how alpha channel of the image looks on various background colors.
The ability to load and save even some uncommon image formats (DDS, RGBE)
is also very useful.</p>

<p>Various image formats are supported:

<ul>
  <li><b>PNG</b> - Portable Network Graphic,
  <li><b>BMP</b> - Windows Bitmap,
  <li><b>PPM</b> - Portable Pixel Map,
  <li><b>DDS</b> - <a href="http://en.wikipedia.org/wiki/DirectDraw_Surface">Direct
    Draw Surface</a>. This image format is commonly used for advanced
    texturing, as it can store compressed
    textures, possibly with mipmaps, cube maps, volume textures.
    You can view all subimages within one DDS file, see menu items
    <i>Images-&gt;Next/Previous subimage in DDS</i>. Saving to DDS images
    is also supported.
  <li><b>RGBE</b> - Red + Green + Blue + Exponent,
    format made by Greg Ward, described in "<i>Graphic Gems II</i>",
    used e.g. in <a href="http://floyd.lbl.gov/radiance/">
    Radiance</a>. It allows storing colors with high dynamic range.
  <li><b>IPL</b> - IPLab, only 16 bits per pixel are supported (gray-scale).
  <li>Many image formats are loaded using the excellent <a href="http://wiki.freepascal.org/fcl-image">FPC fcl-image</a>
    library: <b>JPEG, GIF, TGA, XPM, PSD, PCX</b>.
    This gives us native support for these formats (no extra libraries needed,
    fcl-image is compiled in).
  <li>Many image formats may be loaded by using external programs.
    This means that some other program is run "under the hood"
    to convert to some format supported natively. If all goes well,
    this is completely transparent for user.
    <!-- (usually PNG, because it's lossless and preserves alpha channel). -->
    For now, this is used to load
    <b>TIFF</b>, <b>SGI</b>, <b>JP2</b> and <b>EXR</b> files.
    <tt>convert</tt> program from
    <a href="http://www.imagemagick.org/">ImageMagick</a>
    package must be available on $PATH for this to work.
</ul>

<?php echo $toc->html_section(); ?>

<p>You can simply run the <tt>glViewImage</tt> without parameters,
and open / save images using the menu commands.</p>

<?php echo $toc->html_section(); ?>

<table border="1" class="key_list">
  <tr><th colspan="2">
    Keys not available as menu items:
  <tr><td>Arrows            <td>move image
  <tr><td>Arrows + Ctrl     <td>move image 10 x faster
  <tr><td>- / +             <td>scale image (smaller / larger)
  <tr><td>x / X             <td>scale only horizontally (smaller / larger)
  <tr><td>y / Y             <td>scale only vertically (smaller / larger)
</table>

<p>Notes about opening image:
glViewImage guesses image format using file extension (yes, yes,
I will change it at some time to recognize image format based on
file content), so it's important for files to have good
filename extension. JPEG images may have extension <tt>jpg</tt>
or <tt>jpeg</tt>, RGBE images - <tt>rgbe</tt> or <tt>pic</tt>,
rest is obvious.

<p>Opened image is also added to the image list.

<?php echo $toc->html_section(); ?>

<p>glViewImage remembers image list <!-- (i.e., it's actually a filename list) -->
that you can browse using <b>N</b> (next image on the list) and
<b>P</b> (previous image on the list) keys. When you run
glViewImage you can give it as parameters a list of images to browse.

<p>Every parameter must be one of:
<ul>
  <li><p>A directory name &mdash; glViewImage will display all images
    found in this directory.

    <p>E.g. run <tt>glViewImage ~/my_images/</tt> to display
    all images in <tt>~/my_images/</tt> directory.

  <li><p>A filename mask &mdash; glViewImage will display all images
    matching given mask, where <tt>'?'</tt> matches any char and
    <tt>'*'</tt> matches any number of any chars.
    Of course, users of bash and similar non-Windows shells can utilize
    shell filename expansion instead.

    <p>E.g. run <tt>glViewImage ~/my_images/*.png</tt> to display
    all PNG images in directory <tt>~/my_images/</tt>.
    Or just run <tt>glViewImage ~/my_image.png</tt>
    to display only one image in file <tt>~/my_image.png</tt>.

  <li><p><tt>@&lt;filename&gt;</tt> &mdash; glViewImage will read
    image filenames from file <tt>&lt;filename&gt;</tt>,
    each line is one filename
    (<tt>&lt;filename&gt;</tt> "-" means "standard input", as usual;
    so you can pipe output of e.g. <tt>find</tt> program to glViewImage).
</ul>

<!-- Example of use: find . -name '*.jpg' -print | glViewImage.exe @- -->

<p>Running <tt>glViewImage</tt> with no parameters is equivalent
to running<br>
<tt>&nbsp;&nbsp;glViewImage .</tt><br>
so you will view all images (that glViewImage can handle) in the current directory.
If none found, the default welcome image will be displayed.

<p>Oh, and (as usual) all parameters described in those pages:
<?php echo a_href_page("standard options understood by my OpenGL programs",
"opengl_options") ?> and <?php echo a_href_page(
"some notes about command-line options understood by my programs",
"common_options") ?>
 are available. If you will not give any parameter that forces some
window size (like <tt>--geometry</tt>) then program will open a window
with the same size as the first displayed image.

<a name="about_saving"></a>
<?php echo $toc->html_section(); ?>

<p>Resulting image format is determined by filename extension, unknown
extension will result in BMP format.

<p>Image loaded and displayed by glViewImage is internally always stored
in format comfortable for OpenGL. This includes many formats, but not RGBE.
It means that if you will load RGBE image to glViewImage and then
you will save it (even to the RGBE format again)
then you loose RGBE precision (and clamp color values above 1.0).

<p>Also, S3TC compressed images (from DDS files) will be always decompressed,
and saving them back will always make uncompressed files.

<a name="section_depends"></a>
<?php echo $toc->html_section(); ?>

<?php echo depends_ul(array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  DEPENDS_UNIX_GLWINDOW_GTK_2,
  DEPENDS_MACOSX)); ?>

<p><tt>convert</tt> program from
<a href="http://www.imagemagick.org/">ImageMagick</a>
package must be available on $PATH for some image formats to work.

<!--
<hr>  ===================================================
Commented out, below is development info.

<h2>For curious, a few words about implementation</h2>

Compiled with <a href="http://www.freepascal.org/">FreePascal</a>.
PNG format handled using <a href="http://www.libpng.org">libpng</a>.
JPEG handled using
<a href="http://www.nomssi.de/pasjpeg/pasjpeg.html">pasjpeg</a>
(this is code originally written by
<a href="http://www.ijg.org/">Independent JPEG Group</a> in ANSI C
and translated to Delphi by Nomssi Nzali Jacques H. C).
-->

<!-- li> Mój własny kod obsługuje BMP, PPM, RGBE, czystym przypadkiem
zrobiłem też PCXy 256 kolorowe i pewien specyficzny przypadek formatu IPLab.
-->

<?php
  vrmlengine_footer();
?>
