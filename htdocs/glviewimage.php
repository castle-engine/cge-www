<?php
  require_once 'vrmlengine_functions.php';

  common_header("glViewImage", LANG_EN,
    "glViewImage - a small image viewer using OpenGL. " .
    "Can handle various image formats (PNG, JPEG, BMP, PPM, RGBE). " .
    "Has some nice features that allow you to scale images and test " .
    "are they &quot;tileable&quot;.");
?>

<?php echo pretty_heading("glViewImage", VERSION_GLVIEWIMAGE); ?>

   <!-- Technologicznie ten program
   to nic wielkiego, ale zasadnicza cze¶æ jego kodu jest u¿ywana
   we wszystkich moich wiêkszych programach, m.in.
   do odczytu tekstur i zapisu save screenów. -->

<p>This is a simple image viewer. It allows you to scale and move
displayed image, you can browse image list, you can also
test is image "tileable" (e.g. to test is it good for a texture or
desktop wallpaper). It can handle various image formats:

<ul>
  <li><b>PNG</b> - Portable Network Graphic,
  <li><b>JPEG</b> - actually JFIF, JPEG File Interchange Format,
  <li><b>BMP</b> - Windows Bitmap,
  <li><b>PPM</b> - Portable Pixel Map,
  <li><b>RGBE</b> - Red + Green + Blue + Exponent,
    format made by Greg Ward, described in "<i>Graphic Gems II</i>",
    used e.g. in <a href="http://floyd.lbl.gov/radiance/">
    Radiance</a>. It allows storing colors with high dynamic range.
  <li>And there are some other formats that I implemented
    only "by accident"; their implementation is only partial
    and I do not plan to make it better.
    Those are: <b>PCX</b> (only 256-colored PCX are supported)
    and <b>IPL</b> (IPLab, only 16 bits per pixel are supported (gray-scale)).
  <li>Other image formats may be loaded by using external programs.
    This means that some other program is run "under the hood"
    to convert to some format supported natively. If all goes well,
    this is completely transparent for user.
    <!-- (usually PNG, because it's lossless and preserves alpha channel). -->
    For now, this is used to load
    <b>GIF</b> and <b>TGA</b> files. <tt>convert</tt> program from
    <a href="http://www.imagemagick.org/">ImageMagick</a>
    package must be available on $PATH for this to work.
</ul>

<p>These are binaries of the program. No special installation is required,
just unpack the archive and run <tt>glViewImage</tt>.
<?php echo_standard_program_download('glViewImage', 'glviewimage',
  VERSION_GLVIEWIMAGE,  $std_releases_pre_1_2_0); ?>

<?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?>

<h2>Using</h2>

<p>glViewImage remembers image list (i.e., it's actually a filename list)
that you can browse using <b>N</b> (next image on the list) and
<b>P</b> (previous image on the list) keys. When you run
glViewImage you give it as parameters list of images to browse.

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
so you will view all images (that glViewImage can handle) in current directory.

<p>Oh, and (as usual) all parameters described in those pages:
<?php echo a_href_page("standard options understood by my OpenGL programs",
"opengl_options") ?> and <?php echo a_href_page(
"some notes about command-line options understood by my programs",
"common_options") ?>
 are available. If you will not give any parameter that forces some
window size (like <tt>--geometry</tt>) then program will open a window
with the same size as the first displayed image.

<h2>Controls</h2>

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

<p>Opened image is also added to image list.

<h2><a name="about_saving">Some notes about saving image</a></h2>

<p>Resulting image format is determined by filename extension, unknown
extension will result in BMP format.

<p>Image loaded and displayed by glViewImage is internally always stored
in 24-bit RGB format. Any other information (like other pixel format,
alpha channel of PNG images, high precision of RGBE images -
really <b>any</b> other information) is not saved by glViewImage.
It means that if you will load an image to glViewImage and then
you will save it (even to the same image format, e.g. load PNG
and save as PNG) then you can loose some important information contained
in original image.

<p>This is a serious disadvantage of saving images from glViewImage,
but I am not planning to improve this - glViewImage was written as a
simple image viewer, not as a professional image converter.
If you need professional image converter, take a look at
<a href="http://www.gimp.org/">GIMP</a> or
<a href="http://www.imagemagick.org/">ImageMagick</a>
(both of them free (<i>free</i> in both senses), for various operating
systems etc.).

<h2><a name="section_depends">Requirements</a></h2>

<?php echo depends_ul(array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  DEPENDS_UNIX_GLWINDOW_GTK_2,
  DEPENDS_MACOSX)); ?>

<hr> <!-- =================================================== -->

<h2>For curious, a few words about implementation</h2>

Compiled with <a href="http://www.freepascal.org/">FreePascal</a>.
PNG format handled using <a href="http://www.libpng.org">libpng</a>.
JPEG handled using
<a href="http://www.nomssi.de/pasjpeg/pasjpeg.html">pasjpeg</a>
(this is code originally written by
<a href="http://www.ijg.org/">Independent JPEG Group</a> in ANSI C
and translated to Delphi by Nomssi Nzali Jacques H. C).

<!-- li> Mój w³asny kod obs³uguje BMP, PPM, RGBE, czystym przypadkiem
zrobi³em te¿ PCXy 256 kolorowe i pewien specyficzny przypadek formatu IPLab.
-->

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("glviewimage", TRUE);
  };

  common_footer();
?>