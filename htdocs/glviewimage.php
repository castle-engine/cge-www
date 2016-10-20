<?php
  define('CASTLE_GITHUB_NAME', 'glviewimage');

  require_once 'castle_engine_functions.php';
  castle_header("glViewImage",
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
?>

<?php
  echo pretty_heading("glViewImage", VERSION_GLVIEWIMAGE);
  echo castle_thumbs(array(
    array('filename' => 'glviewimage_welcome.png', 'titlealt' => 'Screenshot from &quot;glViewImage&quot;'),
    array('filename' => 'glviewimage_dds.png', 'titlealt' => 'Screenshot from &quot;glViewImage&quot;'),
  ));
 ?>

<p><code>glViewImage</code> is an image viewer, converter and even
a very limited image editor.</p>

<?php
  echo $toc->html_toc();
  echo $toc->html_section();
?>

<?php echo_standard_program_download('glViewImage', 'glviewimage',
  VERSION_GLVIEWIMAGE, true); ?>

<p><?php echo S_INSTALLATION_INSTRUCTIONS_SHORT; ?></p>
<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?></p>

<?php echo $toc->html_section(); ?>

<p><code>glViewImage</code> was originally developed to demonstrate
the power of image handling inside our <?php echo a_href_page(
'Castle Game Engine', 'index'); ?>. It became quite useful utility on it's own, with the following features:

<ul>
  <li><b>Load and save many image formats</b>, including some "exotic" ones: <b>DDS, RGBE</b>.
  <li>Easily <b>scale and move</b> around the image (use keys or mouse dragging and mouse wheel).
  <li><b>Browse</b> all the images within a single directory (use keys <code>N</code>, <code>P</code> for next, previous).
  <li>Test how image looks when <b>tiled</b> (to test is it good for a texture or desktop wallpaper).
  <li>See how <b>alpha channel</b> of the image looks (menu <i>View -&gt; Use Image Alpha Channel</i>, <i>View -&gt; Background Color</i>),
  <li>Edit image to <b>mirror, rotate, resize, make grayscale</b> and so on.
  <li>Perform <b>alpha bleeding</b> (fixes the artifacts when scaling textures with transparent parts).
  <li>You can browse all <b>subimages (like mipmaps, or layers of 3D textures)</b> within a composite image formats (like DDS).
</ul>

<p>Many image formats are supported:

<ul>
  <li><b>PNG</b>: Portable Network Graphic. Excellent open format for images,
    offering good lossless image compression
    and full alpha channel.
  </li>

  <li><b>JPEG, GIF, TGA, XPM, PSD, PCX, PNM (PBM, PGM, PPM)</b>:
    are loaded using the excellent <a href="http://wiki.freepascal.org/fcl-image">FPC
    fcl-image</a> library.
    This gives us full support for these formats, without any extra
    libraries necessary (fcl-image is compiled inside our programs).
  </li>

  <li><b>DDS</b>: <a href="http://en.wikipedia.org/wiki/DirectDraw_Surface">Direct
    Draw Surface</a>. This image format may be used for advanced
    texturing, as it can store textures compressed for GPUs,
    possibly with mipmaps, cube maps, volume textures.
    With <code>glViewImage</code> you can view all subimages
    within one DDS file, see menu items
    <i>Images-&gt;Next/Previous subimage in DDS</i>. Saving to DDS images
    is also supported. <?php echo a_href_page_hashlink(
    'Details about using DDS format for textures in our engine are here.',
    'x3d_implementation_texturing', 'section_dds'); ?>
  </li>

  <li><b>RGBE</b>: simple HDR (high dynamic range) format.
    The format name is an acronym for <i>Red + Green + Blue + Exponent</i>,
    it was developed by by Greg Ward, described in "<i>Graphic Gems II</i>",
    used e.g. in <a href="http://floyd.lbl.gov/radiance/">Radiance</a>.
  </li>

  <li><b>BMP</b>: Windows Bitmap. Native support.</li>

  <!--li><b>PPM</b>: Portable Pixel Map. Simple uncompressed image format,
    supported by practically all graphic programs. Native support.
    (Actually for now PPM handling doesn't use FpImage, but this may be temporary,
    and doesn't really matter for user.)
  -->

  <li><b>IPL</b>: IPLab image format. Only 16 bits per pixel are supported (gray-scale).</li>

  <li><p><b>TIFF</b>, <b>SGI</b>, <b>JP2</b>, <b>EXR</b>:
    To load and save these images, you need to install
    the <a href="http://www.imagemagick.org/">ImageMagick</a>
    package, and make sure it's <code>convert</code> program is available on $PATH.
    Then our engine will recognize these image formats,
    and seamlessly load/save them (by running <code>convert</code> "under the hood").
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>You can simply run the <code>glViewImage</code> without parameters,
and open / save images using the menu commands.</p>

<?php echo $toc->html_section(); ?>

<table class="key_list">
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
filename extension. JPEG images may have extension <code>jpg</code>
or <code>jpeg</code>, RGBE images - <code>rgbe</code> or <code>pic</code>,
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

    <p>E.g. run <code>glViewImage ~/my_images/</code> to display
    all images in <code>~/my_images/</code> directory.

  <li><p>A filename &mdash; glViewImage will display this image,
    and also allow to browse (by previous/next) other images in the same
    directory.

  <li><p><code>@&lt;filename&gt;</code> &mdash; glViewImage will read
    image filenames from file <code>&lt;filename&gt;</code>,
    each line is one filename
    (<code>&lt;filename&gt;</code> "-" means "standard input", as usual;
    so you can pipe output of e.g. <code>find</code> program to glViewImage).
</ul>

<!-- Example of use: find . -name '*.jpg' -print | glViewImage.exe @- -->

<p>Running <code>glViewImage</code> with no parameters is equivalent
to running<br>
<code>&nbsp;&nbsp;glViewImage .</code><br>
so you will view all images (that glViewImage can handle) in the current directory.
If none found, the default welcome image will be displayed.

<p>Oh, and (as usual) all parameters described in those pages:
<?php echo a_href_page("standard options understood by my OpenGL programs",
"opengl_options") ?> and <?php echo a_href_page(
"some notes about command-line options understood by my programs",
"common_options") ?>
 are available. If you will not give any parameter that forces some
window size (like <code>--geometry</code>) then program will open a window
with the same size as the first displayed image.

<?php echo $toc->html_section(); ?>

<p>Resulting image format is determined by filename extension, unknown
extension will result in error.

<p>Image loaded and displayed by glViewImage is internally always stored
in format comfortable for OpenGL. This includes many formats, but not RGBE.
It means that if you will load RGBE image to glViewImage and then
you will save it (even to the RGBE format again)
then you loose RGBE precision (and clamp color values above 1.0).

<p>Also, S3TC compressed images (from DDS files) will be always decompressed,
and saving them back will always make uncompressed files.

<?php echo $toc->html_section(); ?>

<?php echo depends_ul(array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  DEPENDS_UNIX_CASTLE_WINDOW_GTK_2,
  DEPENDS_MACOSX)); ?>

<p><code>convert</code> program from
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
  castle_footer();
?>
