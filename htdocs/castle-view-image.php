<?php
define('CASTLE_GITHUB_NAME', 'castle-view-image');

require_once 'castle_engine_functions.php';
castle_header("castle-view-image", array(
  'social_share_image' => 'castle-view-image_welcome.png',
  'meta_description' => 'Image viewer using Castle Game Engine. Handles various image formats - common ones (PNG, JPEG, BMP, PPM) and some exotic (KTX, DDS, RGBE). Includes some specialized editing functions - to scale, to perform alpha bleeding (fix alpha on textures with transparency)',
));

$toc = new TableOfContents(
  array(
    new TocItem('Features', 'features'),
    new TocItem('Running', 'run'),
    new TocItem('Special keys', 'keys'),
    new TocItem('Command-line options', 'command_line_options'),
    new TocItem('Image saving notes', 'saving'),
    new TocItem(DEPENDS, 'depends'),
  )
);

echo pretty_heading("castle-view-image", VERSION_CASTLE_VIEW_IMAGE);
echo castle_thumbs(array(
  array('filename' => 'castle-view-image_welcome.png', 'titlealt' => 'Screenshot from &quot;castle-view-image&quot;'),
  array('filename' => 'castle-view-image_dds.png', 'titlealt' => 'Screenshot from &quot;castle-view-image&quot;'),
));
?>

<p><code>castle-view-image</code> is an image viewer, converter and even
a very limited image editor.</p>

<p><b>If you already have <a href="index.php">Castle Game Engine</a>, then just run <code>castle-view-image</code> executable in CGE <code>bin</code> subdirectory. There's no need to download it separately.</b>

<?php
echo_standard_program_download(
  'castle-view-image', 'castle-view-image', VERSION_CASTLE_VIEW_IMAGE,
  array(
    'win-x86_64' => 'https://github.com/castle-engine/castle-view-image/releases/download/v' . VERSION_CASTLE_VIEW_IMAGE . '/castle-view-image-' . VERSION_CASTLE_VIEW_IMAGE . '-win64-x86_64.zip',
    'linux-x86_64' => 'https://github.com/castle-engine/castle-view-image/releases/download/v' . VERSION_CASTLE_VIEW_IMAGE . '/castle-view-image-' . VERSION_CASTLE_VIEW_IMAGE . '-linux-x86_64.tar.gz',
  )
);
?>

<p>Documentation:</p>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><code>castle-view-image</code> was originally developed to demonstrate
the power of image handling inside our <?php echo a_href_page(
'Castle Game Engine', 'index'); ?>. It became quite useful utility on it's own, with the following features:

<ul>
  <li><b>Load and save many image formats</b>, including some "exotic" ones: <b>KTX, DDS, RGBE</b>.
  <li>Easily <b>scale and move</b> around the image (use keys or mouse dragging and mouse wheel).
  <li><b>Browse</b> all the images within a single directory (use keys <code>N</code>, <code>P</code> for next, previous).
  <li>Test how image looks when <b>tiled</b> (to test is it good for a texture or desktop wallpaper).
  <li>See how <b>alpha channel</b> of the image looks (menu <i>View -&gt; Use Image Alpha Channel</i>, <i>View -&gt; Background Color</i>),
  <li>Edit image to <b>mirror, rotate, resize, make grayscale</b> and so on.
  <li>Perform <b>alpha bleeding</b> (fixes the artifacts when scaling textures with transparent parts).
  <li>You can browse all <b>subimages (like mipmaps, or layers of 3D textures)</b> within a composite image formats (like KTX, DDS).
</ul>

<p>Many image formats are supported:

<ul>
  <li>
    <p><b>PNG</b>: Portable Network Graphic. Excellent open format for images,
    offering good lossless image compression,
    full alpha channel if you need it,
    supported by almost every other image software in the world.

    <p>Loaded using <a href="http://www.libpng.org/">LibPNG</a>, with a fallback
    (in case LibPNG is missing) on Pascal loaders in
    <a href="http://wiki.freepascal.org/fcl-image">FPC fcl-image</a>
    or <a href="https://castle-engine.io/wp/2021/12/18/integration-with-vampyre-imaging-library-new-example-image_display-to-test-image-loading-speed-and-format-support/">Vampyre Imaging Library</a>.
  </li>

  <li>
    <p><b>JPEG, GIF, TGA, XPM, PSD, PCX, PNM, PBM, PGM, PPM</b>.

    <p>Loaded using <a href="http://wiki.freepascal.org/fcl-image">FPC fcl-image</a> library
    or <a href="https://castle-engine.io/wp/2021/12/18/integration-with-vampyre-imaging-library-new-example-image_display-to-test-image-loading-speed-and-format-support/">Vampyre Imaging Library</a>
    (depending on your compiler and configuration).

    <p><i>Note:</i> We do not recommend using PSD images in CGE data. While it works, PSD is still an internal file format that is really implemented perfectly only by Photoshop. It's like XCF in GIMP, like BLEND in Blender, like MAX in 3ds Max... Applications are not really supposed to be able to read these formats, they are complicated (if one would strive to handle 100% of their features). Our PSD readers support some common features, they for sure don't support 100% of PSD features.

    <p>You of course can use Photoshop, and save your files as PSD for development. But then export them to PNG for CGE to read. That's a normal workflow, for usage of PSD anywhere (not only in CGE). PSD is not a final format, it is a development format, like a source code -&gt; you want to export it to something else to use it (PNG is usually the best choice, full-featured and lossless).

    <p><i>Note:</i> For most of the above formats, PNG is a better alternative. E.g. PNG will result in smaller file size (without losing any features) compared to PNM, PBM, PGM, PPM, XCF. PNG will be loaded much much faster than XCF (text-only format that is very slow to load). PNG is more widely supported and has proper specification, unlike PCX.
  </li>

  <li>
    <p><b>KTX</b>: <a href="https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/">Khronos
    Texture format</a>.

    <p>Excellent image format to utilize various graphic features,
    like GPU compression (images stay compressed in GPU memory),
    explicit mipmaps, cube maps, volume (3D) textures.

    <?php echo a_href_page_hashlink(
    'Details about using KTX format for textures in our engine are here.',
    'x3d_implementation_texturing', 'section_ktx'); ?>

    <p>With <code>castle-view-image</code> you can view all subimages
    within one DDS file, see menu items
    <i>Images-&gt;Next/Previous Subimage in composite (DDS, KTX)</i>.
  </li>

  <li>
    <p><b>DDS</b>: <a href="https://en.wikipedia.org/wiki/DirectDraw_Surface">Direct
    Draw Surface</a>. Similar to KTX.
    Saving to DDS images is also supported. <?php echo a_href_page_hashlink(
    'Details about using DDS format for textures in our engine are here.',
    'x3d_implementation_texturing', 'section_dds'); ?>
  </li>

  <li>
    <p><b>RGBE</b>: Simple HDR (high dynamic range) format.
    The format name is an acronym for <i>Red + Green + Blue + Exponent</i>,
    it was developed by by Greg Ward, described in "<i>Graphic Gems II</i>",
    used e.g. in <a href="http://floyd.lbl.gov/radiance/">Radiance</a>.

    <p>It makes sense to use this if you need data in floating-point format,
    e.g. to load it to <?php echo cgeRef('TRGBFloatImage'); ?>.
  </li>

  <li>
    <p><b>BMP</b>: Windows Bitmap. Native support. Only for non-compressed (RLE compressed are not handled).</li>

  <!--li><b>PPM</b>: Portable Pixel Map. Simple uncompressed image format,
    supported by practically all graphic programs. Native support.
    (Actually for now PPM handling doesn't use FpImage, but this may be temporary,
    and doesn't really matter for user.)
  -->

  <li>
    <p><b>TIFF</b>, <b>JP2</b>:
    Supported using
    <a href="https://castle-engine.io/wp/2021/12/18/integration-with-vampyre-imaging-library-new-example-image_display-to-test-image-loading-speed-and-format-support/">Vampyre Imaging Library</a>.

    <p>Note: JP2 support is not available on all platforms.
    Quoting <i>Vampyre</i> comments: <i>JPEG2000 only for 32bit Windows/Linux/OSX and for 64bit Unix with FPC</i>.
    See the <a href="https://github.com/castle-engine/castle-engine/blob/master/src/vampyre_imaginglib/src/Extensions/ImagingJpeg2000.pas#L19">exact rule in source code</a> that sets allowed platforms.

    <p>Same for TIFF. As it was causing trouble on mobile (not possible to use on Arm, would require distributing extra libs for Aarch64) we decided to disable it by default, for now. You can use <code>ImagingTiff</code> unit (add it to uses clause anywhere in your source code) and define <code>CASTLE_ENABLE_TIFF</code> to use it.
  </li>

  <li>
    <p><b>IPL</b>: IPLab image format. Only 16 bits per pixel are supported (gray-scale).
    <i>Deprecated:</i> Due to the fact that it has probably 0 usage nowadays.
    Let me know <a href="talk.php">on forum or Discord</a> if you use this image format
    and want to keep using it.
</ul>

<?php echo $toc->html_section(); ?>

<p>You can simply run the <code>castle-view-image</code> without parameters,
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
castle-view-image guesses image format using file extension (yes, yes,
I will change it at some time to recognize image format based on
file content), so it's important for files to have good
filename extension. JPEG images may have extension <code>jpg</code>
or <code>jpeg</code>, RGBE images - <code>rgbe</code> or <code>pic</code>,
rest is obvious.

<p>Opened image is also added to the image list.

<?php echo $toc->html_section(); ?>

<p>castle-view-image remembers image list <!-- (i.e., it's actually a filename list) -->
that you can browse using <b>N</b> (next image on the list) and
<b>P</b> (previous image on the list) keys. When you run
castle-view-image you can give it as parameters a list of images to browse.

<p>Every parameter must be one of:
<ul>
  <li><p>A directory name &mdash; castle-view-image will display all images
    found in this directory.

    <p>E.g. run <code>castle-view-image ~/my_images/</code> to display
    all images in <code>~/my_images/</code> directory.

  <li><p>A filename &mdash; castle-view-image will display this image,
    and also allow to browse (by previous/next) other images in the same
    directory.

  <li><p><code>@&lt;filename&gt;</code> &mdash; castle-view-image will read
    image filenames from file <code>&lt;filename&gt;</code>,
    each line is one filename
    (<code>&lt;filename&gt;</code> "-" means "standard input", as usual;
    so you can pipe output of e.g. <code>find</code> program to castle-view-image).
</ul>

<!-- Example of use: find . -name '*.jpg' -print | castle-view-image.exe @- -->

<p>Running <code>castle-view-image</code> with no parameters is equivalent
to running<br>
<code>&nbsp;&nbsp;castle-view-image .</code><br>
so you will view all images (that castle-view-image can handle) in the current directory.
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

<p>Image loaded and displayed by castle-view-image is internally always stored
in format comfortable for OpenGL. This includes many formats, but not RGBE.
It means that if you will load RGBE image to castle-view-image and then
you will save it (even to the RGBE format again)
then you loose RGBE precision (and clamp color values above 1.0).

<p>Also, S3TC compressed images (from KTX, DDS files) will be always decompressed,
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

Compiled with <a href="http://www.freepascal.org/">Free Pascal Compiler</a>.
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
