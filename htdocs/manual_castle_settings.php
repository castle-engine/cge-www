<?php
require_once 'castle_engine_functions.php';
manual_header('Customize look by CastleSettings.xml');

$toc = new TableOfContents(
  array(
    new TocItem('Description', 'description'),
    new TocItem('Simple example', 'example'),
    new TocItem('All possible settings', 'all_settings'),
      new TocItem('UI scaling', 'ui_scaling', 1),
      new TocItem('Default font', 'default_font', 1),
      new TocItem('Warmup cache', 'warmup_cache', 1),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>You can place a file named <code>CastleSettings.xml</code> in your
<a href="manual_data_directory.php">data directory</a>
to influence the look of your application.
From the Pascal code (usually, early in the
<code>Application.OnInitialize</code> callback) load it by calling <code>Window.Container.LoadSettings('castle-data:/CastleSettings.xml');</code>.

<p>The <a href="manual_editor.php">Castle Game Engine Editor</a> will also
automatically load this file. This way the editor can show (at design-time)
your application in the same way as it will appear to the user.
In the future, the <code>CastleSettings.xml</code> file will be editable by
the CGE editor GUI. For now just edit it directly, in any text editor (like Lazarus).

<?php echo $toc->html_section(); ?>

<?php echo xml_full_highlight(
'<?xml version="1.0" encoding="utf-8"?>
<castle_settings>
  <ui_scaling
    mode="EncloseReferenceSize"
    reference_width="1600"
    reference_height="900"
  />
  <default_font
    url="castle-data:/MyFontFile.ttf"
  />
</castle_settings>'); ?>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>Loading the <code>CastleSettings.xml</code> sets properties
related to user interface scaling: <?php api_link('Container.UIScaling', 'CastleUIControls.TUIContainer.html#UIScaling'); ?>,
<?php api_link('Container.UIReferenceWidth', 'CastleUIControls.TUIContainer.html#UIReferenceWidth'); ?>,
<?php api_link('Container.UIReferenceHeight', 'CastleUIControls.TUIContainer.html#UIReferenceHeight'); ?>.

<?php echo xml_full_highlight(
'<ui_scaling
  mode="EncloseReferenceSize"
  reference_width="1600"
  reference_height="900"
/>'); ?>

<p>These settings control <i>user interface scaling</i>.
See the <?php api_link('UIScaling', 'CastleUIControls.TUIContainer.html#UIScaling'); ?>
 documentation and <a href="manual_2d_user_interface.php">manual page about 2D user interface</a>
for an explanation how it works. In short, it means that you can "think" that an application
is designed for a specified window size (like 1600x900).
You set the positions and sizes (in code, in editor) relative to this desired window size.
When the application runs, the UI controls are scaled to adjust to the actual window size.
The scaling is "smart", which means that a different aspect ratio is not a problem
(as long as you set proper anchors).
The scaling only transforms the UI coordinates &mdash; the controls are actually rendered
at the final size. So the scaling doesn't reduce quality in any way.

<p>The default value of <code>UIScaling</code> is <code>usNone</code>,
so no scaling is done.
If you don't specify <code>&lt;ui_scaling&gt;</code> element in <code>CastleSettings.xml</code>,
we keep using this default.

<p><b>We advise all new cross-platform projects to activate UI scaling by <code>mode="EncloseReferenceSize"</code>
in <code>CastleSettings.xml</code></b>.
This is the best practical way to achieve a consistent look
on various screen sizes and devices.
It is useful even for desktop-only applications (since people have
various monitor resolutions), and it is crucial for mobile and console applications
(where devices have wildly different resolutions).

<p>The <i>"New Project"</i> templates provided by the CGE editor
all set up by default UI scaling to the reference sizes of 1600x900
(the most popular aspect ratio in 2018).

<p>The allowed values for <code>mode</code> are
<ul>
  <li>None
  <li>EncloseReferenceSize
  <li>FitReferenceSize
  <li>ExplicitScale
  <li>DpiScale
</ul>

<p>See the <?php api_link('TUIScaling', 'CastleUIControls.html#TUIScaling'); ?>
 documentation for their description.

<?php echo $toc->html_section(); ?>

<p>Loading the <code>CastleSettings.xml</code> also sets the
<?php api_link('Container.DefaultFont', 'CastleUIControls.TUIContainer.html#DefaultFont'); ?>.

<p>This controls the default font look (font file, font size)
for all user-interface controls.
Note that this is only a default, each control can still customize it
(using <?php api_link('TCastleUserInterfaceFont.CustomFont', 'CastleControls.TCastleUserInterfaceFont.html#CustomFont'); ?>,
<?php api_link('TCastleUserInterfaceFont.FontSize', 'CastleControls.TCastleUserInterfaceFont.html#FontSize'); ?>).

<p>An example code (only the <code>url</code> attribute is required):

<?php echo xml_full_highlight(
'<default_font
  url="castle-data:/MyFontFile.ttf"
  size="20"
  sizes_at_load="10 20 30"
  anti_aliased="true"
/>'); ?>

<p>In effect,
<?php api_link('Container.DefaultFont', 'CastleUIControls.TUIContainer.html#DefaultFont'); ?>
 will be set to a proper
<?php api_link('TCastleFont', 'CastleFonts.TCastleFont.html'); ?> instance.

<p>You can also provide separate font variants for <code>regular</code>,
<code>bold</code>, <code>italic</code> and <code>bold_italic</code> variants.
In this case, the <code>&lt;default_font&gt;</code> element is only a container
for the specification of font variants (do not use any attributes at <code>&lt;default_font&gt;</code> in this case).
Like this:

<?php echo xml_full_highlight(
'<default_font>
  <regular
    url="castle-data:/fonts/DejaVuSans.ttf"
    size="25"
  />
  <bold
    url="castle-data:/fonts/DejaVuSans-Bold.ttf"
    size="25"
  />
  <italic
    url="castle-data:/fonts/DejaVuSans-Italic.ttf"
    size="25"
  />
  <bold_italic
    url="castle-data:/fonts/DejaVuSans-BoldItalic.ttf"
    size="25"
  />
</default_font>'); ?>

<p>In effect,
<?php api_link('Container.DefaultFont', 'CastleUIControls.TUIContainer.html#DefaultFont'); ?>
 will be set to a proper
<?php api_link('TFontFamily', 'CastleFontFamily.TFontFamily.html'); ?> instance.
This is useful if you plan to use
<?php api_link('TCastleLabel', 'CastleControls.TCastleLabel.html'); ?>
 with <?php api_link('HTML markup', 'CastleControls.TCastleLabel.html#Html'); ?>,
this way bold and italic tags in HTML will work. You have to define at least the <code>regular</code>
font variant (others are optional).

<p>Specification of attributes available at the font element (either
<code>&lt;default_font&gt;</code>, or
<code>&lt;regular&gt;</code>,
<code>&lt;bold&gt;</code> etc.):

<dl>
  <dt><p><code>url</code> (required)

  <dd><p>URL pointing to a font file.
    Any format supported by the <a href="https://www.freetype.org/">FreeType library</a>
    is allowed, in particular OTF and TTF.
    This file will be loaded at runtime by the compiled application,
    so it should almost always be a URL leading <a href="manual_data_directory.php">to the application data</a>,
    or a relative URL (since the <code>CastleSettings.xml</code> should already be inside data).

  <dt><p><code>size</code> (default: 20)

  <dd><p>Determines the font size (<?php api_link('TCastleFont.Size', 'CastleFonts.TCastleFont.html#Size'); ?>),
    which determines how large the font is on the screen.

  <dt><p><code>size_at_load</code> (default: use <code>size</code> value)

  <dd><p>The font size used to create an internal texture
    with letters. By default it is equal to <code>size</code>,
    but it can be set to something larger to improve the quality of the font.
    This is useful if in your game you will often use this font
    with other sizes.
    (E.g. your controls leave <?php api_link('TCastleUserInterfaceFont.CustomFont', 'CastleControls.TCastleUserInterfaceFont.html#CustomFont'); ?> = nil,
    but often use large <?php api_link('TCastleUserInterfaceFont.FontSize', 'CastleControls.TCastleUserInterfaceFont.html#FontSize'); ?> or
    <?php api_link('TCastleUserInterfaceFont.FontScale', 'CastleControls.TCastleUserInterfaceFont.html#FontScale'); ?>).

    <p>This attribute is used only when the list <code>sizes_at_load</code> is not defined
    (see below).

    <p>Internally, one <code>TTextureFont</code> instance will be created,
    for given size.
    During rendering it will be scaled to the requested size on screen.
    If you try to render font much smaller or much larger than
    the loaded size, results may look aliased or blurry.

  <dt><p><code>sizes_at_load</code> (default: not used)

  <dd><p>If this attribute is defined, it should be a list of font sizes
    (integers separated by whitespace).
    The font sizes should represent a spectrum of sizes you plan
    to use in your application, e.g. <code>"10 15 20 25"</code>.

    <p>Internally, the font is loaded into a number of textures,
    one for each size, using <code>TCustomizedFont.Load</code> that creates
    a number of <code>TTextureFont</code> instances.
    When rendering using this font, the closest size will be used,
    and scaled to the requested size, to be rendered exactly at
    requested size.

    <p>This gives greatest font quality.

  <dt><p><code>anti_aliased</code> (default: true)

  <dd><p>This boolean attribute controls whether the font texture will be rendered
    with smooth scaling (bilinear filtering).
    Practically, you want to always leave it as <code>true</code>,
    especially if you use UI scaling or change font size at various UI controls.

  <dt><p><code>sample_text</code> (default: empty)

  <dd><p>By default, the font texture has only ASCII characters loaded.
    Use this attribute to specify additional characters to load.

  <dt><p><code>only_sample_text</code> (default: false)

  <dd><p>By default, the font texture includes the ASCII characters
    (regardless if they are listed, or not, in <code>sample_text</code>).
    Set this to <code>true</code> to <i>not</i> add the ASCII characters
    automatically.
    This way the font texture will only contain characters mentioned
    in <code>sample_text</code>, <code>sample_code</code>, <code>sample_get_text_mo</code>.

    <p>A typical use-case is when the font is used only to render
    specific characters, e.g. only digits, and you want to conserve font
    texture size.

  <dt><p><code>sample_code</code> (default: empty)

  <dd><p>A list (separated by whitespace or commas) of Unicode character numbers
    to include in the font texture.

    <p>By default, the font texture has only ASCII characters loaded.
    Use this (or some other <code>sample_xxx</code>) to extend the available character set.

  <dt><p><code>sample_get_text_mo</code> (default: empty)

  <dd><p>A list (separated by whitespace or commas) of GetText MO files.
    All the characters from the translated strings will be included in the font texture.
    This is useful to load all characters used by your translators.
    <a href="manual_text.php">See the manual chapter about fonts and localization (translation).</a>

    <p>By default, the font texture has only ASCII characters loaded.
    Use this (or some other <code>sample_xxx</code>) to extend the available character set.
</dl>

<?php echo $toc->html_section(); ?>

<p>When loading settings (at <?php api_link('LoadSettings call', 'CastleUIControls.TUIContainer.html#LoadSettings'); ?>),
we can load and keep in cache some resources.

<p>It may be useful to cache large images (used in UI or referenced
from TCastleScene) or audio files (referenced from TCastleScene).
Thanks to keeping them in the cache, any loading of them later in the game
will be instant.

<?php echo xml_full_highlight(
'<warmup_cache>
  <!-- Put in cache an image.
       Loading this image in UI (like TCastleImageControl, TCastleButton
       or anything else that uses TCastleImagePersistent cache)
       will be instant. -->
  <image_ui url="castle-data:/gui/image.png" />

  <!-- Put in cache a scene, along with all resources it references
       (textures, sounds).
       Loading this scene in TCastleScene will be much faster,
       since all resources will be already in cache. -->
  <scene url="castle-data:/gui/space_map/map_big_1.json" />
  <scene url="castle-data:/gui/space_map/map_big_2.json" />
</warmup_cache>'); ?>

<p>Note that if you load designed UI using
 <?php api_link('TUIState.DesignUrl', 'CastleUIState.TUIState.html#DesignUrl'); ?>
 then you have a more straightforward way: set
 <?php api_link('TUIState.DesignPreload', 'CastleUIState.TUIState.html#DesignPreload'); ?> to <code>true</code>.
 This will effectively cache all state resources, without the need to list them among
 <code>&lt;warmup_cache&gt;</code>.


<?php
manual_footer();
?>
